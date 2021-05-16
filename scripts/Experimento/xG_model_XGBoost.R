# Entrada: [Experimento] Modelo xG con XGBoost - Fecha - url

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse", "caret", "xgboost", "viridis", "doParallel", "pROC")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)


packages.git <- c("ggsoccer")
if(packages.git[[1]] %in% installed.packages() == F) devtools::install_github("torvaney/ggsoccer")
if(length(packages.git[!inst]) > 0) install.packages(packages.git[!inst])
lapply(packages.git, require, character.only=T)


# Cargamos los datos ------------------------------------------------------

tiros <- read.csv("data/tiros_final.csv",
                  encoding = "UTF-8")


# Preparamos los datos para el modelo -------------------------------------

## Dividimos en train y test
set.seed(1234)
index <- createDataPartition(tiros$result, p = 0.70, list = F)
tiros.train <- tiros[index,]
tiros.test <- tiros[-index,]

train.result <- as.numeric(as.character(tiros.train$result))
test.result <- as.numeric(as.character(tiros.test$result))
tiros.train$result <- NULL
tiros.test$result <- NULL

## Centrado y escalado de variables numéricas
# center.scale <- preProcess(tiros.train, method = c("center", "scale"))
# tiros.train <- predict(center.scale, newdata = tiros.train)
# center.scale <- preProcess(tiros.test, method = c("center", "scale"))
# tiros.test <- predict(center.scale, newdata = tiros.test)

## Variables dummy
dummy <- dummyVars(~ ., data = tiros.train, fullRank = T)
pred.dummy <- predict(dummy, tiros.train)
tiros.train <- as.data.frame(pred.dummy)

dummy <- dummyVars(~ ., data = tiros.test, fullRank = T)
pred.dummy <- predict(dummy, tiros.test)
tiros.test <- as.data.frame(pred.dummy)

## Eliminamos variables con varianza cero o cercana a cero
nzv <- nearZeroVar(tiros.train, saveMetrics = T)
nzv <- nzv %>% 
  filter(nzv == F)

tiros.train <- tiros.train %>% 
  select(rownames(nzv))
tiros.test <- tiros.test %>% 
  select(rownames(nzv))


# Hacemos el primer modelo ------------------------------------------------

## Creamos las matrices con los datos
tiros.train.matrix <- xgb.DMatrix(as.matrix(tiros.train), label = train.result)
tiros.test.matrix <- xgb.DMatrix(as.matrix(tiros.test))

## Paralelizamos el proceso de cración y entrenamiento del modelo
cluster <- makeCluster(detectCores())
registerDoParallel(cluster)

## Buscamos los parámetros óptimos
grid <- expand.grid(max_depth = 1:10,
                    eta = c(0.025, 0.05, 0.1, 0.3, 0.5),
                    colsample_bytree = c(0.5,0.8,1),
                    gamma = 0,
                    subsample = c(0.5, 0.8, 1),
                    min_child_weight = 1,
                    nrounds = 100)
control <- trainControl(method = "cv",
                        number = 10,
                        allowParallel = T)
(grid_model_depth <- train(x = tiros.train,
                           y = factor(train.result),
                           trControl = control,
                           tuneGrid = grid,
                           method = "xgbTree"))
### max_depth = 5
### eta = 0.05 and subsample = 0.8.
### col_sample_by_tree = 1
### subsample = 0.5

## Buscamos el número de iteraciones óptimo con los nuevos parámetros
params <-list(objetive = "binary:logistic",
              eval_metric = "auc",
              max_depth = 5, 
              eta = 0.05, 
              subsample = 0.5,
              colsample_bytree = 1) 
set.seed(1234)
xgboost.model <- xgb.cv(data = tiros.train.matrix,
                        label = train.result,
                        nrounds = 1000,
                        nfold = 10,
                        print_every_n = 10,
                        early_stopping_rounds = 50,
                        params = params)
### nrounds = 184

## Entrenamos el modelo
set.seed(1234)
xgboost.model.opt <- xgb.train(data = tiros.train.matrix,
                               label = train.result,
                               nrounds = xgboost.model$best_iteration,
                               nfold = 10,
                               print_every_n = 10,
                               early_stopping_rounds = 50,
                               params = params,
                               watchlist = list(validation1 = tiros.train.matrix))

## Vemos la importancia de las variables para el modelo
xgb.plot.importance(xgb.importance(model = xgboost.model.opt)) 

## Paramos el procesamiento en paralelo
stopCluster(cluster)

## Predecimos el xG
xG.prediction <- predict(xgboost.model.opt, tiros.test.matrix)

## Vemos el resultado sobre el campo
tiros.test.xg <- cbind(tiros[-index,], as.data.frame(xG.prediction))

ggplot(tiros.test.xg) +
  annotate_pitch(colour = "white",
                 fill = "springgreen4",
                 limits = F) +
  geom_point(aes(x = X, y = 100 - Y,
                 fill = xG.prediction,
                 colour = xG.prediction), 
              shape = 21,
              size = 1) +
  scale_fill_gradient(low = "blue", high = "yellow") +
  scale_colour_gradient(low = "blue", high = "yellow") +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-12, 112)) +
  ggtitle("Mapa de xG")

## Calculamos el AUC
auc(test.result, as.numeric(xG.prediction))

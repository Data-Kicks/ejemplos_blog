# Entrada: [Experimento] Modelo xG con XGBoost - Fecha - url

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse", "LearnGeom")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)

packages.git <- c("ggsoccer")
if(packages.git[[1]] %in% installed.packages() == F) devtools::install_github("torvaney/ggsoccer")
if(length(packages.git[!inst]) > 0) install.packages(packages.git[!inst])
lapply(packages.git, require, character.only=T)


# Cargamos los datos ------------------------------------------------------

tiros <- read.csv("data/US_shots_Total.csv",
                  encoding = "UTF-8")

str(tiros)
summary(tiros)


# Elegimos las columnas y filas que nos interesan ---------------------------------

tiros.filt <- tiros %>% 
  filter(situation == "OpenPlay") %>% 
  select(minute,
         result,
         X,
         Y,
         h_a,
         shotType,
         lastAction) 


# Ingeniería de Características -------------------------------------------

str(tiros.filt)
summary(tiros.filt)

## Convertimos coordenadas a 0-100 (campo Opta)
tiros.filt <- tiros.filt %>% 
  mutate(X = X * 100, Y = Y * 100)

## Convertimos result a gol(1) o no gol(0)
tiros.filt <- tiros.filt %>% 
  mutate(result = as.factor(ifelse(result == "Goal", "1", "0")))

## Calculamos distancia a gol
tiros.filt <- tiros.filt %>% 
  rowwise() %>% 
  mutate(distToGoal = round(dist(rbind(c(X, Y), c(100, 50))), 2))

## Calculamos ángulo de tiro
tiros.filt <- tiros.filt %>% 
  rowwise() %>% 
  mutate(angleToGoal = round(Angle(c(100, 55.8), c(X, Y), c(100, 44.2)), 
                             2))


# Visualizamos los tiros --------------------------------------------------

## Tiros
ggplot(tiros.filt) + 
  geom_bar(aes(x = result, fill = result)) +
  ggtitle("Resultado de los tiros")


## Goles
ggplot(tiros.filt %>% filter(result == "1")) +
  annotate_pitch(colour = "white",
                 fill = "springgreen4",
                 limits = F) +
  geom_point(aes(x = X, y = 100 - Y),
             fill = "yellow", 
             shape = 21,
             size = 2) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-12, 112)) +
  ggtitle("Mapa de goles")


# Guardamos el dataset limpio ---------------------------------------------

write.csv(tiros.filt, 
          "data/tiros_final.csv",
          row.names = F)

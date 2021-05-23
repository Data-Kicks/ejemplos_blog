# Entrada: [Herramienta] YouTube Coder de FC Python - 17/05/2021 - https://data-kicks.com/index.php/2021/05/23/herramienta-youtube-coder-de-fc-pytho/

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)


packages.git <- c("FC.rSTATS")
if(packages.git[[1]] %in% installed.packages() == F) devtools::install_github("FCrSTATS/fc.rstats")
if(length(packages.git[!inst]) > 0) install.packages(packages.git[!inst])
lapply(packages.git, require, character.only=T)


# Cargamos los datos ------------------------------------------------------

tiros <- read.csv("data/eventsPonDep.csv",
                  encoding = "UTF-8")


# Selccionamos las columnas que necesitamos y las preparamos --------------

tiros.prep <- tiros %>% 
  mutate(Result = ifelse(Event == "Gol", "Gol", "No Gol")) %>% 
  select(Team, Result, X, Y)


# Creamos el mapa de tiros ------------------------------------------------

create_OPTA_pitch(grass_colour = "#39b54a",
                  background_colour = "#39b54a",
                  line_colour = "white",
                  goaltype = "box",
                  middlethird = F) +
  geom_point(data = tiros.prep,
             aes(x = X,
                 y = 100 - Y,
                 shape = Result,
                 color = Team,
                 fill = Team),
             size = 5) +
  scale_alpha_discrete(range = c(1, 0.4)) +
  labs(title = "Pontevedra - Deportivo 2020-2021",
       subtitle = "Mapa de tiros")

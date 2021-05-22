# Entrada: [Visualización] Mapa de tiros - 22/05/2021 - https://data-kicks.com/index.php/2021/05/22/visualizacion-mapa-de-tiros/

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)


packages.git <- c("FC.rSTATS", "understatr")
if(packages.git[[1]] %in% installed.packages() == F) devtools::install_github("FCrSTATS/fc.rstats")
if(packages.git[[2]] %in% installed.packages() == F) devtools::install_github("ewenme/understatr")
if(length(packages.git[!inst]) > 0) install.packages(packages.git[!inst])
lapply(packages.git, require, character.only=T)


# Descargamos los datos de Understat

## Descargamos los datos del Alavés
alaves.data = get_team_players_stats("Alaves", 2020)

## Descargamos los datos de Joselu de esta temporada
joselu.tiros <- get_player_shots(866) %>% 
  filter(year == 2020)


# Seleccionamos las filas que nos interesan

joselu.tiros.filt <- joselu.tiros %>% 
  select(X, Y, xG, result, situation, shotType)


# Añadimos campos nuevos

joselu.tiros.prep <- joselu.tiros.filt %>% 
  mutate(is.goal = ifelse(result == "Goal", result, "No Goal"))


# Creamos el mapa de disparos

create_OPTA_pitch(grass_colour = "#39b54a",
                           background_colour = "#39b54a",
                           line_colour = "white",
                           goaltype = "box",
                           middlethird = F) +
  geom_point(data = joselu.tiros.prep,
             aes(x = X * 100,
                 y = 100 - (Y * 100),
                 size = xG,
                 alpha = is.goal),
             color = "#0233a0",
             fill = "#0233a0",
             shape = 21) +
  scale_alpha_discrete(range = c(1, 0.4)) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-12, 112)) +
  ggtitle("Joselu - Mapa de tiros 2020-2021")

## Añadimos símbolos según la variable result
create_OPTA_pitch(grass_colour = "#39b54a",
                  background_colour = "#39b54a",
                  line_colour = "white",
                  goaltype = "box",
                  middlethird = F) +
  geom_point(data = joselu.tiros.prep,
             aes(x = X * 100,
                 y = 100 - (Y * 100),
                 size = xG,
                 alpha = is.goal,
                 shape = result),
             color = "#0233a0",
             fill = "#0233a0") +
  scale_alpha_discrete(range = c(1, 0.4)) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-12, 112)) +
  ggtitle("Joselu - Mapa de tiros 2020-2021")

## Filtramos sólo los tiros libres directos
create_OPTA_pitch(grass_colour = "#39b54a",
                  background_colour = "#39b54a",
                  line_colour = "white",
                  goaltype = "box",
                  middlethird = F) +
  geom_point(data = joselu.tiros.prep %>% 
               filter(situation == "DirectFreekick"),
             aes(x = X * 100,
                 y = 100 - (Y * 100),
                 size = xG,
                 alpha = is.goal),
             color = "#0233a0",
             fill = "#0233a0",
             shape = 21) +
  scale_alpha_discrete(range = c(1, 0.4)) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-12, 112)) +
  ggtitle("Joselu - Mapa de tiros 2020-2021")

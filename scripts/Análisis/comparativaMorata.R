# Entrada: [Análisis] Comparando a Morata con otros delanteros - 12/07/2021 - https://data-kicks.com/index.php/2021/07/12/analisis-comparando-a-morata-con-otros-delanteros/

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse", "datapasta", "ggthemes", "Cairo")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)


packages.git <- c("understatr")
if(packages.git[[1]] %in% installed.packages() == F) devtools::install_github("ewenme/understatr")
if(length(packages.git[!inst]) > 0) install.packages(packages.git[!inst])
lapply(packages.git, require, character.only=T)


# Descargamos los datos de Understat

## Descargamos los datos de Morata
morata.minutes <- data.frame(player_id = 1804,
                             season = c(2020, 2019, 2018,
                                        2018, 2017, 2016),
                             min = c(2019L, 2120L, 1094L,
                                     951L, 2069L, 1326L)) %>% 
  mutate(N90 = round(min/90, 2)) %>%
  select(player_id, season, N90)
  
morata.tiros <- get_player_shots(1804)

## Descargamos los datos de Kane
kane.minutes <- data.frame(player_id = 647,
                           season = c(2020, 2019, 2018,
                                     2017, 2016),
                           min = c(3097L, 2595L, 2437L, 
                                  3094L, 2556L)) %>% 
  mutate(N90 = round(min/90, 2)) %>%
  select(player_id, season, N90)

kane.tiros <- get_player_shots(647)

## Descargamos los datos de Lukaku
lukaku.minutes <- data.frame(player_id = 594,
                             season = c(2020, 2019, 2018,
                                        2017, 2016),
                             min = c(2885L, 2985L, 2113L,
                                     2869L, 3271L)) %>%
  mutate(N90 = round(min/90, 2)) %>%
  select(player_id, season, N90)

lukaku.tiros <- get_player_shots(594)


# Creamos el dataset final
total.minutes <- morata.minutes %>% 
  union(kane.minutes) %>% 
  union(lukaku.minutes) %>% 
  group_by(player_id, season) %>% 
  summarise(N90 = sum(N90))

total.shot.data <- morata.tiros %>% 
  union(kane.tiros) %>% 
  union(lukaku.tiros) %>% 
  filter(year > 2015 & situation != "Penalty") %>%
  mutate(is.goal = if_else(result == "Goal", 1, 0),
         good.chance = if_else(xG > 0.4, 1, 0),
         good.chance.scored = if_else(is.goal == 1 & good.chance == 1, 1, 0),
         shots = 1) %>% 
  select(year, player_id, player, xG, is.goal, good.chance, good.chance.scored, shots) %>%
  group_by(year, player) %>% 
  summarise(player_id = first(player_id),
            xG = round(sum(xG), 2),
            goals = sum(is.goal),
            good.chances = sum(good.chance),
            good.chances.scored = sum(good.chance.scored),
            total.shots = sum(shots)) %>% 
  left_join(total.minutes, by = c("year" = "season", "player_id" = "player_id")) %>% 
  mutate(goals.xG = goals - xG,
         goals90 = round(goals/N90, 2),
         xG90 = round(xG/N90, 2),
         total.shots90 = round(total.shots/N90, 2),
         shots.goal = round(total.shots/goals, 2),
         xG.Shot = round(xG/total.shots, 2),
         good.chances.scored.per = round(good.chances.scored/good.chances * 100), 2) %>% 
  select(year, player, N90,
         total.shots, total.shots90, 
         goals, goals90, shots.goal, 
         xG, xG90, xG.Shot, goals.xG, 
         good.chances, good.chances.scored, good.chances.scored.per)


# Visualizamos

## Goles por temporada
ggplot(total.shot.data) +
  geom_line(aes(year, goals, color = player),
            size = 1) +
  labs(title = "Goles por temporada",
       subtitle = "En liga sin contar penaltis",
       color = "Jugador") +
  xlab("Temporada") +
  ylab("Goles") +
  scale_color_viridis_d(option = "D") +
  theme_fivethirtyeight()

ggsave("images/plots/golesTempMorata.png", type = "cairo" )

## Goles cada 90 minutos por temporada
ggplot(total.shot.data) +
  geom_line(aes(year, goals90, color = player),
            size = 1) +
  labs(title = "Goles cada 90 minutos por temporada",
       subtitle = "En liga sin contar penaltis",
       color = "Jugador") +
  xlab("Temporada") +
  ylab("Goles") +
  scale_color_viridis_d(option = "D") +
  theme_fivethirtyeight()

ggsave("images/plots/goles90Morata.png", type = "cairo" )

## Tiros por gol
ggplot(total.shot.data) +
  geom_line(aes(year, shots.goal, color = player),
            size = 1) +
  labs(title = "Tiros por gol por temporada",
       subtitle = "En liga sin contar penaltis",
       color = "Jugador") +
  xlab("Temporada") +
  ylab("Tiros") +
  scale_color_viridis_d(option = "D") +
  theme_fivethirtyeight()

ggsave("images/plots/tirosGolMorata.png", type = "cairo" )

## Diferencia entre goles y xG
ggplot(total.shot.data) +
  geom_line(aes(year, goals.xG, color = player),
            size = 1) +
  labs(title = "Diferencia entre goles y xG",
       subtitle = "En liga sin contar penaltis",
       color = "Jugador") +
  xlab("Temporada") +
  ylab("Goles - xG") +
  scale_color_viridis_d(option = "D") +
  theme_fivethirtyeight()

ggsave("images/plots/golesXGMorata.png", type = "cairo" )

## Porcentaje de ocasiones claras marcadas
ggplot(total.shot.data) +
  geom_line(aes(year, good.chances.scored.per, color = player),
            size = 1) +
  labs(title = "% Ocasiones claras marcadas (xG > 0.4)",
       subtitle = "En liga sin contar penaltis",
       color = "Jugador") +
  xlab("Temporada") +
  ylab("Porcentaje marcadas") +
  scale_color_viridis_d(option = "D") +
  theme_fivethirtyeight()

ggsave("images/plots/ocasionesPerMorata.png", type = "cairo" )

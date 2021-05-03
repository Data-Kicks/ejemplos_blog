# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse", "ggtext")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)


# Cargamos los datos ------------------------------------------------------

datos.laliga <- read.csv("data/datos_estandar_laliga.csv",
                         encoding = "UTF-8")


# Elegimos las columnas que nos interesan ---------------------------------

xg.goles.laliga <- datos.laliga %>% 
  select(Equipo, GF, xG) %>% 
  arrange(-(GF - xG))


# Creamos el lollipop plot ------------------------------------------------

ggplot(xg.goles.laliga) +
  geom_segment(aes(x = reorder(Equipo, -(GF - xG)), xend = reorder(Equipo, -(GF - xG)),
                   y = GF, yend = xG),
               size = 1,
               color = ifelse((xg.goles.laliga$GF - xg.goles.laliga$xG) > 0, 
                              rgb(0.2,0.7,0.1),
                              rgb(0.7,0.2,0.1))) +
  geom_point(aes(x = reorder(Equipo, -(GF - xG)), y = GF), 
             color = rgb(0.2,0.7,0.1), 
             size = 2) +
  geom_point(aes(x = reorder(Equipo, -(GF - xG)), y = xG), 
             color = rgb(0.7,0.2,0.1), 
             size = 2) +
  coord_flip() +
  labs(title = "<span style='color:#b3331a;'>xG</span> 
                vs 
                <span style='color:#33b31a;'>Goles</span>") +
  theme_fivethirtyeight() +
  theme(plot.title = element_markdown(lineheight = 1.1, hjust = 0.5)) +
  xlab("") +
  ylab("")


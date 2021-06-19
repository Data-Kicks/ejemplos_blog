# Entrada: [Métrica]  PSPSxG o goles esperados después del tiro - 18/06/2021 - https://data-kicks.com/index.php/2021/06/18/metrica-psPSxG-o-goles-esperados-despues-del-tiro/

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse", "ggthemes", "ggtext")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)


# Cargamos los datos de FBRef y seleccionamos los campos

la.liga.fbref.porteros.data <- read.csv("data/datos_portero_laliga.csv",
                               encoding = "UTF-8") %>% 
  mutate(Jugador = gsub("[\\].*", "", Jugador)) %>% 
  mutate(Jugador = if_else(Jugador == "Marc-André ter Stegen", "ter Stegen", Jugador)) %>%
  mutate(Jugador = if_else(Jugador == "Álvaro Fernández", "Álvaro Fdz", Jugador)) %>%
  filter(X90.s >= 15) %>% 
  select(Jugador, X90.s, GC, PSPSxG)


# Visualizamos

ggplot(la.liga.fbref.porteros.data, aes(x=PSPSxG, y=GC, fill=Jugador)) +
  geom_vline(xintercept = mean(la.liga.fbref.porteros.data$PSPSxG), linetype="dashed", alpha = 0.8, colour = "black") +
  geom_hline(yintercept = mean(la.liga.fbref.porteros.data$GC), linetype="dashed", alpha = 0.8, colour = "black") +
  annotate("rect", xmin = -Inf, xmax = mean(la.liga.fbref.porteros.data$PSPSxG), ymin = -Inf, ymax = mean(la.liga.fbref.porteros.data$GC), fill = "chartreuse3", alpha = 0.4) +
  annotate("rect",xmin = -Inf, xmax = mean(la.liga.fbref.porteros.data$PSPSxG), ymin = mean(la.liga.fbref.porteros.data$GC), ymax = Inf, fill = "brown2", alpha = 0.2) +
  annotate("rect",xmin = mean(la.liga.fbref.porteros.data$PSPSxG), xmax = Inf, ymin = -Inf, ymax = mean(la.liga.fbref.porteros.data$GC), fill = "chartreuse3", alpha = 0.2) +
  annotate("rect",xmin = mean(la.liga.fbref.porteros.data$PSPSxG), xmax = Inf, ymin = mean(la.liga.fbref.porteros.data$GC), ymax = Inf, fill = "brown2", alpha = 0.4) +
  geom_point(alpha=0.7, shape=21, color="black", size=2) +
  geom_text(label=la.liga.fbref.porteros.data$Jugador, 
            nudge_x = 0, nudge_y = 0.5, 
            check_overlap = F,
            size = 3) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="D") +
  theme_ipsum() +
  ggtitle("GC y PSPSxG por portero") +
  ylab("GC") +
  xlab("PSPSxG") +
  theme(legend.position = "none")

ggplot(la.liga.fbref.porteros.data) +
  geom_segment(aes(x = reorder(Jugador, (PSxG - GC)), xend = reorder(Jugador, (PSxG - GC)),
                   y = GC, yend = PSxG),
               size = 1,
               color = ifelse((la.liga.fbref.porteros.data$GC - la.liga.fbref.porteros.data$PSxG) > 0, 
                              rgb(0.7,0.2,0.1),
                              rgb(0.2,0.7,0.1))) +
  geom_point(aes(x = reorder(Jugador, (PSxG - GC)), y = GC), 
             color = rgb(0.7,0.2,0.1), 
             size = 2) +
  geom_point(aes(x = reorder(Jugador, (PSxG - GC)), y = PSxG), 
             color = rgb(0.2,0.7,0.1), 
             size = 2) +
  coord_flip() +
  labs(title = "<span style='color:#33b31a;'>PSxG</span> 
                vs 
                <span style='color:#b3331a;'>Goles encajados</span>") +
  theme_fivethirtyeight() +
  theme(plot.title = element_markdown(lineheight = 1.1, hjust = 0.5)) +
  xlab("") +
  ylab("")

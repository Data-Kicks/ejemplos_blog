# Entrada: [Visualización] Profundizando en el PPDA - 06/06/2021 - https://data-kicks.com/index.php/2021/06/06/visualizacion-profundizando-en-el-ppda/

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse", "datapasta", "hrbrthemes", "viridis", "plotly")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)


# Descargamos los datos de Understat

## Copiamos los datos generales de La Liga con datapasta y seleccionamos los campos
la.liga.understat.data <- tibble::tribble(
                                          ~Team,   ~PPDA,  ~ODC,
                                 "Atletico Madrid", 10.32, 165L,
                                     "Real Madrid", 10.14, 171L,
                                       "Barcelona",  9.51, 166L,
                                         "Sevilla",  8.15, 139L,
                                   "Real Sociedad",  8.06, 160L,
                                           "Betis",  9.12, 180L,
                                      "Villarreal",  9.85, 182L,
                                      "Celta Vigo",   7.4, 187L,
                                   "Athletic Club",  9.75, 197L,
                                         "Granada", 10.74, 234L,
                                         "Osasuna", 10.91, 243L,
                                           "Cadiz", 16.57, 274L,
                                        "Valencia", 10.94, 262L,
                                         "Levante",  9.47, 245L,
                                          "Getafe",  8.81, 155L,
                                          "Alaves", 12.25, 212L,
                                           "Elche", 13.23, 280L,
                                          "Huesca", 11.32, 215L,
                                      "Valladolid", 10.57, 243L,
                                           "Eibar",  8.99, 156L
                                 ) %>% 
  select(Team, PPDA, ODC)


# Cargamos los datos deFBRef y seleccionamos los campos

la.liga.fbref.data <- read.csv("data/datos_def_laliga.csv",
                               encoding = "UTF-8") %>% 
  mutate(tackles = X3.º.cent. + X3.º.ataq., presion = X3.º.cent..1 + X3.º.ataq..1) %>% 
  select(Equipo, tackles, presion)

la.liga.data <- la.liga.understat.data %>% 
  left_join(la.liga.fbref.data, by = c("Team" = "Equipo"))


# Realizamos las visualizaciones

## ODC - PPDA
ggplot(la.liga.data, aes(x=PPDA, y=ODC, fill=Team)) +
  geom_vline(xintercept = median(la.liga.data$PPDA), linetype="dashed", alpha = 0.8, colour = "black") +
  geom_hline(yintercept = mean(la.liga.data$ODC), linetype="dashed", alpha = 0.8, colour = "black") +
  annotate("rect", xmin = -Inf, xmax = median(la.liga.data$PPDA), ymin = -Inf, ymax = mean(la.liga.data$ODC), fill = "chartreuse3", alpha = 0.4) +
  annotate("rect",xmin = -Inf, xmax = median(la.liga.data$PPDA), ymin = mean(la.liga.data$ODC), ymax = Inf, fill = "brown2", alpha = 0.2) +
  annotate("rect",xmin = median(la.liga.data$PPDA), xmax = Inf, ymin = -Inf, ymax = mean(la.liga.data$ODC), fill = "chartreuse3", alpha = 0.2) +
  annotate("rect",xmin = median(la.liga.data$PPDA), xmax = Inf, ymin = mean(la.liga.data$ODC), ymax = Inf, fill = "brown2", alpha = 0.4) +
  geom_point(alpha=0.7, shape=21, color="black", size=2) +
  geom_text(label=la.liga.data$Team, 
            nudge_x = 0, nudge_y = 2.7, 
            check_overlap = F,
            size = 3) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="D") +
  theme_ipsum() +
  ggtitle("ODC y PPDA por equipo") +
  ylab("ODC") +
  xlab("PPDA") +
  theme(legend.position = "none")

## Presión - PPDA
ggplot(la.liga.data, aes(x=PPDA, y=presion, fill=Team)) +
  geom_vline(xintercept = median(la.liga.data$PPDA), linetype="dashed", alpha = 0.8, colour = "black") +
  geom_hline(yintercept = median(la.liga.data$presion), linetype="dashed", alpha = 0.8, colour = "black") +
  annotate("rect", xmin = -Inf, xmax = median(la.liga.data$PPDA), ymin = -Inf, ymax = median(la.liga.data$presion), fill = "chartreuse3", alpha = 0.4) +
  annotate("rect",xmin = -Inf, xmax = median(la.liga.data$PPDA), ymin = median(la.liga.data$presion), ymax = Inf, fill = "chartreuse3", alpha = 0.2) +
  annotate("rect",xmin = median(la.liga.data$PPDA), xmax = Inf, ymin = -Inf, ymax = median(la.liga.data$presion), fill = "deepskyblue4", alpha = 0.2) +
  annotate("rect",xmin = median(la.liga.data$PPDA), xmax = Inf, ymin = median(la.liga.data$presion), ymax = Inf, fill = "deepskyblue4", alpha = 0.4) +
  geom_point(alpha=0.7, shape=21, color="black", size=2) +
  geom_text(label=la.liga.data$Team, 
            nudge_x = 0, nudge_y = 0.9, 
            check_overlap = F,
            size = 3) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="D") +
  theme_ipsum() +
  ggtitle("Presión en los 2 últimos tercios y PPDA por equipo") +
  ylab("Presión") +
  xlab("PPDA") +
  theme(legend.position = "none")

## Tackles - PPDA
ggplot(la.liga.data, aes(x=PPDA, y=tackles, fill=Team)) +
  geom_vline(xintercept = median(la.liga.data$PPDA), linetype="dashed", alpha = 0.8, colour = "black") +
  geom_hline(yintercept = mean(la.liga.data$tackles), linetype="dashed", alpha = 0.8, colour = "black") +
  annotate("rect", xmin = -Inf, xmax = median(la.liga.data$PPDA), ymin = -Inf, ymax = mean(la.liga.data$tackles), fill = "chartreuse3", alpha = 0.2) +
  annotate("rect",xmin = -Inf, xmax = median(la.liga.data$PPDA), ymin = mean(la.liga.data$tackles), ymax = Inf, fill = "chartreuse3", alpha = 0.4) +
  annotate("rect",xmin = median(la.liga.data$PPDA), xmax = Inf, ymin = -Inf, ymax = mean(la.liga.data$tackles), fill = "deepskyblue4", alpha = 0.2) +
  annotate("rect",xmin = median(la.liga.data$PPDA), xmax = Inf, ymin = mean(la.liga.data$tackles), ymax = Inf, fill = "deepskyblue4", alpha = 0.4) +
  geom_point(alpha=0.7, shape=21, color="black", size=2) +
  geom_text(label=la.liga.data$Team, 
            nudge_x = 0, nudge_y = 0.05, 
            check_overlap = F,
            size = 3) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="D") +
  theme_ipsum() +
  ggtitle("Tackles en los 2 últimos tercios y PPDA por equipo") +
  ylab("Tackles") +
  xlab("PPDA") +
  theme(legend.position = "none")

# Entrada: [Visualización] Comparando jugadores con el gráfico de radar - 19/06/2021 - https://data-kicks.com/index.php/2021/06/19/visualizacion-comparando-jugadores-con-el-grafico-de-radar/

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse", "fmsb", "Cairo")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)


# Cargamos los datos de FBRef y seleccionamos los campos

bundesliga.fbref.porteros.est.data <- read.csv("data/datos_portero_est_bundesliga.csv",
                                        encoding = "UTF-8") %>% 
  mutate(Player = gsub("[\\].*", "", Player))

bundesliga.fbref.porteros.adv.data <- read.csv("data/datos_portero_adv_bundesliga.csv",
                                               encoding = "UTF-8") %>% 
  mutate(Player = gsub("[\\].*", "", Player))

bundesliga.fbref.porteros.data <- bundesliga.fbref.porteros.est.data %>% 
  left_join(bundesliga.fbref.porteros.adv.data, by = "Player") %>% 
  select(Rk.x,
         Player,
         Complete.Matches = X90s.x,
         Goals.Against = GA,
         PSxG,
         "PSxg+-" = PSxG...,
         Saves.Per = Save.,
         Penalties.Stp.Per = Save..1,
         Clean.Sheets.Per = CS.,
         Cross.Stp.Per = Stp.,
         OPA.Def.Act = X.OPA)

bundesliga.fbref.porteros.data.max <- bundesliga.fbref.porteros.data %>%
  filter(Complete.Matches >= 15) %>% 
  summarise(Goals.Against = max(Goals.Against),
            PSxG = max(PSxG),
            "PSxg+-" = max(`PSxg+-`),
            Saves.Per = max(Saves.Per),
            Penalties.Stp.Per = max(Penalties.Stp.Per),
            Clean.Sheets.Per = max(Clean.Sheets.Per),
            Cross.Stp.Per = max(Cross.Stp.Per),
            OPA.Def.Act = max(OPA.Def.Act))

bundesliga.fbref.porteros.data.min <- bundesliga.fbref.porteros.data %>%
  filter(Complete.Matches >= 15) %>% 
  summarise(Goals.Against = min(Goals.Against),
            PSxG = min(PSxG),
            "PSxg+-" = min(`PSxg+-`),
            Saves.Per = min(Saves.Per),
            Penalties.Stp.Per = min(Penalties.Stp.Per),
            Clean.Sheets.Per = min(Clean.Sheets.Per),
            Cross.Stp.Per = min(Cross.Stp.Per),
            OPA.Def.Act = min(OPA.Def.Act))

rownames(bundesliga.fbref.porteros.data) <- bundesliga.fbref.porteros.data$Player
bundesliga.fbref.porteros.data <- bundesliga.fbref.porteros.data %>%
  filter(Rk.x == 14 | Rk.x == 32) %>%
  select(-c(Rk.x, Player, Complete.Matches)) %>% 
  arrange(desc(Goals.Against))


# Función para crear los gráficos de radar (fmsb)

radar.plot <- function(player.data, max, min, plot.title){
  player.data <- rbind(max, min, player.data)
  colors_border=c(rgb(0.51,0.55,0.55,0.9), rgb(0.8,0.15,0.15,0.9), rgb(0,0.39,0))
  colors_in=c(rgb(0.51,0.55,0.55,0.4), rgb(0.8,0.15,0.15,0.4), rgb(0,0.39,0))
  
  radarchart( player.data, axistype=0,
              pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
              cglcol="black", cglty=1, cglwd=0.8,
              vlcex=1, title = plot.title
  )
  
  legend(x = 1, y = 1.4, y.intersp=0.6, legend = rownames(player.data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , 
         text.col = "black", cex=1, pt.cex=2.5)
}


#CairoPNG("images/plots/graficoRadarFMSBCairo.png", bg="transparent")
radar.plot(bundesliga.fbref.porteros.data, 
           bundesliga.fbref.porteros.data.max, 
           bundesliga.fbref.porteros.data.min,
           "Hrádecký vs Sommer")  




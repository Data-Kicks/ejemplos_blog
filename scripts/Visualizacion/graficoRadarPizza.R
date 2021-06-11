# Entrada: [Visualización] Evaluando jugadores con radar de percentiles o gráfico de pizza - 11/06/2021 - https://data-kicks.com/index.php/2021/06/11/visualizacion-evaluando-jugadores-con/

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)

packages.git <- c("worldfootballR")
if(packages.git[[1]] %in% installed.packages() == F) devtools::install_github("JaseZiv/worldfootballR")
if(length(packages.git[!inst]) > 0) install.packages(packages.git[!inst])
lapply(packages.git, require, character.only=T)


# Cargamos los datos de los jugadores
soyuncu.data <- fb_player_scouting_report("https://fbref.com/en/jugadores/21166ff4/Caglar-Soyuncu")
maehle.data <- fb_player_scouting_report("https://fbref.com/en/jugadores/c459a650/Joakim-Maehle")


pizza.plot <- function(player.data, player.pos, bar.color){
  temp <- (360 /(length(player.data$player_name)) / 2)
  myAng <- seq(-temp, -360 + temp, length.out = length(player.data$player_name))
  ang<-ifelse(myAng < -90, myAng + 180, myAng)
  ang<-ifelse(ang < -90, ang + 180, ang) 
  
  player.data$Statistic <- gsub(" ", "\n", player.data$Statistic)
  
  pizza <- ggplot(player.data, aes(Statistic, Percentile)) +                      
              geom_bar(aes(y = 100), fill = "darkgreen", stat = "identity", width = 1,
                       colour = "white", alpha = 1) +                                                                          
              geom_bar(stat = "identity", width = 1, fill = bar.color, colour = "white") +   
              geom_hline(yintercept = 25, colour = "white", alpha = 0.5) +
              geom_hline(yintercept = 50, colour = "white", alpha = 0.5) +
              geom_hline(yintercept = 75, colour = "white", alpha = 0.5) + 
              geom_hline(yintercept = 100, colour = "white", alpha = 0.5) + 
              coord_polar() +                                                                     
              geom_label(aes(label = Percentile), fill = bar.color, size = 4, 
                         color = "white", show.legend = FALSE) +     
              scale_y_continuous(limits = c(-10, 100)) +  
              labs(fill = "white", 
                   title = player.data$player_name,
                   subtitle = paste0(player.data$season, " | Percentile. Compared to ", 
                                     player.pos, " Top 5 competitions | Stats per 90")) +
              theme_minimal() +                                                                     
              theme(plot.background = element_rect(fill = "darkgreen", color = "darkgreen"),
                    panel.background = element_rect(fill = "darkgreen", color = "darkgreen"),
                    axis.title.y = element_blank(),
                    axis.title.x = element_blank(),
                    axis.text.y = element_blank(),
                    plot.title = element_text(hjust = 0.5, size = 20, color = "white"),
                    axis.text.x = element_text(size = 12, angle = ang, color = "white"),
                    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "white"),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    plot.margin = margin(5, 2, 2, 2))
  return(pizza)
}


# Creamos las visualizaciones de los jugadores
pizza.plot(soyuncu.data, "CB", "#D20222")
ggsave("images/plots/soyuncuPizzaGG.png", type = "cairo" )

pizza.plot(maehle.data, "FB", "#D20222")
ggsave("images/plots/maehlePizzaGG.png", type = "cairo" )

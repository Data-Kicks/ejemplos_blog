# Entrada: [Métrica] xPoints - 08/10/2021 - https://data-kicks.com/index.php/2021/05/22/visualizacion-mapa-de-tiros/

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse", "httr", "ggtext")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)


# Descargamos los datos de Fotmob y los preparamos

response <- GET("https://www.fotmob.com/matchDetails?matchId=3714037&ccode3=ESP&refresh=true&includeBuzzTab=true")
content_parsed <- content(response, as = "parsed")

shots <- as.data.frame(do.call(rbind, content_parsed$content$shotmap$shots))


# Recalculamos los tiros de rechace

recalcular.rechaces <- function(tiros, lista.rechaces){
  tiros$rechace <- F
  tiros[lista.rechaces, ]$rechace <- T
  
  for(i in 1:nrow(tiros)){
    if(!tiros[i,]$rechace){
      cont <- i + 1
      xg.aux <- as.numeric(tiros[i,]$expectedGoals)
      for(j in 1:(nrow(tiros) - i)){
        if(cont <= nrow(tiros) & tiros[cont,]$rechace){
          xg.aux <- xg.aux + ((1 - xg.aux) * as.numeric(tiros[cont,]$expectedGoals))
          tiros[cont,]$expectedGoals <- 0
          cont <- cont + 1
        }
        else{
          break
        }
      }
      tiros[i,]$expectedGoals <- round(xg.aux, 4)
    }
  }
  
  tiros <- tiros %>% 
    filter(!rechace) %>% 
    select(-rechace)
  
  return(tiros)
}

shots <- recalcular.rechaces(shots, c(10,12,17))

real.madrid.shots <- shots %>% 
  filter(teamId == 8633) %>% 
  select(expectedGoals)

seriff.shots <- shots %>% 
  filter(teamId == 9729) %>% 
  select(expectedGoals)


# Función para realizar la simulación-------------------------------------------

get.simulations <- function(shots){
  simulation.goals <- data.frame()
  for(i in 1:nrow(shots)){
    set.seed(i)
    simulation.goals <- rbind(simulation.goals, rbinom(n = 10000, 
                                                       size = 1, 
                                                       prob = as.numeric(shots$expectedGoals[i])))
  }
  return(simulation.goals)
}


# Simulamos 10000 veces los tiros de cada equipo--------------------------------

real.madrid.simulation.goals <- data.frame(goles.real.madrid = colSums(get.simulations(real.madrid.shots)))
rownames(real.madrid.simulation.goals) <- NULL
sheriff.simulation.goals <- data.frame(goles.sheriff = colSums(get.simulations(seriff.shots)))
rownames(sheriff.simulation.goals) <- NULL

## Vemos los porcentajes de cada resultado y los xPoints de cada equipo---------

simulacion.partidos <- cbind(real.madrid.simulation.goals, sheriff.simulation.goals) %>% 
  mutate(local = ifelse(goles.real.madrid > goles.sheriff, 1, 0),
         empate = ifelse(goles.real.madrid == goles.sheriff, 1, 0),
         visitante = ifelse(goles.real.madrid < goles.sheriff, 1, 0)) %>% 
  summarise(total.local = sum(local), 
            total.empate = sum(empate), 
            total.visitante = sum(visitante)) %>% 
  mutate(c.local.per = (total.local/10000) * 100,
         b.empate.per = (total.empate/10000) * 100,
         a.visitante.per = (total.visitante/10000) * 100,
         b.local.xpoints = round((3 * (c.local.per/100)) + (1 * (b.empate.per/100)), 2),
         a.visitante.xpoints = round((3 * (a.visitante.per/100)) + (1 * (b.empate.per/100)), 2)) %>% 
  select(c.local.per, b.empate.per, a.visitante.per, b.local.xpoints, a.visitante.xpoints)

## Vemos el porcentaje de cada número de goles----------------------------------

probabilidad.goles.real.madrid <- real.madrid.simulation.goals %>% 
  mutate(t0 = ifelse(goles.real.madrid == 0, 1, 0),
         t1 = ifelse(goles.real.madrid == 1, 1, 0),
         t2 = ifelse(goles.real.madrid == 2, 1, 0),
         t3 = ifelse(goles.real.madrid == 3, 1, 0),
         t4 = ifelse(goles.real.madrid == 4, 1, 0),
         t5 = ifelse(goles.real.madrid == 5, 1, 0),
         t6 = ifelse(goles.real.madrid == 6, 1, 0)) %>% 
  summarise(t0 = sum(t0),
            t1 = sum(t1), 
            t2 = sum(t2), 
            t3 = sum(t3), 
            t4 = sum(t4), 
            t5 = sum(t5), 
            t6 = sum(t6)) %>% 
  mutate(p0 = (t0/10000) * 100,
         p1 = (t1/10000) * 100,
         p2 = (t2/10000) * 100,
         p3 = (t3/10000) * 100,
         p4 = (t4/10000) * 100,
         p5 = (t5/10000) * 100,
         p6 = (t6/10000) * 100) %>% 
  select(p0, p1, p2, p3, p4, p5, p6)

probabilidad.goles.sheriff <- sheriff.simulation.goals %>% 
  mutate(t0 = ifelse(goles.sheriff == 0, 1, 0),
         t1 = ifelse(goles.sheriff == 1, 1, 0),
         t2 = ifelse(goles.sheriff == 2, 1, 0),
         t3 = ifelse(goles.sheriff == 3, 1, 0),
         t4 = ifelse(goles.sheriff == 4, 1, 0),
         t5 = ifelse(goles.sheriff == 5, 1, 0),
         t6 = ifelse(goles.sheriff == 6, 1, 0)) %>% 
  summarise(t0 = sum(t0),
            t1 = sum(t1), 
            t2 = sum(t2), 
            t3 = sum(t3), 
            t4 = sum(t4), 
            t5 = sum(t5), 
            t6 = sum(t6)) %>% 
  mutate(p0 = (t0/10000) * 100,
         p1 = (t1/10000) * 100,
         p2 = (t2/10000) * 100,
         p3 = (t3/10000) * 100,
         p4 = (t4/10000) * 100,
         p5 = (t5/10000) * 100,
         p6 = (t6/10000) * 100) %>% 
  select(p0, p1, p2, p3, p4, p5, p6)


# Graficamos--------------------------------------------------------------------

simulacion.percentages <- simulacion.partidos %>% 
  select(c.local.per, b.empate.per, a.visitante.per) %>% 
  gather(stat, val)

simulacion.xpoints <- simulacion.partidos %>% 
  select(b.local.xpoints, a.visitante.xpoints) %>% 
  gather(stat, val)

simulacion.goles.local <- probabilidad.goles.real.madrid %>% 
  gather(stat, val)

simulacion.goles.visitante <- probabilidad.goles.sheriff %>% 
  gather(stat, val)

## Gráfico porcentaje de resultado y xPoints------------------------------------

(gg.per.xpoints <- ggplot(simulacion.percentages %>% mutate(plot = "xPoints"), aes(x = plot, y = val, fill = stat)) +
   geom_bar( stat = "identity", width = 0.6) +
   geom_text(aes(label = paste0(ifelse(stat == "c.local.per", "1\n", ifelse(stat == "a.visitante.per", "2\n", "X\n")), scales::percent(val / 100))), 
             position = position_stack(vjust = 0.5), size = 4.2) + 
   annotate("text", x = 1.4, 3.8, 
            label = paste0("xPoints: ", simulacion.xpoints[simulacion.xpoints$stat == "b.local.xpoints",]$val),
            color = "royalblue4", size = 6) +
   annotate("text", x = 1.34, 3.8, 
            label = paste0("xPoints: ", simulacion.xpoints[simulacion.xpoints$stat == "a.visitante.xpoints",]$val),
            color = "darkorange1", size = 6) +
   labs(title = "<span style='font-size:22pt'>
                   <span style='font-weight:bold; color:#27408B;'>Real Madrid 1</span> - <span style='font-weight:bold; color:#FF7F00;'>2 FC Sheriff</span>
                 </span>") +
   coord_flip() +
   theme_void() +
   theme(legend.position = "none",
         plot.title = element_markdown(hjust = 0.5, margin=margin(40,0,-50,0))) +
   scale_fill_manual(values = c("c.local.per" = "royalblue4", "a.visitante.per" = "darkorange1", "b.empate.per" = "ivory4")))

ggsave(paste0("images/plots/", "realMadridSheriffXPointsGG.png"), type = "cairo", width = 24.58, height = 4.896)

## Gráfico porcentaje de goles por equipo---------------------------------------

(gg.per.goles.local <- ggplot(simulacion.goles.local, aes(stat, val)) +
   geom_bar(color = ifelse(simulacion.goles.local$stat == "p1", "royalblue4", "ivory4"),
            fill = ifelse(simulacion.goles.local$stat == "p1", "royalblue4", "ivory4"), 
            stat = "identity")) +
  geom_text(aes(label = val), 
            nudge_y = 1, size = 4.2) +
  labs(title = "<span style='font-size:22pt; font-weight:bold; color:#27408B;'>Porcentaje de goles en la simulación - Real Madrid</span>") +
  theme_void() +
  theme(axis.text.x = element_text(margin = margin(-8,0,10,0)),
        plot.title = element_markdown(hjust = 0.5, margin=margin(10,0,10,0))) + 
  scale_x_discrete(labels=c("0","1","2","3","4","5","6"))

ggsave(paste0("images/plots/", "realMadridGoalsPercGG.png"), type = "cairo", width = 10.96, height = 7.875)

(gg.per.goles.visitante <- ggplot(simulacion.goles.visitante, aes(stat, val)) +
    geom_bar(color = ifelse(simulacion.goles.local$stat == "p2", "darkorange1", "ivory4"),
             fill = ifelse(simulacion.goles.local$stat == "p2", "darkorange1", "ivory4"), 
             stat = "identity")) +
  geom_text(aes(label = val), 
            nudge_y = 1, size = 4.2) +
  labs(title = "<span style='font-size:22pt; font-weight:bold; color:#FF7F00;'>Porcentaje de goles en la simulación - FC Sheriff</span>") +
  theme_void() +
  theme(axis.text.x = element_text(margin = margin(-8,0,10,0)),
        plot.title = element_markdown(hjust = 0.5, margin=margin(10,0,10,0))) + 
  scale_x_discrete(labels=c("0","1","2","3","4","5","6"))

ggsave(paste0("images/plots/", "sheriffGoalsPercGG.png"), type = "cairo", width = 10.96, height = 7.875)

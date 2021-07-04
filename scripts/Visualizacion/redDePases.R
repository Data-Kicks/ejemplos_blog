# Entrada: [Visualización] Creando Redes de Pase con ggplot2 - 04/07/2021 - https://data-kicks.com/index.php/2021/07/04/visualizacion-creando-redes-de-pase-con-ggplot2/

# Instalamos y cargamos los paquetes necesarios ---------------------------

packages.cran = c("tidyverse", "devtools", "xml2", "plotly", "Cairo")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)

packages.git <- c("SBpitch")
if(packages.git[[1]] %in% installed.packages() == F) install_github("FCrSTATS/SBpitch")
lapply(packages.git, require, character.only=T)


# Grafos con los pases del equipo en un partido ---------------------------

## Función para leer formato XML
getVar <- function(parent, tag, data){
  path <- paste("//", parent, "//", tag, sep = "")
  result <- xml_text(xml_find_all(data, path))
  return(result)
}

## Función para buscar el receptor del pase
findReciever <- function(pass.data, action){
  pass.data.complete <- pass.data %>% 
    mutate(receptor = "-")
  
  for(i in 1:nrow(pass.data.complete)){
    if(pass.data.complete[i, "accion"] == action){
      for(j in i:nrow(pass.data.complete)){
        if(pass.data.complete[j, "jugador"] != pass.data.complete[i, "jugador"]){
          pass.data.complete[i, "receptor"] <- pass.data.complete[j, "jugador"]
          break
        }
      }
    }
  }
  
  return(pass.data.complete)
}

## Función para calcular relaciones unidireccionales
findUniRelations <- function(pass.data){
  countAux <- c()
  for(i in 1:nrow(pass.data)){
    for(j in i:nrow(pass.data)){
      if(pass.data[i, ]$pasador == pass.data[j, ]$receptor & pass.data[j, ]$pasador == pass.data[i, ]$receptor){
        pass.data[i, ]$pases <- pass.data[i, ]$pases + pass.data[j, ]$pases
        countAux <- append(countAux, j)
      }
    }
  }
  pass.data <- pass.data[-countAux,]
  
  return(pass.data)
}

## Alavés - Valencia
alaves.valencia.pases.xml <- read_xml("data/Alaves_Valencia_20_21_LaLiga.xml")

alaves.valencia.pases.desc <- as.data.frame(cbind(tipo = getVar("label", "group", alaves.valencia.pases.xml),
                                                  texto = getVar("label", "text", alaves.valencia.pases.xml)),
                                            stringsAsFactors = F)

alaves.valencia.pases.jugadores <- data.frame("id" = as.numeric(getVar("instance", "ID", alaves.valencia.pases.xml)),
                                              "inicio" = as.numeric(getVar("instance", "start", alaves.valencia.pases.xml)),
                                              "fin" = as.numeric(getVar("instance", "end", alaves.valencia.pases.xml)),
                                              "jugador" = getVar("instance", "code", alaves.valencia.pases.xml),
                                              stringsAsFactors = F)

alaves.valencia.pases.df <- data.frame(cbind(alaves.valencia.pases.jugadores[alaves.valencia.pases.jugadores$jugador != "Empezar marca de tiempo",],
                                             "x" = as.numeric(getVar("instance", "pos_x", alaves.valencia.pases.xml)),
                                             "y" = as.numeric(getVar("instance", "pos_y", alaves.valencia.pases.xml)),
                                             "equipo" = alaves.valencia.pases.desc[alaves.valencia.pases.desc$tipo == "Equipo",]$texto,
                                             "accion" = alaves.valencia.pases.desc[alaves.valencia.pases.desc$tipo == "Accion",]$texto,
                                             "tiempo" = alaves.valencia.pases.desc[alaves.valencia.pases.desc$tipo == "Medio",]$texto),
                                       stringsAsFactors = F) %>% 
  arrange(inicio, fin) %>% 
  mutate(jugador = sub("...? ", "", jugador))

alaves.pases <- alaves.valencia.pases.df %>% 
  filter(equipo == "Deportivo Alaves" & (accion == "Pases precisos" | accion == "Ataques posicionales / elaborados")) %>% 
  select(inicio, fin, jugador, accion, x, y)

alaves.pases <- findReciever(alaves.pases, "Pases precisos") %>% 
  filter(accion == "Pases precisos")

alaves.posicion.jugadores <- alaves.pases %>% 
  filter(accion == "Pases precisos") %>%
  mutate(pases = 1) %>% 
  group_by(jugador) %>% 
  summarise(x = 0.95 * mean(x), y = 1.176 * mean(y), pases = sum(pases))

alaves.pases.relacion <- alaves.pases %>% 
  mutate(pases= 1) %>% 
  group_by(pasador = jugador, receptor) %>% 
  summarise(pases = sum(pases))

alaves.pases.relacion.uni <- findUniRelations(alaves.pases.relacion)

alaves.pases.grafo <- alaves.pases.relacion.uni %>% 
  left_join(alaves.posicion.jugadores, by = c("pasador" = "jugador")) %>% 
  left_join(alaves.posicion.jugadores, by = c("receptor" = "jugador")) %>% 
  rename(jugador.1 = pasador,
         jugador.2 = receptor,
         pases.relacion = pases.x,
         jug.1.x = x.x,
         jug.1.y = y.x,
         jug.1.tot.pases = pases.y,
         jug.2.x = x.y,
         jug.2.y = y.y,
         jug.2.tot.pases = pases)

alaves.grafo <- create_Pitch(goaltype = "box",
                             grass_colour = "#3CB44C", 
                             line_colour =  "white", 
                             background_colour = "#3CB44C") +
  geom_segment(data = alaves.pases.grafo %>% filter(pases.relacion > 3), 
               aes(jug.1.x, jug.1.y, xend = jug.2.x, yend = jug.2.y, 
                   size = pases.relacion/3,
                   alpha = pases.relacion/3,
                   color = I("blue4"))) +
  geom_point(data = alaves.posicion.jugadores %>% filter(pases > 3),
             aes(x, y, size = pases/3, fill = "blue", color = "white")) +
  geom_text(data = alaves.posicion.jugadores %>% filter(pases > 3), 
            aes(x, y, label = paste0('<b>',jugador, '<b>'), 
                color = "black", size = 3)) +
  scale_size_identity() +
  scale_fill_manual(values = c("blue")) +
  theme(legend.position="none", plot.margin = margin(1.5, 0, 0, 0, "cm")) +
  guides(size = F,
         fill = F,
         color = F,
         alpha = F)

ggplotly(alaves.grafo) %>% 
  style(hoverinfo = "none",
        textposition = "right") %>%
  layout(title = list(text = paste0('<br>',
                                    "Deportivo Alavés - Valencia CF",
                                    '<br>'))) %>% 
  plotly::config(displayModeBar = F)

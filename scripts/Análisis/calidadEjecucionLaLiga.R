# Entrada: [Análisis] Calidad de ejecución de tiro en La Liga  - 31/12/2021 - https://data-kicks.com/index.php/2021/12/31/analisis-calidad-de-ejecucion-de-tiro-en-la-liga/


# Instalamos y cargamos los paquetes necesarios --------------------------------

packages.cran = c("tidyverse", "jsonlite", "gt", "paletteer")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)

packages.git <- c("gtExtras")
if(packages.git[[1]] %in% installed.packages() == F) devtools::install_github("jthomasmock/gtExtras")
if(length(packages.git[!inst]) > 0) install.packages(packages.git[!inst])
lapply(packages.git, require, character.only=T)


# Cargamos los datos de The Analyst --------------------------------------------

json_file <- "https://dataviz.theanalyst.com/season-reviews/2021/player_xg_23_2021.json"
xg.laliga.data <- fromJSON(json_file)


# Preparamos los datos ---------------------------------------------------------

xg.laliga.data.prep <- xg.laliga.data %>%
  mutate(Goles = round(nineties * goals_per_90, 0),
         total.shots = round(total_xG/xG_per_shot, 0),
         total.xGOT = round(nineties * xGOT_per_90, 2),
         xGOT.per.shot = round((nineties * xGOT_per_90)/total.shots, 2),
         shot.execution.quality = xGOT.per.shot-xG_per_shot) %>% 
  arrange(-xG_per_90) %>% 
  select(Equipo = team_badge,
         Jugador = last_name, 
         Minutos = mins_played,
         "Tiros Total" = total.shots,
         Goles,
         "xG Total" = total_xG,
         "xGOT Total" = total.xGOT,
         "xG/90" = xG_per_90, 
         "xGOT/90" = xGOT_per_90,
         "xG/tiro" = xG_per_shot, 
         "xGOT/tiro" = xGOT.per.shot, 
         "(xGOT-xG)/Tiro" = shot.execution.quality)


# Visualizamos con tablas ------------------------------------------------------

## Tabla general de xG y xGOT --------------------------------------------------
xg.laliga.data.prep %>% 
  select(-c("xG/tiro", "xGOT/tiro", "(xGOT-xG)/Tiro")) %>% 
  gt() %>% 
  tab_header(
    title = md("**xG y xGOT - La Liga 2021/2022 - Parón invernal**"),
    subtitle = html("<em>Jugadores con mayor xG/90 con al menos 540 minutos disputados</em>")
  ) %>% 
  tab_source_note(
    source_note = html("<em><b>Datos:</b> Stats Perform - The Analyst | <b>Tabla:</b> @DataKicks <br> <b>Inspiración:</b> @markrstats/@DanielKatona17</em>")
  ) %>% 
  tab_options(
    table.border.top.color = "#39b54a",
    table.border.bottom.color = "#39b54a",
    column_labels.border.bottom.color = "#39b54a"
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "grey", alpha = 0.2),
      cell_text(font = google_font("Fira Mono")),
      cell_borders(
        side = c("left", "right"), 
        color = "#39b54a",
        weight = px(2)
      )
    ),
    locations = cells_body(
      columns = c("Jugador", "Equipo")
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = google_font("Calibri"))
    ),
    locations = cells_body(everything())
  ) %>% 
  cols_align(
    align = "center",
    columns = Equipo
  ) %>% 
  data_color(
    columns = c("xG Total"),
    colors = scales::col_numeric(
      palette = c(
        "tomato2", "seagreen"),
      domain = c(2.55, 10.1))
  ) %>% 
  data_color(
    columns = c("xGOT Total"),
    colors = scales::col_numeric(
      palette= paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = c(1.13, 10.4))
  ) %>% 
  gt_img_rows(Equipo) %>% 
  gtsave(
    "xg_xgot_laliga.png", expand = 10,
    path = "images/plots/"
  )

## Tabla )
xg.laliga.data.prep %>% 
  select(c(Equipo,
         Jugador,
         "xG/tiro",
         "xGOT/tiro",
         "(xGOT-xG)/Tiro")) %>% 
  arrange(-.[[5]]) %>% 
  gt() %>% 
  tab_header(
    title = md("**Calidad de ejecución de tiro - La Liga 2021/2022 - Parón invernal**"),
    subtitle = html("<em>Jugadores con mayor xG/90 y al menos 540 minutos disputados</em>")
  ) %>% 
  tab_source_note(
    source_note = html("<em><b>Datos:</b> Stats Perform - The Analyst | <b>Tabla:</b> @DataKicks <br> <b>Inspiración:</b> @markrstats/@DanielKatona17</em>")
  ) %>% 
  tab_options(
    table.border.top.color = "#39b54a",
    table.border.bottom.color = "#39b54a",
    column_labels.border.bottom.color = "#39b54a"
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = google_font("Calibri"))
    ),
    locations = cells_body(everything())
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "grey", alpha = 0.2),
      cell_borders(
        side = c("left", "right"), 
        color = "#39b54a",
        weight = px(2)
      )
    ),
    locations = cells_body(
      columns = c("Jugador", "Equipo")
    )
  ) %>% 
  cols_align(
    align = "center",
    columns = Equipo
  ) %>% 
  data_color(
    columns = "(xGOT-xG)/Tiro",
    colors = scales::col_numeric(
      palette = c(
        "tomato2", "seagreen"),
      domain = c(-0.151, 0.08))
  ) %>% 
  data_color(
    columns = c("xG/tiro"),
    colors = scales::col_numeric(
      palette= paletteer::paletteer_d(
        palette = "ggsci::orange_material"
      ) %>% as.character(),
      domain = c(0.4, 0.1))
  ) %>% 
  data_color(
    columns = c("xGOT/tiro"),
    colors = scales::col_numeric(
      palette= paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = c(0.4, 0.1))
  ) %>% 
  gt_img_rows(Equipo) %>% 
  gtsave(
    "calidad_ejecucion_laliga.png", expand = 10,
    path = "images/plots/"
  )
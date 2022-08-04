# Instalamos los paquetes necesarios -------------------------------------------

packages.cran = c("dplyr", "arrow", "gt", "paletteer")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)

packages.git <- c("gtExtras")
if(packages.git[[1]] %in% installed.packages() == F) devtools::install_github("jthomasmock/gtExtras")
if(length(packages.git[!inst]) > 0) install.packages(packages.git[!inst])
lapply(packages.git, require, character.only=T)


# Cargamoslos datos de eventing ------------------------------------------------

laliga.events <- read_parquet("data/LaLiga_2122_events.parquet")


# Porcentaje de goles que cambian el estado del partido ------------------------

laliga.events %>% 
  filter(event.type == "Goal" & shot.own.goal.flg == F) %>% 
  mutate(last.ten.minutes.goal = if_else(minute >= 80, T, F)) %>% 
  group_by(Jugador = player.name.x, 
           Equipo = paste0("images/escudos/",team.name, ".png")) %>% 
  summarise(Goles = n(),
            `Cambia estado` = sum(game.state.changing.goal.flg)) %>% 
  filter(Goles >= 10) %>% 
  mutate(Porcentaje = round(`Cambia estado`/Goles*100, 2)) %>%
  arrange(desc(Porcentaje)) %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(
    title = md("**Goles que cambian el estado del partido**"),
    subtitle = html("<em>Jugadores con al menos 10 goles en La Liga 2021/2022</em>")
  ) %>%
  tab_source_note(
    source_note = html("<em><b>Datos:</b> Stats Perform - Whoscored <br> <b>Tabla:</b> @DataKicks</em>")
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
      columns = c("Equipo", "Jugador")
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
    columns = c("Porcentaje"),
    colors = scales::col_numeric(
      palette = c(
        "tomato2", "seagreen"),
      domain = c(min(Porcentaje), max(Porcentaje)))
  ) %>%
  data_color(
    columns = c("Cambia estado"),
    colors = scales::col_numeric(
      palette= paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = c(min(`Cambia estado`), max(`Cambia estado`)))
  ) %>% 
  gt_img_rows(columns = Equipo, img_source = "local") %>% 
  gtsave(
    "goles_cambian_estado_laliga.png", expand = 10,
    path = "images/plots/"
  )


# Porcentaje de goles que son el primer gol del partido ------------------------

laliga.events %>% 
  filter(event.type == "Goal" & shot.own.goal.flg == F) %>% 
  mutate(last.ten.minutes.goal = if_else(minute >= 80, T, F)) %>% 
  group_by(Equipo = paste0("images/escudos/",team.name, ".png"),
           Jugador = player.name.x) %>% 
  summarise(Goles = n(),
            `Cambia estado` = sum(game.state.changing.goal.flg),
            `Primer gol` = sum(game.opener.goal.flg)) %>% 
  filter(Goles >= 10) %>% 
  mutate(Porcentaje = round(`Primer gol`/`Cambia estado`*100, 2)) %>% 
  arrange(desc(Porcentaje)) %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(
    title = md("**% Goles que son el primer gol del partido**"),
    subtitle = html("<em>Jugadores con al menos 10 goles en La Liga 2021/2022</em>")
  ) %>%
  tab_source_note(
    source_note = html("<em><b>Datos:</b> Stats Perform - Whoscored <br> <b>Tabla:</b> @DataKicks</em>")
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
      columns = c("Equipo", "Jugador")
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
    columns = c("Porcentaje"),
    colors = scales::col_numeric(
      palette = c(
        "tomato2", "seagreen"),
      domain = c(min(Porcentaje), max(Porcentaje)))
  ) %>%
  data_color(
    columns = c("Primer gol"),
    colors = scales::col_numeric(
      palette= paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = c(min(`Primer gol`), max(`Primer gol`)))
  ) %>% 
  gt_img_rows(columns = Equipo, img_source = "local") %>% 
  gtsave(
    "goles_primer_gol_laliga.png", expand = 10,
    path = "images/plots/"
  )


# Porcentaje de goles que cambian el estado del partido en los 10 últimos 
# minutos del partido

laliga.events %>% 
  mutate(last.ten.minutes.goal = if_else(minute >= 80, T, F)) %>% 
  filter(event.type == "Goal" & shot.own.goal.flg == F) %>% 
  group_by(Equipo = paste0("images/escudos/",team.name, ".png"),
           Jugador = player.name.x) %>% 
  summarise(Goles = n(),
            `Cambia estado` = sum(game.state.changing.goal.flg),
            `Últimos diez minutos` = sum(last.ten.minutes.goal)) %>% 
  filter(Goles >= 10) %>% 
  mutate(Porcentaje = round(`Últimos diez minutos`/`Cambia estado`*100, 2)) %>% 
  arrange(desc(Porcentaje)) %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(
    title = md("**% Goles que cambian el estado del partido en los últimos 10 minutos**"),
    subtitle = html("<em>Jugadores con al menos 10 goles en La Liga 2021/2022</em>")
  ) %>%
  tab_source_note(
    source_note = html("<em><b>Datos:</b> Stats Perform - Whoscored <br> <b>Tabla:</b> @DataKicks</em>")
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
      columns = c("Equipo", "Jugador")
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
    columns = c("Porcentaje"),
    colors = scales::col_numeric(
      palette = c(
        "tomato2", "seagreen"),
      domain = c(min(Porcentaje), max(Porcentaje)))
  ) %>%
  data_color(
    columns = c("Últimos diez minutos"),
    colors = scales::col_numeric(
      palette= paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = c(min(`Últimos diez minutos`), max(`Últimos diez minutos`)))
  ) %>% 
  gt_img_rows(columns = Equipo, img_source = "local") %>% 
  gtsave(
    "goles_ultimos_minutos_laliga.png", expand = 10,
    path = "images/plots/"
  )

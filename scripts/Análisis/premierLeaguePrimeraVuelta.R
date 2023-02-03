# Entrada: [Análisis] Rendimiento en la primera vuelta de la Premier League con los xG -03/02/2023 - https://data-kicks.com/index.php/2023/02/03/analisis-rendimiento-en-la-primera-vuelta-de-la-premier-league-con-los-xg/


# Instalamos y cargamos los paquetes necesarios --------------------------------
packages.cran = c("dplyr", "ggplot2", "patchwork", "figpatch", "ggtext",
                  "ggthemes", "forcats", "ggimage", "Cairo",
                  "showtext", "sysfonts", "gt", "paletteer", "devtools")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)

packages.git <- c("gtExtras")
if(packages.git[[1]] %in% installed.packages() == F) devtools::install_github("jthomasmock/gtExtras")
if(length(packages.git[!inst]) > 0) install.packages(packages.git[!inst])
lapply(packages.git, require, character.only=T)


# Opciones de texto y formato de datos -----------------------------------------
#font_add_google("Source Sans Pro", "ssp")
showtext_auto()
showtext_opts(dpi=300)

options(encoding = "UTF-8",
        scipen = 999)


# Cargamos los datos de la Premier League de la primera vuelta -----------------
premier.league.data <- read.csv("data/premier_league_16012023.csv",
                                encoding = "UTF-8") %>%
  filter(Marcador != "") %>%
  select(Jornada = Sem., Fecha, Local, xG.Local = xG, Marcador,
         xG.Visitante = xG.1, Visitante)


# Preparamos los datos ---------------------------------------------------------
premier.teams <- unique(premier.league.data$Local)

matches.by.team <- tibble()

for(team in premier.teams){
  for(i in 1:nrow(premier.league.data)){
    if(premier.league.data[i,]$Local == team){
      matches.by.team <- rbind(matches.by.team,
                               tibble(Equipo = team,
                                      Jornada = premier.league.data[i,]$Jornada,
                                      Fecha = premier.league.data[i,]$Fecha,
                                      Contrario = premier.league.data[i,]$Visitante,
                                      GF = as.numeric(strsplit(premier.league.data[i,]$Marcador, "-")[[1]][[1]]),
                                      xGF = premier.league.data[i,]$xG.Local,
                                      GC = as.numeric(strsplit(premier.league.data[i,]$Marcador, "-")[[1]][[2]]),
                                      xGC = premier.league.data[i,]$xG.Visitante))
    }
    else if(premier.league.data[i,]$Visitante == team){
      matches.by.team <- rbind(matches.by.team,
                               tibble(Equipo = team,
                                      Jornada = premier.league.data[i,]$Jornada,
                                      Fecha = premier.league.data[i,]$Fecha,
                                      Contrario = premier.league.data[i,]$Local,
                                      GF = as.numeric(strsplit(premier.league.data[i,]$Marcador, "-")[[1]][[2]]),
                                      xGF = premier.league.data[i,]$xG.Visitante,
                                      GC = as.numeric(strsplit(premier.league.data[i,]$Marcador, "-")[[1]][[1]]),
                                      xGC = premier.league.data[i,]$xG.Local))
    }
  }
}

teams.totals.by.match <- matches.by.team %>%
  mutate(Equipo = factor(Equipo),
         xGD = round(xGF - xGC, 2),
         GD = GF - GC,
         "GF-xGF" = round(GF - xGF, 2),
         "GC-xGC" = round(GC - xGC, 2),
         "GD-xGD" = GD-xGD,
         xG.igualado = if_else(xGD > -1 & xGD < 1, 1, 0),
         resultado = if_else(GD > 0, "W",
                             if_else(GD == 0, "D", "L")),
         xG.igualado.W = if_else(xG.igualado == 1 & resultado == "W", 1, 0)) %>%
  arrange(Equipo, Jornada)

levels(teams.totals.by.match$Equipo) <- c("Arsenal" , "Aston Villa", "Bournemouth",
                                          "Brentford", "Brighton", "Chelsea",
                                          "Crystal Palace", "Everton", "Fulham",
                                          "Leeds United", "Leicester City",
                                          "Liverpool", "Manchester City",
                                          "Manchester United", "Newcastle",
                                          "Nottingham Forest", "Southampton",
                                          "Tottenham", "West Ham", "Wolverhampton")

teams.totals.by.match <- teams.totals.by.match %>%
  mutate(Imagen = paste0("images/escudos/Premier League/", gsub(" ", "", Equipo), ".png"))

team.colors <- c(`Arsenal` = "#f00000", `Aston Villa` = "#7d1142",
                 `Bournemouth` = "#d71921", `Brentford` = "#c10000",
                 `Brighton` = "#0054a6", `Chelsea` = "#001489",
                 `Crystal Palace` = "#004484", `Everton` = "#2627ac",
                 `Fulham` = "#000000", `Leeds United` = "#fdcb00",
                 `Leicester City` = "#003090", `Liverpool` = "#dc0714",
                 `Manchester City` = "#98c5e9", `Manchester United` = "#c70101",
                 `Newcastle` = "#000000", `Nottingham Forest` = "#c8102e",
                 `Southampton` = "#ff0028", `Tottenham` = "#111836",
                 `West Ham` = "#7c2c3b", `Wolverhampton` = "#fdb913")


teams.totals <- teams.totals.by.match %>%
  group_by(Equipo) %>%
  summarise(GF = sum(GF), GC = sum(GC), xGF = round(sum(xGF), 2),
            xGC = round(sum(xGC), 2), "GF-xGF" = round(median(`GF-xGF`), 2),
            "GC-xGC" = round(median(`GC-xGC`), 2), "GD-xGD" = round(median(`GD-xGD`),2),
            xG.igualado = sum(xG.igualado),  xG.igualado.W = sum(xG.igualado.W),
            Imagen = first(Imagen)) %>%
  mutate(p.xG.igualado.W = round(xG.igualado.W/xG.igualado*100, 2))


# Hacemos los gráficos ---------------------------------------------------------

## Tablas ----------------------------------------------------------------------
gt(teams.totals %>% select(Equipo = Imagen, "GD-xGD", "GF-xGF", "GC-xGC",
                           "Partidos xG igualado*" = xG.igualado,
                           "% Partidos ganados xG igualado" = p.xG.igualado.W) %>%
     arrange(-`GD-xGD`)) %>%
  tab_header(
    title = md("**Premier League 2022/2023 - Primera Vuelta**"),
    subtitle = html("<em>Análisis de rendimiento utilizando diferencias entre Goles y xG</em>"),
    cell_text(font = google_font("Source Sans Pro"))
  ) %>%
  tab_source_note(
    source_note = html("<em><b>GD-xGD, GF-xGF, GC-xGC: </b> Valores de la mediana</em><br><em><b>*Partidos xG igualado: </b>1 &#62; xGD &#62; -1</em><br><em><b>Datos:</b> Opta via FotMob | <b>Tabla:</b> @DataKicks</em>")
  ) %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    column_labels.border.bottom.color = "black"
  ) %>%
  tab_style(
    style = list(
      cell_text(font = google_font("Source Sans Pro"),
                weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = list(
      #cell_fill(color = "black", alpha = 0.2),
      cell_text(font = google_font("Source Sans Pro")),
      cell_borders(
        side = c("left", "right"),
        color = "black",
        weight = px(2)
      )
    ),
    locations = cells_body(
      columns = c("Equipo")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(font = google_font("Source Sans Pro"))
    ),
    locations = cells_body(everything())
  ) %>%
  cols_align(
    align = "center",
    columns = Equipo
  ) %>%
  data_color(
    columns = c("GD-xGD"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::pink_material"
      ) %>% as.character(),
      domain = c(min(teams.totals$`GD-xGD`), max(teams.totals$`GD-xGD`)))
  ) %>%
  data_color(
    columns = c("GF-xGF"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::indigo_material"
      ) %>% as.character(),
      domain = c(min(teams.totals$`GF-xGF`), max(teams.totals$`GF-xGF`)))
  ) %>%
  data_color(
    columns = c("GC-xGC"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::indigo_material",
        direction = -1,
      ) %>% as.character(),
      domain = c(min(teams.totals$`GC-xGC`), max(teams.totals$`GC-xGC`)))
  ) %>%
  data_color(
    columns = c("Partidos xG igualado*"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::indigo_material"
      ) %>% as.character(),
      domain = c(min(teams.totals$xG.igualado),
                 max(teams.totals$xG.igualado)))
  ) %>%
  data_color(
    columns = c("% Partidos ganados xG igualado"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::indigo_material"
      ) %>% as.character(),
      domain = c(min(teams.totals$p.xG.igualado.W),
                 max(teams.totals$p.xG.igualado.W)))
  ) %>%
  gt_img_rows(Equipo, img_source = "local") %>%
  gtsave(
    "premier_league_g_xG_diff_xG_igualado.png", expand = 10,
    path = "images/plots/"
  )

gt(teams.totals %>% select(Equipo = Imagen, "Partidos xG igualado" = xG.igualado,
                           "% Partidos ganados xG igualado" = p.xG.igualado.W) %>%
     arrange(-`% Partidos ganados xG igualado`)) %>%
  tab_header(
    title = md("**Premier League 2022/2023 - Primera Vuelta**"),
    subtitle = html("<em>Análisis de rendimiento en partidos con xG igualado (1 &#62; xGD &#62; -1)</em>"),
    cell_text(font = google_font("Source Sans Pro"))
  ) %>%
  tab_source_note(
    source_note = html("<em><b>Datos:</b> Opta via FotMob | <b>Tabla:</b> @DataKicks</em>")
  ) %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    column_labels.border.bottom.color = "black"
  ) %>%
  tab_style(
    style = list(
      cell_text(font = google_font("Source Sans Pro"),
                weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = list(
      #cell_fill(color = "black", alpha = 0.2),
      cell_text(font = google_font("Source Sans Pro")),
      cell_borders(
        side = c("left", "right"),
        color = "black",
        weight = px(2)
      )
    ),
    locations = cells_body(
      columns = c("Equipo")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(font = google_font("Source Sans Pro"))
    ),
    locations = cells_body(everything())
  ) %>%
  cols_align(
    align = "center",
    columns = Equipo
  ) %>%
  data_color(
    columns = c("Partidos xG igualado"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::indigo_material"
      ) %>% as.character(),
      domain = c(min(teams.totals$xG.igualado),
                 max(teams.totals$xG.igualado)))
  ) %>%
  data_color(
    columns = c("% Partidos ganados xG igualado"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::pink_material"
      ) %>% as.character(),
      domain = c(min(teams.totals$p.xG.igualado.W),
                 max(teams.totals$p.xG.igualado.W)))
  ) %>%
  gt_img_rows(Equipo, img_source = "local") %>%
  gtsave(
    "premier_league_xG_igualado.png", expand = 10,
    path = "images/plots/"
  )

gt(teams.totals %>% select(Equipo = Imagen, "GD-xGD", "GF-xGF", "GC-xGC") %>%
     arrange(-`GD-xGD`)) %>%
  tab_header(
    title = md("**Premier League 2022/2023 - Primera Vuelta**"),
    subtitle = html("<em>Análisis de rendimiento utilizando diferencias entre Goles y xG</em><br><em>Valores de la mediana</em>"),
    cell_text(font = google_font("Source Sans Pro"))
  ) %>%
  tab_source_note(
    source_note = html("<em><b>Datos:</b> Opta via FotMob | <b>Tabla:</b> @DataKicks</em>")
  ) %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    column_labels.border.bottom.color = "black"
  ) %>%
  tab_style(
    style = list(
      cell_text(font = google_font("Source Sans Pro"),
                weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = list(
      #cell_fill(color = "black", alpha = 0.2),
      cell_text(font = google_font("Source Sans Pro")),
      cell_borders(
        side = c("left", "right"),
        color = "black",
        weight = px(2)
      )
    ),
    locations = cells_body(
      columns = c("Equipo")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(font = google_font("Source Sans Pro"))
    ),
    locations = cells_body(everything())
  ) %>%
  cols_align(
    align = "center",
    columns = Equipo
  ) %>%
  data_color(
    columns = c("GD-xGD"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::pink_material"
      ) %>% as.character(),
      domain = c(min(teams.totals$`GD-xGD`), max(teams.totals$`GD-xGD`)))
  ) %>%
  data_color(
    columns = c("GF-xGF"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::indigo_material"
      ) %>% as.character(),
      domain = c(min(teams.totals$`GF-xGF`), max(teams.totals$`GF-xGF`)))
  ) %>%
  data_color(
    columns = c("GC-xGC"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::indigo_material",
        direction = -1,
      ) %>% as.character(),
      domain = c(min(teams.totals$`GC-xGC`), max(teams.totals$`GC-xGC`)))
  ) %>%
  gt_img_rows(Equipo, img_source = "local") %>%
  gtsave(
    "premier_league_g_xG_diff.png", expand = 10,
    path = "images/plots/"
  )

gt(teams.totals %>% select(Equipo = Imagen, "GD-xGD", "GF-xGF", "GC-xGC") %>%
     arrange(-`GD-xGD`)) %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    column_labels.border.bottom.color = "black"
  ) %>%
  tab_style(
    style = list(
      cell_text(font = google_font("Source Sans Pro"),
                weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = list(
      #cell_fill(color = "black", alpha = 0.2),
      cell_text(font = google_font("Source Sans Pro")),
      cell_borders(
        side = c("left", "right"),
        color = "black",
        weight = px(2)
      )
    ),
    locations = cells_body(
      columns = c("Equipo")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(font = google_font("Source Sans Pro"))
    ),
    locations = cells_body(everything())
  ) %>%
  cols_align(
    align = "center",
    columns = Equipo
  ) %>%
  data_color(
    columns = c("GD-xGD"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::pink_material"
      ) %>% as.character(),
      domain = c(min(teams.totals$`GD-xGD`), max(teams.totals$`GD-xGD`)))
  ) %>%
  data_color(
    columns = c("GF-xGF"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::indigo_material"
      ) %>% as.character(),
      domain = c(min(teams.totals$`GF-xGF`), max(teams.totals$`GF-xGF`)))
  ) %>%
  data_color(
    columns = c("GC-xGC"),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::indigo_material",
        direction = -1,
      ) %>% as.character(),
      domain = c(min(teams.totals$`GC-xGC`), max(teams.totals$`GC-xGC`)))
  ) %>%
  gt_img_rows(Equipo, img_source = "local") %>%
  gtsave(
    "premier_league_g_xG_diff_no_haeder.png", expand = 10,
    path = "images/plots/"
  )

## Gráfico de densidad de GD-xGD por equipo ------------------------------------
density.plot <- ggplot(teams.totals.by.match) +
  geom_density(aes(`GD-xGD`,
                   color = fct_reorder(Equipo, -`GD-xGD`, median),
                   fill = fct_reorder(Equipo, -`GD-xGD`, median)),
               alpha = 0.5) +
  geom_vline(xintercept = 0, linetype="dashed", colour = "black", alpha = 0.7) +
  geom_text(aes(x = -6, y = 0.2,
                label = fct_reorder(Equipo, -`GD-xGD`, median),
                colour = fct_reorder(Equipo, -`GD-xGD`, median)),
            hjust = 0, vjust = 0,
            family = "ssp",
            size = 3) +
  theme_minimal(base_size = 10,
                base_family = "ssp") +
  labs(title = "Distribución de GD-xGD") +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(face = "bold",
                                  margin=margin(0,0,7,0))) +
  scale_color_discrete(type = team.colors) +
  scale_fill_discrete(type = team.colors) +
  facet_wrap(~ fct_reorder(Equipo, -`GD-xGD`, median),
             ncol = 1)

ggsave("images/plots/gd_xgd_density_plot.png",
       width = 640, height = 1500, units = "px", 
       dpi = 300, type = "cairo")


## Hacemos los gráficos de cuadrantes de GF-xGF  y GC-xGC ----------------------
xGF.plot <- ggplot(teams.totals) +
  geom_vline(xintercept = median(teams.totals$xGF), linetype="dashed", alpha = 0.8, colour = "darkgrey") +
  geom_hline(yintercept = mean(teams.totals$GF), linetype="dashed", alpha = 0.8, colour = "darkgrey") +
  geom_image(aes(xGF, GF, image=Imagen)) +
  theme_minimal(base_size = 10,
                base_family = "ssp") +
  labs(title = "Diferencia GF-xGF") +
  theme(plot.title = element_text(face = "bold",
                                  margin=margin(0,0,7,0)))

ggsave("images/plots/xgf_plot.png",
       width = 1600, height = 900, units = "px", 
       dpi = 300, type = "cairo")

xGC.plot <- ggplot(teams.totals) +
  geom_vline(xintercept = median(teams.totals$xGC), linetype="dashed", alpha = 0.8, colour = "darkgrey") +
  geom_hline(yintercept = mean(teams.totals$GC), linetype="dashed", alpha = 0.8, colour = "darkgrey") +
  geom_image(aes(xGC, GC, image=Imagen)) +
  theme_minimal(base_size = 10,
                base_family = "ssp") +
  labs(title = "Diferencia GC-xGC") +
  theme(plot.title = element_text(face = "bold",
                                  margin=margin(0,0,7,0)))
ggsave("images/plots/xgc_plot.png",
       width = 1600, height = 900, units = "px", 
       dpi = 300, type = "cairo")

## Unimos los gráficos ---------------------------------------------------------
layout <- "
ABBBD
ACCCD
"
xG.diff.img <- fig("images/plots/premier_league_g_xG_diff_no_haeder.png",
                   b_margin = margin(0,50,0,50)) +
  labs(title = "Valores de la mediana") +
  theme_minimal(base_size = 10,
                base_family = "ssp") +
  theme(plot.title = element_text(face = "bold",
                                  margin=margin(0,0,7,0)))

density.plot + xGF.plot + xGC.plot + xG.diff.img +
  plot_annotation(
    title = 'Vistazo general de la primera vuelta en la Premier League',
    subtitle = 'La diferencia entre GD y xGD nos puede dar indicios de que equipos están rindiendo por encima o por debajo de lo esperado',
    caption = 'Datos: Opta via FotMob\nViz: @DataKicks',
    theme = theme(plot.title = element_text(size = 16,
                                            face = "bold"),
                  plot.subtitle = element_text(margin=margin(0,0,10,0)))
  ) +
  plot_layout(design = layout)

#ggsave("images/result.png", width = 35, height = 20, units = "cm", dpi = 350)
ggsave("images/plots/total_plot.png",
       width = 40.64, height = 22.86, units = "cm", 
       dpi = 300, type = "cairo")

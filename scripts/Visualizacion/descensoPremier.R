# Instalamos y cargamos los paquetes necesarios --------------------------------
packages.cran = c("dplyr", "ggplot", "ggbeeswarm", "ggtext", "scales", "magick",
                  "sysfonts", "camcorder")
inst <- packages.cran %in% installed.packages()
if(length(packages.cran[!inst]) > 0) install.packages(packages.cran[!inst])
lapply(packages.cran, require, character.only=T)


# Añadimos las fuentes ---------------------------------------------------------
font_add_google("Source Sans Pro", "ssp")
font_add("fa-brands", 'fonts/Font Awesome 6 Brands-Regular-400.otf')


# Para ver el resultado final en el viewer y guardarlo -------------------------
gg_record(
  dir = "plots/",
  device = "png",
  width = 20.5,
  height = 15,
  units = "in",
  dpi = 100,
)


# Cargamos los datos -----------------------------------------------------------
plot.data <- read.csv("data/pl_final_data.csv") %>% 
  mutate(descenso.inv = factor(if_else(dist.descenso > 0, "No", "Sí")),
         descenso.final = factor(if_else(descenso.final == 0, "No", "Sí")),
         fake.column = factor(0)) %>% 
  filter(!is.na(descenso.final))


# Textos e imágenes ------------------------------------------------------------
subtitle <- "Analizamos el descenso en la Premier League durante las últimas 5 temporadas. Nos fijamos en si el equipo estaba en posiciones de descenso y en la diferencia de valor de su plantilla al finalizar el mercado de invierno."

#image.datakicks <- as.raster(image_fill(image_read("images/logo-dk-2.png"), "none"))

text.southampton <- "<b>Southampton 2017-2018:</b> Después del mercado de<br>invierno estaban en puestos de descenso y, además,<br>decrementaron más de un 7% el valor de su plantilla<br>tras vender a Van Dijk. Al final, consiguieron la <br>permanencia, después de una buena racha en las<br>últimas jornadas."
image.southampton <- as.raster(image_fill(image_read("images/Southampton.png"), "none"))

text.norwich <- "<b>Norwich 2021-2022:</b> Sólo estuvieron una jornada fuera<br>del descenso en toda la temporada y, casualmente, fue<br>justo cuando el cierre del mercado de invierno. Bajó su<br>valor de mercado más de un 11%, pero no fue por ventas<br>importantes, si no por la devaluación de sus jugadores.<br>Descendieron cuando quedaban cuatro jornadas por<br>disputarse."
image.norwich <- as.raster(image_fill(image_read("images/Norwich.png"), "none"))

text.westham <- "<b>West Ham 2019-2020:</b> Después de estar coqueteando desde<br>el inicio de temporada con el descenso, acaban cayendo en<br>él tras 25 jornadas, justo al cierre del mercado de invierno.<br>Hicieron una inversión de más de 23 millones en enero con<br>fichajes del nivel de Bowen o Soucek. Acabaron la temporada<br>5 puntos por encima de la zona de descenso."
image.westham <- as.raster(image_fill(image_read("images/WestHam.png"), "none"))

text.newcastle <- "<b>Newcastle 2021-2022:</b> Las urracas estuvieron en descenso<br>en 18 de las 21 primeras jornadas. Consiguieron salir de él<br>ganando en su partido número 22, el siguiente al cierre del<br>mercado invernal. En este periodo, después de la inyección<br>millonaria por parte de sus nuevos dueños, adquirieron<br>jugadores de la talla de Trippier, Wood o Bruno Guimaraes,<br>aumentando así el valor de la plantilla en un 26%. Tras esta<br>victoria, llegarían otras 11 en posteriores jornadas para<br>acabar la temporada en una cómoda 11ª posición."
image.newcastle <- as.raster(image_fill(image_read("images/Newcastle.png"), "none"))

text.stoke <- "<b>Stoke City 2017-2018:</b> Estuvieron coqueteando con los puestos<br>de descenso durante la primera mitad de la temporada, entrando<br>en ellos sólo una vez y terminando fuera de ellos cuando se<br>cierra el mercado de invierno. El 31 de enero concretan el fichaje<br>de N'Diaye por 14 millones de libras, lo que hace que el valor<br>de su plantilla se incremente en más de un 12%. La siguiente<br>jornada, tras una derrota, vuelven a meterse en la zona de<br>descenso, de la que no volverían a salir."
image.stoke <- as.raster(image_fill(image_read("images/Stoke.png"), "none"))


# Visualización final ----------------------------------------------------------
ggplot(data = plot.data) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -10, fill = "#FF4500", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -10, ymax = 0, fill = "#FF4500", alpha = 0.07) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10, fill = "#90EE90", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20, fill = "#90EE90", alpha = 0.15) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 20, ymax = 30, fill = "#90EE90", alpha = 0.2) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 30, ymax = 40, fill = "#90EE90", alpha = 0.25) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 40, ymax = Inf, fill = "#90EE90", alpha = 0.32) +
  geom_point(aes(y = pct.mercado.inv, x = fake.column, 
                 color = descenso.final,
                 fill = descenso.final, 
                 shape = descenso.inv),
             position = position_beeswarm(cex = 4.3),
             size = 13,
             stroke = 3,
             alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black", alpha = 0.7,
             size = 1) +
  
  # annotation_raster(image.datakicks, 1.58, 1.5, -18.5, -13) +
  
  # annotation_raster(image.southampton, 1.47, 1.39, 12.8, 14.6) +
  # annotate(geom = "richtext", x = 1.35, y = 14.8, label = text.southampton, size = 2.5, hjust = 0, fill = NA, label.color = NA) +
  # annotate(geom = "segment", x = 1.09, xend = 1.45, y = -7.8, yend = 12.5, size = 1, alpha = 0.5) +
  # geom_point(aes(x = 1.09, y = -7.8), size = 3) +
  
  # annotation_raster(image.norwich, 1.48, 1.4, 12.8, 14.6) +
  # annotate(geom = "richtext", x = 1.35, y = 14.8, label = text.norwich, size = 2.5, hjust = 0, fill = NA, label.color = NA) +
  # annotate(geom = "segment", x = 1, xend = 1.45, y = -11.9, yend = 12.5, size = 1, alpha = 0.5) +
  # annotation_raster(image.norwich, 0.81, 0.73, 12.8, 14.6) +
  # annotate(geom = "richtext", x = 0.69, y = 14.8, label = text.norwich, size = 2.5, hjust = 0, fill = NA, label.color = NA) +
  # annotate(geom = "segment", x = 1, xend = 0.8, y = -11.9, yend = 12.5, size = 1, alpha = 0.5) +
  # geom_point(aes(x = 1, y = -11.9), size = 3) +
  
  # annotation_raster(image.westham, 1.47, 1.39, 12.8, 14.6) +
  # annotate(geom = "richtext", x = 1.35, y = 14.8, label = text.westham, size = 2.5, hjust = 0, fill = NA, label.color = NA) +
  # annotate(geom = "segment", x = 1, xend = 1.45, y = 7, yend = 12.5, size = 1.5, alpha = 0.5) +
  # geom_point(aes(x = 1, y = 7), size = 3) +
  
  # annotation_raster(image.newcastle, 1.53, 1.45, 12.8, 14.6) +
  # annotate(geom = "richtext", x = 1.35, y = 14.8, label = text.newcastle, size = 2.5, hjust = 0, fill = NA, label.color = NA) +
  # annotate(geom = "segment", x = 0.925, xend = 1.17, y = 26.1, yend = 26.1, size = 1) +
  # geom_point(aes(x = 0.925, y = 26.1), size = 3) +
  
  # annotation_raster(image.stoke, 1.51, 1.43, 12.8, 14.6) +
  # annotate(geom = "richtext", x = 1.35, y = 14.8, label = text.stoke, size = 2.5, hjust = 0, fill = NA, label.color = NA) +
  # annotate(geom = "segment", x = 1.12, xend = 1.41, y = 12.3, yend = 13.5, size = 1) +
  # geom_point(aes(x = 1.12, y = 12.3), size = 3) +

scale_color_manual(values = c("#008B00", "#CD2626")) +
  scale_fill_manual(values = c("#008B00", "#CD2626")) +
  scale_shape_manual(values = c(21, 25)) +
  scale_y_continuous(breaks = seq(-20, 45, by = 10),
                     labels = number_format(suffix = "%")) +
  coord_flip() +
  labs(
    title = "¿Invertir en el mercado de invierno ayuda a evitar el descenso?",
    subtitle = subtitle,
    x = "",
    y = "Diferencia del valor de la plantilla",
    caption = "<span>Datos: whoscored.com | transfermarkt.com</span><br>Visualización: <span style='font-family:fa-brands;'> &#xf099; </span><span>@DataKicks | </span><span style='font-family:fa-brands;'>&#xf08c; </span><span>obartolomep</span>",
    shape = "Descenso invierno",
    color = "Descenso final") +
  theme(text = element_text(family = "ssp"),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.justification = "left",
        legend.margin = margin(t = 1, r = 1, b = 1, l = 0, unit='cm'),
        legend.title = element_text(face = "bold"),
        legend.spacing.x = unit(0.7, "cm"),
        legend.key = element_blank(),
        axis.text.x = element_text(margin = margin(10, 0, 20, 0)),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color="black"),
        panel.background = element_blank(),
        plot.title = element_textbox_simple(face = "bold", 
                                            size = 17, 
                                            margin = margin(t = 1, b = 0.7, unit='cm')),
        plot.subtitle = element_textbox_simple(),
        plot.caption = element_textbox_simple(face = "italic",
                                              margin = margin(t = 1, r = 1, b = 1, unit='cm'),
                                              hjust = 0,
                                              lineheight = 3.5),
        plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit='cm'),
        # panel.grid.major.x = element_line(color='grey90'),
        panel.grid = element_blank()) +
  guides(shape = guide_legend(reverse = T, order = 1),
         color = guide_legend(reverse = T, order = 2),
         fill = "none")


# Guardamos la imagen ----------------------------------------------------------
ggsave("plots/1_empty.png",
       dpi = 100, 
       width = 20.5,
       height = 15,
       units = "in",
       type = "cairo")

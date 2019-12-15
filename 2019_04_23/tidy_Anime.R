# Se carga la base de datos ----

#tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
#save(tidy_anime, file = 'Anime.Rdata')
load('2019_04_23/Anime.Rdata')

# Se cargan los paquetes necesarios ----

library(tidyverse)
library(ggplot2)
library(magick)

# Se crea una pequeña paleta de colores (https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2) ----

# 1. Se crea un vector con nombre de códigos hexadecimales para los colores (https://www.december.com/html/spec/colorcodes.html)

leo_colores <- c( 
  `cyan1` = '#00ffff', # Se usan marcas de retroceso (``) para eliminar las restricciones de nombres (por ejemplo, para incluir espacios para `gris claro` y `gris oscuro`)
  `green2` = '#00EE00',
  `yellow` = '#FFFF00	',
  `fuchsia2` = '#FF00AA	',
  `black beauty plum` = '#422C2F',
  `dodgerblue` = '#1E90FF',
  `orangered` = '#FF2400',
  `mistyrose4	` = '#8B7D7B',
  `coffee	` = '#AA5303',
  `black` = '#000000'
)

# 2. Se escribe una función que extrae los códigos hexadecimales del vector de colores anterior

#' Función para acceder a códigos hexadecimales
#'
#' @param ... Nombre de caracteres de "leo_colores" 
#'
leo_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (leo_colores)
  
  leo_colores[cols]
}

leo_cols() # La anterior función puede hacer que se devuelvan todos los colores tal como están, especificar ciertos colores, en un orden particular, agregar argumentos y comprobaciones de funciones adicionales, etc
leo_cols('orangered')

# Se edita los datos y se realizan la gráficas ----

Base_datos <- tidy_anime %>%
  # Filtro primero por mis animes favoritos
  filter(name %in% c('One Piece', 'Boku no Hero Academia', 'One Punch Man', 
                     'Made in Abyss', 'Fullmetal Alchemist: Brotherhood', 
                     'Naruto: Shippuuden', 'Death Note', 'Magi: The Kingdom of Magic', 
                     'Black Clover', 'Shingeki no Kyojin')) %>%
  distinct(name, .keep_all = TRUE) %>%
  # Dejo la información necesaria para hacer la gráfica
  select(name, score) %>%
  # Hago la gráfica
  ggplot(aes(x = name, y = score)) +
  geom_bar(stat = 'identity', color = leo_cols('black'), 
           fill = leo_cols('black'), alpha = 0.4) +
  scale_y_continuous(limits = c(0, 15)) +
  theme_classic() +
  labs(title = 'Algunos de mis animes shōnen favoritos', y = 'Puntuación', 
       x = '', caption = '#tidytuesday by @Leo4Luffy') +
  theme(axis.title = element_text(size = 10, face = 'bold'),
        axis.text.x = element_text(size = 8, color = 'black', face = 'bold', angle = 20),
        axis.text.y = element_text(size = 8, color = 'black', face = 'bold'),
        plot.title = element_text(size = 12, color = 'black', face = 'bold')) +
  ggsave('2019_04_23/anime.png')

grafica <- image_read('2019_04_23/anime.png') # Se carga de nuevo el anterior gráfico

image_read('https://pa1.narvii.com/6138/129c193c23791c79ecc6909a994826651a3d8cc8_hq.gif') %>%
  image_apply( function(Naruto){
    image_composite(grafica, Naruto, offset = '+1000+8')
  }) %>%
  image_resize('40%') %>%
  image_animate() %>%
  image_write('2019_04_23/Anime_tidy.gif')

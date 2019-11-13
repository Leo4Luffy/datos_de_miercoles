
# La base de datos a usar esta semana contiene los paquetes disponibles en el CRAN y la información de que lenguaje esta escrito

# Se importa la base de datos de interés ----
#cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")
#save(cran_code, file = '2019_11_13/tidyverse.Rdata')
load('2019_11_13/tidyverse.Rdata')

# Se cargan los paquetes necesarios y se edita la base de datos para hacer la gráfica ----
library(dplyr)
library(ggplot2)
library(gridExtra)

linea_codigo <- cran_code %>%
  # Se filtran solo los paquetes que hacen parte del tidyverse
  filter(pkg_name %in% c('ggplot2', 'dplyr', 'tidyr', 'readr', 'purrr', 'tibble', 'stringr', 'forcats')) %>%
  # Se realiza la gráfica para observar las líneas de código escrito para cada paquete en cada lenguaje de programación
  ggplot(aes(x = language, y = code, colour = pkg_name, fill = pkg_name)) +
  geom_bar(stat = 'identity', alpha = 0.4) +
  scale_colour_manual(values = c('yellow', 'turquoise1', 'violetred1', 'chartreuse', 'magenta', 'springgreen', 'green', 'gray54')) +
  scale_fill_manual(values = c('yellow', 'turquoise1', 'violetred1', 'chartreuse', 'magenta', 'springgreen', 'green', 'gray54')) +
  labs(x = 'Lenguaje de programación', y = 'Líneas de código') +
  facet_wrap(~ pkg_name) +
  theme_classic() +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 8, color = 'black', face = 'bold', angle = 90),
        axis.text.y = element_text(size = 8, color = 'black', face = 'bold'),
        axis.title = element_text(size = 10, face = 'bold'),
        strip.text = element_text(size = 10, color = 'black', face = 'bold'), 
        strip.background = element_rect(color = 'black', fill = 'gray84')) +
  ggsave('2019_11_13/linea_codigo.png')

linea_comentario <- cran_code %>%
  # Se filtran solo los paquetes que hacen parte del tidyverse
  filter(pkg_name %in% c('ggplot2', 'dplyr', 'tidyr', 'readr', 'purrr', 'tibble', 'stringr', 'forcats')) %>%
  # Se realiza la gráfica para observar las líneas de código escrito para cada paquete en cada lenguaje de programación
  ggplot(aes(x = language, y = comment, colour = pkg_name, fill = pkg_name)) +
  geom_bar(stat = 'identity', alpha = 0.4) +
  scale_colour_manual(values = c('yellow', 'turquoise1', 'violetred1', 'chartreuse', 'magenta', 'springgreen', 'green', 'gray54')) +
  scale_fill_manual(values = c('yellow', 'turquoise1', 'violetred1', 'chartreuse', 'magenta', 'springgreen', 'green', 'gray54')) +
  labs(x = 'Lenguaje de programación', y = 'Líneas de comentario') +
  facet_wrap(~ pkg_name) +
  theme_classic() +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size = 8, color = 'black', face = 'bold', angle = 90),
        axis.text.y = element_text(size = 8, color = 'black', face = 'bold'),
        axis.title = element_text(size = 10, face = 'bold'),
        strip.text = element_text(size = 10, color = 'black', face = 'bold'), 
        strip.background = element_rect(color = 'black', fill = 'gray84')) +
  ggsave('2019_11_13/linea_comentario.png')

grid.arrange(linea_codigo, linea_comentario, ncol = 2, nrow = 1)


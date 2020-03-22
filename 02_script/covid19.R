# Paquetes ----
require(tidyverse)
require(readxl)
require(forecast)

# Directorios ----
inp <- "covid19_mex/01_datos/"
out <- "covid19_mex/03_gráficas/"
fiuffi <- "Elaboración propia con datos de la Secretaría de Salud | @guzmart_"

# Datos ----
d <- read_excel(paste0(inp, "covid_mex_20200321.xlsx"), 
                          col_types = c("numeric", "text", "text", 
                                        "numeric", "date", "text", "text", 
                                        "date", "date")) 

# Transformaciones ----
data_fecha <- d %>% 
  group_by(
    fecha_corte
  ) %>% 
  summarise(
    n = n()
  ) 


data_fecha_acumulado <- data_fecha %>% 
  mutate(
    n_acumulada = cumsum(n)
  ) 


# Plots ----
fiuf <- "Número de casos confirmados de COVID-19 en México"
fiuff <- paste0("Fecha de corte: ", str_sub(max(data_fecha_acumulado$fecha_corte), end = -1))

# Acumulados
ggplot(data_fecha_acumulado, 
         aes(x = as.Date(fecha_corte),
             y = n_acumulada,
             label = n_acumulada)) +
  geom_line() + geom_label(size=6) +
  scale_x_date(date_breaks = "2 day") +
  theme_minimal() + 
  labs(title=fiuf, 
       subtitle=fiuff,
       caption=fiuffi,
       x="",
       y="Número de casos acumulados") +
  theme(plot.title = element_text(size = 35, face = "bold"),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        strip.text = element_text(size = 15),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15))
ggsave(filename = paste0(
  out, "01_acumulados_", str_replace_all(str_sub(max(data_fecha_acumulado$fecha_corte), end = -1), "-", "_"), ".png"
), width = 15, height = 10, dpi = 100)


# Nuevos por día
fiuf <- "Número de *nuevos* casos confirmados de COVID-19 en México por día"
fiuff <- paste0("Fecha de corte: ", 
                str_sub(max(data_fecha$fecha_corte), end = -1),"\nTotal: ", as.character(sum(data_fecha$n)))
ggplot(data_fecha, 
       aes(x = as.Date(fecha_corte),
           y = n,
           label = n)) +
  geom_col() + geom_text(size=6, vjust=-0.5) +
  scale_x_date(date_breaks = "2 day") +
  theme_minimal() + 
  labs(title=fiuf, 
       subtitle=fiuff,
       caption=fiuffi,
       x="",
       y="Número de nuevos acumulados") +
  theme(plot.title = element_text(size = 35, face = "bold"),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        strip.text = element_text(size = 15),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15))
ggsave(filename = paste0(
  out, "02_nuevos_", str_replace_all(str_sub(max(data_fecha_acumulado$fecha_corte), end = -1), "-", "_"), ".png"
), width = 15, height = 10, dpi = 100)
 
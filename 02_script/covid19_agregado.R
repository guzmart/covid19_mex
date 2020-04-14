# Paquetes ----
require(tidyverse)
require(readxl)
require(ggthemes)
require(lubridate)
require(RColorBrewer)

# Directorios y funciones ----
inp <- "Github/covid19_mex/01_datos/"
out <- "Github/covid19_mex/03_gráficas/"
pdf <- "covid19_mex/99_ssalud_pdf/"
fiuffi <- "Elaboración propia con datos de la Secretaría de Salud\n@guzmart_ | @regi_medina | @lolo7no"
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

# CREACIÓN EN LOOP DE BASE DE DATOS AGREGADA ----
# Datos ----
d <- read_excel(paste0(inp, "99_old/covid_mex_20200316.xlsx"))

# Loops ----
# (hasta el 06 de abril de 2020)
tablas <- 
  c(
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.17.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.18.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.19.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.20.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.21.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.22.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.23.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.24.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.25.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.26.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.27.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.28.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.29.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.30.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.31.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.04.01.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.04.02.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.04.03.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.04.04.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.04.05.xlsx",
    "Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.04.06.xlsx"
  )

fechas <- c(
  "2020-03-17",
  "2020-03-18",
  "2020-03-19",
  "2020-03-20",
  "2020-03-21",
  "2020-03-22",
  "2020-03-23",
  "2020-03-24",
  "2020-03-25",
  "2020-03-26",
  "2020-03-27",
  "2020-03-28",
  "2020-03-29",
  "2020-03-30",
  "2020-03-31",
  "2020-04-01",
  "2020-04-02",
  "2020-04-03",
  "2020-04-04",
  "2020-04-05",
  "2020-04-06"
)


# Totales y nuevos
data_fecha <- d %>% 
  group_by(
    fecha_corte
  ) %>% 
  summarise(
    n = n()
  ) %>% 
  mutate(
    n_acumulada = cumsum(n)
  ) 

for(i in 1:length(tablas)){
  d_pdf <- read_excel(
    paste0(pdf, tablas[i]), 
    col_types = c("numeric", "text", "text", 
                  "numeric", "date", "text", "text", 
                  "date")
  ) 
  
  names(d_pdf) <- c("num_caso","ent","sexo","edad","fecha_inicio","identificado",
                    "procedencia","fecha_llegada_mexico")
  
  d_pdf <- drop_na(d_pdf, num_caso)
  
  d_pdf_acumulado <- d_pdf %>% 
    mutate(
      fecha_corte = fechas[i],
      fecha_corte = as.Date.character(fecha_corte, format = c("%Y-%m-%d"))
    ) %>% 
    count(
      fecha_corte
    ) %>% 
    rename(
      n_acumulada = n
    ) %>% 
    mutate(
      n = n_acumulada-data_fecha$n_acumulada[nrow(data_fecha)]
    )
  
  data_fecha <- data_fecha %>% 
    mutate(fecha_corte = as.Date.character(fecha_corte, format = c("%Y-%m-%d"))) %>% 
    bind_rows(d_pdf_acumulado)
  
  openxlsx::write.xlsx(
    data_fecha, paste0(
      inp,
      str_replace_all(str_sub(max(data_fecha$fecha_corte), end =-1), "-", "_"),
      "_01_acumulada.xlsx"
    )
  )
  
}

# CASOS CONFIRMADOS EN MÉXICO ----
# Datos ----
data_fecha <- readxl::read_excel("Github/covid19_mex/01_datos/2020_04_13_01_acumulada.xlsx", 
                         col_types = c("date", "numeric", "numeric"))

# Plots ----
fiuf <- "Número de casos confirmados de COVID-19 en México"
fiuff <- paste0("Fecha de corte: ", str_sub(max(data_fecha$fecha_corte), end = -1))

# Acumulados
ggplot(data_fecha, 
       aes(x = as.Date(fecha_corte),
           y = n_acumulada,
           label = n_acumulada)) +
  geom_line() + geom_label(size=4.5) +
  scale_x_date(date_breaks = "1 day",
               limits = c(
                 min(as.Date(data_fecha$fecha_corte)-0.7),
                 max(as.Date(data_fecha$fecha_corte)+0.8)
               ),
               expand = c(0,0)) +
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
  out, str_replace_all(str_sub(max(data_fecha$fecha_corte), end = -1), "-", "_"), "_01_acumulados.png"
), width = 15, height = 10, dpi = 100)


# Nuevos por día
fiuf <- "Número de *nuevos* casos confirmados de COVID-19 en México por día"
fiuff <- paste0("Fecha de corte: ", 
                str_sub(max(data_fecha$fecha_corte), end = -1),"\nTotal: ", as.character(sum(data_fecha$n)))
ggplot(data_fecha, 
       aes(x = as.Date(fecha_corte),
           y = n,
           label = n)) +
  geom_col() + geom_text(size=4.5, vjust=-0.5) +
  scale_x_date(date_breaks = "1 day",
               limits = c(
                 min(as.Date(data_fecha$fecha_corte)-0.7),
                 max(as.Date(data_fecha$fecha_corte)+0.7)
               ),
               expand = c(0,0)) +
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
  out, str_replace_all(str_sub(max(data_fecha$fecha_corte), end = -1), "-", "_"), "_02_nuevos.png"
), width = 15, height = 10, dpi = 100)


# CASOS CONFIRMADOS EN EL MUNDO ----
# Datos ----
data_world <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
  filter(`Country/Region`=="Spain" | `Country/Region`== "Italy" |
           `Country/Region`== "France" | `Country/Region` == "Germany" | `Country/Region`== "US" |
           `Country/Region`== "Mexico" ) %>% 
  filter(is.na(`Province/State`)) %>% 
  select(-"Province/State", -Lat, -Long) %>% 
  rename("pais" = "Country/Region") %>% 
  pivot_longer(-pais,
               names_to = "fecha_corte", 
               values_to = "n") %>% 
  group_by(pais, fecha_corte) %>% 
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>% 
  mutate(fecha_corte = lubridate::mdy(fecha_corte),
         pais = case_when(
           str_starts(pais, "Fr")~ "Francia",
           str_starts(pais, "Ger")~ "Alemania",
           str_starts(pais, "It")~ "Italia",
           str_starts(pais, "Sp")~ "España",
           str_starts(pais, "Mex")~ "México",
           str_starts(pais, "U")~ "EEUU",
           T ~ "Colombia"
         )) %>% 
  select(pais, fecha_corte, n)  %>% 
  complete(fecha_corte, pais) %>% 
  replace(., is.na(.), 0) %>% 
  filter(n>0)%>%
  group_by(pais) %>% 
  mutate(Día = as.numeric(fecha_corte-min(fecha_corte)),
         max = max(n))%>%
  ungroup()               

data_world$n[nrow(data_world)] <- max(data_fecha$n_acumulada)
data_world$max[nrow(data_world)] <- max(data_fecha$n_acumulada)

fiuffi <- "Fuente: elaboración propia con datos de la Universidad Johns Hopkins | @guzmart_"
  
fiuf <- "Número de casos acumulados en distintos países\ndesde el primer caso confirmado en el país"
ggplot(data_world, 
       aes(x = Día,
           y = n,
           color = pais,
           label = ifelse(
             fecha_corte==max(fecha_corte), 
             paste0(pais, "\n", 
                    format(max, big.mark = ",")), ""
           ))) +
  geom_line(lineend = "round",
            size = 1) +
  ggrepel::geom_text_repel(color="black") +
  labs(title = fiuf,
       # subtitle = "Sólo se muestran los primeros 39 días",
       caption = fiuffi,
       colour = "",
       x = "Días desde el primer caso confirmado en el país",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold"),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        strip.text = element_text(size = 15),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 15, vjust = 0.5),
        axis.text.y = element_text(size = 13),
        legend.position = "none")

ggsave(filename = paste0(
  out, str_replace_all(str_sub(max(data_world$fecha_corte), end = -1), "-", "_"), "_03_mundial.png"
), width = 15, height = 10, dpi = 100)

fiuf <- "Número de casos acumulados en distintos países\ndesde el primer caso confirmado en el país"
ggplot(data_world %>% filter(Día<46), 
       aes(x = Día,
           y = n,
           color = pais,
           label = ifelse(
             Día==max(Día), 
             paste0(pais, "\n", 
                    format(n, big.mark = ",")), ""
           ))) +
  geom_line(lineend = "round",
            size = 1) +
  ggrepel::geom_text_repel(color="black", box.padding = 0.9) +
  labs(title = fiuf,
       subtitle = "Sólo se muestran los primeros 45 días",
       caption = fiuffi,
       colour = "",
       x = "Días desde el primer caso confirmado en el país",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 35, face = "bold"),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        strip.text = element_text(size = 15),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 15, vjust = 0.5),
        axis.text.y = element_text(size = 13),
        legend.position = "none")

ggsave(filename = paste0(
  out, str_replace_all(str_sub(max(data_world$fecha_corte), end = -1), "-", "_"), "_03_mundial_filtro_dias.png"
), width = 15, height = 10, dpi = 100)
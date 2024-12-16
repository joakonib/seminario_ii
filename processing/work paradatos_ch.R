####Procesamiento paradatos:
rm(list=ls())

library(sjPlot)

pacman::p_load(
  tidyverse,
  data.table,
  tidylog,
  haven,
  ggplot2,
  RColorBrewer,
  sjPlot
)

###Trabajar paradatos_ch


paradatos_ch <- readRDS("paradatos_ch.RDS") 


###diferencia está en minutos

tiempos_ch <- 
  paradatos_ch %>% 
  mutate(diferencia = as.numeric(diferencia),
         hhsize2 = as.factor(hhsize2)) %>% 
  group_by(interview__key,hhsize2,recol_ch_sexo,recol_ch_experiencia_ine,hay_0_14,hay_psdf,hay_trabaja) %>% 
  summarize(tiempo = sum(diferencia, na.rm = TRUE)) %>% 
  ungroup() %>% 
  


colores_hhsize <- c(
  
  "#8cd4de",  # Color 1
  "#66c8d5",  # Color 2
  "#2d93ad",  # Color 3 (color medio)
  "#007f8d",  # Color 4
  "#006f78",  # Color 5
  "#005f66"
  
)

# Creamos el gráfico
ggplot(tiempos_ch, aes(x = hhsize2,
                       y = tiempo,
                       fill = hhsize2)) +  # Usamos modulo para el relleno
  geom_boxplot() +
  scale_fill_manual(values = colores_hhsize) +  # Aplicamos la paleta de colores
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "none"  # Eliminamos la leyenda
  ) +
  xlab("") +
  ylab("Tiempo (min)")

tiempos_total_ch <- 
  tiempos_ch %>% 
  group_by(hhsize2) %>% 
  summarize(min = min(tiempo, na.rm = TRUE),
            mediana = median(tiempo, na.rm = TRUE),
            promedio = mean(tiempo, na.rm = TRUE),
            p01 = quantile(tiempo, 0.01, na.rm = TRUE),
            p05 = quantile(tiempo, 0.05, na.rm = TRUE),
            p10 = quantile(tiempo, 0.1, na.rm = TRUE),
            p25 = quantile(tiempo, 0.25, na.rm = TRUE),
            p33 = quantile(tiempo, 0.33, na.rm = TRUE),
            p60 = quantile(tiempo, 0.60, na.rm = TRUE),
            p75 = quantile(tiempo, 0.75, na.rm = TRUE),
            p90 = quantile(tiempo, 0.90, na.rm = TRUE),
            p95 = quantile(tiempo, 0.95, na.rm = TRUE),
            p99 = quantile(tiempo, 0.99, na.rm = TRUE),
            max = max(tiempo, na.rm = T),
            sd = sd(tiempo, na.rm = TRUE),
            n = n())

tiempos_total_ch2 <- 
  tiempos_ch %>% 
  summarize(min = min(tiempo, na.rm = TRUE),
            mediana = median(tiempo, na.rm = TRUE),
            promedio = mean(tiempo, na.rm = TRUE),
            p01 = quantile(tiempo, 0.01, na.rm = TRUE),
            p05 = quantile(tiempo, 0.05, na.rm = TRUE),
            p10 = quantile(tiempo, 0.1, na.rm = TRUE),
            p25 = quantile(tiempo, 0.25, na.rm = TRUE),
            p33 = quantile(tiempo, 0.33, na.rm = TRUE),
            p60 = quantile(tiempo, 0.60, na.rm = TRUE),
            p75 = quantile(tiempo, 0.75, na.rm = TRUE),
            p90 = quantile(tiempo, 0.90, na.rm = TRUE),
            p95 = quantile(tiempo, 0.95, na.rm = TRUE),
            p99 = quantile(tiempo, 0.99, na.rm = TRUE),
            max = max(tiempo, na.rm = T),
            sd = sd(tiempo, na.rm = TRUE),
            n = n())



  









decimal_a_minutos

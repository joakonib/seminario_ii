###0. Cargar paquetes y cosas-----

### Limpiar enviroment
rm(list=ls())


### Cargar paquetes
pacman::p_load(
  tidyverse,
  data.table,
  tidylog,
  haven,
  ggplot2,
  RColorBrewer,
  sjPlot
)

### Usar el punto como decimal
options(decimal.mark = ",")


### Abrir base de poaradatos_ch
paradatos_ch <- readRDS("paradatos_ch.RDS") 
paradatos_cut <- readRDS("paradatos_cut.RDS") 

### Crear tiempos CH
tiempos_ch <- 
  paradatos_ch %>% 
  mutate(diferencia = as.numeric(diferencia),
         hhsize2 = as.factor(hhsize2)) %>% 
  group_by(interview__key,hhsize2,recol_ch_sexo,recol_ch_experiencia_ine,hay_0_14,hay_psdf,hay_trabaja, hay_estudia) %>% 
  summarize(tiempo = sum(diferencia, na.rm = TRUE)) %>% 
  ungroup()


### Crear tiempos CUT
tiempos_cut <- 
  paradatos_cut %>% 
  group_by(id_per,recol_cut_sexo,recol_cut_experiencia_ine,
           noesyo_0_14, noesyo_psdf, noesyo_trabaja, noesyo_estudia) %>% 
  summarize(tiempo = sum(diferencia, na.rm = TRUE)) %>% 
  ungroup()


### Abrir funciones
source('processing/funciones.R')

###1. Botones de duración-----
### Abrir procesamiento de botones
source('processing/botones.R')


####1.1 CH-----
botones_ch <-
  recuento_ch %>%
  uni_fun(condicion_boton, rename_final = F) %>%
  filter(condicion_boton != 'Total') %>%
  ggplot(aes(x = str_wrap(condicion_boton, width = 10), y = Porcentaje, fill = as.factor(condicion_boton))) +  
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1, decimal.mark = ",")), 
            vjust = -0.8, color = "black", size = 3.5) +  # Cambiar el punto por la coma
  theme_bw(base_size = 11) +
  theme(
    legend.key.height = unit(1, "cm"),
    legend.text = element_text(size = 9, family = "Arial"),
    legend.position = "none",
    axis.text.x = element_text(size = 10, family = "Arial"),
    axis.text.y = element_text(size = 11, family = "Arial"),
    plot.caption = element_text(size = 9, family = "Arial"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    limits = c(0, 1.09), expand = c(0, 0), n.breaks = 10,
    labels = scales::percent_format(accuracy = 1, decimal.mark = ",")  # Cambiar el punto por la coma en el eje Y
  ) +
  scale_fill_manual(values = c("#4ab0c1", "#2d93ad", "#1B5868")) +
  labs(caption = paste0("N total de CH = ", prettyNum(nrow(recuento_ch), big.mark = ".", scientific = FALSE))) +
  xlab('') + ylab('')

####1.2 CUT-----
botones_cut <-
  recuento_cut %>%
  uni_fun(condicion_boton, rename_final = F) %>%
  filter(condicion_boton != 'Total') %>%
  ggplot(aes(x = str_wrap(condicion_boton, width = 10), y = Porcentaje, fill = as.factor(condicion_boton))) +  
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.2, decimal.mark = ",")), 
            vjust = -0.8, colour = "black", size = 3.5) +  # Cambiar el punto por la coma
  theme_bw(base_size = 11) +
  theme(
    legend.key.height = unit(1, "cm"),
    legend.text = element_text(size = 9, family = "Arial"),
    legend.position = "none",
    axis.text.x = element_text(size = 10, family = "Arial"),
    axis.text.y = element_text(size = 11, family = "Arial"),
    plot.caption = element_text(size = 9, family = "Arial"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    limits = c(0, 1.09), expand = c(0, 0), n.breaks = 10,
    labels = scales::percent_format(accuracy = 1, decimal.mark = ",")  # Cambiar el punto por la coma en el eje Y
  ) +
  scale_fill_manual(values = c("#B7A0CD", "#8760ac", "#513A67")) +
  labs(caption = paste0("N total de CUT = ", prettyNum(nrow(tiempos_cut), big.mark = ".", scientific = FALSE))) +
  xlab('') + ylab('')


####1.3 Juntar todo-----
botones_enut <- 
ggpubr::ggarrange(
  botones_ch, 
  botones_cut, 
  labels = c("CH", "CUT"),
  font.label = list(family = 'TT Arial'),
  ncol = 2, nrow = 1, 
  widths = c(1, 1.1)  # Ajustar la relación de ancho para agregar separación
)




###2. TIEMPOS DE DURACIÓN-----
####2.1 CH-----

promedio_ch <- tiempos_ch %>%
  mutate(tiempo = as.numeric(tiempo)) %>%  # Convertir 'tiempo' a numeric
  summarize(
    promedio = mean(tiempo, na.rm = TRUE)) %>% pull()

tiempos_por_modulo_ch <- 
  paradatos_ch %>% 
  mutate(modulo = if_else(modulo=='a3','a',modulo),
         diferencia = as.numeric(diferencia)) %>% 
  group_by(modulo,interview__key) %>% 
  filter(modulo!='horas') %>% 
  summarize(tiempo = sum(diferencia, na.rm = TRUE)) %>% 
  mutate(
    modulo = case_when(
      modulo == 'h' ~ 1,
      modulo == 'c' ~ 2,
      modulo == 'e' ~ 3,
      modulo == 'o' ~ 4,
      modulo == 'y' ~ 5,
      modulo == 'd' ~ 6,
      modulo %in% c('a','a3') ~ 7
    ),
    # Convertir en factor y definir orden de niveles
    modulo = haven::labelled(modulo, labels = c(
      "Módulo H: Identificación del Hogar" = 1,
      "Módulo C: Características sociodemográficas" = 2,
      "Módulo E: Educación" = 3,
      "Módulo O: Trabajo en la ocupación" = 4,
      "Módulo Y: Ingresos" = 5,
      "Módulo D: PSDF" = 6,
      "Módulo A: Ayudas que recibe el hogar" = 7
    ))
  )

# ggplot(tiempos_por_modulo, aes(x = 1, y = tiempo, group = modulo)) +
#   geom_boxplot(fill = "#2d93ad", alpha = 0.5) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
#     axis.text.y = element_text(size = 10),
#     axis.title = element_text(size = 11),
#     panel.grid.minor = element_blank()
#   ) +
#   scale_x_continuous(breaks = 1:7, labels = attr(tiempos_por_modulo$modulo, "labels")) +
#   xlab("Módulo") +
#   ylab("Tiempo (min)")

colores_ch <- c(
  "#b2e0e7",  # Color 1
  "#8cd4de",  # Color 2
  "#66c8d5",  # Color 3
  "#2d93ad",  # Color 4 (color medio)
  "#007f8d",  # Color 5
  "#006f78",  # Color 6
  "#005f66"   # Color 7
)

# Creamos el gráfico
####boxplot por módulo
plot_ch_modulo <- 
ggplot(tiempos_por_modulo_ch, aes(x = factor(modulo, levels = 1:7,
                                          labels = c(
                                            "Módulo H",
                                            "Módulo C",
                                            "Módulo E",
                                            "Módulo O",
                                            "Módulo Y",
                                            "Módulo D",
                                            "Módulo A")),
                               y = tiempo,
                               fill = factor(modulo))) +  # Usamos modulo para el relleno
  geom_boxplot() +
  scale_fill_manual(values = colores_ch) +  # Aplicamos la paleta de colores
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


tiempo_ch <- ggplot() +
  geom_histogram(data = tiempos_ch, aes(x = as.numeric(tiempo)), fill = "#2d93ad", alpha = 0.5, bins = 30) +
  theme_bw()+
  theme(
    legend.key.height = unit(1, "cm"),
    legend.text = element_text(size = 9, family = "arial"),
    legend.position = "none",
    axis.text.x = element_text(size = 10, family = "arial"),
    axis.text.y = element_text(size = 11, family = "arial"),
    plot.caption = element_text(size = 9, family = "arial"),
    panel.grid.minor = element_blank()
  ) +
  ylab('Frecuencia') +
  xlab('') +
  labs(caption = paste0("N total de CH = ", prettyNum(nrow(tiempos_ch), big.mark = ".", decimal.mark = ',',scientific = FALSE))) +
  geom_vline(aes(xintercept = promedio_ch),
             color = "black",linetype="dashed") +
  annotate("label", x = promedio_ch + 19.5, y = 4000,
           label = paste0(bquote("x\u0305"), " total = ", 
                          # format(round(promedio_ch, digits = 2), decimal.mark = ",")),
                          decimal_a_minutos(round(promedio_ch, digits = 2)), ' min'), 
           size = 3)

tiempos_total_ch <- tiempos_ch %>%
  mutate(tiempo = as.numeric(tiempo)) %>%  # Convertir 'tiempo' a numeric
  group_by(hhsize2) %>% 
  summarize(
    'Mínimo' = decimal_a_minutos(min(tiempo, na.rm = TRUE)),
    'Mediana' = decimal_a_minutos(median(tiempo, na.rm = TRUE)),
    'Promedio' = decimal_a_minutos(mean(tiempo, na.rm = TRUE)),
    'Percentil 01' = decimal_a_minutos(quantile(tiempo, 0.01, na.rm = TRUE)),
    'Percentil 05' = decimal_a_minutos(quantile(tiempo, 0.05, na.rm = TRUE)),
    'Percentil 10' = decimal_a_minutos(quantile(tiempo, 0.1, na.rm = TRUE)),
    'Percentil 25' = decimal_a_minutos(quantile(tiempo, 0.25, na.rm = TRUE)),
    'Percentil 33' = decimal_a_minutos(quantile(tiempo, 0.33, na.rm = TRUE)),
    'Percentil 60' = decimal_a_minutos(quantile(tiempo, 0.60, na.rm = TRUE)),
    'Percentil 75' = decimal_a_minutos(quantile(tiempo, 0.75, na.rm = TRUE)),
    'Percentil 90' = decimal_a_minutos(quantile(tiempo, 0.90, na.rm = TRUE)),
    'Percentil 95' = decimal_a_minutos(quantile(tiempo, 0.95, na.rm = TRUE)),
    'Percentil 99' = decimal_a_minutos(quantile(tiempo, 0.99, na.rm = TRUE)),
    'Máximo' = decimal_a_minutos(max(tiempo, na.rm = TRUE)),
    'Des.Est' = sd(tiempo, na.rm = TRUE),
    'N de Casos' = n()
  ) 

writexl::write_xlsx(tiempos_total_ch, 'tiempos_ch_n_integrantes')

#### Regresión -----

lm_ch <- tiempos_ch %>% 
  filter(!is.na(recol_ch_experiencia_ine)) %>%
  lm(tiempo ~ hhsize2 + recol_ch_experiencia_ine + hay_0_14 + hay_psdf + hay_trabaja, data = .)

summary(lm_ch)

resumen <- broom::tidy(lm_ch) %>% 
  mutate(estimado_minutos = map_chr(estimate, decimal_a_minutos), .after = estimate)

r2_value <- round(summary(lm_ch)$r.squared, 2)

plot_model(lm_ch, vline.color = "#2d93ad", value.offset = .3) +
  labs(y = "Coeficientes ajustados en minutos", x = "Variables predictoras") +
  labs(title = NULL) +
  theme_bw(base_size = 11)+
  theme(
    legend.key.height = unit(1, "cm"),
    legend.text = element_text(size = 9, family = "Arial"),
    legend.position = "none",
    axis.text.x = element_text(size = 10, family = "Arial"),
    axis.text.y = element_text(size = 11, family = "Arial"),
    plot.caption = element_text(size = 9, family = "Arial"),
    panel.grid.minor = element_blank()
  ) +
  geom_text(
    aes(label = paste(resumen$estimado_minutos[-1],'***')),  # Ajusta las coordenadas según necesites
    size = 4.5,   # Tamaño de la etiqueta
    hjust = 0.2,  # Ajuste horizontal para las etiquetas (similar a la posición de los valores)
    vjust = -0.5,  # Ajuste vertical para que no se sobrepongan con las líneas
    family = "Arial"
  ) +
  labs(caption = glue::glue(
    "Todos los predictores tienen p-valor < 5%\n",
    "{expression(R^2)} = {r2_value}"
  )
  )
  # labs(caption = bquote(paste("R"^2, " = ", .(r2_value)))) +



tiempo_neto <- 
  paradatos_ch %>% 
  mutate(diferencia = as.numeric(diferencia)) %>% 
  group_by(interview__key, modulo, hhsize, sdt_cd_ch) %>% 
  summarize(conteo = n(),
            tiempo = sum(diferencia, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(neto = tiempo/conteo)




####2.2 CUT-----

tiempos_por_modulo_cut <- 
  paradatos_cut %>% 
  mutate(diferencia = as.numeric(diferencia)) %>% 
  group_by(modulo,id_per) %>% 
  filter(modulo!='horas') %>% 
  summarize(tiempo = sum(diferencia, na.rm = TRUE)) %>% 
  mutate(
    modulo = case_when(
      modulo == 'cd' ~ 1,
      modulo == 'to' ~ 2,
      modulo == 'tc' ~ 3,
      modulo == 'td' ~ 4,
      modulo == 'tv' ~ 5,
      modulo == 'cp' ~ 6,
      modulo == 'ed' ~ 7,
      modulo == 'vs' ~ 8,
      modulo == 'sim' ~ 9,
      modulo == 'bs' ~ 10,
      modulo == 'id' ~ 11
      
    ),
    # Convertir en factor y definir orden de niveles
    modulo = haven::labelled(modulo, labels = c(
      "Módulo CD: Contextualización de los días asignados" = 1,
      "Módulo TO: Trabajo en la ocupación" = 2,
      "Módulo TC: Trabajo de cuidados no remunerado" = 3,
      "Módulo TD: Trabajo doméstico no remunerado" = 4,
      "Módulo TV: Trabajo voluntario y ayudas a otros hogares" = 5,
      "Módulo CP: Cuidados personales" = 6,
      "Módulo ED: Educación" = 7,
      "Módulo VS: Ocio y vida social" = 8,
      "Módulo SIM: Simultaneidad" = 9,
      "Módulo BS: Bienestar subjetivo" = 10,
      "Módulo ID: Características específicas de la persona" = 11
    ))
  )


colores <- c(
  "#d3cce3",  # Color 1 - Muy claro
  "#bfa9d6",  # Color 2
  "#a985ca",  # Color 3
  "#9361be",  # Color 4
  "#7d3db2",  # Color 5 - Ligeramente claro
  "#8760ac",  # Color 6 - Color central
  "#6f5291",  # Color 7 - Ligeramente oscuro
  "#5e4577",  # Color 8
  "#4c3860",  # Color 9
  "#3a2b4a",  # Color 10
  "#281f35"   # Color 11 - Muy oscuro
)

ggplot(tiempos_por_modulo_cut, aes(x = factor(modulo, levels = 1:11,
                                             labels = c(
                                               "Módulo CD",
                                               "Módulo TO",
                                               "Módulo TC",
                                               "Módulo TD",
                                               "Módulo TV",
                                               "Módulo CP",
                                               "Módulo ED",
                                               "Módulo VS",
                                               "Módulo SIM",
                                               "Módulo BS",
                                               "Módulo ID"
                                             )),
                                  y = tiempo,
                                  fill = factor(modulo))) +  # Usamos modulo para el relleno
  geom_boxplot() +
  scale_fill_manual(values = colores) +  # Aplicamos la paleta de colores
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

tiempos_total_cut <- tiempos_cut %>%
  mutate(tiempo = as.numeric(tiempo)) %>%  # Convertir 'tiempo' a numeric
  summarize(
    'Mínimo' = decimal_a_minutos(min(tiempo, na.rm = TRUE)),
    'Mediana' = decimal_a_minutos(median(tiempo, na.rm = TRUE)),
    'Promedio' = decimal_a_minutos(mean(tiempo, na.rm = TRUE)),
    'Percentil 01' = decimal_a_minutos(quantile(tiempo, 0.01, na.rm = TRUE)),
    'Percentil 05' = decimal_a_minutos(quantile(tiempo, 0.05, na.rm = TRUE)),
    'Percentil 10' = decimal_a_minutos(quantile(tiempo, 0.1, na.rm = TRUE)),
    'Percentil 25' = decimal_a_minutos(quantile(tiempo, 0.25, na.rm = TRUE)),
    'Percentil 33' = decimal_a_minutos(quantile(tiempo, 0.33, na.rm = TRUE)),
    'Percentil 60' = decimal_a_minutos(quantile(tiempo, 0.60, na.rm = TRUE)),
    'Percentil 75' = decimal_a_minutos(quantile(tiempo, 0.75, na.rm = TRUE)),
    'Percentil 90' = decimal_a_minutos(quantile(tiempo, 0.90, na.rm = TRUE)),
    'Percentil 95' = decimal_a_minutos(quantile(tiempo, 0.95, na.rm = TRUE)),
    'Percentil 99' = decimal_a_minutos(quantile(tiempo, 0.99, na.rm = TRUE)),
    'Máximo' = decimal_a_minutos(max(tiempo, na.rm = TRUE)),
    'Des.Est' = sd(tiempo, na.rm = TRUE),
    'N de Casos' = n()
  )

promedio_cut <- tiempos_cut %>%
  mutate(tiempo = as.numeric(tiempo)) %>%  # Convertir 'tiempo' a numeric
  summarize(
    promedio = mean(tiempo, na.rm = TRUE)) %>% pull()

tiempo_cut <-
ggplot() +
  geom_histogram(data = tiempos_cut, aes(x = as.numeric(tiempo)), fill = "#8760ac", alpha = 0.5, bins = 30) +
  theme_bw()+
  theme(
    legend.key.height = unit(1, "cm"),
    legend.text = element_text(size = 9, family = "Arial"),
    legend.position = "none",
    axis.text.x = element_text(size = 10, family = "Arial"),
    axis.text.y = element_text(size = 11, family = "Arial"),
    plot.caption = element_text(size = 9, family = "Arial"),
    panel.grid.minor = element_blank()
  )  +
  # scale_x_continuous(expand = c(0, 0), limits = c(0, 60)) +
  ylab('Frecuencia') +
  xlab('') +
  labs(caption = paste0("N total de CUT = ", prettyNum(nrow(tiempos_cut), big.mark = ".", decimal.mark = ',',scientific = FALSE))) +
  geom_vline(aes(xintercept = promedio_cut),
             color = "black",linetype="dashed") +
  annotate("label", x = promedio_cut+19.5, y = 4000,
           label =
             paste0(bquote("x\u0305"), " total = ", 
                    decimal_a_minutos(round(promedio_cut, digits = 2)), ' min'),
           size = 3
  )

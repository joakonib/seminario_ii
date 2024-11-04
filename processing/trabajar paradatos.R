####Procesamiento paradatos:
rm(list=ls())
options(decimal.mark = ",")

###0.1 Cargar Paquetes----
pacman::p_load(
  tidyverse,
  data.table,
  tidylog,
  haven,
  ggplot2,
  RColorBrewer
)

###0.2 Funciones----
uni_fun <- function(df, x, rename_final = TRUE) {
  col_name <- as.character(substitute(x))
  question_label <- attributes(df[[col_name]])$label
  
  # Fallback to col_name if question_label is NULL
  if (is.null(question_label)) {
    question_label <- col_name
  }
  
  resultado <- tibble(!!col_name := levels(forcats::as_factor(unique(df[[col_name]])))) %>% 
    add_row(!!col_name := "Total") %>%
    left_join(
      df %>%
        tidyr::drop_na({{x}}) %>% 
        mutate({{x}} := forcats::as_factor({{x}})) %>%
        dplyr::count({{x}}) %>%
        mutate(Porcentaje = n / sum(n)) %>%
        dplyr::rename(Frecuencia = n) %>%
        janitor::adorn_totals("row"),
      by = col_name
    ) %>%
    mutate(across(matches("Frecuencia|Porcentaje"), ~ ifelse(is.na(.), 0, .)))
  
  if (rename_final) {
    resultado <- resultado %>% rename(!!question_label := {{x}})
  }
  
  return(resultado)
}

# Aclarar el color
lighten_color <- function(color, factor) {
  rgb_values <- col2rgb(color)  # Convertir a RGB
  rgb_values_light <- pmin(rgb_values + (255 - rgb_values) * factor, 255)  # Aclarar
  rgb(rgb_values_light[1] / 255, rgb_values_light[2] / 255, rgb_values_light[3] / 255)  # Convertir a hex
}

darken_color <- function(color, factor) {
  rgb_values <- col2rgb(color)  # Convertir a RGB
  rgb_values_dark <- pmax(rgb_values - rgb_values * factor, 0)  # Oscurecer
  rgb(rgb_values_dark[1] / 255, rgb_values_dark[2] / 255, rgb_values_dark[3] / 255)  # Convertir a hex
}



###1. Abrir Paradatos----
paradatos <- readRDS('//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.1_integrar/data/otros/paradata_tesis.RDS') %>% 
  relocate(sdt_cd_cut, .after = sdt_cd_ch)

###2. Abrir Base ENUT----
enut <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.1_integrar/data/enut_5.1_integrada.RDS") %>% 
  mutate(v4 = if_else(coalesce(o1, 0) == 1 | coalesce(o2, 0) == 1 | coalesce(o3, 0) == 1, 1, 0),
         d8 = coalesce(d8,0)) %>%
  group_by(interview__key) %>%
  mutate(
    noesyo_0_14 = sum(c3 <= 14 & d8!=1, na.rm = TRUE) - (c3 <= 14 & d8!=1),
    noesyo_psdf = sum(d8 == 1, na.rm = TRUE) - (d8 == 1),
    noesyo_trabaja = sum(v4, na.rm = TRUE) - (v4 == 1),
    idoneo = as.numeric(informante_idoneo== as.numeric(sub(".*-(\\d+)$", "\\1", id_per)))
    ) %>%
  ungroup() %>% 
  select(interview__key,id_per2, 
         hhsize,
         noesyo_0_14,
         noesyo_psdf,noesyo_trabaja,
         idoneo)


####2.1 Parad_ch----
paradatos_ch <- paradatos[role == 1 & event == "AnswerSet" & cuestionario == 'ch' & sdt_cd_ch==11][,
                                                                                                   diferencia := shift(hora_real, type = "lead", fill = hora_real[.N]) - hora_real, 
                                                                                                   by = interview__id][
                                                                                                     quantile(diferencia, 0.991, na.rm = TRUE) > diferencia
                                                                                                   ][, 
                                                                                                     diferencia := fifelse(diferencia < 0, as.difftime(0, units = "secs"), diferencia)
                                                                                                     
                                                                                                   ][, 
                                                                                                     diferencia := as.difftime(as.numeric(diferencia) / 60, units = "mins")
                                                                                                     
                                                                                                   ]

# Actualizar `fecha_antes` después de que `conteo_i` ha sido creado
paradatos_ch <- paradatos_ch[, conteo_i := seq_len(.N), by = interview__key]
paradatos_ch[, fecha :=  format(hora_real, "%Y-%m-%d")]
paradatos_ch[, fecha_antes := shift(fecha, type = "lead"), by = interview__key]
paradatos_ch[, fecha_antes := fifelse(conteo_i == 1, fecha, fecha_antes)]

# Calcular `dia_dif` y `diferencia` con condiciones
paradatos_ch[, dia_dif_for := fifelse(fecha > fecha_antes, 1, 0)]
paradatos_ch[, dia_dif_rew := fifelse(fecha < fecha_antes, 1, 0)]
paradatos_ch[, dia_dif := fifelse(fecha != fecha_antes, 1, 0)]
paradatos_ch[, dia_dif_mismo := fifelse(fecha == fecha_antes, 1, 0)]
paradatos_ch[, diferencia := fifelse(conteo_i == 1, as.difftime(0, units = "mins"), diferencia)]
paradatos_ch[, diferencia := fifelse(dia_dif_for == 1, as.difftime(0, units = "mins"), diferencia)]
paradatos_ch[, diferencia := fifelse(dia_dif_rew == 1, as.difftime(0, units = "mins"), diferencia)]

####BOTONES: CONTAR CUÁNTOS CUÁLES SE APLICARON DE FORMA CONSECUTIVA
paradatos_ch[, var_antes := shift(var, type = "lag"), by = interview__key]
paradatos_ch[, conteo_dif_termino := fifelse(var=='termino_ch', max(conteo_i)-conteo_i,NA), by = interview__key]

####2.1.1 tiempos----
tiempos_ch <- 
  paradatos_ch %>% 
  group_by(interview__key) %>% 
  summarize(tiempo = sum(diferencia, na.rm = TRUE)) %>% 
  ungroup()

tiempos_total_ch <- 
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

promedio_ch <- 
  paste0("Promedios CH:","\n",
         round(tiempos_total_ch$promedio,digits=2),"min")



  
####2.1.2 botones----

####BOTONES: CONTAR CUÁNTOS SE APLICARON MÁS DE UNA VEZ (INICIO Y TERMINO)
botones_ch <- 
  paradatos_ch %>% 
  filter(modulo == 'horas') %>% 
  select(interview__key,order,hora_real,parameters,var,sm,sdt_cd_ch,sdt_cd_cut,
         cdf,fecha,conteo_i,conteo_dif_termino) 


hog_dic23 <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.6_ponderar/cdf/intermedias/cdf_hog_inicial_dic23.RDS") %>%
  select(interview__key,
         recol_ch_sexo,
         recol_ch_experiencia_ine,
         recol_ch_nivel_educacional) %>%     
  mutate(recol_ch_experiencia_ine = haven::labelled(
    recol_ch_experiencia_ine,
    labels = c("Sí" = 1, 
               "No" = 0),
    label = "Experiencia previa INE"))
  

recuento_ch <- 
  botones_ch %>% 
  filter(var %in% c("inicio_ch", "termino_ch")) %>%
  group_by(interview__key, var) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = var, values_from = count, values_fill = 0
              ) %>% 
  left_join(paradatos_ch %>% distinct(interview__key, .keep_all =T) %>% select(interview__key, sdt_cd_ch)) %>% 
  left_join(hog_dic23, by = 'interview__key')


recuento_ch <- 
  recuento_ch %>% 
  bind_rows(tiempos_ch %>% filter(!interview__key %in% recuento_ch$interview__key) %>%  select(interview__key)) %>% 
  ungroup() %>% 
  # mutate(
    # condicion_boton = if_else(inicio_ch==1 & termino_ch==1, 1,0))
  mutate(
    condicion_boton = case_when(
           is.na(inicio_ch) & is.na(termino_ch) ~ 99,
           inicio_ch==1 & termino_ch==1 ~ 1,
           TRUE ~ 0
         ),
         condicion_boton = haven::labelled(condicion_boton,
                         labels = c("Apretados correctamente" = 1, 
                                    "Apretados más de una vez" = 0,
                                    "No se apretó el botón" = 99),
                         label = "Veces que ha sido apretado el botón de Inicio y Termino")
         ) 



# saveRDS(recuento_ch,'output/recuento_ch.RDS')



# recuento_ch2 <- 
#   recuento_ch %>% 
#   left_join(botones_ch %>% filter(var=='inicio_ch' & interview__key %in% recuento$interview__key[recuento$condicion_boton==1]) %>% select(interview__key,inicio_ch_hora = hora_real, inicio_order = order, inicio_conteo_i = conteo_i)) %>% 
#   left_join(botones_ch %>% filter(var=='termino_ch' & interview__key %in% recuento$interview__key[recuento$condicion_boton==1]) %>% select(interview__key,termino_ch_hora = hora_real, termino_order = order, termino_conteo_i = conteo_i,conteo_dif_termino)
#             
#   ) %>% 
#   relocate(termino_ch_hora, .after = inicio_ch_hora) %>% 
#   relocate(termino_order, .after = inicio_order)


# recuento2 <- recuento %>% 
#   mutate(condicion_termino_first = if_else(termino_conteo_i < inicio_conteo_i,1,0),
#          diferencia_n = termino_conteo_i - inicio_conteo_i,
#          diferencia = if_else(diferencia_n <= 8,1,0)
#          ) 
# 
# ver <- paradatos_ch %>% filter(interview__key=='01-22-41-47')

# %>% 
#   filter(diferencia==0) %>% 
#   arrange(diferencia_n)

####2.2 Parad_cut----

###CREAR ID_PER NORMAL
paradatos_cut <- paradatos[role == 1 & event == "AnswerSet" & cuestionario == 'cut' & sdt_cd_cut==11][,
                                                                                                   diferencia := shift(hora_real, type = "lead", fill = hora_real[.N]) - hora_real, 
                                                                                                   by = id_per2][
                                                                                                     quantile(diferencia, 0.991, na.rm = TRUE) > diferencia
                                                                                                   ][, 
                                                                                                     diferencia := fifelse(diferencia < 0, as.difftime(0, units = "secs"), diferencia)
                                                                                                     
                                                                                                   ][, 
                                                                                                     diferencia := as.difftime(as.numeric(diferencia) / 60, units = "mins")
                                                                                                     
                                                                                                   ][,
                                                                                                     id_per := paste(interview__key,sub(".*-", "", id_per2), sep = "-") 
                                                                                                   ]






# Actualizar `fecha_antes` después de que `conteo_i` ha sido creado

paradatos_cut <- paradatos_cut[, conteo_i := seq_len(.N), by = id_per]
paradatos_cut[, fecha :=  format(hora_real, "%Y-%m-%d")]
paradatos_cut[, fecha_antes := shift(fecha, type = "lead"), by = id_per]
paradatos_cut[, fecha_antes := fifelse(conteo_i == 1, fecha, fecha_antes)]


# Calcular `dia_dif` y `diferencia` con condiciones
paradatos_cut[, dia_dif_for := fifelse(fecha > fecha_antes, 1, 0)]
paradatos_cut[, dia_dif_rew := fifelse(fecha < fecha_antes, 1, 0)]
paradatos_cut[, dia_dif := fifelse(fecha != fecha_antes, 1, 0)]
paradatos_cut[, dia_dif_mismo := fifelse(fecha == fecha_antes, 1, 0)]
paradatos_cut[, diferencia := fifelse(conteo_i == 1, as.difftime(0, units = "mins"), diferencia)]
paradatos_cut[, diferencia := fifelse(dia_dif_for == 1, as.difftime(0, units = "mins"), diferencia)]
paradatos_cut[, diferencia := fifelse(dia_dif_rew == 1, as.difftime(0, units = "mins"), diferencia)]


####BOTONES: CONTAR CUÁNTOS SE APLICARON MÁS DE UNA VEZ (INICIO Y TERMINO)
####BOTONES: CONTAR CUÁNTOS CUÁLES SE APLICARON DE FORMA CONSECUTIVA
paradatos_cut[, var_antes := shift(var, type = "lag"), by = id_per2]
paradatos_cut[, conteo_dif_termino := fifelse(var=='termino_ch', max(conteo_i)-conteo_i,NA), by = id_per2]

####2.2.1 tiempos----
tiempos_cut <- 
  paradatos_cut %>% 
  group_by(id_per) %>% 
  summarize(tiempo = sum(diferencia, na.rm = TRUE)) %>% 
  ungroup()

tiempos_total_cut <- 
  tiempos_cut %>% 
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

promedio_cut <- 
  paste0("Promedios CUT:","\n",
         round(tiempos_total_cut$promedio,digits=2),"min")



# +
#   annotate("label", x = unique(cut_plot_reg$promedio[cut_plot_reg$region=="Total"])+10.5, y = 4.5,
#            label =   
#              paste0(bquote("x\u0305")," total = ", round(unique(cut_plot_reg$promedio[cut_plot_reg$region=="Total"]),digits=2)),
#            size = 3)






####2.2.2 botones----
botones_cut <- 
  paradatos_cut %>% 
  filter(modulo == 'horas') %>% 
  select(id_per2,id_per,order,hora_real,parameters,var,sm,sdt_cd_ch,sdt_cd_cut,
         cdf,fecha,conteo_i,conteo_dif_termino) 


per_dic23 <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.6_ponderar/cdf/intermedias/cdf_per_inicial_dic23.RDS") %>%
  select(id_per,
         recol_cut_sexo,
         recol_cut_experiencia_ine,
         recol_cut_nivel_educacional) %>% 
  mutate(recol_cut_experiencia_ine = haven::labelled(
    recol_cut_experiencia_ine,
     labels = c("Sí" = 1, 
               "No" = 0),
    label = "Experiencia previa INE"))


recuento_cut <- 
  botones_cut %>% 
  filter(var %in% c("inicio_cut", "termino_cut")) %>%
  group_by(id_per2, id_per,var) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = var, values_from = count, values_fill = 0
  ) %>% 
  left_join(paradatos_cut %>% distinct(id_per2, .keep_all =T) %>% select(id_per2, sdt_cd_cut)) %>% 
  left_join(per_dic23, by = 'id_per')


recuento_cut <- 
  recuento_cut %>% 
  bind_rows(tiempos_cut %>% filter(!id_per %in% recuento_cut$id_per) %>%  select(id_per)) %>% 
  ungroup() %>% 
  # mutate(
  # condicion_boton = if_else(inicio_ch==1 & termino_ch==1, 1,0))
  mutate(
    condicion_boton = case_when(
      is.na(inicio_cut) ~ 99,
      is.na(termino_cut) ~ 99,
      inicio_cut==1 & termino_cut==1 ~ 1,
      TRUE ~ 0
    ),
    condicion_boton = haven::labelled(condicion_boton,
                                      labels = c("Apretados correctamente" = 1, 
                                                 "Apretados más de una vez" = 0,
                                                 "No se apretó el botón" = 99),
                                      label = "Veces que ha sido apretado el botón de Inicio y Termino")
  ) 


consolidado_tiempos <- 
dput(bind_rows(tiempos_total_ch %>% mutate(encuesta = 'CH', .before=1),
               tiempos_total_ch %>% mutate(encuesta = 'CUT', .before=1))) 


writexl::write_xlsx(consolidado_tiempos,"consolidado_tiempos.xlsx")

# agregar 0-4
# agregar 5-14
# agregar psdf
# agregar informante_idoneo


saveRDS(paradatos_ch, 'paradatos_ch.RDS')
saveRDS(paradatos_cut, 'paradatos_cut.RDS')
saveRDS(enut, 'enut.RDS')
saveRDS(hog_dic23, 'hogares_dic23.RDS')
saveRDS(per_dic23, 'per_dic23.RDS')

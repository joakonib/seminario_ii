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
    noesyo_estudia = sum(e4 == 1, na.rm = TRUE) - (e4==1),
    hay_0_14 = as.numeric(sum(c3 <= 14 & d8!=1, na.rm = TRUE) > 1),
    hay_psdf = as.numeric(sum(d8 == 1, na.rm = TRUE) > 1),
    hay_trabaja = as.numeric(sum(v4, na.rm = TRUE) > 1),
    hay_estudia = sum(e4 == 1, na.rm = TRUE),
    idoneo = as.numeric(informante_idoneo== as.numeric(sub(".*-(\\d+)$", "\\1", id_per)))
    ) %>%
  ungroup() %>% 
  group_by(interview__key) %>% mutate(hhsize = n()) %>% ungroup() %>% 
  mutate(hhsize2 = case_when(
    hhsize == 1 ~ 1,
    hhsize == 2 ~ 2,
    hhsize == 3 ~ 3,
    hhsize == 4 ~ 4,
    hhsize == 5 ~ 5,
    hhsize >= 6 ~ 6,
  ),
  hhsize2 = haven::labelled(
    hhsize2,
    labels = c("1 integrante" = 1,
               "2 integrantes" = 2,
               "3 integrantes" =  3,
               "4 integrantes" = 4,
               "5 integrantes" =  5,
               "6 o más integrantes" = 6),
    label = "N Integrantes del hogar"),
  hay_0_14 = haven::labelled(
    hay_0_14,
    labels = c("Sí hay 0-14 años" = 1,
               "No hay 0-14 años" = 0),
    label = "Hay personas de 0-14 años"),
  hay_psdf = haven::labelled(
    hay_psdf,
    labels = c("Sí hay PSDF" = 1,
               "No hay PSDF" = 0),
    label = "Hay PSDF"),
  hay_trabaja = haven::labelled(
    hay_trabaja,
    labels = c("Sí hay personas ocupadas" = 1,
               "No hay personas ocupadas" = 0),
    label = "Hay personas ocupadas"),
  hay_estudia = haven::labelled(
    hay_estudia,
    labels = c("Sí hay personas estudiando" = 1,
               "No hay personas estudiando" = 0),
    label = "Hay personas estudiando"),
  hhsize2 = haven::as_factor(hhsize2),
  hay_0_14 = haven::as_factor(hay_0_14),
  hay_psdf = haven::as_factor(hay_psdf),
  hay_trabaja = haven::as_factor(hay_trabaja)) %>% 
  select(interview__key,id_per2, 
         hhsize, hhsize2,
         noesyo_0_14,
         noesyo_psdf,noesyo_trabaja,
         noesyo_estudia,
         hay_0_14,hay_psdf,hay_trabaja,
         hay_estudia,
         idoneo)


paradatos <- 
  paradatos %>% 
  left_join(enut %>% select(interview__key,hhsize, hhsize2,hay_0_14,hay_psdf,hay_trabaja, hay_estudia), by = 'interview__key') %>% 
  left_join(enut %>% select(id_per2,noesyo_0_14,
                            noesyo_psdf,noesyo_trabaja,
                            noesyo_estudia,
                            idoneo), by = 'id_per2') %>% 
  mutate(correlativo = paste(interview__key,order, sep = '-')) %>% 
  distinct(correlativo, .keep_all =T) %>% 
  select(-correlativo)

# test <- paradatos2 %>% filter(interview__key=='00-02-57-10')

####2.1 Procesamiento: Parad_ch----
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
paradatos_ch[, diferencia := fifelse(diferencia < 0, as.difftime(0, units = "mins"), diferencia)]
# paradatos_ch[, diferencia := fifelse(dia_dif_rew == 1, as.difftime(0, units = "mins"), diferencia)]

####BOTONES: CONTAR CUÁNTOS CUÁLES SE APLICARON DE FORMA CONSECUTIVA
paradatos_ch[, var_antes := shift(var, type = "lag"), by = interview__key]
paradatos_ch[, conteo_dif_termino := fifelse(var=='termino_ch', max(conteo_i)-conteo_i,NA), by = interview__key]



### Agregar datos sobre encuestador

hog_dic23 <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.6_ponderar/cdf/intermedias/cdf_hog_inicial_dic23.RDS") %>%
  select(interview__key,
         recol_ch_sexo,
         recol_ch_experiencia_ine,
         recol_ch_nivel_educacional) %>%     
  mutate(recol_ch_experiencia_ine = haven::labelled(
    recol_ch_experiencia_ine,
    labels = c("Con exp previa INE" = 1, 
               "Sin exp previa INE" = 0),
    label = "Experiencia previa INE"))


paradatos_ch <- 
  paradatos_ch %>% left_join(hog_dic23, by = 'interview__key')


####2.1.1 tiempos----
# 
# 
# tiempos_total_ch <- 
#   tiempos_ch %>% 
#   summarize(min = min(tiempo, na.rm = TRUE),
#             mediana = median(tiempo, na.rm = TRUE),
#             promedio = mean(tiempo, na.rm = TRUE),
#             p01 = quantile(tiempo, 0.01, na.rm = TRUE),
#             p05 = quantile(tiempo, 0.05, na.rm = TRUE),
#             p10 = quantile(tiempo, 0.1, na.rm = TRUE),
#             p25 = quantile(tiempo, 0.25, na.rm = TRUE),
#             p33 = quantile(tiempo, 0.33, na.rm = TRUE),
#             p60 = quantile(tiempo, 0.60, na.rm = TRUE),
#             p75 = quantile(tiempo, 0.75, na.rm = TRUE),
#             p90 = quantile(tiempo, 0.90, na.rm = TRUE),
#             p95 = quantile(tiempo, 0.95, na.rm = TRUE),
#             p99 = quantile(tiempo, 0.99, na.rm = TRUE),
#             max = max(tiempo, na.rm = T),
#             sd = sd(tiempo, na.rm = TRUE),
#             n = n())
# 
# promedio_ch <- 
#   paste0("Promedios CH:","\n",
#          round(tiempos_total_ch$promedio,digits=2),"min")

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
paradatos_cut[, diferencia := fifelse(diferencia < 0, as.difftime(0, units = "mins"), diferencia)]
# paradatos_cut[, diferencia := fifelse(dia_dif_rew == 1, as.difftime(0, units = "mins"), diferencia)]


####BOTONES: CONTAR CUÁNTOS SE APLICARON MÁS DE UNA VEZ (INICIO Y TERMINO)
####BOTONES: CONTAR CUÁNTOS CUÁLES SE APLICARON DE FORMA CONSECUTIVA
paradatos_cut[, var_antes := shift(var, type = "lag"), by = id_per2]
paradatos_cut[, conteo_dif_termino := fifelse(var=='termino_ch', max(conteo_i)-conteo_i,NA), by = id_per2]

 ### Agregar datos sobre encuestador

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

paradatos_cut <- 
  paradatos_cut %>% left_join(per_dic23, by = 'id_per')

####2.2.1 tiempos----
# tiempos_cut <- 
#   paradatos_cut %>% 
#   group_by(id_per) %>% 
#   summarize(tiempo = sum(diferencia, na.rm = TRUE)) %>% 
#   ungroup()
# 
# tiempos_total_cut <- 
#   tiempos_cut %>% 
#   summarize(min = min(tiempo, na.rm = TRUE),
#             mediana = median(tiempo, na.rm = TRUE),
#             promedio = mean(tiempo, na.rm = TRUE),
#             p01 = quantile(tiempo, 0.01, na.rm = TRUE),
#             p05 = quantile(tiempo, 0.05, na.rm = TRUE),
#             p10 = quantile(tiempo, 0.1, na.rm = TRUE),
#             p25 = quantile(tiempo, 0.25, na.rm = TRUE),
#             p33 = quantile(tiempo, 0.33, na.rm = TRUE),
#             p60 = quantile(tiempo, 0.60, na.rm = TRUE),
#             p75 = quantile(tiempo, 0.75, na.rm = TRUE),
#             p90 = quantile(tiempo, 0.90, na.rm = TRUE),
#             p95 = quantile(tiempo, 0.95, na.rm = TRUE),
#             p99 = quantile(tiempo, 0.99, na.rm = TRUE),
#             max = max(tiempo, na.rm = T),
#             sd = sd(tiempo, na.rm = TRUE),
#             n = n())
# 
# promedio_cut <- 
#   paste0("Promedios CUT:","\n",
#          round(tiempos_total_cut$promedio,digits=2),"min")
# 
# 
# 
# 
# consolidado_tiempos <- 
# dput(bind_rows(tiempos_total_ch %>% mutate(encuesta = 'CH', .before=1),
#                tiempos_total_ch %>% mutate(encuesta = 'CUT', .before=1))) 
# 
# 
# writexl::write_xlsx(consolidado_tiempos,"consolidado_tiempos.xlsx")

# agregar 0-4
# agregar 5-14
# agregar psdf
# agregar informante_idoneo


saveRDS(paradatos_ch, 'paradatos_ch.RDS')
saveRDS(paradatos_cut, 'paradatos_cut.RDS')
saveRDS(enut, 'enut.RDS')
saveRDS(hog_dic23, 'hogares_dic23.RDS')
saveRDS(per_dic23, 'per_dic23.RDS')

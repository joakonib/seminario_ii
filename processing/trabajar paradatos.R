####Procesamiento paradatos:
rm(list=ls())

###Cargar Paquetes----
pacman::p_load(
  tidyverse,
  data.table,
  tidylog
)


paradatos <- readRDS('//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.1_integrar/data/otros/paradata_tesis.RDS') %>% 
  relocate(sdt_cd_cut, .after = sdt_cd_ch)

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
         noesyo_0_14,
         noesyo_psdf,noesyo_trabaja,
         idoneo)



paradatos_ch <- paradatos[role == 1 & event == "AnswerSet" & cuestionario == 'ch'][,
                                              diferencia := hora_real - shift(hora_real, 1, type = "lag"), 
                                              by = interview__id][,
                                                                  diferencia_original := shift(hora_real, type = "lead", fill = hora_real[.N]) - hora_real, 
                                                                  by = interview__id][
                                                                    quantile(diferencia_original, 0.999, na.rm = TRUE) > diferencia_original
                                                                  ][, 
                                                                    diferencia_original := fifelse(diferencia_original < 0, as.difftime(0, units = "secs"), diferencia_original)
                                                                    
                                                                  ][, 
                                                                    diferencia_original := as.difftime(as.numeric(diferencia_original) / 60, units = "mins")
                                                                    
                                                                  ]


# AGREGAR ESTO A MI DATA_TABLE
# mutate(
#   conteo_i = row_number(id),
#   fecha = format(tiempo_real, "%Y-%m-%d"),
#   fecha_antes = lead(fecha, order_by = id),
#   fecha_antes = if_else(conteo_i == 1, fecha, fecha_antes),
#   dia_dif = if_else(fecha != fecha_antes, 1,0),
#   tiempo_anterior = lag(tiempo_real, order_by = id),
#   fecha = format(tiempo_real, "%Y-%m-%d"),
#   diferencia = tiempo_real - tiempo_anterior,
#   diferencia = if_else(conteo_i == 1, as.difftime(0, units = "secs"), diferencia),
#   diferencia = if_else(dia_dif == 1, as.difftime(0, units = "secs"), diferencia)
#   
#   
# )



ola <- paradatos_ch[, .(sum_diferencia_original = sum(diferencia_original, na.rm = TRUE)), by = interview__key]



paradatos_cut <- 
  paradatos[role == 1 & event == "AnswerSet" & cuestionario == 'cut'][, diferencia := hora_real - shift(hora_real, 1, type = "lag"), 
                                                                     by = id_per2][,diferencia_original := shift(hora_real, type = "lead", fill = hora_real[.N]) - hora_real, 
                                                                                         by = id_per2][,id_per := paste(interview__key,"-",n_linea_p)]

paradatos_cut2 <- paradatos_cut %>% left_join(per_dic23,by='id_per'
  
)



saveRDS(paradatos_ch, "paradatos_ch.RDS")
saveRDS(paradatos_cut, "paradatos_cut.RDS")


hog_dic23 <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.6_ponderar/cdf/intermedias/cdf_hog_inicial_dic23.RDS") %>% 
  select(interview__key,
         recol_ch_sexo,
         recol_ch_experiencia_ine,
         recol_ch_nivel_educacional
         
         )



per_dic23 <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.6_ponderar/cdf/intermedias/cdf_per_inicial_dic23.RDS") %>% 
  select(id_per,
         recol_cut_sexo,
         recol_cut_experiencia_ine,
         recol_cut_nivel_educacional)
  


agregar 0-4
agregar 5-14
agregar psdf
agregar informante_idoneo


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


paradatos_ch <- paradatos[role == 1 & event == "AnswerSet" & cuestionario == 'ch'][,
                                              diferencia := hora_real - shift(hora_real, 1, type = "lag"), 
                                              by = interview__id][,
                                                                  diferencia_original := shift(hora_real, type = "lead", fill = hora_real[.N]) - hora_real, 
                                                                  by = interview__id]


paradatos_cut <- 
  paradatos[role == 1 & event == "AnswerSet" & cuestionario == 'cut'][, diferencia := hora_real - shift(hora_real, 1, type = "lag"), 
                                                                     by = id_per2][,diferencia_original := shift(hora_real, type = "lead", fill = hora_real[.N]) - hora_real, 
                                                                                         by = id_per2]

saveRDS(paradatos_ch, "paradatos_ch.RDS")
saveRDS(paradatos_cut, "paradatos_cut.RDS")


hog_dic23 <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.6_ponderar/cdf/intermedias/cdf_hog_inicial_dic23.RDS")
per_dic23 <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.6_ponderar/cdf/intermedias/cdf_per_inicial_dic23.RDS")

recol_ch_sexo
recol_ch_experiencia_ine
recol_ch_nivel_educacional
recol_ch_nvisitviv
recol_ch_nvisitcut_viv

per_dic23$recol_cut_sexo
per_dic23$recol_cut_experiencia_ine
per_dic23$recol_cut_nivel_educacional

agregar 0-4
agregar 5-14
agregar psdf
agregar informante_idoneo


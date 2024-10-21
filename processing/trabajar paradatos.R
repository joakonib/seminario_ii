####Procesamiento paradatos:
rm(list=ls())

###Cargar Paquetes----
pacman::p_load(
  tidyverse,
  data.table,
  tidylog
)


paradatos <- readRDS('//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.1_integrar/data/otros/paradata_tesis.RDS')


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

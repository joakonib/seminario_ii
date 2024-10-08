####Procesamiento paradatos:
rm(list=ls())

###Cargar Paquetes----
pacman::p_load(
  tidyverse,
  data.table,
  tidylog
)

###Abrir paradatos y filtrarlos----
paradata <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.1_integrar/data/enut_5.1.7_paradata_integrada_filtrada.RDS") %>% 

# Convertir a data.table (si es necesario)
setDT(paradata)

# Filtro paradatos para dejar solo gestiones hasta dic y cuestionario ch+cut
paradata <- paradata[sm %in% c(1:7, 88)][cuestionario %in% c('ch','cut')]

# Corrijo paradatos, sacando el id_per para cosas ch
paradata <- paradata[, id_per2 := ifelse(cuestionario == 'ch', interview__id, id_per2)][
  , .SD, .SDcols = !c("implicancia", "visita_cd_ch", "cdf", "sdt_cd_ch_ene24", "sdt_cdf_hog_ene24", "visita_cd_cut", "sdt_cd_cut_ene24")
]


###TRAERME LA BASE ENUT#####-------

enut <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.1_integrar/data/enut_5.1_integrada.RDS")

enut_ch <- enut %>% distinct(interview__key,.keep_all=T) %>% select(interview__id,sdt_cd_ch = sdt_cd_ch_dic23,
                                                                    cdf= sdt_cdf_hog_dic23)

enut_cut <- enut %>% select(id_per2,sdt_cd_cut = sdt_cd_cut_dic23)

remove(enut)

###POBLAR INFO EN PARADATA#####-------

paradata <- paradata %>% left_join(enut_ch, by = 'interview__id')
paradata <- paradata %>% left_join(enut_cut, by = 'id_per2')

remove(enut_ch,
       enut_cut)


###SIGO FILTRANDO#####-------

####SACO LOS CASOS DE LOS CUTs que no existen-----
paradata <- paradata %>% filter(
  !(is.na(sdt_cd_cut) & cuestionario=='cut')
)

####SACO LOS CASOS DE LOS CUTs que no son ni 11,12,22-----

paradata <- paradata %>% filter(
  cuestionario == 'cut' & sdt_cd_cut %in% c(11,12,22) |
  cuestionario == 'ch'
  )

saveRDS(paradata, '//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.1_integrar/data/otros/paradata_tesis.RDS')

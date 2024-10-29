####Procesamiento paradatos:
rm(list=ls())

###0.1 Cargar Paquetes----
pacman::p_load(
  tidyverse,
  data.table,
  tidylog,
  haven,
  ggplot2
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
         noesyo_0_14,
         noesyo_psdf,noesyo_trabaja,
         idoneo)


####2.1 Parad_ch----
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
# 
# # Agrupar y realizar todas las modificaciones en un solo bloque
# paradatos_ch3 <- paradatos_ch[, `:=`(
#   conteo_i = row_number(interview__key),
#   fecha = format(hora_real, "%Y-%m-%d")
# ), by = interview__key]
# 
# 

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
         recol_ch_nivel_educacional)


recuento <- 
  botones_ch %>% 
  filter(var %in% c("inicio_ch", "termino_ch")) %>%
  group_by(interview__key, var) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = var, values_from = count, values_fill = 0) %>% 
  left_join(paradatos_ch %>% distinct(interview__key, .keep_all =T) %>% select(interview__key, sdt_cd_ch)) %>% 
  left_join(hog_dic23, by = 'interview__key') %>% 
  mutate(condicion_boton = if_else(inicio_ch==1 & termino_ch==1, 1,0),
         condicion_boton = haven::labelled(condicion_boton,
                         labels = c("Apretados correctamente" = 1, 
                                    "Apretados más de una vez" = 0),
                         label = "Veces que ha sido apretado el botón de Inicio y Termino")
         ) 

saveRDS(recuento,'output/recuento_ch.RDS')


recuento %>%
  uni_fun(condicion_boton, rename_final = F) %>%
  filter(condicion_boton != 'Total') %>%
  ggplot(aes(x = condicion_boton, y = Porcentaje, fill=as.factor(condicion_boton))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Porcentaje,accuracy = .2)), 
            vjust = -0.2,position = position_dodge(.9), colour = "black",size=4) +
  theme_bw()+
  theme(legend.key.height=unit(1, "cm"),legend.text = element_text(lineheight = 0.6),
        legend.position = "none",
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0,1.01), expand = c(0, 0),n.breaks=10,labels=scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#4ab0c1", "#2d93ad")) +  # Cambia a scale_fill_manual
  labs(caption = paste0("N total de CH = ", nrow(recuento)))+
  xlab('')




recuento <- 
  recuento %>% 
  left_join(botones_ch %>% filter(var=='inicio_ch' & interview__key %in% recuento$interview__key[recuento$condicion_boton==1]) %>% select(interview__key,inicio_ch_hora = hora_real, inicio_order = order, inicio_conteo_i = conteo_i)) %>% 
  left_join(botones_ch %>% filter(var=='termino_ch' & interview__key %in% recuento$interview__key[recuento$condicion_boton==1]) %>% select(interview__key,termino_ch_hora = hora_real, termino_order = order, termino_conteo_i = conteo_i,conteo_dif_termino)
            
  ) %>% 
  relocate(termino_ch_hora, .after = inicio_ch_hora) %>% 
  relocate(termino_order, .after = inicio_order)


recuento2 <- recuento %>% 
  mutate(condicion_termino_first = if_else(termino_conteo_i < inicio_conteo_i,1,0),
         diferencia_n = termino_conteo_i - inicio_conteo_i,
         diferencia = if_else(diferencia_n <= 8,1,0)
         ) 

ver <- paradatos_ch %>% filter(interview__key=='01-22-41-47')

# %>% 
#   filter(diferencia==0) %>% 
#   arrange(diferencia_n)

####2.2 Parad_cut----
  
             
recuento_f <- recuento2 %>% filter(diferencia_n==25)                     

         

remove(hog_dic23,
       paradatos)


# ola <- botones_ch %>%
#   group_by(interview__key) %>%
#   pivot_wider(
#     names_from = var,
#     values_from = order,
#     values_fn = list(order = list)   # Mantiene duplicados en listas
#     
#   ) %>%
#   unnest(cols = everything()) %>% 
#   select(interview__key,
#          inicio_ch,
#          termino_ch)


# paradatos_cut <- paradatos[role == 1 & event == "AnswerSet" & cuestionario == 'cut'][,
#                                                                                    diferencia := hora_real - shift(hora_real, 1, type = "lag"), 
#                                                                                    by = id_per2][,
#                                                                                                        diferencia_original := shift(hora_real, type = "lead", fill = hora_real[.N]) - hora_real, 
#                                                                                                        by = id_per2][
#                                                                                                          quantile(diferencia_original, 0.999, na.rm = TRUE) > diferencia_original
#                                                                                                        ][, 
#                                                                                                          diferencia_original := fifelse(diferencia_original < 0, as.difftime(0, units = "secs"), diferencia_original)
#                                                                                                          
#                                                                                                        ][, 
#                                                                                                          diferencia_original := as.difftime(as.numeric(diferencia_original) / 60, units = "mins")
#                                                                                                          
#                                                                                                        ]
# 
# paradatos_cut <- paradatos_cut[, conteo_i := seq_len(.N), by = id_per2]
# paradatos_cut[, fecha :=  format(hora_real, "%Y-%m-%d")]
# paradatos_cut[, fecha_antes := shift(fecha, type = "lead"), by = id_per2]
# paradatos_cut[, fecha_antes := fifelse(conteo_i == 1, fecha, fecha_antes)]
# 
# 
# # Calcular `dia_dif` y `diferencia` con condiciones
# paradatos_cut[, dia_dif_for := fifelse(fecha > fecha_antes, 1, 0)]
# paradatos_cut[, dia_dif_rew := fifelse(fecha < fecha_antes, 1, 0)]
# paradatos_cut[, dia_dif := fifelse(fecha != fecha_antes, 1, 0)]
# paradatos_cut[, dia_dif_mismo := fifelse(fecha == fecha_antes, 1, 0)]
# paradatos_cut[, diferencia := fifelse(conteo_i == 1, as.difftime(0, units = "mins"), diferencia)]
# paradatos_cut[, diferencia := fifelse(dia_dif_for == 1, as.difftime(0, units = "mins"), diferencia)]
# paradatos_cut[, diferencia := fifelse(dia_dif_rew == 1, as.difftime(0, units = "mins"), diferencia)]
# 
# ####BOTONES: CONTAR CUÁNTOS CUÁLES SE APLICARON DE FORMA CONSECUTIVA
# paradatos_cut[, var_antes := shift(var, type = "lag"), by = id_per2]



# %>% 
#   group_by(interview__key)

####BOTONES: CONTAR CUÁNTOS CUÁLES APLICARON EL DE TÉRMINO DESPUÉS DEL DE INICIO


# 
# paradatos_ch2 <- paradatos_ch %>% 
#   group_by(interview__key) %>% 
#   mutate(
#   conteo_i = row_number(interview__key),
#   fecha = format(hora_real, "%Y-%m-%d"),
#   fecha_antes = lead(fecha, order_by = interview__key),
#   fecha_antes = if_else(conteo_i == 1, fecha, fecha_antes),
#   dia_dif = if_else(fecha != fecha_antes, 1, 0),
#   dia_dif_for = if_else(fecha > fecha_antes, 1, 0),
#   dia_dif_rew = if_else(fecha < fecha_antes, 1, 0),
#   dia_dif_mismo = if_else(fecha == fecha_antes, 1, 0),
#   diferencia = if_else(conteo_i == 1, as.difftime(0, units = "mins"), diferencia),
#   diferencia = if_else(dia_dif_for == 1, as.difftime(0, units = "mins"), diferencia),
#   diferencia = if_else(dia_dif_rew == 1, as.difftime(0, units = "mins"), diferencia)
# )
# 
# library(compare)
# 
# # Comparar dos data.frames
# ola <- compare(paradatos_ch2, paradatos_ch3)
# 


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
# 
# 
# 
# ola <- paradatos_ch[, .(sum_diferencia_original = sum(diferencia_original, na.rm = TRUE)), by = interview__key]
# 
# 
# 
# paradatos_cut <- 
#   paradatos[role == 1 & event == "AnswerSet" & cuestionario == 'cut'][, diferencia := hora_real - shift(hora_real, 1, type = "lag"), 
#                                                                      by = id_per2][,diferencia_original := shift(hora_real, type = "lead", fill = hora_real[.N]) - hora_real, 
#                                                                                          by = id_per2][,id_per := paste(interview__key,"-",n_linea_p)]
# 
# paradatos_cut2 <- paradatos_cut %>% left_join(per_dic23,by='id_per'
#   
# )


# 
# saveRDS(paradatos_ch, "paradatos_ch.RDS")
# saveRDS(paradatos_cut, "paradatos_cut.RDS")
# 
# 

# 
# 
# 
# per_dic23 <- readRDS("//Buvmfswinp01/SEET_ENUT/ii_enut/5_procesamiento/5.6_ponderar/cdf/intermedias/cdf_per_inicial_dic23.RDS") %>% 
#   select(id_per,
#          recol_cut_sexo,
#          recol_cut_experiencia_ine,
#          recol_cut_nivel_educacional)
  


# agregar 0-4
# agregar 5-14
# agregar psdf
# agregar informante_idoneo


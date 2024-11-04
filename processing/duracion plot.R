ggplot(revisar, aes(x = as.numeric(tiempo), fill = n_integrantes)) +
  geom_histogram(alpha = 0.5, bins = 30, position = "identity") +  # Histograma traslapado
  theme_bw() +
  theme(
    legend.key.height = unit(1, "cm"),
    legend.text = element_text(size = 9, family = "arial"),
    legend.position = "top",  # Cambiar la posición de la leyenda si es necesario
    axis.text.x = element_text(size = 10, family = "arial"),
    axis.text.y = element_text(size = 11, family = "arial"),
    plot.caption = element_text(size = 9, family = "arial"),
    panel.grid.minor = element_blank()
  ) +
  ylab('Frecuencia') +
  xlab('Tiempo (minutos)') +
  labs(caption = paste0("N total de CH = ", prettyNum(nrow(tiempos_ch), big.mark = ".", decimal.mark = ',', scientific = FALSE))) +
  # geom_vline(aes(xintercept = tiempos_total_ch$promedio),
  #            color = "black", linetype = "dashed") +
  # annotate("label", x = tiempos_total_ch$promedio + 19.5, y = 4000,
  #          label = paste0(bquote("x\u0305"), " total = ", 
  #                         format(round(tiempos_total_ch$promedio, digits = 2), decimal.mark = ",")),
  #          size = 3) +
  scale_fill_brewer(palette = "Set1") 

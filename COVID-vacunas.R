source("R/data-preparation.R")

# dosis_entregadas_totales
# personas_con_pauta_completa
# dosis_administradas
variable_name_str = "personas_con_pauta_completa"
list_DF_vacunas = get_vacunas(ccaa_filter = "España", ultimos_n_dias = 12, fecha_final = "2021/10/1", variable_name = variable_name_str)

DF_futuro_prediction = list_DF_vacunas$DF_futuro_prediction


# Plot Proyeccion [personas_con_pauta_completa] --------------------------------------------------------------

DF_intercept_personas_con_pauta_completa = 
  DF_futuro_prediction %>% 
  group_by(ccaa) %>% 
  filter(get(variable_name_str) > poblacion * .7) %>% 
  drop_na() %>% 
  filter(fecha_publicacion == min(fecha_publicacion))



plot_proyeccion_personas_con_pauta_completa = 
  DF_futuro_prediction %>%
  # filter(ccaa == "Galicia") %>%
  ggplot(aes_string("fecha_publicacion", variable_name_str, fill = "ccaa")) +
  geom_bar(stat = "identity", aes(alpha = source)) +
  # geom_hline(aes(yintercept = poblacion), linetype = "dashed") +
  geom_hline(aes(yintercept = poblacion * .7), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = last_day_data), linetype = "dashed", color = "grey") +
  # geom_vline(data = DF_intercept_personas_con_pauta_completa, aes(xintercept = fecha_publicacion), linetype = "dashed", color = "darkgreen") +
  # annotate(geom = "text", x = as.Date("2021-10-05"), y = 100, label = "Proyección", size = 2) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, n.breaks = 5) +
  scale_x_date(breaks = "1 month", guide = guide_axis(angle = 90), date_labels = "%Y-%m") +
  facet_wrap(~ccaa, scales = "free") +
  theme(legend.position = "none") +
  labs(title = "Dosis entregadas",
       subtitle = paste0("Proyección desde ", last_day_data),
       caption = paste0("Usando información del ritmo de vacunación de los últimos ", ultimos_n_dias - 1, " días\n\nlinea roja = 70% población\n\nDatos extraidos de @datadista"))

plot_proyeccion_personas_con_pauta_completa
ggsave("outputs/plot_proyeccion_personas_con_pauta_completa.png", plot_proyeccion_personas_con_pauta_completa, width = 20, height = 12, dpi = 300)


# Plot Proyeccion [dosis_administradas] --------------------------------------------------------------

DF_intercept_dosis_administradas = 
  DF_futuro_prediction %>% 
  group_by(ccaa) %>% 
  filter(dosis_administradas > poblacion * .7) %>%
  filter(fecha_publicacion == min(fecha_publicacion))

plot_proyeccion_dosis_administradas = 
  DF_futuro_prediction %>%
  # filter(ccaa == "Andalucía") %>%
  ggplot(aes(fecha_publicacion, dosis_administradas, fill = ccaa)) +
  geom_bar(stat = "identity", aes(alpha = source)) +
  # geom_hline(aes(yintercept = poblacion), linetype = "dashed") +
  geom_hline(aes(yintercept = poblacion * .7), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = last_day_data), linetype = "dashed", color = "grey") +
  geom_vline(data = DF_intercept_dosis_administradas, aes(xintercept = fecha_publicacion), linetype = "dashed", color = "darkgreen") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, n.breaks = 5) +
  scale_x_date(breaks = "1 month", guide = guide_axis(angle = 90), date_labels = "%Y-%m") +
  facet_wrap(~ccaa, scales = "free") +
  theme(legend.position = "none") +
  labs(title = "Dosis administradas de la vacuna",
       subtitle = paste0("Proyección desde ", last_day_data),
       caption = paste0("Usando información del ritmo de vacunación de los últimos ", ultimos_n_dias - 1, " días\n\nlinea roja = 70% población\n\nAsumiendo que solo se administra 1 dosis por persona\n\nDatos extraidos de @datadista"))

plot_proyeccion_dosis_administradas
ggsave("outputs/plot_proyeccion_dosis_administradas.png", plot_proyeccion_dosis_administradas, width = 20, height = 12, dpi = 300)

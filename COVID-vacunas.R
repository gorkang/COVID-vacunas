
# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(janitor)
library(readr)
library(scales)
library(tidyr)

# Parameters --------------------------------------------------------------

options(scipen = 999)

ultimos_n_dias = 21
fecha_final = "2022/6/1"



# Read and prepare data-----------------------------------------------------

DF_poblacion = read_csv("datos/2915c.csv", 
                        locale = locale(grouping_mark = "."),
                        col_types = 
                          cols(
                            ccaa = col_character(),
                            poblacion = col_number())) %>% 
  filter(ccaa != "España")
  

DF = read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_vacunas.csv", 
           locale = locale(grouping_mark = "."),
           col_types = 
              cols(
                .default = col_number(),
                `Fecha publicación` = col_date(format = "%Y-%m-%d"),
                CCAA = col_character(),
                `Porcentaje sobre entregadas` = col_number(),
                `Fecha de la última vacuna registrada` = col_date(format = "%d/%m/%Y"),
                `Última fecha de actualización` =  col_date(format = "%d/%m/%Y"))) %>% 
  janitor::clean_names() %>% 
  filter(ccaa != "España") %>% 
  select(fecha_publicacion, ccaa, dosis_administradas, personas_con_pauta_completa) %>% 
  mutate(source = "vacunas")

last_day_data = max(DF %>% filter(source == "vacunas") %>% .$fecha_publicacion)

DF_futuro = tibble(ccaa = unique(DF_poblacion$ccaa),
                   dosis_administradas = NA,
                   personas_con_pauta_completa = NA) %>% 
  group_by(ccaa) %>% 
  expand_grid(fecha_publicacion = seq(max(DF$fecha_publicacion) + 1, as.Date(fecha_final), "days")) %>% 
  mutate(source = "prediction")




# Models-------------------------------------------------------------------

model_pauta_completa = lm(personas_con_pauta_completa ~ ccaa * fecha_publicacion,
           data = DF %>% filter(fecha_publicacion > max(fecha_publicacion) - ultimos_n_dias)) # Usamos datos de los ultimos_n_dias

model_dosis_administradas = lm(dosis_administradas ~ ccaa * fecha_publicacion,
           data = DF %>% filter(fecha_publicacion > max(fecha_publicacion) - ultimos_n_dias)) # Usamos datos de los ultimos_n_dias


DF_futuro_prediction =
  DF %>%
  bind_rows(
    DF_futuro %>%
      mutate(personas_con_pauta_completa = predict(model_pauta_completa, newdata = DF_futuro),
             dosis_administradas = predict(model_dosis_administradas, newdata = DF_futuro))
  ) %>%
left_join(DF_poblacion, by = "ccaa")



# Plot Proyeccion [personas_con_pauta_completa] --------------------------------------------------------------

DF_intercept_personas_con_pauta_completa = 
  DF_futuro_prediction %>% 
  group_by(ccaa) %>% 
  filter(personas_con_pauta_completa > poblacion * .7) %>% 
  filter(fecha_publicacion == min(fecha_publicacion))



plot_proyeccion_personas_con_pauta_completa = 
  DF_futuro_prediction %>%
  # filter(ccaa == "Galicia") %>%
  ggplot(aes(fecha_publicacion, personas_con_pauta_completa, fill = ccaa)) +
  geom_bar(stat = "identity", aes(alpha = source)) +
  # geom_hline(aes(yintercept = poblacion), linetype = "dashed") +
  geom_hline(aes(yintercept = poblacion * .7), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = last_day_data), linetype = "dashed", color = "grey") +
  geom_vline(data = DF_intercept_personas_con_pauta_completa, aes(xintercept = fecha_publicacion), linetype = "dashed", color = "darkgreen") +
  # annotate(geom = "text", x = as.Date("2021-10-05"), y = 100, label = "Proyección", size = 2) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, n.breaks = 5) +
  scale_x_date(breaks = "1 month", guide = guide_axis(angle = 90), date_labels = "%Y-%m") +
  facet_wrap(~ccaa, scales = "free") +
  theme(legend.position = "none") +
  labs(title = "Personas con pauta de vacunación completa",
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

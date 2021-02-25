
get_vacunas <- function(ccaa_filter, ultimos_n_dias = 21, fecha_final = "2022/6/1") {
  
  
  # Libraries ---------------------------------------------------------------
  
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(janitor)
  library(readr)
  library(scales)
  library(tidyr)
  
  
# Parameters --------------------------------------------------------------

options(scipen = 999)

# ccaa_filter = ccaa_menu[1]
# ccaa_filter = c("España", "Canarias")
# ultimos_n_dias = 21
# fecha_final = "2022/6/1"



# Read and prepare data-----------------------------------------------------

DF_poblacion = read_csv("datos/2915c.csv", 
                        locale = locale(grouping_mark = "."),
                        col_types = 
                          cols(
                            ccaa = col_character(),
                            poblacion = col_number())) %>% #%>% filter(ccaa != "España")
  filter(ccaa %in% ccaa_filter)


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
  # filter(ccaa != "España") %>% 
  select(fecha_publicacion, ccaa, dosis_administradas, personas_con_pauta_completa) %>% 
  mutate(source = "vacunas") %>% 
  filter(ccaa %in% ccaa_filter) %>% 
  mutate(ccaa = as.factor(ccaa)) #ccaa = forcats::fct_relevel(ccaa, "España") # Error si filtro no incluye a España


last_day_data = max(DF %>% filter(source == "vacunas") %>% .$fecha_publicacion)

DF_futuro = tibble(ccaa = unique(DF_poblacion$ccaa),
                   dosis_administradas = NA,
                   personas_con_pauta_completa = NA) %>% 
  group_by(ccaa) %>% 
  expand_grid(fecha_publicacion = seq(max(DF$fecha_publicacion) + 1, as.Date(fecha_final), "days")) %>% 
  mutate(source = "prediction")




# Models-------------------------------------------------------------------

if (length(ccaa_filter) == 1) {
  
  model_pauta_completa = lm(personas_con_pauta_completa ~ fecha_publicacion,
                            data = DF %>% filter(fecha_publicacion > max(fecha_publicacion) - ultimos_n_dias)) # Usamos datos de los ultimos_n_dias
  
  model_dosis_administradas = lm(dosis_administradas ~ fecha_publicacion,
                                 data = DF %>% filter(fecha_publicacion > max(fecha_publicacion) - ultimos_n_dias)) # Usamos datos de los ultimos_n_dias
  
} else {
  
  model_pauta_completa = lm(personas_con_pauta_completa ~ ccaa * fecha_publicacion,
                            data = DF %>% filter(fecha_publicacion > max(fecha_publicacion) - ultimos_n_dias)) # Usamos datos de los ultimos_n_dias
  
  model_dosis_administradas = lm(dosis_administradas ~ ccaa * fecha_publicacion,
                                 data = DF %>% filter(fecha_publicacion > max(fecha_publicacion) - ultimos_n_dias)) # Usamos datos de los ultimos_n_dias
  
}




DF_futuro_prediction =
  DF %>%
  bind_rows(
    DF_futuro %>%
      mutate(personas_con_pauta_completa = predict(model_pauta_completa, newdata = DF_futuro),
             dosis_administradas = predict(model_dosis_administradas, newdata = DF_futuro))
  ) %>%
  left_join(DF_poblacion, by = "ccaa") %>% 
  mutate(source_alpha = 
           case_when(
             source == "vacunas" ~ 1,
             source == "prediction" ~ .2
           ))

list_DFs = list(last_day_data = last_day_data,
                DF_vacunas = DF,
                DF_futuro_prediction = DF_futuro_prediction)

return(list_DFs)

}

# XXX = get_vacunas()

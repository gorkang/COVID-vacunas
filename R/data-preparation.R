
get_vacunas <- function(ccaa_filter, ultimos_n_dias = 21, fecha_final = "2022/6/1", variable_name = "personas_con_pauta_completa") {
  
  
  # Libraries ---------------------------------------------------------------
  
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(janitor)
  library(readr)
  library(scales)
  library(tidyr)
  library(vroom)
  
  
# Parameters --------------------------------------------------------------

options(scipen = 999)

# variable_name = "personas_con_pauta_completa"
# ccaa_filter = ccaa_menu[1]
# ccaa_filter = c("España", "Canarias")
# ultimos_n_dias = 21
# fecha_final = "2022/6/1"



# Read and prepare data-----------------------------------------------------

DF_poblacion_raw = read_csv("datos/2915c.csv", 
                        locale = locale(grouping_mark = "."),
                        col_types = 
                          cols(
                            ccaa = col_character(),
                            poblacion = col_number())) #%>% filter(ccaa != "España")

DF_poblacion = 
  DF_poblacion_raw %>% 
  filter(ccaa %in% ccaa_filter)


DF_raw = vroom::vroom("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_vacunas.csv",
# DF_raw = read_csv("datos/DF_raw.csv",
                      
              locale = locale(grouping_mark = "."),
              col_types = 
                cols(
                  .default = col_number(),
                  `Fecha publicación` = col_date(format = "%Y-%m-%d"),
                  CCAA = col_character(),
                  `Porcentaje sobre entregadas` = col_number(),
                  `Fecha de la última vacuna registrada` = col_date(format = "%d/%m/%Y"),
                  `Última fecha de actualización` =  col_date(format = "%d/%m/%Y"))) %>% 
  janitor::clean_names()


write_csv(DF_raw, "datos/DF_raw.csv")

DF = 
  DF_raw %>% 
  # filter(ccaa != "España") %>% 
  select(fecha_publicacion, ccaa, all_of(variable_name)) %>% 
  mutate(source = "vacunas") %>% 
  filter(ccaa %in% ccaa_filter) %>% 
  mutate(ccaa = as.factor(ccaa)) #ccaa = forcats::fct_relevel(ccaa, "España") # Error si filtro no incluye a España


last_day_data = max(DF %>% filter(source == "vacunas") %>% .$fecha_publicacion)

DF_futuro = tibble(ccaa = unique(DF_poblacion$ccaa),
                   !!variable_name := NA) %>% 
  group_by(ccaa) %>% 
  expand_grid(fecha_publicacion = seq(max(DF$fecha_publicacion) + 1, as.Date(fecha_final), "days")) %>% 
  mutate(source = "prediction")




# Models-------------------------------------------------------------------

if (length(ccaa_filter) == 1) {
  
  # model = lm(personas_con_pauta_completa ~ fecha_publicacion,
  #                           data = DF %>% filter(fecha_publicacion > max(fecha_publicacion) - ultimos_n_dias)) # Usamos datos de los ultimos_n_dias

  model = lm(get(variable_name) ~ fecha_publicacion,
             data = DF %>% filter(fecha_publicacion > max(fecha_publicacion) - ultimos_n_dias)) # Usamos datos de los ultimos_n_dias
  

  } else {
  
  model = lm(get(variable_name) ~ ccaa * fecha_publicacion,
             data = DF %>% filter(fecha_publicacion > max(fecha_publicacion) - ultimos_n_dias)) # Usamos datos de los ultimos_n_dias
}




DF_futuro_prediction =
  DF %>%
  bind_rows(
    DF_futuro %>%
      mutate(!!variable_name := predict(model, newdata = DF_futuro))
  ) %>%
  left_join(DF_poblacion, by = "ccaa") %>% 
  mutate(source_alpha = 
           case_when(
             source == "vacunas" ~ 1,
             source == "prediction" ~ .2
           ))


# Si esta Espana, poner en primer lugar
if ("España" %in% DF_futuro_prediction$ccaa) {
  ccaa_vector = as.character(unique(DF_futuro_prediction$ccaa))
  DF_futuro_prediction$ccaa = factor(DF_futuro_prediction$ccaa, levels = c('España', ccaa_vector[!ccaa_vector %in% "España"]))
}


list_DFs = list(last_day_data = last_day_data,
                DF_vacunas = DF,
                DF_futuro_prediction = DF_futuro_prediction)

return(list_DFs)

}

# XXX = get_vacunas()

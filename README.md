# COVID-vacunas

Predicción sobre el avance ritmo de vacunación COVID por comunidad autónoma en España.  

Se emplea un modelo lineal muy sencillo: `lm(personas_con_pauta_completa ~ ccaa * fecha_publicacion)`  

Se puede consultar una versión interactiva aquí: https://gorkang.shinyapps.io/COVID-vacunas/


### Datos

- Datos sobre vacunación extraidos de @datadista: https://github.com/datadista/datasets/  
- Datos sobre población extraidos del INE: https://www.ine.es/jaxiT3/Datos.htm?t=2915


### Web interactiva

Proyección de avance de personas con pauta de vacunación completa:  https://gorkang.shinyapps.io/COVID-vacunas/  

![](https://github.com/gorkang/COVID-vacunas/raw/master/outputs/movie.gif)

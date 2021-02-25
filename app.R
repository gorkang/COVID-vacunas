
# Libraries ---------------------------------------------------------------

library(dplyr)
# library(DT)
library(forcats)
library(ggplot2)
# library(ggrepel)
# library(httr)
# library(jsonlite)
library(janitor)
library(lubridate)
library(readr)
# library(rvest)
library(tidyr)
library(scales)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)

# library(vroom)



# Data preparation --------------------------------------------------------

source(here::here("R/data-preparation.R"))

ccaa_menu = read_csv("datos/2915c.csv", 
                        locale = locale(grouping_mark = "."),
                        col_types = 
                            cols(
                                ccaa = col_character(),
                                poblacion = col_number())) %>% 
    # filter(ccaa != "España") %>% 
    pull(ccaa)


# UI ----------------------------------------------------------------------

ui <- 
    function(request) {
        fluidPage(
            tags$head(includeHTML(("google-analytics.html"))),
            useShinyjs(),
            
        titlePanel(
            windowTitle = "Vacunación COVID - Facultad de Psicología - UAI",title = ""
            # fluidRow(
            #     column(10, HTML("<a href=\"https://gorkang.shinyapps.io/COVID-vacunas/\">Vacunación COVID</a>")), 
            #     column(1, HTML("<a href=\"http://psicologia.uai.cl/\", target = \"_blank\"><img src=\"UAI_mini.png\", alt ='Universidad Adolfo Ibáñez'></a>"))
            # )
        ),
    theme = shinytheme("flatly"),
    
    sidebarLayout(
        sidebarPanel(
            width = 2,

            div(
                
            HTML(paste0(
                "<a href=\"https://gorkang.shinyapps.io/COVID-vacunas/\" style='font-size: 150%'>Vacunación COVID</a>", br(), br(),
                a(img(src = "github_small.png", title = "Github repository"), href = "https://github.com/gorkang/COVID-vacunas", target = "_blank"), "&nbsp;&nbsp;",
                a(img(src = "issue_small.png", title = "Report an issue!"), href = "https://github.com/gorkang/COVID-vacunas/issues", target = "_blank"), "&nbsp;&nbsp;",
                a(img(src = "twitter_small.png", title = "@gorkang"), href = "https://twitter.com/gorkang", target = "_blank"), "&nbsp;&nbsp;", 
                a(img(src = "https://cdn.buymeacoffee.com/buttons/bmc-new-btn-logo.svg", title = "Buy me a coffee", height = "26px"), href = "https://www.buymeacoffee.com/I0rkAbM", target = "_blank"), "&nbsp;", 
                "<BR><BR>")
                ),
            align = "center"),

    shiny::selectInput(inputId = 'ccaa', 
                       label = 'Comunidades autónomas',
                       choices = ccaa_menu,
                       multiple = TRUE, 
                       selectize = TRUE, 
                       width = "100%", 
                       selected = c(ccaa_menu)),
    
    shiny::dateInput("fecha_final", "Proyección hasta", value = "2022-02-01"),
    shiny::sliderInput('ultimos_n_dias', "Días usados para calcular ritmo de vacunación", min = 2, max = 60, value = 21, step = 1),
    

    HTML("<BR>"),

    div( HTML("&nbsp;&nbsp;"), style = "display:inline-block;65%;text-align: center;",
        bookmarkButton(label = "URL")), 
    HTML("&nbsp;&nbsp;"),
    div(style = "display:inline-block;30%;text-align: center;",
        downloadButton('downloadPlot', 'Plot')),
    
    # HTML("<BR><BR>"),
    br(),
    br(),
    
    uiOutput("WARNING"),
    
    hr(),
    
    HTML(paste0("Datos del Ministerio de Sanidad obtenidos a través de ",  
            a("@datadista", href = "https://github.com/datadista/datasets/tree/master/COVID%2019", target = "_blank"), 
            ". Datos sobre población obtenidos desde la página del ", a("INE", href = "https://www.ine.es/jaxiT3/Datos.htm?t=2915", target = "_blank"), ".")
         )
        ),

                
        # SHOW PLOT
        mainPanel( width = 10,
                   HTML('<span style="font-size: 150%">Personas con pauta de vacunación completa</span>', 
                        '[<span style="color: ', alpha("#F8766D", 1), ';">Vacunadas</span>, <span style="color: ', alpha("#F8766D", .2), ';">Previsión</span>]',
                        '<a href=\"http://psicologia.uai.cl/\", target = \"_blank\"><img src=\"UAI_mini.png\", alt ="Universidad Adolfo Ibáñez", style = "float:right;"></a>'),
            
            hr(),
            
            plotOutput("distPlot", height = "850px", width = "100%"),
            
            hr()
            
            )
        )
    )

}


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

    # setBookmarkExclude(c('mytable_rows_current'))
    

    # WARNING -----------------------------------------------------------------
    output$WARNING <- renderUI({
        span(
            HTML(
            # h6(
                paste0("La proyección está basada en una sencilla extrapolación del ritmo de vacunación en los últimos ", input$ultimos_n_dias, " días.", br(), br(), 
               "Puedes reportar errores ", a("aquí!", href = "https://github.com/gorkang/COVID-vacunas/issues", target = "_blank"))),
             style = "color:darkred")
             # )
    })


    # Debounce critical vars --------------------------------------------------

    INPUT_ccaa = reactive({input$ccaa})
    INPUT_ccaa_debounced <- debounce(INPUT_ccaa, 600)
    
    INPUT_ultimos_n_dias = reactive({input$ultimos_n_dias})
    INPUT_ultimos_n_dias_debounced <- debounce(INPUT_ultimos_n_dias, 500)
    
    

    # final_df() creation -----------------------------------------------------
    
    final_df = reactive({ 

        withProgress(message = 'Preparando datos', value = 1, min = 0, max = 3, {
            
            req(INPUT_ultimos_n_dias_debounced())
            req(input$fecha_final)
            req(INPUT_ccaa_debounced())
            
            list_DF_vacunas = get_vacunas(ccaa_filter = INPUT_ccaa_debounced(), ultimos_n_dias = (INPUT_ultimos_n_dias_debounced() + 1), fecha_final = input$fecha_final)
            list_DF_vacunas
            
        })
    }) 



    # Prepare plot
    final_plot <- reactive({

        withProgress(message = 'Preparando gráfica', value = 2, min = 0, max = 3, {
                
            DF_futuro_prediction = final_df()$DF_futuro_prediction
            last_day_data = final_df()$last_day_data
            
            
            
            # Plot Proyeccion [personas_con_pauta_completa] --------------------------------------------------------------
            
            DF_intercept_personas_con_pauta_completa = 
                DF_futuro_prediction %>% 
                group_by(ccaa) %>% 
                filter(personas_con_pauta_completa > poblacion * .7) %>% 
                filter(fecha_publicacion == min(fecha_publicacion))
            
            
            
            plot_proyeccion_personas_con_pauta_completa = 
                DF_futuro_prediction %>%
                ggplot(aes(fecha_publicacion, personas_con_pauta_completa, fill = ccaa)) +
                geom_bar(stat = "identity", aes(alpha = source_alpha)) +
                # geom_hline(aes(yintercept = poblacion), linetype = "dashed") +
                geom_hline(aes(yintercept = poblacion * .7), linetype = "dashed", color = "red") +
                geom_vline(aes(xintercept = last_day_data), linetype = "dashed", color = "grey") +
                geom_vline(data = DF_intercept_personas_con_pauta_completa, aes(xintercept = fecha_publicacion), linetype = "dashed", color = "darkgreen") +
                # annotate(geom = "text", x = as.Date("2021-10-05"), y = 100, label = "Proyección", size = 2) +
                theme_minimal(base_size = 14) +
                # scale_y_continuous(labels = scales::comma, n.breaks = 5) +  
                scale_y_continuous(labels = scales::label_number_si(), n.breaks = 5) +

                scale_x_date(breaks = "1 month", guide = guide_axis(angle = 90), date_labels = "%Y-%m") +
                facet_wrap(~ ccaa, scales = "free_y") +
                theme(legend.position = "none",
                      # axis.text.x=element_blank()
                      ) +
                labs(x = "", y = "Personas con pauta de vacunación completa",
                    # title = "Personas con pauta de vacunación completa",
                    # title = paste0("Proyección desde ", last_day_data),
                    caption = paste0("linea roja = 70% población\n\nDatos extraidos de @datadista. Por @gorkang")
                    )
            # Usando información del ritmo de vacunación de los últimos ", INPUT_ultimos_n_dias_debounced(), " días\n\n
            

            
            plot_proyeccion_personas_con_pauta_completa
                
                

        })
    })
    
    # Show plot
    output$distPlot <- renderCachedPlot({
        
        withProgress(message = 'Mostrando gráfica', value = 3, min = 0, max = 3, {
            
            final_plot()
            
        })
    }, cacheKeyExpr = list(final_df()))

    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(Sys.Date(), "_vacunacion.png", sep = "") },
        content = function(file) { ggsave(file, plot = final_plot(), device = "png", width = 18, height = 10, dpi = 300) }
    )

}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url") #, options = "test.mode"

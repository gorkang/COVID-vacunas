
# Libraries ---------------------------------------------------------------

library(dplyr)
# library(DT)
library(forcats)
library(ggplot2)
library(janitor)
library(lubridate)
library(readr)
library(tidyr)
library(scales)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(stringr)
library(vroom)



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
        
        ## Sidepanel ----
        sidebarPanel(
            width = 2,

            div(
                
            HTML(paste0(
                "<a href=\"https://gorkang.shinyapps.io/COVID-vacunas/\" style='font-size: 150%;'>Vacunación COVID</a>", br(), br(),
                a(img(src = "github_small.png", title = "Github repository"), href = "https://github.com/gorkang/COVID-vacunas", target = "_blank"), "&nbsp;&nbsp;",
                a(img(src = "issue_small.png", title = "Report an issue!"), href = "https://github.com/gorkang/COVID-vacunas/issues", target = "_blank"), "&nbsp;&nbsp;",
                a(img(src = "twitter_small.png", title = "@gorkang"), href = "https://twitter.com/gorkang", target = "_blank"), "&nbsp;&nbsp;", 
                a(img(src = "https://cdn.buymeacoffee.com/buttons/bmc-new-btn-logo.svg", title = "Buy me a coffee", height = "26px"), href = "https://www.buymeacoffee.com/I0rkAbM", target = "_blank"), "&nbsp;", 
                "<BR><BR>")
                ),
            align = "center"),

            shiny::selectInput(inputId = 'variable_name', 
                               label = 'Variable',
                               choices = c("dosis_entregadas_totales", "personas_con_pauta_completa", "dosis_administradas"),
                               multiple = FALSE, 
                               width = "100%", 
                               selected = c("dosis_administradas")),

            shiny::selectInput(inputId = 'ccaa', 
                               label = 'Comunidades autónomas',
                               choices = ccaa_menu,
                               multiple = TRUE, 
                               selectize = TRUE, 
                               width = "100%", 
                               selected = "España"), # c(ccaa_menu)),
            
            shiny::dateInput("fecha_final", "Proyección hasta", value = "2021-12-31"),
            shiny::sliderInput('ultimos_n_dias', "Días anteriores usados para la previsión", min = 2, max = 60, value = 21, step = 1),
            shiny::sliderInput('poblacion_objetivo', "% población objetivo", min = 0, max = 100, value = 70, step = 1),
            
            HTML("<BR>"),

            div( HTML("&nbsp;&nbsp;"), style = "display:inline-block;65%;text-align: center;",
                bookmarkButton(label = "URL")), 
            HTML("&nbsp;&nbsp;"),
            div(style = "display:inline-block;30%;text-align: center;",
                downloadButton('downloadPlot', 'Plot')),
            
            br(),
            br(),
            
            uiOutput("WARNING"),
            
            hr(),
            
            HTML(paste0("Datos del Ministerio de Sanidad obtenidos a través de ",  
                    a("@datadista", href = "https://github.com/datadista/datasets/tree/master/COVID%2019", target = "_blank"), 
                    ". Datos sobre población obtenidos desde la página del ", a("INE", href = "https://www.ine.es/jaxiT3/Datos.htm?t=2915", target = "_blank"), ".")
                 )
                ),

                
        ## Main panel ----
        mainPanel( width = 10,
                   h4(htmlOutput("VARIABLE")),
                   htmlOutput("VARIABLE_pie"),
                   
                   # Loading ggplot message
                   tags$head(tags$style(type = "text/css", paste0("#loadmessage {position: fixed; top: 0px; left: 0px; width: 100%; padding: 100px 0px 100px 0px; text-align: center; font-weight: bold; font-size: 120%; color: #158C00; background-color: #E0E0E070; z-index: 105;}"))),
                   conditionalPanel(condition = "$('html').hasClass('shiny-busy')", tags$div( "Generando gráfico. Por favor, espera...", id = "loadmessage")),
                   
                   plotOutput("distPlot", height = "850px", width = "100%"),
                   hr()
                   )
        )
    )
}


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

    # setBookmarkExclude(c('mytable_rows_current'))
    

    # Dynamic text (WARNING / VARIABLE)  -----------------------------------------------------------------
    output$WARNING <- renderUI({
        span(
            HTML(
            # h6(
                paste0("La proyección está basada en una sencilla extrapolación del ritmo de vacunación/dosis entregadas en los últimos ", input$ultimos_n_dias, " días.", br(), br(), 
               "Puedes reportar errores ", a("aquí!", href = "https://github.com/gorkang/COVID-vacunas/issues", target = "_blank"))),
             style = "color:darkred")
             # )
    })

    output$VARIABLE <- renderUI({
        
        if (input$variable_name == "dosis_entregadas_totales") variable_text = "Dosis"
        if (input$variable_name == "personas_con_pauta_completa") variable_text = "Vacunadas"
        if (input$variable_name == "dosis_administradas") variable_text = "Dosis"
        
        HTML(
            paste0(stringr::str_to_sentence(gsub("_", " ", input$variable_name), locale = "en"), ' a <span style="font-size: 90%;">', final_df()$last_day_data, '</span>',
                   '<span style="font-size: 80%;"> 
                        <BR><span style="color: ', alpha("#F8766D", 1), ';">', variable_text, '</span> | <span style="color: ', alpha("#F8766D", .2), ';">Previsión</span>
                   </span>',
                   '<a href=\"http://psicologia.uai.cl/\", target = \"_blank\"><img src=\"UAI_mini.png\", alt ="Universidad Adolfo Ibáñez", 
                   style = "position: fixed; top: 0px; right: 0%; width: 200px; padding: 25px 20px 0px 0px;"></a><BR>'))
        })

    output$VARIABLE_pie <- renderUI({
        
        if (input$variable_name == "dosis_entregadas_totales") variable_text_pie = "Vacunas totales entregadas (no todas han sido administradas)<BR>"
        if (input$variable_name == "personas_con_pauta_completa") variable_text_pie = "Personas completamente inmunizadas (e.g. 2 dosis Pfizer, 1 dosis Jansen)<BR>"
        if (input$variable_name == "dosis_administradas") variable_text_pie = "Número de dosis individuales administradas<BR>"
        
        HTML(paste0('<span style="font-size: 80%;"><span style = "font-size: 13px;">', variable_text_pie, '</span></span>'))
        
    })
    
    
    # Debounce critical vars --------------------------------------------------

    INPUT_ccaa = reactive({input$ccaa})
    INPUT_ccaa_debounced <- debounce(INPUT_ccaa, 600)
    
    INPUT_ultimos_n_dias = reactive({input$ultimos_n_dias})
    INPUT_ultimos_n_dias_debounced <- debounce(INPUT_ultimos_n_dias, 600)
    
    INPUT_poblacion_objetivo = reactive({input$poblacion_objetivo / 100})
    INPUT_poblacion_objetivo_debounced <- debounce(INPUT_poblacion_objetivo, 600)


    # final_df() creation -----------------------------------------------------
    
    final_df <- reactive({ 

        withProgress(message = 'Preparando datos', value = 1, min = 0, max = 3, {
            
            req(INPUT_ultimos_n_dias_debounced())
            req(input$fecha_final)
            req(INPUT_ccaa_debounced())
            
            list_DF_vacunas = get_vacunas(ccaa_filter = INPUT_ccaa_debounced(), ultimos_n_dias = (INPUT_ultimos_n_dias_debounced() + 1), fecha_final = input$fecha_final, variable_name = input$variable_name)
            list_DF_vacunas
            
        })
    }) 

    DF_intercept <- reactive({
        
        req(final_df())
        req(input$variable_name)
        req(INPUT_poblacion_objetivo_debounced())
        
        DF_futuro_prediction = final_df()$DF_futuro_prediction
        
        DF_intercept = DF_futuro_prediction %>% 
            group_by(ccaa) %>% 
            filter(get(input$variable_name) > (poblacion * INPUT_poblacion_objetivo_debounced())) %>% 
            filter(fecha_publicacion == min(fecha_publicacion))
        
        DF_intercept
        
        })

    # Prepare plot
    final_plot <- reactive({

        withProgress(message = 'Preparando gráfica', value = 2, min = 0, max = 3, {
            
            req(final_df())
            
            req(DF_intercept())
            req(INPUT_poblacion_objetivo_debounced())
            
            
            DF_futuro_prediction = final_df()$DF_futuro_prediction
            last_day_data = final_df()$last_day_data
            
            
            
            # Plot Proyeccion --------------------------------------------------------------
            
            plot_proyeccion_personas_con_pauta_completa = 
                DF_futuro_prediction %>%
                ggplot(aes_string("fecha_publicacion", input$variable_name, fill = "ccaa")) +
                geom_bar(stat = "identity", aes(alpha = source_alpha)) +
                # gghighlight::gghighlight() +
            
                geom_hline(aes(yintercept = (poblacion * INPUT_poblacion_objetivo_debounced())), linetype = "dashed", color = "red") +
                geom_vline(aes(xintercept = last_day_data), linetype = "dashed", color = "grey") +
                geom_vline(data = DF_intercept(), aes(xintercept = fecha_publicacion), linetype = "dashed", color = "darkgreen") +
                theme_minimal(base_size = 16) +
                scale_y_continuous(labels = scales::label_number_si(), n.breaks = 7) +
                scale_x_date(breaks = "1 month", guide = guide_axis(angle = 90), date_labels = "%Y-%m") +
                facet_wrap(~ ccaa, scales = "free_y") +
                theme(legend.position = "none") +
                labs(x = "", y = "Personas con pauta de vacunación completa",
                    caption = paste0("linea roja = ", INPUT_poblacion_objetivo_debounced() * 100, "% población. Usando ", INPUT_ultimos_n_dias_debounced(), " días para la estimación.\n\n M: millones, K: miles\n\nDatos extraidos de @datadista. Por @gorkang")
                    )
            
            plot_proyeccion_personas_con_pauta_completa

        })
    })
    
    # Show plot
    output$distPlot <- renderCachedPlot({
        
        withProgress(message = 'Mostrando gráfica', value = 3, min = 0, max = 3, {
            
            final_plot()
            
        })
    },
    
    # INCLUDE variables that affect plot!
    cacheKeyExpr = list(final_df(), DF_intercept(), INPUT_poblacion_objetivo_debounced())
    
    )

    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(Sys.Date(), "_vacunacion.png", sep = "") },
        content = function(file) { ggsave(file, plot = final_plot(), device = "png", width = 18, height = 10, dpi = 300) }
    )

}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url") #, options = "test.mode"

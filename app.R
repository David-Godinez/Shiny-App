# ---
# Labor Force Shiny App
# ---


# Prelim ------------------------------------------------------------------

library(shiny)

library(tidyverse)

library(plotly)


# UI ----------------------------------------------------------------------

ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    navbarPage(title = "InterPoli for Political Analysis",
               tabPanel("Widgets",
                        fluidRow(
                            column(12,
                                   textOutput("text_main"),
                                   tags$head(tags$style("#text_main{font-size: 20px"))
                            )),
                        br(), br(),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(inputId = "sel_country", label = "Select Countries", "Names", multiple = TRUE)),
                            mainPanel(
                                plotlyOutput("plot")))
               ),
               
               tabPanel("Data"),
               tabPanel("About")
    ))



# Server ------------------------------------------------------------------


server <- function(input, output, session){
    
    
### Global Options:

 
    labor_data <- read.csv("labor_data.csv") # Reads data
    
    my_data <- reactive({
        req(input$sel_country)
        countries <- labor_data %>% filter(country %in% input$sel_country) # Makes data reactive
    })
    
    
    observe({
        updateSelectInput(session, "sel_country", choices = labor_data$country) # Makes `selectInput` dynamic
    })
    

    
    

### Widgets Tab:
    
    output$text_main <- renderText("The InterPoli app is the most intuitive way of analyzing socioeconomic trends across the world. 
                                   InterPoli makes drawing insights from data easier than ever by providing immersive visualization tools that empower political researchers at every 
                                   step of the way. The 'Labor Force' widget below showcases the app's accessibility and use. Start by selecting any combination of countries,
                                   and InterPoli will generate a time-series graph that is fully interactive:")
    
    
## Time Plot
    
    # Initial plot with gray lines
    
    
      main_plot <- ggplotly(ggplot() +
                     geom_line(data = labor_data, aes(x = Year, y = laborforce, group = country), alpha = .015) +
                     theme_minimal() +
                     scale_x_continuous("Year", breaks = c(2001, 2005, 2009, 2013, 2017)) +
                     scale_y_continuous("Labor Force Rate", breaks = c(-0.05, -0.025, 0.00, 0.025, 0.05)) +
                     ggtitle("Labor Force by Country")) 
      
      output$plot <- renderPlotly({ main_plot }) 
      
      
     # Reactive plot
      
      observeEvent(input$sel_country, {
          output$plot <- renderPlotly({
              ggplotly(ggplot() + 
                           geom_line(data = labor_data, aes(x = Year, y = laborforce, group = country), alpha = .015) +
                           geom_line(data = my_data(), aes(x = Year, y = laborforce, color = country), size = .6) +
                           geom_point(data = my_data(), aes(x = Year, y = laborforce, color = country), size = .8) +
                           theme_minimal() +
                           scale_x_continuous("Year", breaks = c(2001, 2005, 2009, 2013, 2017)) +
                           scale_y_continuous("Labor Force Rate", breaks = c(-0.05, -0.025, 0.00, 0.025, 0.05)) +
                           ggtitle("Labor Force by Country") +
                           labs(color = "Country"))
              })
          
         })
          

      
}
    

shinyApp(ui, server)




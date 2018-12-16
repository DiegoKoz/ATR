library(tidyverse)
library(shiny)
library(plotly)
library(gapminder)
library(ggthemes)

data <- gapminder

data <- data %>% 
  gather(., var, value, 4:6)

ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tableros dinámicos"),
  
  sidebarPanel(
    h3("Esperanza de vida"),
    # Elegir pais
    selectizeInput("name",
                   label = "Paises de interes",
                   choices = unique(data$country),
                   multiple = T,
                   options = list(maxItems = 5, placeholder = 'eliga un pais'),
                   selected = "Argentina"),

    # Elegir variables
    selectizeInput("var",
                   label = "Elegir variables",
                   choices = unique(data$var),
                   multiple = T,
                   options = list(placeholder = 'eliga una variable'),
                   selected = "lifeExp")
   ),
  
  # mostrar gráfico
  mainPanel(
    plotlyOutput("plot")
  )
)
)

server <- shinyServer(function(input, output, session) {
  
  output$plot <- renderPlotly({
    
    if (length(input$name) == 0) {
      print("por favor eliga al menos un país")
    } 
    if (length(input$var) == 0) {
      print("por favor eliga al menos una variable")
    } else {
      df_trend <- data[data$country == input$name & data$var %in% input$var, ]
      ggplot(df_trend, aes(label = var)) +
        geom_line(aes(x = year, y = value, by = country, color = country)) +
        labs(x = "Año", y = "var", title = "variables por país") +
        facet_grid(var~., scales = "free")+
        scale_colour_hue("pais", l = 70, c = 150) + 
        theme_few()
    }
  })
})

shinyApp(ui, server)

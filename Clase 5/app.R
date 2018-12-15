library(shiny)
library(plotly)
library(gapminder)

data <- gapminder

ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tableros dinámicos"),
  
  sidebarPanel(
    h3("Esperanza de vida"),
    # Select Justices name here
    selectizeInput("name",
                   label = "Paises de interes",
                   choices = unique(data$country),
                   multiple = T,
                   options = list(maxItems = 5, placeholder = 'eliga un pais'),
                   selected = "Argentina"),
    # Term plot
    plotOutput("termPlot", height = 200)
    # helpText("Data: Bailey, Michael, Anton  Strezhnev and Erik Voeten. Forthcoming.  'Estimating Dynamic State Preferences from United Nations Voting Data.' Journal of Conflict Resolution. ")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotlyOutput("trendPlot")
  )
)
)


server <- shinyServer(function(input, output, session) {
  
  output$trendPlot <- renderPlotly({
    
    if (length(input$name) == 0) {
      print("por favor eliga al menos un país")
    } else {
      df_trend <- data[data$country == input$name, ]
      ggplot(df_trend) +
        geom_line(aes(x = year, y = lifeExp, by = country, color = country)) +
        labs(x = "Año", y = "Esperanza de vida", title = "Esperanza de vida por año") +
        scale_colour_hue("clarity", l = 70, c = 150) + ggthemes::theme_few()
    }
    
  })
})


shinyApp(ui, server)

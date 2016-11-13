library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Pollutant Information in Taiwan"),
    sidebarLayout(
      sidebarPanel(
        actionButton("fetch", label = "Update data"),
        hr(),
        selectInput(inputId = "pollutant",
                    label = "Pollutant",
                    choices = c("PM2.5", "PM10", "NOx", "NO", "SO2", "CO", "PSI"),
                    selected = 20)
      ),
      mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Map", plotOutput("spot_map", width = "90%", height = "500px")),
                    tabPanel("3D Scatter Plot", plotlyOutput("plot")),
                    tabPanel("Data", 
                             h4("Summary Statistics"),
                             verbatimTextOutput("stats"),
                             h4("Data"),
                             tableOutput("table")),
                    tabPanel("Kriging prediction", plotlyOutput("krig"))
        )
      )
    )
  )
)



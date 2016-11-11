library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Taiwan Pollutant"),
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
        plotOutput("spot_map")
      )
    )
  )
)



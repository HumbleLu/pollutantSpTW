library(shiny)

## get current data
current_data<- function(){
  stations<- read.table("http://opendata.epa.gov.tw/ws/Data/AQXSite/?format=csv", sep = ",", header = T)
  hourly<- read.table("http://opendata.epa.gov.tw/ws/Data/AQX/?format=csv", sep = ",", header = T)
  air.quality<- merge(stations, hourly,by="SiteName")
  air.quality
}


## Spot map
library(ggmap)
library(gridExtra)

spot_map<- function(data, pollutant, color = "red"){
  map <- get_map(location = 'Taiwan', zoom = 8, maptype = "toner-lite")
  p.map<- ggmap(map) + labs(title = "Taiwan") + xlab(" ") + ylab(" ")
  p.map<- p.map + geom_point(aes_string(x = "TWD97Lon", 
                                        y = "TWD97Lat", 
                                        size = pollutant, 
                                        alpha = pollutant),
                             data = data,
                             color = color)
  p.map
}



# Server
shinyServer(
  function(input, output) {
    v <- reactiveValues(data = NULL)
    
    withProgress(message = 'Preparing Map..', {
      map <- get_map(location = 'Taiwan', zoom = 8, maptype = "toner-lite")
      startPlot<- ggmap(map) + labs(title = "Taiwan") + xlab(" ") + ylab(" ") + annotate("text", x = 121, y = 24.2, size = 8, label = "Please click \"Update data\"")
    })
  
    observeEvent(input$fetch, {
      withProgress(message = 'Update current data..', {
        v$data <- current_data()
      })
    })

    output$spot_map <- renderPlot({
      if (is.null(v$data)) return(startPlot)
      withProgress(message = 'Plotting..', {
        spot_map(v$data, input$pollutant)
      })
    })
  }
)




library(shiny)

## get current data
current_data<- function(){
  stations<- read.table("http://opendata.epa.gov.tw/ws/Data/AQXSite/?format=csv", sep = ",", header = T)
  hourly<- read.table("http://opendata.epa.gov.tw/ws/Data/AQX/?format=csv", sep = ",", header = T)
  air.quality<- merge(stations, hourly,by="SiteName")
  air.quality
}


## Spot map
spot_map<- function(air.quality, selection){
  map <- get_map(location = 'Taiwan', zoom = 7, maptype = "toner-lite")
  p.map<- ggmap(map) + 
    labs(title = "Taiwan") + 
    xlab(" ") + ylab(" ") + 
    geom_point(
      aes(x = TWD97Lon, y = TWD97Lat, size =  NO2, alpha = NO2), 
      data = air.quality, 
      color = "red") + 
    theme(legend.position="none") + 
    coord_equal() + coord_fixed()
  
  p.lon<- ggplot(data = air.quality) + 
    geom_point(
      aes_string("TWD97Lon", selection, 
                 size = selection, 
                 alpha = selection),
      color = "red"
    ) + 
    xlab("Longitude") +
    theme(legend.position="none") +
    xlim(as.numeric(attr(map, "bb")[, c("ll.lon", "ur.lon")]))
  
  p.lat<- ggplot(data = air.quality) + 
    geom_point(
      aes_string(selection, "TWD97Lat",
                 size = selection, 
                 alpha = selection),
      color = "red"
    ) + 
    ylab("Latitude") +
    ylim(as.numeric(attr(map, "bb")[, c("ll.lat", "ur.lat")]))
  
  empty<- ggplot() + geom_blank() + 
    theme(panel.background = element_blank())
  
  grid.arrange(p.lon, empty, p.map, p.lat, 
               nrow = 2, ncol = 2,
               widths = c(2, 1), heights = c(1,3))
}

data_display<- function(air.quality, selection){
  dt<- air.quality[, c("SiteName", "TWD97Lon", "TWD97Lat", selection, "PublishTime")]
  colnames(dt)<- c("SiteName", "Longitude", "Latitude", selection, "PublishTime")
  dt
}

# Server
shinyServer(
  function(input, output, session) {
    v <- reactiveValues(data = NULL)
    
    dataset<- reactive({
      data_display(v$data, input$pollutant)
    })
    
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
    
    output$plot <- renderPlotly({
      if (is.null(v$data)) return(NULL)
      
      pt.3d<- v$data[, c("TWD97Lon", "TWD97Lat", input$pollutant, "SiteName")]
      colnames(pt.3d)<- c("Longitude", "Latitude", "value", "SiteName")
      
      pt.3d$size<- pt.3d[, 3]
      
      scene = list(camera = list(eye = list(x = -.5, y = -.8, z = 1)))
      plot_ly(pt.3d, 
              x = ~Longitude, y = ~Latitude, z = ~value, 
              type = "scatter3d",
              size = ~size,
              text = ~paste('Site: ', SiteName)) %>%
        layout(scene = scene)
    })
    
    output$table<- renderTable({
      if (is.null(v$data)) return(data.frame())
      dataset()
    })
  }
)




#Loading in Libraries: 
library(tidyverse)
library(maps)
library(tidycensus)
library(leaflet)
library(dplyr)
library(shiny)
library(shinythemes)
library(sf)
library(rgdal) 
library(readxl)
library(tidygeocoder)
library(ggmap)
library(simplevis)
library(stars)
library(mapview)
library(leafgl)

#Loading in Shape Files:
janjuly2016 <- read_sf("Geo_Data/2016/NIJ2016_JAN01_JUL31.shp")
aug2016<- read_sf("Geo_Data/2016/NIJ2016_AUG01_AUG31.shp")
sept2016<- read_sf("Geo_Data/2016/NIJ2016_SEP01_SEP30.shp")
oct2016<- read_sf("Geo_Data/2016/NIJ2016_OCT01_OCT31.shp")
nov2016<- read_sf("Geo_Data/2016/NIJ2016_NOV01_NOV30.shp")
dec2016<- read_sf("Geo_Data/2016/NIJ2016_DEC01_DEC31.shp")


jan2017<- read_sf("Geo_Data/2017/NIJ2017_JAN01_JAN31.shp")
feb1to142017<- read_sf("Geo_Data/2017/NIJ2017_FEB01_FEB14.shp")
feb15to212017<- read_sf("Geo_Data/2017/NIJ2017_FEB15_FEB21.shp")
feb22to26 <- read_sf("Geo_Data/2017/NIJ2017_FEB22_FEB26.shp")

feb27<- read_sf("Geo_Data/2017/NIJ2017_FEB27.shp")
feb28<- read_sf("Geo_Data/2017/NIJ2017_FEB28.shp")
marchmay2017<- read_sf("Geo_Data/2017/NIJ2017_MAR01_MAY31.shp")


#Full years: 2012 only has data from march - dec. 
full_2012<-  read_sf("Geo_Data/2012/NIJ2012_MAR01_DEC31.shp")
full_2013<- read_sf("Geo_Data/2013/NIJ2013_JAN01_DEC31.shp")
full_2014<- read_sf("Geo_Data/2014/NIJ2014_JAN01_DEC31.shp")
full_2015<- read_sf("Geo_Data/2015/NIJ2015_JAN01_DEC31.shp")

# file merges:

#We cannot merge the full 2016 together yet until we push the dbf files. 
full2016 <- rbind(janjuly2016,aug2016,sept2016,oct2016,nov2016,dec2016)

#Full 2017 is good if you want to start a geo-spatial technique on this! 
full2017<- rbind(jan2017,feb1to142017,feb15to212017, feb22to26, feb27, feb28, marchmay2017)

#We cannot merge all years together until dbfs have been uploaded. 
all_data <- rbind(full_2012, full_2013, full_2014, full_2015, full2016, full2017)


ui<- fluidPage(
  navbarPage(
    theme = shinytheme("united"),
    "Portland Crime Analysis",
    tabPanel("Interactive Map",
             actionButton("revert", "Reset Back to Full Portland View"),
             sidebarPanel(width = 5, 

                          selectInput("Year", 
                                      label = "Choose a Year to Display",
                                      choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018"),
                                      selected = "2017")),
             div(
               id = "map_container",
               leafletOutput(height = "700px", "map")
             ),
             textOutput("coords")),
    tabPanel("About", "This panel is intentionally left blank"),
    tabPanel("Contact Us", "This panel is intentionally left blank")
  ))


my_centers <<- list()

finalfull2013 <- st_transform(full_2013, crs=4326)


server<- function(input, output, session) {

  output$map <-  renderLeaflet({
    states_map$zoom <- sample(5:8, size = length(states_map$name), replace = T)
    
    leaflet(data = finalfull2013, options = leafletOptions(minZoom = 11, preferCanvas = TRUE, zoomToLimits="first"))%>%
      addProviderTiles(provider = "CartoDB.Positron", options = providerTileOptions(
        updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
        updateWhenIdle = FALSE          # map won't load new tiles when panning
      ))%>%
      addGlPoints(data = finalfull2013[finalfull2013$CALL_GROUP == 'DISORDER',], group = "Disorder",  fillColor = 'lightyellow',fillOpacity = 1, radius = 6, popup = ~finalfull2013$CALL_GROUP)%>%
      addGlPoints(data = finalfull2013[finalfull2013$CALL_GROUP == 'NON CRIMINAL/ADMIN',],  fillColor = 'salmon',fillOpacity = 1, radius = 6, popup = ~finalfull2013$CALL_GROUP, group = 'Non Criminal/Admin')%>%
      addGlPoints(data = finalfull2013[finalfull2013$CALL_GROUP == 'TRAFFIC',], fillColor = 'lightpink', popup = ~finalfull2013$CALL_GROUP, group = 'Traffic', fillOpacity = 5, radius = 6)%>%
      addGlPoints(data = finalfull2013[finalfull2013$CALL_GROUP == 'SUSPICIOUS',],  fillColor = 'lightgrey', popup = ~finalfull2013$CALL_GROUP, group = 'Suspicious', fillOpacity = 5, radius = 6)%>%
      addGlPoints(data = finalfull2013[finalfull2013$CALL_GROUP == 'PROPERTY CRIME',],fillColor = 'orange', popup = ~finalfull2013$CALL_GROUP, group = 'Property Crime', fillOpacity = 5, radius = 6)%>%
      addGlPoints(data = finalfull2013[finalfull2013$CALL_GROUP == 'PERSON CRIME',],  fillColor = 'mediumaquamarine', popup = ~finalfull2013$CALL_GROUP, group = 'Person Crime', fillOpacity = 5, radius = 6)%>%
      addLayersControl(overlayGroups = c("Disorder", "Non Criminal/Admin", "Traffic", "Suspicious", "Property Crime", "Person Crime" ),
                       options = layersControlOptions(collapsed =FALSE),
                       position = 'bottomleft')%>%
      addLegend(colors = c('lightyellow', 'salmon', 'lightpink', 'lightgrey', 'orange', 'mediumaquamarine'),labels = c("Disorder", "Non Criminal/Admin", "Traffic", "Suspicious", "Property Crime", "Person Crime" ), opacity = 1)%>%
      setView(lng = -122.676483, 
              lat = 45.523064, 
              zoom = 11)%>%
      setMaxBounds(-122.75, 45.45, -122.5, 45.6)
  })
  
  observeEvent(input$revert, {
    
    if(is.null(input$revert))
      return()
    leafletProxy('map') %>% 
      setView(-122.676483,  45.523064, zoom = 11)
    
  })  
  
}



shinyApp(ui, server)


# QUESTION 3

# install.packages("shiny")
# install.packages("leaflet")
# install.packages("geosphere")
# install.packages("httr")
# install.packages("jsonlite")

library(shiny)
library(leaflet)
library(geosphere)
library(httr)
library(jsonlite)

# First, let's specify UCF's latitudinal
# and longitudinal coordinates (obtained
# from Google).

ucf_lat <- 28.6024
ucf_long <- -81.2001

# Next, let's specify the range that Florida's
# longitude and latitude can go from. 


florida_bounds <- list(lat_range = c(24.396308, 31.000888), 
                       long_range = c(-87.634896, -80.031362))

# Beginning with our user interface, we know
# that to create the shiny app, we need at
# least a textInput (for a user to put in a
# string) and two numericInputs (for a user
# to put in particular latitude and longitude
# points). Additionally, I added an action button
# so that once an individual typed in a specific
# address, city, state, and so on, pressing the
# button would cause the map to proceed to that
# location.


ui <- fluidPage(
  titlePanel("Florida Map with UCF and User Location"),
  sidebarLayout(
    sidebarPanel(
      textInput("location", "Location (city, address, etc.):", ""),
      actionButton("geocode_btn", "Find Location"),
      numericInput("latitude", "Latitude:", value = 28.5, min = 24.38, max = 31.1, step = 0.01),
      numericInput("longitude", "Longitude:", value = -81.3, min = -87.64, max = -80, step = 0.01)
    ),
    mainPanel(
      leafletOutput("map"),
      textOutput("distance")
    )
  )
)


# Now for the server. First, we tell the server
# that we are outputting a map, making clear the
# bounds of Florida and the indicator for UCF
# (given by the longitude and latitude point). The
# default beginning map is just a map of Florida.

# Next, because there is a chance one will input
# latitudinal and longitudinal coordinates that
# are outside of Florida (or types in an address,
# city, etc. outside of Florida), we need an if/else
# statement. If the coordinates/address specified
# lie within the bounds of Florida, the distance
# of that place from UCF is calculated in kilometers.
# A new indicator shows up at that point, so one
# might visualize the distance on the map. The
# distance is then outputted. However, if the
# coordinates are out of bounds, the text
# on the app changes to indicate that.

# The next part, with observeEvent, describes what
# happens when the user presses the button. It
# requires an input [req(input$location)], and then
# proceeds to get the location from the website con-
# taining location data. It then displays the indicator
# on that location (assuming it's within the bounds),
# and updates the latitude and longitude coordinates
# based on the location the user typed in. An error will
# also pop up if the location cannot be found.


server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      fitBounds(lng1 = florida_bounds$long_range[1], lat1 = florida_bounds$lat_range[1], 
                lng2 = florida_bounds$long_range[2], lat2 = florida_bounds$lat_range[2]) %>%
      addMarkers(lng = ucf_long, lat = ucf_lat, popup = "UCF")
  })
  
  observe({
    lat <- input$latitude
    lon <- input$longitude
    
    if (lat >= florida_bounds$lat_range[1] && lat <= florida_bounds$lat_range[2] &&
        lon >= florida_bounds$long_range[1] && lon <= florida_bounds$long_range[2]) {
      
      dist <- distHaversine(c(ucf_long, ucf_lat), c(lon, lat)) / 1000
      
      leafletProxy("map") %>%
        clearGroup("user_location") %>%
        addMarkers(lng = lon, lat = lat, popup = input$label, group = "user_location")
      
      output$distance <- renderText({
        paste("Distance to UCF:", round(dist, 2), "km")
      })
    } else {
      output$distance <- renderText({"Coordinates out of bounds."})
    }
  })
  
  observeEvent(input$geocode_btn, {
    req(input$location)
    
    url <- paste0("https://nominatim.openstreetmap.org/search?format=json&q=", 
                  URLencode(input$location))
    
    response <- GET(url)
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    if (length(data) > 0) {
      lat <- as.numeric(data$lat[1])
      lon <- as.numeric(data$lon[1])
      
      updateNumericInput(session, "latitude", value = lat)
      updateNumericInput(session, "longitude", value = lon)
      
      leafletProxy("map") %>%
        clearGroup("user_location") %>%
        addMarkers(lng = lon, lat = lat, popup = input$location, group = "user_location")
      
    } else {
      showNotification("Location not found. Try a different search.", type = "error")
    }
  })
}

shinyApp(ui, server)
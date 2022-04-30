library(shiny)
library(dplyr)
library(sf)
library(tmap)
bike_isochrones <- readRDS("data/bike_isochrones.RDS")
tmap_mode("view")
tmap_options(check.and.fix = TRUE)

start_points <- bike_isochrones
bike_isochrones <- bike_isochrones %>% 
  mutate(time_formatted = as.factor(paste0(time/60, " minutes"))) %>% 
  st_set_geometry(., "iso") %>% 
  st_make_valid() #some isochrones have geometry issues that tmap doesn't like

#set map marker icon
tmapIcon <- tmap_icons("https://raw.githubusercontent.com/Rush/Font-Awesome-SVG-PNG/master/black/png/48/map-marker.png")

#list of start locations for the pulldown menu in the UI
start_locations <- bike_isochrones %>%
  st_drop_geometry() %>% 
  distinct(name) %>% 
  arrange(name) %>% 
  pull()

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("🚍 How far can you go? 🚲"),

    # Sidebar with input for start location mode 
    
    fluidRow(
      column(3,
            radioButtons("mode_input",
                         "Bike or e-bike?",
                         c("Bike" = "bike", 
                           "E-bike" = "e-bike")),
            selectInput(
              "location_input",
              "Select start location",
              start_locations,
              multiple = F,
              selected = "Allied at Lovell"
            )
            ),
      column(7,
             p(
               "Metro is ", a("redesigning Madison's bus network.", href = "https://www.cityofmadison.com/metro/routes-schedules/transit-network-redesign"), "The goal is to create a more efficient system, with more frequent service and a less complicated network. However, since the budget is fixed, more frequent service means that some areas currently served by transit will have less or no service."
             ),
             p(
               "To show the impact of the proposed network, Metro published maps that show how far you would be be able to get by bus within 45 minutes (including the walk to the nearest stop and waiting time) for a number of locations in the city. The maps show access both before and after the redesign. Note that a number of changes have been proposed to the draft network; the maps here do not include those."
             ),
             p(
               "Transit is awesome, and so are bikes. For some trips, biking or e-biking may be an alternative for some people. How far can you get from those same locations by bike or e-bike, in 15, 30, or 45 minutes? Select your start location and whether you travel by bike or e-bike, and the app will show you a map."
             )
             )
    ),
        # Show a plot of the generated distribution
      fluidRow(
            column(6,
          h1("How far you can travel by bus:"),
          imageOutput("busMap",
                      #width = "80%",
                      inline = F)
            ),
          column(6,
          h1(textOutput("mode")),
          tmapOutput("bikeMap",
                     width = "100%",
                     height = "850px")
        )),
      fluidRow(
        column(10, div(style = "padding-top:10px"),
p("Do you have feedback on the network redesign? Take a survey by April 30:", a("https://www.cityofmadison.com/metro/routes-schedules/transit-network-redesign", href = "https://www.cityofmadison.com/metro/routes-schedules/transit-network-redesign")),
  p("Bus maps: City of Madison/Jarrett Walker Associates", 
  br(), 
  "Bike maps:", a("OpenRouteService", href="https://openrouteservice.org/"), 
  br(), 
  "App:", a("Harald Kliems", href="https://haraldkliems.netlify.app"),
  br(),
  "Code:", a("Github", href = "https://github.com/vgXhc/network_redesign_isochrones"),
  br(),
  "Comments about the app? Email", a("Harald", href = "mailto:kliems@gmail.com"), "or ", a("create an issue on Github", href = "https://github.com/vgXhc/network_redesign_isochrones/issues")
)
))) 


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$busMap <- renderImage({
    
    width  <- session$clientData$output_busMap_width
    #generate filename
    outfile <- paste0("img/", input$location_input, ".png")
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = width,
         #height = height,
         alt = "A map showing how far you can get in 45 minutes by bus")
    }, deleteFile = F)
  
  output$bikeMap <- renderTmap({
    bike_isochrones %>% 
      filter(bike_type == input$mode_input & name == input$location_input) %>% 
      tm_shape(unit = "imperial") +
      tm_polygons(col = "time_formatted",
                  title = "Riding time",
                  alpha = .2) +
    tm_basemap(leaflet::providers$Stamen.TonerLite) +
      tm_scale_bar() + #scale bar to make comparisons easier
      tm_view(set.view = 12) + #initial zoom level to be similar to bus map
      tm_shape(start_points %>% filter(bike_type == input$mode_input & name == input$location_input)) +
      tm_symbols(shape = tmapIcon)
  }
  )
  output$location <- renderText({
    input$location_input
    })
  
  output$mode <- renderText({
    paste0("How far you can travel from ", input$location_input, " by ", input$mode_input, ":")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

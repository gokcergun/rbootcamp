
# Libraries that are used

library(lubridate)
library(tidyverse)
library(shiny)
library(leaflet)
library(data.table)


##read data 

sf_trees <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv", 
                  select = c("species", 'latitude', "longitude"))
  

##Separate the species column into latin and common name columns.

sf_trees <- sf_trees %>% 
  separate(species, sep = "::", remove = FALSE, into = c('species_lat', 'species_nor'))

sf_trees$species_nor <- as.factor(sf_trees$species_nor)

##map of recommended species per  (https://sfenvironment.org/sites/default/files/fliers/files/sf_tree_guide.pdf) : 
recommended_sp <- c(' Japanese Blueberry Tree', ' Flaxleaf Paperbark', ' Red Flowering Gum', ' Flowering Cherry', 
                    ' Little Gem Magnolia', ' Southern Magnolia', ' Weeping Bottlebrush', ' Hybrid Strawberry Tree', 
                    ' Primrose Tree', ' Brisbane Box', ' Bronze Loquat', ' Peppermint Willow', ' Mediterranean Fan Palm', 
                    ' Fruitless Olive', ' Chilean Soapbark', " Small-leaf Tristania 'Elegant'", ' Chinese Pistache', 
                    ' Trident Maple', ' Chinese Elm', ' Cork Oak', ' Ginkgo: Autumn Gold', ' Fairmont Ginkgo', 
                    ' Ginkgo: Saratoga', ' Autumn Sentinel Ginkgo'
)


df_trees_recommended <- sf_trees %>% 
  filter(species_nor %in% recommended_sp)


##map center
mlong = -122.4446
mlat  = 37.72

pal <- colorFactor('Paired', domain = recommended_sp)


##Define the UI
ui = fluidPage(
  titlePanel(h1("Recommended Tree Species of San Francisco", align = "left")),
  helpText( a("See the Source of Recommended Trees",  
              href="https://sfenvironment.org/sites/default/files/fliers/files/sf_tree_guide.pdf"), align='left'),
  helpText( a("View Code",  
              href="https://github.com/gokcergun/rbootcamp/blob/main/Erguen_Garzon/Scripts/App.R"), align='left'),
  sidebarLayout(          ##Sidebar layout with input and output definitions
    sidebarPanel(         ##Sidebar panel for input
        checkboxGroupInput(inputId = "species", #name of the input, widget
                           label = "Choose a species:",
                           choices = c('All Species', ' Trident Maple', ' Weeping Bottlebrush', ' Mediterranean Fan Palm', ' Bronze Loquat', 
                                       ' Little Gem Magnolia', ' Flowering Cherry', ' Hybrid Strawberry Tree', " Small-leaf Tristania 'Elegant'",
                                       ' Peppermint Willow', ' Japanese Blueberry Tree',' Flaxleaf Paperbark', ' Fruitless Olive', 
                                       ' Chinese Pistache', ' Red Flowering Gum', ' Primrose Tree', ' Brisbane Box', ' Southern Magnolia', ' Cork Oak', 
                                       ' Chilean Soapbark', ' Ginkgo: Autumn Gold', ' Fairmont Ginkgo', ' Ginkgo: Saratoga', 
                                       ' Autumn Sentinel Ginkgo', ' Chinese Elm'
                           ), 
                           selected = ' Little Gem Magnolia'
        )),
    
    mainPanel(             ##Main panel for displaying outputs
      tags$style(type = "text/css",                   ##Hide errors
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        
   
      leafletOutput("map", height = 800)    ##Output: interactive leaflet map
        
    )
   )
)
  
server = function(input, output) {        ##Define the server
    filteredData <- reactive({
      if (input$species == "All Species") {
        df_trees_recommended
      } else {
        filter(df_trees_recommended, species_nor == input$species)
      }
    })
    
  output$map <- renderLeaflet({
      
    leaflet(filteredData(), 
            width = '100%', 
            options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
      addTiles() %>%
      setView(lng = mlong, lat = mlat, zoom = 12) %>%
      addCircleMarkers(lng = filteredData()$longitude, 
                       lat = filteredData()$latitude,
                       popup = filteredData()$species_nor,
                       color = ~pal(filteredData()$species_nor),
                       label = ~species_nor, 
                       radius = 4, 
                       fillOpacity = 0.99)
  })
}


shinyApp(ui, server)




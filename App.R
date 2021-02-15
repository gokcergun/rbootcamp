#map center
mlong = -122.4446
mlat  = 37.72

pal <- colorFactor('Paired', domain = recommended_sp, reverse = TRUE)

shinyApp(
  
  # Define the UI
  ui = fluidPage(
    
    # App title
    titlePanel("Recommended Tree Species of San Francisco"),
    
    #tabPanel(a(href='https://sfenvironment.org/sites/default/files/fliers/files/sf_tree_guide.pdf', 'Source')),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
      
      # Sidebar panel for inputs 
      sidebarPanel(
        
        # First input: Type of data
        
        checkboxGroupInput(inputId = "species", #name of the input, widget
                           label = "Choose a species:",
                           choices = c('All Species',  ' Japanese Blueberry Tree', ' Flaxleaf Paperbark', ' Red Flowering Gum', 
                                       ' Flowering Cherry', ' Little Gem Magnolia', ' Southern Magnolia', ' Weeping Bottlebrush', 
                                       ' Hybrid Strawberry Tree', ' Primrose Tree', ' Brisbane Box', ' Mediterranean Fan Palm', 
                                       ' Fruitless Olive', ' Chilean Soapbark', " Small-leaf Tristania 'Elegant'", ' Chinese Pistache', 
                                       ' Trident Maple', ' Chinese Elm', ' Cork Oak', ' Ginkgo: Autumn Gold', ' Fairmont Ginkgo', 
                                       ' Ginkgo: Saratoga', ' Autumn Sentinel Ginkgo'
                           ), 
                           selected = ' Japanese Blueberry Tree'
        )),
      
      # Main panel for displaying outputs
      mainPanel(
        
        # Hide errors
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        
        # Output: interactive world map
        leafletOutput("map", height = 800)
        
      )
    )
  ),
  
  # Define the server
  server = function(input, output) {
    
    filteredData <- reactive({
      if (input$species == "All Species") {
        df_trees_filtered
      } else {
        filter(df_trees_filtered, species_nor == input$species)
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
)



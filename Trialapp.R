
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
recommended_sp <- c(' Trident Maple', ' Weeping Bottlebrush', ' Mediterranean Fan Palm', ' Bronze Loquat', 
                    ' Little Gem Magnolia', ' Flowering Cherry', ' Hybrid Strawberry Tree', " Small-leaf Tristania 'Elegant'",
                    ' Peppermint Willow', ' Japanese Blueberry Tree',' Flaxleaf Paperbark', ' Fruitless Olive', 
                    ' Chinese Pistache', ' Red Flowering Gum', ' Primrose Tree', ' Brisbane Box', ' Southern Magnolia', ' Cork Oak', 
                    ' Chilean Soapbark', ' Ginkgo: Autumn Gold', ' Fairmont Ginkgo', ' Ginkgo: Saratoga', 
                    ' Autumn Sentinel Ginkgo', ' Chinese Elm'
)



small_trees <- c(' Trident Maple', ' Weeping Bottlebrush', ' Mediterranean Fan Palm', ' Bronze Loquat', ' Little Gem Magnolia',
                 ' Flowering Cherry')

medium_trees <- c(' Hybrid Strawberry Tree', " Small-leaf Tristania 'Elegant'", ' Peppermint Willow', ' Japanese Blueberry Tree', 
                  ' Flaxleaf Paperbark', ' Fruitless Olive', ' Chinese Pistache', ' Red Flowering Gum', ' Primrose Tree')

large_trees <- c(' Brisbane Box', ' Southern Magnolia', ' Cork Oak', ' Chilean Soapbark', ' Ginkgo: Autumn Gold', 
                 ' Fairmont Ginkgo', ' Ginkgo: Saratoga', ' Autumn Sentinel Ginkgo', ' Chinese Elm') 



df_trees_recommended <- sf_trees %>% 
  filter(species_nor %in% recommended_sp)


##Create height column. 
df_trees_recommended <- df_trees_recommended %>%
  mutate(height = case_when(species_nor %in% small_trees ~ 'small', 
                            species_nor %in% medium_trees ~ 'medium',
                            species_nor %in% large_trees ~ 'large'))


##map center
mlong = -122.4446
mlat  = 37.72

pal <- colorFactor('Paired', domain = recommended_sp)


##Define the UI

ui = fluidPage(
  titlePanel(h1("Recommended Tree Species of San Francisco", align = "left")),
  #titlePanel( h3('NAME', align = 'right')),
  helpText( a("See the Source of Recommended Trees",  
              href="https://sfenvironment.org/sites/default/files/fliers/files/sf_tree_guide.pdf"), align='left'),
  helpText( a("View Code",  
              href="https://github.com/gokcergun/rbootcamp/blob/main/App.R"), align='left'),
  sidebarLayout(          ##Sidebar layout with input and output definitions
    sidebarPanel(         ##Sidebar panel for input
      checkboxGroupInput(inputId = "height", #name of the input, widget
                         label = "Choose a height:",
                         choices = c('All Species', 'Small', 'Medium', "Large")
      ), 
 
      checkboxGroupInput(inputId = "species", #name of the input, widget
                         label = "Choose a species:",
                         choices = c(' Trident Maple', ' Weeping Bottlebrush', ' Mediterranean Fan Palm', ' Bronze Loquat', 
                                     ' Little Gem Magnolia', ' Flowering Cherry', ' Hybrid Strawberry Tree', " Small-leaf Tristania 'Elegant'",
                                     ' Peppermint Willow', ' Japanese Blueberry Tree',' Flaxleaf Paperbark', ' Fruitless Olive', 
                                     ' Chinese Pistache', ' Red Flowering Gum', ' Primrose Tree', ' Brisbane Box', ' Southern Magnolia', ' Cork Oak', 
                                     ' Chilean Soapbark', ' Ginkgo: Autumn Gold', ' Fairmont Ginkgo', ' Ginkgo: Saratoga', 
                                     ' Autumn Sentinel Ginkgo', ' Chinese Elm'
                         ), 
                         selected = ' Little Gem Magnolia'
                         
      ))
    ,
    
    mainPanel(           ##Main panel for displaying outputs
      
      tags$style(type = "text/css",                   ##Hide errors
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      
      
      leafletOutput("map", height = 800)    ##Output: interactive leaflet map
    )
  ))

server = function(input, output, session) {    ##Define the server
  
  observe({
    
    x <- input$height
    
    # Can use character(0) to remove all choices
    if (is.null(x)){
      updateCheckboxGroupInput(session, "species", selected = ' Little Gem Magnolia')
     } else if (x == 'All Species'){
        updateCheckboxGroupInput(session, "species", choices = recommended_sp, selected = recommended_sp)
      } else if (x == 'Small'){ 
          updateCheckboxGroupInput(session, "species", choices = recommended_sp, selected = small_trees)
      } else if (x == 'Medium'){
          updateCheckboxGroupInput(session, "species", choices = recommended_sp, selected = medium_trees)
      } else if (x == 'Large'){
          updateCheckboxGroupInput(session, "species", choices = recommended_sp, selected = large_trees)
      } else if (x == 'Small' & x == 'Medium'){ 
        updateCheckboxGroupInput(session, "species", choices = recommended_sp, selected = c(small_trees, medium_trees))
      } else if (x == 'Small' & x == 'Medium' & x == 'Large'){ 
        updateCheckboxGroupInput(session, "species", choices = recommended_sp, selected = recommended_sp)
      } else if (x == 'Small' & x == 'Larger'){ 
        updateCheckboxGroupInput(session, "species", choices = recommended_sp, selected = c(small_trees, large_trees))
      } else if (x == 'Large' & x == 'Medium'){ 
          updateCheckboxGroupInput(session, "species", choices = recommended_sp, selected = c(large_trees, medium_trees))
        
            }
    
  
    
    })
  
  filteredData <- reactive({
    if (input$height == "All Species") {
      df_trees_recommended
    } else {
      filter(df_trees_recommended, height == input$height)
    }
  })
  filteredData <- reactive({
    filter(df_trees_recommended, species_nor == input$species)
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


# choiceNames = list(HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#A6CEE3;margin-top:3px;"></i><div style="color:black;padding:5px;">1</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#1F78B4;margin-top:3px;"></i><div style="color:black;padding:5px;">2</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#B2DF8A;margin-top:3px;"></i><div style="color:black;padding:5px;">3</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#33A02C;margin-top:3px;"></i><div style="color:black;padding:5px;">1</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:"#FB9A99";margin-top:3px;"></i><div style="color:black;padding:5px;">2</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#E31A1C;margin-top:3px;"></i><div style="color:black;padding:5px;">3</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#FDBF6F;margin-top:3px;"></i><div style="color:black;padding:5px;">1</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#FF7F00;margin-top:3px;"></i><div style="color:black;padding:5px;">2</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#CAB2D6;margin-top:3px;"></i><div style="color:black;padding:5px;">3</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#6A3D9A;margin-top:3px;"></i><div style="color:black;padding:5px;">1</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#FFFF99;margin-top:3px;"></i><div style="color:black;padding:5px;">2</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#B15928;margin-top:3px;"></i><div style="color:black;padding:5px;">3</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#A6CEE3;margin-top:3px;"></i><div style="color:black;padding:5px;">1</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#1F78B4;margin-top:3px;"></i><div style="color:black;padding:5px;">2</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#B2DF8A;margin-top:3px;"></i><div style="color:black;padding:5px;">3</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#33A02C;margin-top:3px;"></i><div style="color:black;padding:5px;">1</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:"#FB9A99";margin-top:3px;"></i><div style="color:black;padding:5px;">2</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#E31A1C;margin-top:3px;"></i><div style="color:black;padding:5px;">3</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#FDBF6F;margin-top:3px;"></i><div style="color:black;padding:5px;">1</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#FF7F00;margin-top:3px;"></i><div style="color:black;padding:5px;">2</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#CAB2D6;margin-top:3px;"></i><div style="color:black;padding:5px;">3</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#6A3D9A;margin-top:3px;"></i><div style="color:black;padding:5px;">1</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#FFFF99;margin-top:3px;"></i><div style="color:black;padding:5px;">2</div></div>'),
#                    HTML('<div style="display:flex"><i class="fa fa-circle"
#                                          style="color:#B15928;margin-top:3px;"></i><div style="color:black;padding:5px;">3</div></div>')), 
# 
# choiceValues =   c(' Trident Maple', ' Weeping Bottlebrush', ' Mediterranean Fan Palm', ' Bronze Loquat', 
#                    ' Little Gem Magnolia', ' Flowering Cherry', ' Hybrid Strawberry Tree', " Small-leaf Tristania 'Elegant'",
#                    ' Peppermint Willow', ' Japanese Blueberry Tree',' Flaxleaf Paperbark', ' Fruitless Olive', 
#                    ' Chinese Pistache', ' Red Flowering Gum', ' Primrose Tree', ' Brisbane Box', ' Southern Magnolia', ' Cork Oak', 
#                    ' Chilean Soapbark', ' Ginkgo: Autumn Gold', ' Fairmont Ginkgo', ' Ginkgo: Saratoga', 
#                    ' Autumn Sentinel Ginkgo', ' Chinese Elm')


# Copyright 2019 John Murzaku
#    
#    Permission is hereby granted, free of charge, to any person obtaining 
#    a copy of this software and associated documentation files (the "Software"), 
#    to deal in the Software without restriction, including without limitation the 
#    rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
#    sell copies of the Software, and to permit persons to whom the Software is 
#    furnished to do so, subject to the following conditions:
#    
#    The above copyright notice and this permission notice shall be included in 
#    all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
# SOFTWARE.

library(shiny)
library(leaflet)
source("global.R", local = TRUE)

#Define UI
ui <- fluidPage(
    # Application title
    titlePanel("openSenseMap data for Nordrhein Westfalen"),
    sidebarLayout(
        sidebarPanel(
            selectInput("phenom",
                        label = "Choose a phenomenon to display",
                        choices = "Temperature",
                        selected = "Temperature"
                        ),
            br(),
            #Create input for phenomena
            selectInput("stat",
                        label = "Choose the outlier detection method",
                        choices = c("Cook's distance", "IQR"),
                        selected = "Cook's distance"
            ),
            br(),
            #Create input for type of data
            selectInput("type",
                        label = "Choose the type of data to display",
                        choices = c("All", "Potential anomaly", "Defective box", "Normal"),
                        selected = "All"),
            br(),
            tags$div(class="header", checked=NA,
                     tags$p("Summary of data")
                     ),
            verbatimTextOutput("summary"),
            br(),
            downloadButton("download", "Download selected data")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(leafletOutput("map", width = "100%", height = 500))
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #Convert data to reactive 
    data <- reactive({
        region_boxes
        phenom_df
        normal_temp_df
    })
    
    #Make leaflet
    output$map <- renderLeaflet({
        region_boxes <- data()
        
        leaflet(region_boxes) %>%
            addTiles() %>%
            addLegend(
                colors = c('#00851f', '#f2ff00', '#ff0000'),
                labels = c('Normal data', 'Potential anomaly', 'Defective box'),
                title = "Legend for data point colors",
                opacity = 1
            ) %>%
            fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })
    
    #Show data on leaflet based on selected input
    observeEvent({input$type
        input$stat}, {
        proxy <- leafletProxy('map')
        
        #Cook's Distance input
        if(input$type == "Normal" & input$stat == "Cook's distance"){
            proxy %>% 
                clearMarkers() %>%
                addCircleMarkers(lng = normal_temp$lon, lat = normal_temp$lat, radius = 4, color = '#00851f', popup = paste("Box ID:", normal_temp_df$box_id, "<br>","Temperature:", normal_temp_df$value, "Celsius", "</br>"), stroke = FALSE, fillOpacity = 1)
            output$summary <- renderPrint({
                summary(normal_temp_df$value)
            })
            output$download <- downloadHandler(
                filename = function() {
                    paste("normal_cooks_values", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(unique(normal_temp_df), file)
                }
            )
        }
        
        else if(input$type == "Potential anomaly" & input$stat == "Cook's distance"){
            proxy %>% 
                clearMarkers() %>%
                addCircleMarkers(lng = local_anomaly_df$lon, lat = local_anomaly_df$lat, radius = 6, color = '#f2ff00', popup = paste("Box ID:", local_anomaly_df$box_id, "<br>", "Temperature:", local_anomaly_df$value, "Celsius", "</br>"),  stroke = FALSE, fillOpacity = 1)
            output$summary <- renderPrint({
                summary(local_anomaly_df$value)
            })
            output$download <- downloadHandler(
                filename = function() {
                    paste("cooks_potential_anomaly_values", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(unique(local_anomaly_df), file)
                }
            )
        }
        
        else if(input$type == "Defective box" & input$stat == "Cook's distance"){
            proxy %>% 
                clearMarkers() %>%
                addCircleMarkers(lng = influential_boxes$lon, lat = influential_boxes$lat, radius = 6, color = '#ff0000', popup = paste("Box ID:", influential_boxes$box_id, "<br>","Temperature:", influential_boxes$value, "Celsius", "</br>"), stroke = FALSE, fillOpacity = 1)
            output$summary <- renderPrint({
                summary(influential_boxes$value)
            })
            output$download <- downloadHandler(
                filename = function() {
                    paste("cooks_defective_sensors", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(unique(influential_df), file)
                }
            )
        }
        else if(input$type == "All" & input$stat == "Cook's distance"){
            proxy %>% 
                clearMarkers() %>%
                addCircleMarkers(lng = (normal_temp$lon), lat = (normal_temp$lat), radius = 4, color = '#00851f', popup = paste("Box ID:", normal_temp_df$box_id, "<br>","Temperature:", normal_temp_df$value, "Celsius", "</br>"), stroke = FALSE, fillOpacity = 1) %>%
                addCircleMarkers(lng = (local_anomaly_df$lon), lat = (local_anomaly_df$lat), radius = 6, color = '#f2ff00', popup = paste("Box ID:", local_anomaly_df$box_id, "<br>", "Temperature:", local_anomaly_df$value, "Celsius", "</br>"),  stroke = FALSE, fillOpacity = 1) %>%
                addCircleMarkers(lng = (influential_boxes$lon), lat = (influential_boxes$lat), radius = 6, color = '#ff0000', popup = paste("Box ID:", influential_boxes$box_id, "<br>","Temperature:", influential_boxes$value, "Celsius", "</br>"), stroke = FALSE, fillOpacity = 1)
            output$summary <- renderPrint({
                summary(phenom_df$value)
            })
            output$download <- downloadHandler(
                filename = function() {
                    paste("all_temp_data", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(phenom_df, file)
                }
            )
        }
        
        #IQR input
        if(input$type == "Defective box" & input$stat == "IQR"){
            proxy %>%
                clearMarkers() %>%
                addCircleMarkers(lng = defective_boxes_iqr$lon, lat = defective_boxes_iqr$lat, radius = 6, color = '#ff0000', popup = paste("Box ID:", defective_boxes_iqr$box_id, "<br>","Temperature:", defective_boxes_iqr$value, "Celsius", "</br>"), stroke = FALSE, fillOpacity = 1)
            output$summary <- renderPrint({
                summary(defective_boxes_iqr$value)
            })
            output$download <- downloadHandler(
                filename = function() {
                    paste("defective_sensors_iqr", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(unique(defective_boxes_iqr), file)
                }
            )
        }
        
        else if(input$type == "Potential anomaly" & input$stat == "IQR"){
            proxy %>%
                clearMarkers() %>%
                addCircleMarkers(lng = potential_anomalies_iqr$lon, lat = potential_anomalies_iqr$lat, radius = 6, color = '#f2ff00', popup = paste("Box ID:", potential_anomalies_iqr$box_id, "<br>", "Temperature:", potential_anomalies_iqr$value, "Celsius", "</br>"),  stroke = FALSE, fillOpacity = 1)
            output$summary <- renderPrint({
                summary(potential_anomalies_iqr$value)
            })
            output$download <- downloadHandler(
                filename = function() {
                    paste("iqr_potential_anomaly", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(unique(potential_anomalies_iqr), file)
                }
            )
        }
        else if(input$type == "Normal" & input$stat == "IQR"){
            proxy %>%
                clearMarkers() %>%
                addCircleMarkers(lng = normal_iqr_values_df$lon, lat = normal_iqr_values_df$lat, radius = 4, color = '#00851f', popup = paste("Box ID:", normal_iqr_values_df$box_id, "<br>","Temperature:", normal_iqr_values_df$value, "Celsius", "</br>"), stroke = FALSE, fillOpacity = 1)
            output$summary <- renderPrint({
                summary(normal_iqr_values_df$value)
            })
            output$download <- downloadHandler(
                filename = function() {
                    paste("normal_iqr_values", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(unique(normal_iqr_values_df), file)
                }
            )
        }
        else if(input$type == "All" & input$stat == "IQR"){
            proxy %>%
                clearMarkers() %>%
                addCircleMarkers(lng = (defective_boxes_iqr$lon), lat = (defective_boxes_iqr$lat), radius = 6, color = '#ff0000', popup = paste("Box ID:", defective_boxes_iqr$box_id, "<br>","Temperature:", defective_boxes_iqr$value, "Celsius", "</br>"), stroke = FALSE, fillOpacity = 1) %>%
                addCircleMarkers(lng = (potential_anomalies_iqr$lon), lat = (potential_anomalies_iqr$lat), radius = 6, color = '#f2ff00', popup = paste("Box ID:", potential_anomalies_iqr$box_id, "<br>", "Temperature:", potential_anomalies_iqr$value, "Celsius", "</br>"),  stroke = FALSE, fillOpacity = 1) %>%
                addCircleMarkers(lng = (normal_iqr_values_df$lon), lat = (normal_iqr_values_df$lat), radius = 4, color = '#00851f', popup = paste("Box ID:", normal_iqr_values_df$box_id, "<br>","Temperature:", normal_iqr_values_df$value, "Celsius", "</br>"), stroke = FALSE, fillOpacity = 1)
            output$summary <- renderPrint({
                summary(phenom_df$value)
            })
            output$download <- downloadHandler(
                filename = function() {
                    paste("all_temp_data", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(phenom_df, file)
                }
            )
        }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

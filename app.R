#Code to create chart of life expectancy and healthy life expectancy by Scottish council.

############################.
## Global ----
############################.
############################.
##Packages 

library(dplyr) #data manipulation
library(plotly) #charts
library(shiny)

#Preparing data - not needed unless new data coming through
# library(tidyr)
library(readr)

le_hle_by_council <- read_csv("shiny_app/data/le_hle_by_councils.csv")

#Use for selection of areas
council_list <- sort(unique(le_hle_by_council$council[le_hle_by_council$council != "Scotland"]))

############################.
## Visual interface ----
############################.
#Height and widths as percentages to allow responsiveness
#Using divs as issues with classing css 
ui <- fluidPage(style="width: 650px; height: 500px; ", 
                div(style= "width:100%", #Filters on top of page
                    h4("Chart 2. Life Expectancy and Healthy Life Expectancy by Scottish Councils"),
                    div(style = "width: 40%; float: left;",
                        selectInput("measure", label = "Select a measure type",
                                    choices = c("Life Expectancy", "Healthy Life Expectancy"), selected = "Life Expectancy")
                    ),
                    div(style = "width: 40%; float: left;",
                        selectInput("council", label = "Select council", 
                                    choices = council_list))
                ),
                
                div(style = "width: 20%; float: left;",
                    selectInput("sex", label = "Select sex",
                                choices = c("Male", "Female"), selected = "Female")
                ),
                
                
                div(style= "width:100%; float: left;", #Main panel
                    plotlyOutput("chart", width = "100%", height = "350px"),
                    p(div(style = "width: 25%; float: left;", #Footer
                          HTML("Source: <a href='https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy' target='_blank'>NRS</a>")),
                      div(style = "width: 25%; float: left;",
                          downloadLink('download_data', 'Download data'))
                      
                    )
                )
)

############################.
## Server ----
############################.
server <- function(input, output) {
  
  # Allowing user to download data
  output$download_data <- downloadHandler( 
    filename =  'life_expectancy_and_healthy_life_expectancy_in_scotland_by_nhs_board.csv', content = function(file) { 
      write.csv(life_expectancy_and_healthy_life_expectancy_by_nhs_board, file, row.names=FALSE) })
  
  ############################.
  #Visualization
  output$chart <- renderPlotly({
    
    plot_ly() %>%
      layout(yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
             xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
             font = list(family = 'Arial, sans-serif')) %>% 
      config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
    
    
    #Data for Scotland line
    data_scot <- le_hle_by_council %>% subset(council=="Scotland" & measure==input$measure
                                      & sex==input$sex)
    #Data for council line
    data_council <- le_hle_by_council %>% subset(council==input$council & measure==input$measure
                                       &  sex==input$sex) 
    
    
    #y axis title
    yaxistitle <- ifelse(input$measure == "Life Expectancy", "Life Expectancy", "Healthy Life Expectancy")
    
    plot <- plot_ly(data=data_council, x=~year, y = ~value, 
                    type = "scatter", mode = 'lines',  line = list(color = '#08519c'),
                    name = unique(data_council$council), width = 650, height = 350) %>% 
      add_lines(data = data_scot, y = ~value, mode = 'lines', 
                name = "Scotland", line = list(color = '#000000')) %>%
      
      
      #Layout
      layout(annotations = list(), #It needs this because of a buggy behaviour
             yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
             xaxis = list(title = "Year",  fixedrange=TRUE, tickangle = 270),  
             font = list(family = 'Arial, sans-serif'), #font
             margin = list(pad = 4, t = 50), #margin-paddings
             hovermode = 'false'  # to get hover compare mode as default
      ) %>% 
      config(displayModeBar= T, displaylogo = F, editable =F) # taking out plotly logo and collaborate button
    
  }) 
  
} # end of server part

############################.
## Calling app ----
############################.

shinyApp(ui = ui, server = server)

##END
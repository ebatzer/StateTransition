#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(igraph)

source("stmhelper.R")

msmfits <- read.csv("../data/msmfits.csv", stringsAsFactors = FALSE)
plantings <- unique(msmfits$Planting)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("State-Transition in California Grasslands"),
  
  sidebarLayout(
    sidebarPanel(h3("Select Covariates"),
                 helpText("Choose the species mixture used in planting and 
                          drought stress used to generate a state-transition model"),
                 
                 selectInput("selected_planting", 
                             label = "Planting Mixture",
                             choices = as.list(plantings),
                             selected = plantings[1]),
                 
                 sliderInput("spei_val", 
                             label = "Drought Index:",
                             min = -2, max = 2, value = 0),
                 
                 helpText("SPEI (Standardized Precipitation-Evapotranspiration Index) 
                          varies between -2 (extreme drought) and +2 (extreme wet period)")),
    
  mainPanel(h1("main panel"),
              p("Here I can add some more description"),
            textOutput("selected_vars"),
            plotOutput("stmplot"))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output){
  
  output$selected_vars <- renderText({
    paste("Planting :", input$selected_planting, 
          "SPEI :", input$spei_val)
  })
  
  output$stmplot <- renderPlot({
    
    edge_df <- msmfits[msmfits$Planting == input$selected_planting & 
                         msmfits$Precip == input$spei_val,]
    
    plot_stm(edge_df)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


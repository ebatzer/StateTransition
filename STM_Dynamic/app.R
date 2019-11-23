#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(tidyverse)

source("stmhelper.R")

msmfits <- read.csv("msmfits.csv", stringsAsFactors = FALSE)
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
                             min = -2, max = 2, value = 0, step = .25),
                 
                 helpText("SPEI (Standardized Precipitation-Evapotranspiration Index) 
                          varies between -2 (extreme drought) and +2 (extreme wet period)"),
                 '',
                 h3("Transition Probability Estimates"),
                 tableOutput('ttable'),
                 p(strong("State 1:"), "Native perennial grasses, defined primarily by Stipa pulchra and Elymus glaucus"),
                 p(strong("State 2:"),  "Naturalized annual grasses (subset), defined primarily by Bromus hordeaceous and Festuca perennis"),
                 p(strong("State 3:"),  "Invasive annual grasses, defined by Elymus caput-medusae and Aegilops triuncialis"),
                 p(strong("State 4:"),  "Naturalized annual grasses (subset), defined primarily by Avena fatua and Bromus diandrus")),
    
  mainPanel(em("Note: This Shiny app is designed to accompany a presentation by Batzer et al. at SER 2019"),
            br(),br(),
            p("State-transition models are often used as conceptual tools in management of arid- and semi-arid
              systems. By describing vegetation as a series of state types, these models attempt to identify the stability
              of different vegetation configurations, as well as the drivers of changes between vegetation types."),
              imageOutput("myImage", height = "100%", width = "100%"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  output$selected_vars <- renderText({
    paste("Planting :", input$selected_planting, 
          "SPEI :", input$spei_val)
  })

  output$myImage <- renderImage({ 
    
    edge_df <- msmfits[msmfits$Planting == input$selected_planting & 
                         msmfits$Precip == input$spei_val,]
    
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, 
        width = 500*4, 
        height = 500*4,
        res = 72*4)
    print(plot_stm(edge_df))
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 800,
         height = 800,
         alt = "State-Transition Model Output")
    }, deleteFile = TRUE)
  
  output$ttable <- renderTable({  
    edge_df <- msmfits[msmfits$Planting == input$selected_planting & 
                                        msmfits$Precip == input$spei_val,] %>% 
      select(source, target, weight) %>% 
      mutate(source = as.integer(as.factor(source)),
             target = as.integer(as.factor(target))) %>%
      spread(target, weight) %>%
      rename("State Assignment" = "source")
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)


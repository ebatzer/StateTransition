# Building Shiny App
library(shiny)
library(ggplot2)
library(dplyr)
setwd("C:/Users/ebatz/Box Sync/Eviner lab shared/Evan/Research Projects/WAPS Data/shinyApp")

# Reads in community data
comdata <- read.csv("data/WAPS_comp_long.csv", stringsAsFactors = F)
waps.specs <- read.csv("data/WAPSspecies.csv", stringsAsFactors = F)
comdata$cover[is.na(comdata$cover)] <- 0

for(i in waps.specs$Species){
  comdata$species <- gsub(i, waps.specs$Newval[waps.specs$Species == i], comdata$species)
}

comdata$spcomp <- gsub("[[:space:]]\\+[[:space:]]*", " \\+", comdata$spcomp)

total.cover <- comdata %>% group_by(plot, year) %>% filter(species %in% waps.specs$Newval[waps.specs$Type != "class"]) %>% 
  summarise(total = sum(na.omit(as.numeric(cover))))

comdata <- merge(comdata, total.cover)
comdata[comdata$total == max(comdata$total),]

ui <- fluidPage(
  titlePanel("WAPS Composition Data"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "water",
                         label = "Select H20 treatment",
                         choices = sort(c(unique(comdata$water)))
      ),
      
      selectizeInput(inputId = "clip",
                         label = "Select clipping treatment",
                         choices = c("none", "spring clipping", "fall clipping")
      ),
      
      selectizeInput(inputId = "fert",
                         label = "Select fertilization treatment",
                         choices = c("none", "high fert")
      ),
      
      selectizeInput(inputId = "seed.trt",
                         label = "Select seeding treatment",
                         choices = sort(c(unique(comdata$spcomp)))
      ),
    
      selectizeInput(inputId = "yr",
                     label = "Select year",
                     choices = sort(c(unique(comdata$year))) 
      )
    ),
    mainPanel(
      plotOutput("comp.plot"),
      tags$br(),
      tags$br(),
      plotOutput("sum.plot")
    )
  )
)

server <- function(input, output){
  
  # Generates species-specific plots
  output$comp.plot <- renderPlot({
    graphdat <- comdata %>% 
      filter(water == input$water,
             clipping == input$clip,
             fertilization == input$fert,
             year == input$yr,
             spcomp == input$seed.trt,
             species %in% specs$species)
    
    goodspec <- graphdat %>% group_by(species) %>% 
      summarise(total = sum(na.omit(as.numeric(cover))))
    
    graphdat <- graphdat %>% filter(species %in% goodspec$species[goodspec$total > 0])
    
    max_y <- max(na.omit(as.numeric(graphdat$cover)))
    
    ggplot(data = graphdat,
           aes(x = species,
               y = as.numeric(cover),
               fill = species)) +
      geom_boxplot() +
      xlab("Species") +
      ylab("Percent Cover") + 
      ggtitle(paste("WAPS | water:", input$water, "| clip:", input$clip, 
                    "| fert:", input$fert, "| yr:", input$yr)) +
      ylim(0,(max_y + (max_y %% 25))) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme(legend.position='none')
    
  })
  
  # Generates summary plot (aggregated data)
  output$sum.plot <- renderPlot({
    graphdat <- comdata %>% 
      filter(water == input$water,
             clipping == input$clip,
             fertilization == input$fert,
             year == input$yr,
             species %in% sums$species,
             spcomp == input$seed.trt)
    
    goodspec <- graphdat %>% group_by(species) %>% 
      summarise(total = sum(na.omit(as.numeric(cover))))
    
    graphdat <- graphdat %>% filter(species %in% goodspec$species[goodspec$total > 0])
    
    max_y <- max(na.omit(as.numeric(graphdat$cover)))

    ggplot(data = graphdat,
           aes(x = species,
               y = as.numeric(cover),
               fill = species)) +
      geom_boxplot() +
      xlab("Category") +
      ylab("Percent Cover") + 
      ggtitle(paste("WAPS | water:", input$water, "| clip:", input$clip, 
                    "| fert:", input$fert, "| yr:", input$yr)) +
      ylim(0,(max_y + (max_y %% 25))) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme(legend.position='none')
    
    
  })
  
}

shinyApp(ui = ui, server = server)


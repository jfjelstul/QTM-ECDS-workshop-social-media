###########################################################################
# Josh Fjelstul, Ph.D.
# QTM-ECDS Workshops, Coding in R Series
# Social Media
###########################################################################

# libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(visNetwork)
library(igraph)
library(shinydashboard)

###########################################################################
# UI
###########################################################################

# main content
main_content <- fluidRow(
  column(width = 4,
         box(width = NULL, title = "Network Options", status = "primary", solidHeader = TRUE, collapsible = TRUE,
             selectInput("option_edge_class", label = "Type of Edge", choices = c("Mention", "Reply", "Retweet")),
             selectInput("option_layout", label = "Layout", choices = c("Random", "Fruchterman-Reingold", "Kamada-Kawai")),
             numericInput("option_seed", label = "Seed", value = 12345)
         )
  ),
  column(width = 8,
         box(width = NULL, title = "Network Graph", status = "primary", solidHeader = TRUE, collapsible = TRUE,
             actionButton("network_update_button", label = "Update"),
             visNetworkOutput("plot", width = "100%", height = "500px")
         )
  )
)

# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Upload", tabName = "upload"),
    menuItem("Visualize", tabName = "main")
  )
)

# body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "upload",
            box(width = 6, title = "Upload an Edges Dataset", status = "primary", solidHeader = TRUE,
                fileInput("edges", label = "Choose a file to upload", accept = ".csv", multiple = FALSE)
            )
    ),
    tabItem(tabName = "main",
            main_content
    )
  )
)

# UI function
ui <- dashboardPage(
  dashboardHeader(title = "Network Visualization"),
  sidebar,
  body
)

###########################################################################
# server
###########################################################################

# server function
server <- function(input, output, session) {

  # make a reactive object
  values <- reactiveValues()
  
  # observe file input
  observeEvent(input$edges, {
    values$edges <- read.csv(input$edges$datapath, stringsAsFactors = FALSE)
  })
  
  # observe update button
  observeEvent(input$network_update_button, {

    # render plot
    output$plot <- renderVisNetwork({

      # create edges data frame
      edges <- values$edges
      
      # choose edge type
      if (input$option_edge_class == "Mention") {
        edges <- filter(edges, type == "mention")
      } 
      
      else if(input$option_edge_class == "Reply") {
        edges <- filter(edges, type == "reply")
      } 
      
      else if (input$option_edge_class == "Retweet") {
        edges <- filter(edges, type == "retweet")
      }
      
      # data frame of nodes
      nodes <- data.frame(id = unique(c(edges$from, edges$to)))
      
      # make plot
      plot <- visNetwork(nodes, edges, background = "white") %>%
        visNodes(color = list(background = "#56C1FF", border = "#515151", highlight = list(background = "#FF644E", border = "#515151")),
                 borderWidth = 1,
                 labelHighlightBold = FALSE,
                 shapeProperties = list(borderRadius = 5),
                 shape = "box",
                 size = 10) %>%
        visEdges(physics = FALSE,
                 smooth = list(enabled = FALSE),
                 color = list(color = "#D6D6D6", highlight = "#515151"),
                 width = 1,
                 selectionWidth = 0,
                 arrows = list(middle = list(enabled = TRUE, scaleFactor = 1))) %>%
        visOptions(highlightNearest = list(enabled = TRUE, hideColor = "#D6D6D6", degree = 1, labelOnly = FALSE))
      
      # choose plot layout
      if (input$option_layout == "Random") {
        plot <- plot %>% visIgraphLayout(layout = "layout_randomly", randomSeed = input$option_seed)
      } 
      
      else if(input$option_layout == "Fruchterman-Reingold") {
        plot <- plot %>% visIgraphLayout(layout = "layout_with_fr", randomSeed = input$option_seed)
      } 
      
      else if (input$option_layout == "Kamada-Kawai") {
        plot <- plot %>% visIgraphLayout(layout = "layout_with_kk", randomSeed = input$option_seed)
      }

      # return plot
      return(plot)
    })
  })
}

###########################################################################
# build
###########################################################################

# build app
shinyApp(ui, server)

###########################################################################
# end R script
###########################################################################

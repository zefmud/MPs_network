library(shiny)
library(networkD3)
#server function
server <- function(input, output) {
  load("data/all_nodes.Rda")
  load("data/factions.Rda")
  load("data/graph.Rda")
  draw_graph <- function(graph, min_value, f = unique(factions$faction_title))
  {
    A <- graph[graph$value >= min_value, ]
    active_nodes <- unique(c(A$source, A$target))
    nodes <- all_nodes[(all_nodes$MP_ID %in% active_nodes) & (all_nodes$group %in% f), ]
    A <- A[(A$source %in% nodes$MP_ID)& (A$target %in% nodes$MP_ID), ]
    active_nodes <- unique(c(A$source, A$target))
    nodes <- nodes[nodes$MP_ID %in% active_nodes,]
    nodes <- nodes[order(nodes$name), ]
    nodes$name <- as.factor(nodes$name)
    #changing the numbers in graphs
    for (i in 1:length(A$source))
    {
      A$source[i] <- as.numeric(nodes$name[which(nodes$MP_ID == A$source[i])])-1
      A$target[i] <- as.numeric(nodes$name[which(nodes$MP_ID == A$target[i])])-1
    }
    forceNetwork(Links = A, Nodes = nodes, Source = "source", Target = "target", 
		Value = "value", NodeID = "name", Nodesize = "size", Group = "group", 
		opacity = 0.9, zoom = FALSE, legend = TRUE)
  }
  output$graph <- renderForceNetwork({

    draw_graph(graph, input$min_value, input$factions)
  })
}
#ui function
load("data/factions.Rda")
f <- unique(as.character(factions$faction_title))
ui <- shinyUI(fluidPage(
  titlePanel("Законодавче партнерство"),
  
  sidebarLayout(
    sidebarPanel(
           
      checkboxGroupInput("factions", 
                  label = "Фракції, які треба відобразити:",
                  choices = f,
                  selected = f),
      
      sliderInput("min_value", 
                  label = "Мінімальна кількість спільних законопроектів:",
                  min = 1, max = 30, value = 15)
      ),
    
    mainPanel(forceNetworkOutput("graph"))
  )
))  


shinyApp(ui = ui, server = server)

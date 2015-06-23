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
    colourScale = JS("d3.scale.category10()"),
		opacity = 0.9, zoom = FALSE, legend = TRUE)
  }
  draw_ind_graph <- function(d, level, f = unique(factions$faction_title))
  {
    #   path <- numeric(0)
    #   find_partners <- function(d, level)
    #   {
    #     path <<- c(path, d)
    #     if (level == 0)
    #     {
    #       d
    #     } else {
    #       new_nodes <- c(graph$target[(graph$source == d) & !(graph$target %in% path)],
    #                      graph$source[(graph$target == d) & !(graph$source %in% path)])
    #       ret <- d
    #       for (i in 1:length(new_nodes))
    #       {
    #         ret <- c(ret, find_partners(new_nodes[i], level - 1))
    #       }
    #       ret
    #     }
    #   }
    #   nodes <- find_partners(d, level)
    #   nodes <- all_nodes[all_nodes$MP_ID %in% nodes, ]
    #   A <- graph[(graph$source %in% nodes$MP_ID) & (graph$target %in% nodes$MP_ID), ]
    A <- graph[(graph$source == d)| (graph$target == d),]
    if (length(A$source) > 0)
    {
      for (i in 1:length(A$source))
      {
        if (A$source[i] != d)
        {
          b <- A$source
          A$source <- A$target
          A$target <- b
        }
      }
    }
    active_nodes <- unique(c(A$target, A$source, d))
    nodes <- all_nodes[((all_nodes$MP_ID %in% active_nodes) & (all_nodes$group %in% f)) 
                       | all_nodes$MP_ID == d, ]
    A <- A[(A$source %in% nodes$MP_ID)& (A$target %in% nodes$MP_ID), ]
    #regulating the sizes of nodes
    
    nodes$size[nodes$MP_ID == d] <- 250
    max_value <- max(A$value)
    #adding the numbers to names
    for (i in 1:length(nodes$name))
    {
      if (nodes$MP_ID[i] != d)
      {
        nodes$name[i] <- paste(nodes$name[i], ", ",
                               as.character(A$value[(A$target == nodes$MP_ID[i]) | (A$source == nodes$MP_ID[i])]),
                               sep = ""  )
        nodes$size[i] <- 4 +  (A$value[(A$target == nodes$MP_ID[i]) | (A$source == nodes$MP_ID[i])] / max_value) * 150
      }
      
    }
    #changing the numbers in graphs 
    nodes <- nodes[order(nodes$name), ]
    nodes$name <- as.factor(nodes$name)
    for (i in 1:length(A$source))
    {
      A$source[i] <- as.numeric(nodes$name[which(nodes$MP_ID == A$source[i])])-1
      A$target[i] <- as.numeric(nodes$name[which(nodes$MP_ID == A$target[i])])-1
    }
    #forming an expression to calculate distance
    max_value <- max(A$value)
    dist_expr <- paste('function(d){return 25 +  (1 / d.value) * 200}')
    forceNetwork(Links = A, Nodes = nodes, Source = "source", Target = "target", 
                 Value = "value", NodeID = "name", Nodesize = "size", Group = "group",  
                 colourScale = JS("d3.scale.category10()"), linkDistance = JS(dist_expr), 
                 opacity = 1, zoom = FALSE, legend = TRUE)
    
  }
  get_MP_ID <- reactive({
    all_nodes$MP_ID[all_nodes$name == input$chosen_MP]
  })
  
  output$graph <- renderForceNetwork({

    draw_graph(graph, input$min_value, input$factions)
  })
  
  output$ind_graph <- renderForceNetwork({
    draw_ind_graph(get_MP_ID(), 1, input$factions_ind)
  })
#   observeEvent (input$select_all, {
#     updateCheckboxGroupInput(session, "select_all", label = ifelse(input$select_all$value %% 2 == 1,"Прибрати всі","Обрати всі"), 
#                              choices = f, selected = ifelse(input$select_all$value %% 2 == 1,"Прибрати всі","Обрати всі"), inline = FALSE)
#   })
}
#ui function
load("data/factions.Rda")
load("data/all_nodes.Rda")
f <- unique(as.character(factions$faction_title))
ui <- shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Усі депутати",
      sidebarLayout(
        sidebarPanel(
          h3("Депутатські групи"), 
          helpText("Хто найтісніше співпрацює всередині кожної фракції та між ними?"),
          checkboxGroupInput("factions", 
                      label = "Оберіть фракції:",
                      choices = f),
          
          sliderInput("min_value", 
                      label = "Мінімальна кількість законопроектів, спільно ініційованих депутатами:",
                      min = 1, max = 30, value = 15),
#           fluidRow(
#             column(10, 
#                    actionButton("select_all_ind", "Обрати всі")
#             )
#           ),
            
          helpText("Розмір кружечка залежить від загальної кількості законопроектів, які ініціював депутат.
                   Товшина лінії зв’язку залежить від кількості законопроектів, 
                   які депутати ініціювали разом.")),
        
        mainPanel(forceNetworkOutput("graph", height="600px"))
      )
    ),
    tabPanel("Конкретний депутат",
      sidebarLayout(
        sidebarPanel(
          h3("Кола партнерів окремого депутата"),
          helpText("Побачите усіх, з ким депутат подав хоча б один спільний законопроект"),
          selectInput("chosen_MP", "Оберіть депутата:", 
                      choices = all_nodes$name[order(all_nodes$name)]),
          checkboxGroupInput("factions_ind", 
                             label = "Оберіть фракції партнерів:",
                             choices = f, selected = f),
#           fixedRow(
#             column(10, 
#                    actionButton("select_all_ind", "Прибрати всі")
#             )
#           ),
          helpText("Чим більша відстань і менший кружечок депутатів - тим менше законопроектів він
                   спільно ініціював з обраним нардепом. Наведіть мишкою на кружечок, і після імені депутата 
                   побачите точну кількість спільних законопроектів.")
          #sliderInput("level", "Оберіть глибину пошуку:", min = 1, max = 5, value = 1)
        ),
        mainPanel(forceNetworkOutput("ind_graph", height="600px"))
      )
    )
  ), title = "ЧЕСНО::Графи"
))  


shinyApp(ui = ui, server = server)

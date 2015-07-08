library(shiny)
library(networkD3)
library(shinyjs)
load("data/all_nodes.Rda")
load("data/factions.Rda")
load("data/graph.Rda")
load("data/partners.Rda")
partners$name <- as.character(partners$name)

f <- unique(as.character(factions$faction_title))

#server function
server <- function(input, output, session) {
  table <- FALSE
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
  
    
  update_second_droplist <-  function() {
    selected <- which(partners$name == input$MP1_selectInput)
    partners_names <- as.character(partners$partners[[selected]]$name)
    updateSelectInput(session, inputId = "MP2_selectInput", choices = partners_names, selected = partners_names[1])
  }
  
  get_bills_list <- function(d1, d2)
  {
    d1 <- all_nodes$MP_ID[all_nodes$name == d1]
    d2 <- all_nodes$MP_ID[all_nodes$name == d2]
    if (sum(((graph$source == d1)|(graph$source == d2))&((graph$target == d1)|(graph$target == d2))) == 0)
    {
      update_second_droplist()
      NULL
    } else {
    
      bills_list_number <- which(((graph$source == d1)|(graph$source == d2))&((graph$target == d1)|(graph$target == d2)))
      bills <- graph$bills[[bills_list_number]]
      bills$bill_title <- paste("<a href = '", bills$bill_link,"'>",bills$bill_title,"</a>", sep="")
      bills$bill_link <- NULL
      bills$type <- NULL
      #bills$date <- format(bills$date, format = "%d.%m.%Y")
      names(bills) <- c ("Номер", "Дата реєстрації", "Назва", "Став чинним актом")
      bills
    }
  }
  
  
  update_table <- function()
  {
    d1 <- input$MP1_selectInput
    d2 <- input$MP2_selectInput
    t <- get_bills_list(d1, d2)
    output$bills_table <- DT::renderDataTable(t, options = list(language = list(url = "assets/Ukranian.json")),
                                                 caption = paste(d1, " та ", d2, ": cпільні законопроекти", sep = ""), escape = F, rownames = NULL)
  }
  
  update_ind_plots <- function()
  {
     draw_ind_table(get_MP_ID(), input$factions_ind) 
     outputOptions(x = output, name = 'ind_table', suspendWhenHidden = FALSE)
#     output$ind_graph <- individual_graph(get_MP_ID(), input$factions_ind)
    if (table == TRUE)
    {
     draw_ind_table(get_MP_ID(), input$factions_ind) 
     
    } else {
     output$ind_graph <- individual_graph(get_MP_ID(), input$factions_ind)
    }
  }
  
  observeEvent(input$chosen_MP, 
  {
    update_ind_plots()
    d <- input$chosen_MP
    if (d %in% partners$name) 
    {
      updateSelectInput(session, inputId = "MP1_selectInput", choices = partners$name[order(partners$name)], selected = d)
    }
  })
  observeEvent(input$MP1_selectInput, {
    update_second_droplist()
    d1 <- input$MP1_selectInput
    number_all <- all_nodes$size[all_nodes$name == d1]
    output$table_header1 <- renderUI({h5(paste(d1, " всього подав(-ла) ", number_all, " законопроект(-и,-ів)."), sep="")})
    output$help_second_list <-renderUI({helpText(paste("Депутати в другому списку відсортовані за кількістю законопроектів, яку ", d1, " подав(-ла) у співпраці із ними.", sep = "")) })
    update_table()
  })
  observeEvent(input$MP2_selectInput, {
    update_table()
    d1 <- input$MP1_selectInput
    d2_string <- input$MP2_selectInput
    d1 <- all_nodes$MP_ID[all_nodes$name == d1]
    d2 <- all_nodes$MP_ID[all_nodes$name == d2_string]
    if (sum(((graph$source == d1)|(graph$source == d2))&((graph$target == d1)|(graph$target == d2))) > 0)
    {
      link_number <- which(((graph$source == d1)|(graph$source == d2))&((graph$target == d1)|(graph$target == d2)))
      number_coop <- graph$value[link_number] 
      output$table_header2 <- renderUI({h5(paste("Разом з ",d2_string, " - ", number_coop, " законопроект(-и,-ів)."), sep="")})
    }
   }
  )
  
  draw_ind_table <- function(d, f = unique(factions$faction_title))
  {
    MP_number <- which(partners$MP_ID == d)
    if  (length(MP_number) > 0) 
    {
      p <- partners$partners[[MP_number]]
      factions_all <- factions[factions$faction_title %in% f,1:2]
      p$name <- as.character(p$name)
      p <- p[p$faction_id %in% factions_all$faction_id, ]
      p <- merge(p, factions_all, by = "faction_id")
      p$faction_id <- NULL
      p$MP_ID <- NULL
      a <- p$times
      p$times <- NULL
      p$times <- a
      p <- p[order(-p$times),]
      
    } else {
      p <- data.frame(matrix(NA,0,3))
    }
    names(p) <- c("Депутат", "Фракція", "К-ть спільних законопроектів")
    dep_name <- all_nodes$name[all_nodes$MP_ID == d]
    partners_amount <- length(p[, 1])
    draftlaws <- all_nodes$size[all_nodes$name == dep_name]
    output$ind_table <- DT::renderDataTable(p, options = list(pageLength = 50, language = list(url = "assets/Ukranian_partners.json")),
                        caption = paste(dep_name, " всього подав ", draftlaws, " законопроектів та має ", partners_amount, " законодавчих партнерів з обраних фракцій.", sep = ""), escape = F, rownames = NULL)
  }
  
  draw_ind_graph <- function(d, level, f = unique(factions$faction_title))
  {
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
    
    nodes$size[nodes$MP_ID == d] <- 150
    max_value <- max(A$value)
    #adding the numbers to names
    for (i in 1:length(nodes$name))
    {
      if (nodes$MP_ID[i] != d)
      {
        nodes$name[i] <- paste(nodes$name[i], ", ",
                               as.character(A$value[(A$target == nodes$MP_ID[i]) | (A$source == nodes$MP_ID[i])]),
                               sep = ""  )
        nodes$size[i] <- 4 +  (A$value[(A$target == nodes$MP_ID[i]) | (A$source == nodes$MP_ID[i])] / max_value) * 90
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
    dist_expr <- paste('function(d){return 25 +  (1 / d.value) * 150}')
    forceNetwork(Links = A, Nodes = nodes, Source = "source", Target = "target", 
                 Value = "value", NodeID = "name", Nodesize = "size", Group = "group",  
                 colourScale = JS("d3.scale.category10()"), linkDistance = JS(dist_expr), 
                 opacity = 1, zoom = FALSE, legend = TRUE)
    
  }
  
  individual_graph <- function(d, f)
  {
      renderForceNetwork(draw_ind_graph(d, 1, f))
  }
  
  individual_table <- function(d, f)
  {
    if (table == TRUE)
    {
      draw_ind_table(d, f)
    } else {
      NULL
    }
  }
  
  get_MP_ID <- reactive({
    all_nodes$MP_ID[all_nodes$name == input$chosen_MP]
  })
  
  output$graph <- renderForceNetwork({

    draw_graph(graph, input$min_value, input$factions)
  })
  
  observeEvent(input$factions_ind,{
    update_ind_plots()
  })
  
  observeEvent (input$select_all, {
     updateCheckboxGroupInput(session, "factions", selected = f)
  })
  
  observeEvent (input$deselect_all, {
     updateCheckboxGroupInput(session, "factions", selected = "")
  })
  
  observeEvent (input$select_all_ind, {
     updateCheckboxGroupInput(session, "factions_ind", selected = f)
  })
  
  observeEvent (input$deselect_all_ind, {
     updateCheckboxGroupInput(session, "factions_ind", selected = "")
#      if (table == TRUE)
#      {
       draw_ind_table(get_MP_ID(), "") 
#     } else {
       output$ind_graph <- individual_graph(get_MP_ID(), "")
#     }
  })
  
  observeEvent(input$table_button,  {
    if (table == FALSE)
    {
      #outputOptions(x = output, name = 'ind_table', suspendWhenHidden = FALSE)
      #outputOptions(x = output, name = 'ind_graph', suspendWhenHidden = TRUE)
      #draw_ind_table(get_MP_ID(), input$factions_ind) 
      text("table_button", "Назад до графу")
    } else {
      text("table_button", "У вигляді таблиці")
      outputOptions(x = output, name = 'ind_graph', suspendWhenHidden = FALSE)
    }
    table <<- !table
    toggle("ind_table")
    toggle("ind_graph")
    update_ind_plots()

  })

  #hide(id = 'ind_table')
  hide(id = 'table_button')
}
#ui function


ui <- shinyUI(fluidPage(
  useShinyjs(),
  tabsetPanel(
    tabPanel("Фракція-Фракція",
      sidebarLayout(
        sidebarPanel(width = 3, 
          h4("Законототворчі групи"), 
          helpText("Хто найтісніше співпрацює всередині кожної фракції та між ними?"),
          checkboxGroupInput("factions", 
                      label = "Оберіть фракції:",
                      choices = f),
          fluidRow(
	    column(width=4, actionButton("select_all", "Обрати всі")),
            column(width=4, actionButton("deselect_all", "Скинути всі"))
          ),
          sliderInput("min_value", 
                      label = "Мінімальна кількість законопроектів, спільно ініційованих депутатами:",
                      min = 1, max = 30, value = 15),
          
          helpText("Розмір кружечка залежить від загальної кількості законопроектів, які ініціював депутат.
                   Товшина лінії зв’язку залежить від кількості законопроектів, 
                   які депутати ініціювали разом.")),
        
        mainPanel(forceNetworkOutput("graph", height="500px"))
      )
    ),
    tabPanel("Депутат-Фракція",
      sidebarLayout(
        sidebarPanel(width = 3, 
          h4("Співпраця депутата з депутатами різних фракцій"),
          helpText("Чим менша відстань і більший кружечок депутата - тим більше законопроектів він спільно ініціював з обраним нардепом. Наведіть 		мишкою на кружечок, і після імені депутата побачите точну кількість спільних законопроектів"),
          selectInput("chosen_MP", "Оберіть депутата:", 
                      choices = all_nodes$name[order(all_nodes$name)]),
          checkboxGroupInput("factions_ind", 
                             label = "Оберіть фракції партнерів:",
                             choices = f, selected = f),
          fluidRow(
	          column(width=4, actionButton("select_all_ind", "Обрати всі")) ,
            column(width=4, actionButton("deselect_all_ind", "Скинути всі"))
          ),
          fluidRow(column(width = 5, offset = 1, actionButton("table_button", "У вигляді таблиці")))
          
 
          
        ),
      
        mainPanel(
          fluidRow(forceNetworkOutput(outputId = "ind_graph")),
          fluidRow(DT::dataTableOutput(outputId = 'ind_table'))
	  
	      )
      )
    ),
    tabPanel("Депутат-Депутат",
      sidebarLayout(
	sidebarPanel(width = 3,
	  h4("Спільні законопроекти депутатів"),
	  helpText("Оберіть першого депутата, а потім - другого з числа його партнерів, аби побачити список спільно ініційованих двома депутатами 		законопроектів"),
	  selectInput(inputId = "MP1_selectInput", label = "Виберіть першого депутата", choices = partners$name[order(partners$name)]),
	  selectInput(inputId = "MP2_selectInput", label = "Виберіть другого депутата", choices = NULL),
	  uiOutput("help_second_list")
	  
	),
	mainPanel(
	  uiOutput("table_header1"),
	  uiOutput("table_header2"),
	  br(),
	  DT::dataTableOutput('bills_table')
	)
      )
    )
  ), title = "ЧЕСНО::Графи"
))  


shinyApp(ui = ui, server = server)

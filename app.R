library(shiny)
library(networkD3)
library(shinyjs)
load("data/full_node_table.Rda")
load("data/fr_node_table.Rda")
load("data/laws_node_table.Rda")
load("data/factions.Rda")
load("data/graph_all.Rda")
load("data/graph_fr.Rda")
load("data/graph_laws.Rda")
load("data/partners_all.Rda")
load("data/partners_fr.Rda")
load("data/partners_laws.Rda")

dl_types <- c("Усі законопроекти", "Ухвалені за основу", "Стали законами")

partners_all$name <- as.character(partners_all$name)
f <- unique(as.character(factions$faction_title))


#server function
server <- function(input, output, session) {
  table <- FALSE
  table_hidden <- FALSE
  first_time <- TRUE

  
  graph_switch <- function(s)
  {
    switch(s,
           "Усі законопроекти" = graph_all,
           "Ухвалені за основу" = graph_fr,
           "Стали законами" = graph_laws)
  }
  
  partners_switch <- function(s)
  {
    switch(s,
           "Усі законопроекти" = partners_all,
           "Ухвалені за основу" = partners_fr,
           "Стали законами" = partners_laws)
  }
  
  nodes_switch <- function(s)
  {
    switch(s,
           "Усі законопроекти" = full_node_table,
           "Ухвалені за основу" = fr_node_table,
           "Стали законами" = laws_node_table)
  }
  
  factions_switch <- function(s) {
    sw <- function(ch) {
      switch(ch,
             'ВО "Батьківщина"' = 'ВО "Батьківщина"',
             'Блок Петра Порошенка' = 'Блок Петра Порошенка',
             'Народний фронт' = 'Народний фронт',
             "Радикальна партія" = "Радикальна партія Ляшка",
             "Самопоміч" = 'Об\'єднання "Самопоміч"',
             "Опозиційний блок" = "Опозиційний блок",
             "Позафракційні" = "Не входить до складу жодної фракції",
             "Гр. \"Воля народу\"" = "Група \"Воля народу\"",
             "Гр. \"Відродження\"" = "Група \"Відродження\""  
      )
    }
    sapply(s, sw)
  }
  
  graph <<- graph_switch(dl_types[1])
  partners <<- partners_switch(dl_types[1])
  all_nodes <<- nodes_switch(dl_types[1])
  graph2 <<- graph_switch(dl_types[1])
  partners2 <<- partners_switch(dl_types[1])
  all_nodes2 <<- nodes_switch(dl_types[1])
  graph3 <<- graph_switch(dl_types[1])
  partners3 <<- partners_switch(dl_types[1])
  all_nodes3 <<- nodes_switch(dl_types[1])
  
  # factions_colors <- function(f_vector)
  # {
  #   quotes_wrap <- function(s)
  #   {
  #     paste0('"',s,'"')
  #   }
  #   switch_color <- function(f)
  #   {
  #      switch(as.character(f),
  #             'Блок Петра Порошенка' = '"#FD0E0E"',
  #             'Народний фронт' = '"#DDFF00"',
  #             'Опозиційний блок' = '"#070FFC"',
  #             'Позафракційні' = '"#5E5E5E"',
  #             'Самопоміч'= '"#00B906"',
  #             'Радикальна партія' = '"#4E0101"',
  #             'Гр. "Воля народу"' = '"#BC00AF"',
  #             'ВО "Батьківщина"' = '"#FF9500"',
  #             'Гр. "Відродження"' = '"#07FCE3"'
  #     )
  #   }
  #   ret <- sapply(f_vector, switch_color)
  #   faction_part <- paste0("[",paste0(sapply(f_vector, quotes_wrap), collapse = ", "), "]")
  #   paste0("d3.scale.ordinal().range([", paste(ret, collapse = ", "), "])")
  # }
  # 
  
  factions_colors <- function(f_vector)
  {
    quotes_wrap <- function(s)
    {
      paste0('"',s,'"')
    }
    switch_color <- function(f)
    {
      switch(as.character(f),
             'Блок Петра Порошенка' = '"#FD0E0E"',
             'Народний фронт' = '"#DDFF00"',
             'Опозиційний блок' = '"#070FFC"',
             'Не входить до складу жодної фракції' = '"#5E5E5E"',
             'Об\'єднання "Самопоміч"'= '"#00B906"',
             'Радикальна партія Ляшка' = '"#4E0101"',
             'Група "Воля народу"' = '"#BC00AF"',
             'ВО "Батьківщина"' = '"#FF9500"',
             'Група "Відродження"' = '"#07FCE3"'
      )
    }
    ret <- sapply(f_vector, switch_color)
    faction_part <- paste0("[",paste0(sapply(f_vector, quotes_wrap), collapse = ", "), "]")
    paste0("d3.scale.ordinal().range([", paste(ret, collapse = ", "), "])")
  }
  
  
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
    groups <- unique(nodes$group)
    #print(A$source)
    forceNetwork(Links = A, Nodes = nodes, Source = "source", Target = "target", 
		Value = "value", NodeID = "name", Nodesize = "size", Group = "group",  
    colourScale = JS(factions_colors(groups)),
		opacity = 0.9, zoom = FALSE, legend = TRUE)
  }
  
    
  update_second_droplist <-  function() {
    selected <- which(partners3$name == input$MP1_selectInput)
    partners_names <- as.character(partners3$partners[[selected]]$name)
    updateSelectInput(session, inputId = "MP2_selectInput", choices = partners_names, selected = partners_names[1])
  }
  
  get_bills_list <- function(d1, d2)
  {
    d1 <- all_nodes3$MP_ID[all_nodes$name == d1]
    d2 <- all_nodes3$MP_ID[all_nodes$name == d2]
    if (sum(((graph3$source == d1)|(graph3$source == d2))&((graph3$target == d1)|(graph3$target == d2))) == 0)
    {
      #update_second_droplist()
      bills <- data.frame()
      bills$Номер <- character()
      bills[,"Дата реєстрації"] <- character()
      bills[,"Назва"] <- character()
      bills[,"Став законом"] <- character()
    } else {

      bills_list_number <- which(((graph3$source == d1)|(graph3$source == d2))&((graph3$target == d1)|(graph3$target == d2)))
      bills <- graph3$bills[[bills_list_number]]
      bills$bill_title <- paste("<a href = '", bills$bill_link,"' target='_blank' >",bills$bill_title,"</a>", sep="")
      bills$bill_link <- NULL
      bills$type <- NULL
      #bills$date <- format(bills$date, format = "%d.%m.%Y")
      names(bills) <- c ("Номер", "Дата реєстрації", "Назва", "Став законом")
    }
    bills
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
     draw_ind_table(get_MP_ID(), factions_switch(input$factions_ind))
     outputOptions(x = output, name = 'ind_table', suspendWhenHidden = FALSE)
#     output$ind_graph <- individual_graph(get_MP_ID(), input$factions_ind)
    if (table == TRUE)
    {
     draw_ind_table(get_MP_ID(), factions_switch(input$factions_ind))
     
    } else {
     output$ind_graph <- individual_graph(get_MP_ID(), factions_switch(input$factions_ind))
    }
  }
  
  observeEvent(input$chosen_MP, 
  {
    update_ind_plots()
    d <- input$chosen_MP
    if (d %in% partners2$name) 
    {
      updateSelectInput(session, inputId = "MP1_selectInput", choices = as.character(partners2$name)[order(partners2$name)], selected = d)
    }
  })
  observeEvent(input$MP1_selectInput, {
    update_second_droplist()
    d1 <- input$MP1_selectInput
    number_all <- all_nodes3$size[all_nodes$name == d1]
    output$table_header1 <- renderUI({h5(paste(d1, " всього подав(-ла) ", number_all, " законопроект(-и,-ів)."), sep="")})
    output$help_second_list <-renderUI({helpText(paste("Депутати в другому списку відсортовані за кількістю законопроектів, яку ", d1, " подав(-ла) у співпраці із ними.", sep = "")) })
    update_table()
  })
  observeEvent(input$MP2_selectInput, {
    update_table()
    d1 <- input$MP1_selectInput
    d2_string <- input$MP2_selectInput
    d1 <- all_nodes3$MP_ID[all_nodes$name == d1]
    d2 <- all_nodes3$MP_ID[all_nodes$name == d2_string]
    if (sum(((graph3$source == d1)|(graph3$source == d2))&((graph3$target == d1)|(graph3$target == d2))) > 0)
    {
      link_number <- which(((graph3$source == d1)|(graph3$source == d2))&((graph3$target == d1)|(graph3$target == d2)))
      number_coop <- graph3$value[link_number] 
      output$table_header2 <- renderUI({h5(paste("Разом з ",d2_string, " - ", number_coop, " законопроект(-и,-ів)."), sep="")})
    }
   }
  )
  
  draw_ind_table <- function(d, f = unique(factions$faction_title))
  {
    MP_number <- which(partners2$MP_ID == d)
    if  (length(MP_number) > 0) 
    {
      p <- partners2$partners[[MP_number]]
      factions_all <- factions[factions$faction_title %in% f,1:2]
      p$name <- as.character(p$name)
      p <- p[p$faction_id %in% factions_all$faction_id, ]
      #p <- merge(p, factions_all, by = "faction_id")
      #p$faction_id <- NULL
      #p$MP_ID <- NULL
      p <- p[order(-p$times), c("name", "faction_title", "times")]
      
    } else {
      p <- data.frame(matrix(NA,0,3))
    }
    names(p) <- c("Депутат", "Фракція", "К-ть спільних законопроектів")
    dep_name <- all_nodes2$name[all_nodes2$MP_ID == d]
    partners_amount <- length(p[, 1])
    draftlaws <- all_nodes2$size[all_nodes2$name == dep_name]
    output$ind_table <- DT::renderDataTable(p, options = list(pageLength = 50, language = list(url = "assets/Ukranian_partners.json")),
                        caption = paste(dep_name, " всього подав(-ла) ", draftlaws, " законопроектів та має ", partners_amount, " законодавчих партнерів з обраних фракцій.", sep = ""), escape = F, rownames = NULL)
  }
  
  draw_ind_graph <- function(d, level, f = unique(factions$faction_title))
  {
    A <- graph2[(graph2$source == d)| (graph2$target == d),]
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
    nodes <- all_nodes2[((all_nodes2$MP_ID %in% active_nodes) & (all_nodes2$group %in% f)) 
                       | all_nodes2$MP_ID == d, ]
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
    groups <- unique(nodes$group)
    forceNetwork(Links = A, Nodes = nodes, Source = "source", Target = "target", 
                 Value = "value", NodeID = "name", Nodesize = "size", Group = "group",  
                 colourScale = JS(factions_colors(groups)), linkDistance = JS(dist_expr), 
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

    draw_graph(graph, input$min_value, factions_switch(input$factions))
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
  
  observeEvent(input$draftlaws_type1, 
  {
    s <- input$draftlaws_type1
    graph <<- graph_switch(s)
    partners <<- partners_switch(s)
    all_nodes <<- nodes_switch(s)
    #updateSelectInput(session, inputId = "draftlaws_type2", choices = dl_types, selected = s)
    #updateSelectInput(session, inputId = "draftlaws_type3", choices = dl_types, selected = s)
    output$graph <- renderForceNetwork({
      
      draw_graph(graph, input$min_value, factions_switch(input$factions))
    })
 
  })

  observeEvent(input$draftlaws_type2, 
  {
    s <- input$draftlaws_type2
    graph2 <<- graph_switch(s)
    partners2 <<- partners_switch(s)
    all_nodes2 <<- nodes_switch(s)
    #nodes <- all_nodes[all_nodes$MP_IP %in% []
    #updateSelectInput(session, unputId = "MP1_selectInput", choices = partners2$name[order(partners2$name)])
    draw_ind_table(get_MP_ID(), f = factions_switch(input$factions_ind)) 
    output$ind_graph <- individual_graph(get_MP_ID(), f = factions_switch(input$factions_ind))
    d <- input$chosen_MP
    if (d %in% partners2$name) 
    {
      updateSelectInput(session, inputId = "MP1_selectInput", choices = as.character(partners2$name)[order(partners2$name)], selected = d)
    }
    #updateSelectInput(session, inputId = "draftlaws_type1", choices = dl_types, selected = s)
    #updateSelectInput(session, inputId = "draftlaws_type3", choices = dl_types, selected = s)
  })

  observeEvent(input$draftlaws_type3, 
  {
    s <- input$draftlaws_type3
    prev1 <- input$MP1_selectInput
    prev2 <- input$MP2_selectInput
    prev_graph <- graph3
    graph3 <<- graph_switch(s)
    partners3 <<- partners_switch(s)
    all_nodes3 <<- nodes_switch(s)
    #prev_select1 <- input$
    if (prev1 %in% partners3$name)
    {

      p_number <- which(prev1 == partners3$name)
      p_names <- partners3$partners[[p_number]]$name
      updateSelectInput(session, inputId = "MP1_selectInput", choices = as.character(partners3$name)[order(partners3$name)], selected = prev1)
      
      if (prev2 %in% p_names)
      {
        updateSelectInput(session, inputId = "MP2_selectInput", choices = as.character(p_names)[order(p_names)], selected = prev2)
      } else
      {
        if (!first_time) {
          session$sendCustomMessage(type = "showalert", paste0(prev1, " та ", prev2, " не подавали спільних законопроектів із обраним статусом."))
        } else {
          first_time <<- FALSE
        }
        updateSelectInput(session, inputId = "MP2_selectInput", choices = as.character(p_names)[order(p_names)], selected = p_names[1])
        
      }
    }
    else 
    {
      session$sendCustomMessage(type = "showalert", paste0(prev1, " ні з ким не подавав законопроектів із обраним статусом."))
      updateSelectInput(session, inputId = "MP1_selectInput", choices = as.character(partners3$name)[order(partners3$name)], selected = partners3$name[1])
    }
    update_table()
    #updateSelectInput(session, inputId = "draftlaws_type1", choices = dl_types, selected = s)
    #updateSelectInput(session, inputId = "draftlaws_type2", choices = dl_types, selected = s)
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
  
  observeEvent(input$hide_button, {
    if (table_hidden == FALSE)
    {
      text("hide_button", "показати таблицю >>")
    } else {
      text("hide_button", "<< приховати таблицю")
    }
    table_hidden <<- !table_hidden
    toggle("ind_table")
  })
  #hide(id = 'ind_table')
  hide(id = 'table_button')

}
#ui function


ui <- shinyUI(fluidPage(
  useShinyjs(),
  tags$script(type="text/javascript", "
   $(document).ready(function() {
        Shiny.addCustomMessageHandler('showalert', function(message) {
          alert(message);
        });
   });
  "),
  tabsetPanel(
    tabPanel("Фракція-Фракція",
      sidebarLayout(
        sidebarPanel(width = 3, 
          h4("Законотворчі групи"), 
          helpText("Хто найтісніше співпрацює всередині кожної фракції та між ними?"),
          checkboxGroupInput("factions", 
                      label = "Оберіть фракції:",
                      choices = f, selected = f),
          fluidRow(
	    column(width=4, actionButton("select_all", "Обрати всі")),
            column(width=4, actionButton("deselect_all", "Скинути всі"))
          ),
          sliderInput("min_value", 
                      label = "Мінімальна кількість законопроектів, спільно ініційованих депутатами:",
                      min = 1, max = 50, value = 25),
	        selectInput("draftlaws_type1", "Проходження законопроектів:",
	                      choices = dl_types),
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
                      choices = full_node_table$name[order(full_node_table$name)]),
          checkboxGroupInput("factions_ind", 
                             label = "Оберіть фракції партнерів:",
                             choices = f, selected = f),
          fluidRow(
	          column(width=4, actionButton("select_all_ind", "Обрати всі")) ,
            column(width=4, actionButton("deselect_all_ind", "Скинути всі"))
          ),
          selectInput("draftlaws_type2", "Проходження законопроектів:",
                      choices = dl_types),
          fluidRow(column(width = 5, offset = 1, actionButton("table_button", "У вигляді таблиці"))),
          fluidRow(column(width = 5, offset = 1, actionButton("hide_button", "<< приховати таблицю")))
 
          
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
	  selectInput(inputId = "MP1_selectInput", label = "Виберіть першого депутата", choices = partners_all$name[order(partners_all$name)]),
	  selectInput(inputId = "MP2_selectInput", label = "Виберіть другого депутата", choices = NULL),
	  uiOutput("help_second_list"),
	  selectInput("draftlaws_type3", "Проходження законопроектів:",
	              choices = dl_types)
	  
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

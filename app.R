# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(plotly)
library(networkD3)
library(tidyverse)
library(rgexf)
library(igraph)
library(tnet)
library(plyr)
l

# Load data


stat_ges <- read.table("data/statistics_gesamt.csv", header = T,  dec = ",", sep = ";")
stat_kor <- read.table("data/statistics_korrekt.csv", header = T, dec = ",", sep = ";")

stat_ges$typ <- "Gesamt"
stat_kor$typ <- "Korrekt"

stat_all <- rbind(stat_ges, stat_kor)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                navbarPage("Shiny-Dashboard",theme = shinytheme("lumen"),
                           tabPanel("Netzwerke", fluid = TRUE,                
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                        radioButtons("radio", label = h3(""),
                                                     choices = list("Alle Antworten" = 1, "Nur korrekte Antworten" = 2), 
                                                     selected = 1),
                                        
                                        
                                        selectInput(inputId = "person", label = strong("Sch?ler"),
                                                    choices = c("021", "022", "039", "115", "050", "058", "073", "096", "077", "103"),
                                                    selected = "021")
                                      ),
                                      
                                      # Output: Description, lineplot, and reference
                                      mainPanel(
                                        
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Netzwerk", 
                                                             forceNetworkOutput("network", width = "75%", height = "400"), 
                                                             tableOutput("summary_all")),
                                                    tabPanel("Knoten-Statistik", tableOutput("summary_nodes"))
                                                    
                                        )
                                      )
                                    )
                           ),
                           tabPanel("Statistik", fluid = TRUE,
                                    tabsetPanel(type = "tabs", 
                                                tabPanel("Knoten vs. Kanten", 
                                                         plotlyOutput("plot1")), 
                                                tabPanel("CNet", 
                                                         plotlyOutput("plot2")),
                                                tabPanel("CNet normalized", 
                                                         plotlyOutput("plot3"))
                                    )
                           )


                )
)


# Define server function
server <- function(input, output) {
  
  data <- reactive({
    
    if(input$radio == 1) type <- "ges"
    else type <- "kor"

    file <- list.files(paste0(getwd(), "/data/"), pattern = paste0(input$person, "_", type))
    readxl::read_xlsx((paste0(getwd(),"/data/", file)))
    
  })
  
  list <- reactive({
     prepare_data(data())
  })
  
  output$network <- renderForceNetwork(forceNetwork(Links = list()$links, Nodes = list()$nodes, 
                 Source = "IDsource", Target = "IDtarget",
                 Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.8, zoom = TRUE)
  )
  
  output$summary_all <- renderTable(list()$stats_all)
  output$summary_nodes <- renderTable(list()$stats_nodes)
  
  output$plot1 <- renderPlotly({
    plot_ly(data = stat_all, x = ~n_nodes, y = ~n_links, color = ~typ)
  })
  
  output$plot2 <- renderPlotly({
    fig <- stat_all %>% 
            plot_ly(
              x = ~typ, 
              y = ~cnet, 
              split = ~typ, 
              type = "violin"
              # box = list(
              #   visible = T
              # ),
              # meanline = list(
              #   visible = T
              # )
            ) 
  })
  
  output$plot3 <- renderPlotly({
    fig <- stat_all %>% 
      plot_ly(
        x = ~typ, 
        y = ~cnet_normalized, 
        split = ~typ, 
        type = "violin" 
        #marker = list( 
        #  opacity = 0
        #  ), 
        #points = "all"
        # box = list(
        #   visible = T
        # ),
        # meanline = list(
        #   visible = T
        # )
      )
  })
  
}

prepare_data <- function(dataset){
  
  list_obj <- list()
  
  data <- as.data.frame(dataset)                     
  rownames <- data[,1]
  data <- data[,-1]
  row.names(data) <- rownames
  
  ##fill missing values
  for(r in 1:nrow(data)){
    for(c in 1:ncol(data)){
      data[r,c] <- ifelse(r>c, data[c,r], data[r,c])
    }
  }
  
  links <- data %>% 
    rownames_to_column(var="source") %>% 
    gather(key="target", value="value", -1) %>%
    filter(value != 0)
  
  nodes <- data.frame(
    name=rownames, 
    ID=0:(length(rownames)-1),
    group=as.character(0:(length(rownames)-1))
  )
  
  ##add IDs
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  edges <- links[,c("IDsource", "IDtarget", "value")]
  edges$IDsource <- edges$IDsource +1
  edges$IDtarget <- edges$IDtarget +1
  
  ##closeness calculation
  cc <- closeness_w(edges, directed = FALSE)
  
  ##calculate cnet
  ccnet <- nrow(cc)/sum(cc[,2])
  ccnet_normalized <- nrow(cc)/sum(cc[,3])
  
  ##betweenness calcuation 
  bness <- betweenness_w(edges, directed = FALSE)
  
  df_stats_all <- data.frame(
    Anzahl_Knoten = nrow(nodes), 
    Anzahl_Kanten = nrow(links), 
    cnet_Wert = ccnet, 
    normalisierter_cnet_Wert = ccnet_normalized
  )
  
  df_stats_nodes <- merge(cc, bness)
  df_stats_nodes <- merge(df_stats_nodes, nodes, by.x = "node", by.y = "ID")
  df_stats_nodes <- df_stats_nodes[,c("name", "closeness", "n.closeness", "betweenness")]
  #df_stats_nodes <- df_stats_nodes %>% 
  #  rename(Knoten = name, ClosenessCentrality = closeness, NormalizedClosenessCentrality = n.closeness,BetweennessCentrality = betweenness)
  
  list_obj$links <- links
  list_obj$nodes <- nodes
  list_obj$stats_all <- df_stats_all
  list_obj$stats_nodes <- df_stats_nodes
  
  return(list_obj)
}

# Create Shiny object
shinyApp(ui = ui, server = server)



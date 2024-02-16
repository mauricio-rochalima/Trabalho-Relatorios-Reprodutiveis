#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library("dplyr") #for data manipulation
library("igraph") # for social network analysis
library("ggraph") 
library(readxl)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(graphlayouts)
library(ggplot2)
library(snahelper)
library(miniCRAN)
library(magrittr)
library(remotes)
library(intergraph)
library(Cairo)
library(ggrepel)
library(here)
library(knitr)
library(kableExtra)
library(tidytext)  
library(DiagrammeR)





carregar_dados <- function(caminho_arquivo) {
    
    c <- here::here("BD",caminho_arquivo)  
    
    edges <- readxl::read_excel(c, sheet = "Edges", skip = 1)
    vertices <- readxl::read_excel(c, sheet = "Vertices", skip = 1)
    
    Hashtag <- readxl::read_excel(c, sheet = "Overall Metrics", skip = 1)
    Hashtag <- toString(Hashtag[30, 2])
    
    # Retorna a lista de dados
    return(list(edges = edges, vertices = vertices, Hashtag = Hashtag))
    
}


# Função Plot Grafo

plot_network <- function(graph, layout = "fr", Hashtag) {
    set.seed(123)
    
    page_rank(net.tidy)$vector
    
    g <- net.tidy |>
     #   activate(nodes) %>%
     #   mutate(PageRank = centrality_pagerank()) %>%
     #   mutate(community = as.factor(group_infomap())) %>%
        ggraph(layout = layout) +
        labs(title = Hashtag) +
        geom_edge_arc(alpha=.6,edge_width = 0.015,edge_colour = "#A8A8A8", arrow = arrow(angle = 0, length = unit(0.1, "inches"), ends = "last", type = "closed")) +
        geom_edge_link(width = 1, colour = "lightgray") +
       # geom_node_point(aes(colour = community,size=1)) +
        geom_node_text(aes(label = label,size=10), colour = "#000000",repel=TRUE,
                       family = "serif",fontface = "bold") +
        scale_size(range = c(0, 30)) + 
        theme_graph(fg_text_colour = 'white') + 
        theme(legend.position = "none")
    
    print(g)
}



d <- here::here("BD","Influencers Tratados","Tabelas.xlsx")  

caminho_tab <- read_excel(d,sheet = "Tabs")



c <-  here("BD","Influencers Tratados","00-Influenciadores-localizados.xlsx")

influencers <- read_excel(c,sheet = "Influencers")



influencers <- influencers %>%
    select(User_Name,Usuário, Descrição, PageRank, `N. Seguidores`)  # %>%
  #  arrange(desc(PageRank))   %>%
  #  mutate(`N. Seguidores` = format(`N. Seguidores`, big.mark = ".")) %>%
  #   mutate(PageRank = format(PageRank, decimal.mark = ","))






# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Selecione a Base de Dados"),

    
        selectInput("base", "Escolha uma Base de Dados:",
                    choices = c(caminho_tab$BD_Bruto)),

        # Show a plot of the generated distribution
        
    # Show the selected item
    textOutput("selectedItem"),
    
    
    # Local para exibir o gráfico
    plotOutput("grafico")
    
 
    
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    
    
    
    
    output$selectedItem <- renderText({
        
        caminho_arquivo <- paste0(input$base,".xlsx")
        
        
       
         Hashtag <- carregar_dados(caminho_arquivo)$Hashtag
         net.tidy <- here("BD","Grafos",paste0(input$base,".graphml"))
       
         
         
         paste("Hashtags: ", net.tidy)
         
  
    
    })
    
    
    output$grafico <- renderPlot({
        
        c <- here("BD","Grafos",paste0(input$base,".graphml"))
        # Leia o arquivo graphml
        net.tidy <- read_graph(c, format = "graphml")
        
        
       
        
        plot_network(net.tidy,"kk","Hashtag")
        
    })        
  
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

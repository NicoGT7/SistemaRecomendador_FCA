library(shiny)
library(visNetwork)
library(shinyWidgets)
library(fcaR)
library(igraph)

getIdx <- function(concepts, atr){
  return(Matrix::which(fcaR:::.equal_sets(atr$get_vector(), concepts$intents())))
}

getAttributes <- function(concepts, idx){
  intent <- concepts$sub(idx)$get_intent()
  attributes <- intent$get_attributes()[which(as.vector(intent$get_vector()) == 1)]
  return(attributes)
}

# función para crear grafo a partir de retículo
graph_sublattice <- function(sublattice) {
  # Obtener el número total de conceptos en el retículo
  num <- sublattice$size()
  
  # Crear un grafo vacío
  grafo <- graph(c())
  
  # Agregar nodos al grafo
  grafoRes <- add_vertices(grafo, num)  # Cambio de nombre
  
  # Obtener las conexiones entre los nodos
  for (i in 1:num) {
    if(i != 1){
      # Obtener los vecinos superiores del nodo actual
      upN <- sublattice$upper_neighbours(sublattice$sub(i))
      upN <- upN$to_list()
      for (j in 1:length(upN)) {
        atr <- upN[[j]]$get_intent()
        arista <- getIdx(sublattice, atr)
        
        # Agregar aristas entre el nodo actual y sus vecinos superiores
        grafoRes <- add_edges(grafoRes, c(i, arista))
      }
      
    }
    
    if(i < num){
      # Obtener los vecinos inferiores del nodo actual
      loN <- sublattice$lower_neighbours(sublattice$sub(i))
      loN <- loN$to_list()
      for (j in 1:length(loN)) {
        atr <- loN[[j]]$get_intent()
        arista <- getIdx(sublattice, atr)
        
        # Agregar aristas entre el nodo actual y sus vecinos inferiores
        grafoRes <- add_edges(grafoRes, c(i, arista))
      }
    }
  }
  
  grafoRes <- simplify(grafoRes)
  grafoRes <- as.undirected(grafoRes, mode = "collapse")
  return(grafoRes)  
}

ui <- fluidPage(
  titlePanel("Grafo Interactivo"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("threshold", "Umbral de soporte:",
                  min = 0, max = 1, value = 0.8, step = 0.1)
    ),
    mainPanel(
      visNetworkOutput("network"),
      verbatimTextOutput("selected_node_attributes") 
    )
  )
)

server <- function(input, output, session) {
  # Creamos el contexto forma dado nuestro dataset
  fc <- FormalContext$new(vegas)
  
  # Buscamos los conceptos
  fc$find_concepts()
  
  # Definimos una función para generar el grafo
  generate_graph <- function(threshold) {
    
    # Para quedarnos con el subretículo
    idx <- which(fc$concepts$support() > threshold)
    
    # Nos quedamos con el subreticulo
    sublattice <- fc$concepts$sublattice(idx)
    
    # Crear el grafo del subretículo
    graph <- graph_sublattice(sublattice)
    
    # Convertir el grafo a formato compatible con visNetwork
    vis_data <- toVisNetworkData(graph)
    
    # Retornar el grafo
    return(visNetwork(nodes = vis_data$nodes, edges = vis_data$edges, main = "Grafo Interactivo") %>%
             visIgraphLayout(layout = "layout_with_sugiyama") %>%
             visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
             visInteraction(hover = TRUE) %>%
             visEvents(click = "function(properties) {
               var nodeId = properties.nodes[0];
               Shiny.setInputValue('selected_node_id', nodeId);
             }"))
  }
  
  output$network <- renderVisNetwork({
    generate_graph(input$threshold)
  })
  
  observeEvent(input$selected_node_id, {
    selected_node <- input$selected_node_id
    if (!is.null(selected_node)) {
      attributes <- getAttributes(fc$concepts, as.numeric(selected_node))
      output$selected_node_attributes <- renderPrint({
        cat("Atributos del nodo seleccionado:\n")
        cat(paste(attributes, collapse = ", "))
      })
    }
  })
}

shinyApp(ui, server)

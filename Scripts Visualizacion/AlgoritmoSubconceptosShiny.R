library(arules)
library(dplyr)
library(magrittr)
library(fcaR)
library(shiny)
library(DT)

getIdx <- function(concepts, atr){
  return(Matrix::which(fcaR:::.equal_sets(atr$get_vector(), concepts$intents())))
}

getSupportSub <- function(concepts, idx) {
  # Nos quedamos con el concepto en el índice especificado (idx)
  C <- concepts$sub(idx)
  
  # Calculamos su soporte
  suppC <- concepts$support()[idx]
  
  # Sacamos los subconceptos de dicho concepto
  subconcepts <- concepts$subconcepts(C)
  
  # Creamos la matriz que devolveremos
  # tamaño: filas = nº subconceptos, columnas = índice + confianza + atributos
  matriz <- matrix(nrow = subconcepts$size(), ncol = length(concepts$sub(1)$get_intent()$get_attributes()) + 2)
  colNames <- c("idx", "confidence", concepts$sub(1)$get_intent()$get_attributes())
  colnames(matriz) <- colNames
  
  # Bucle para recorrer los subconceptos
  for (i in 1:subconcepts$size()) {
    # Calculamos el soporte 
    suppD <- subconcepts$support()[i]
    
    # Calculamos la confianza
    confidence <- suppD / suppC
    
    # Añadimos todo a la fila de la matriz
    subConcept <- subconcepts$sub(i)
    matriz[i,1] <- getIdx(concepts, subConcept$get_intent())
    matriz[i, 2] <- confidence
    
    aux <- subconcepts$sub(i)$get_intent()
    aux2 <- as.vector(t(aux$get_vector()))
    
    matriz[i, 3:ncol(matriz)] <- aux2
  }
  
  # Convertimos la matriz en un data.frame
  result_df <- as.data.frame(matriz)
  
  # Asignamos los nombres de las columnas
  colnames(result_df) <- colNames
  
  # Ordenamos los resultados por confianza de manera descendente
  result_df <- result_df[order(result_df$confidence, decreasing = TRUE), ]
  
  return(result_df)
}

ui <- fluidPage(
  titlePanel("Tabla Interactiva de Subconceptos"),
  
  fluidRow(
    column(12,
           sidebarPanel(
             textInput("idx_input", "Índice:", value = "502")
           )
    )
  ),
  
  fluidRow(
    column(12,
           dataTableOutput("tabla")
    )
  )
)

server <- function(input, output) {
  # Creamos el contexto formal dado nuestro dataset
  fc <- FormalContext$new(vegas)
  # Buscamos los conceptos
  fc$find_concepts()
  
  concepts <- fc$concepts
  
  output$tabla <- renderDataTable({
    idx <- as.numeric(input$idx_input)
    data <- getSupportSub(concepts, idx)
    datatable(data, options = list(pageLength = 10), rownames = FALSE)
  })
}


shinyApp(ui = ui, server = server)
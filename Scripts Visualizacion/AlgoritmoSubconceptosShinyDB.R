library(arules)
library(dplyr)
library(magrittr)
library(fcaR)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)

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

# Definir la UI
ui <- fluidPage(
  titlePanel("Gráfico de barras interactivo"),
  
  fluidRow(
    column(12,
           sidebarPanel(
             textInput("idx_input", "Índice:", value = "502")
           )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_bars", "Número de barras a mostrar:", min = 1, max = 20, value = 10)
    ),
    mainPanel(
      plotlyOutput("barplot") 
    )
  )
)

# Definir el servidor
server <- function(input, output) {
  fc <- FormalContext$new(vegas)
  fc$find_concepts()
  concepts <- fc$concepts
  
  output$barplot <- renderPlotly({
    idx <- as.numeric(input$idx_input)
    data <- getSupportSub(concepts, idx)
    data <- data[1:input$num_bars, ]
    
    # Crear el gráfico de barras y mostrarlo
    p <- ggplot(data, aes(x = reorder(idx, -confidence), y = confidence)) +
      geom_bar(stat = "identity") +
      labs(x = "Índice del subconcepto", y = "Confianza") +
      theme_minimal()
    
    # Convertir el gráfico
    p <- ggplotly(p, tooltip = c("y"))
    
    # Agregar mensaje al hacer clic en una barra
    p <- event_register(p, "plotly_click")
    
    # Manejar el evento de clic en el gráfico
    observeEvent(event_data("plotly_click"), {
      click_data <- event_data("plotly_click")
      bar_index <- click_data$x
      confidence <- click_data$y
      
      idx <- data$idx[bar_index]
      
      intent <- concepts$sub(idx)$get_intent()
      attributes <- intent$get_attributes()[which(as.vector(intent$get_vector()) == 1)]
      
      showModal(modalDialog(
        title = "Mensaje",
        paste("El concepto", as.character(idx), "con confianza: ", 
              as.character(confidence), "posee los atributos ", 
              paste(attributes, collapse = ", ")),
        easyClose = TRUE
      ))
      
    })
    
    # Devolver el gráfico
    p
  })
}


shinyApp(ui = ui, server = server)


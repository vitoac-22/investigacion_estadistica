# app/server.R
server <- function(input, output, session) {
  
  # Filtro reactivo: Solo se recalcula si cambian los inputs
  data_filtrada <- reactive({
    req(input$provincia) # Evita errores si está vacío
    
    datos_hospitalarios %>%
      filter(provincia_nombre == input$provincia,
             edad >= input$edad_min)
  })
  
  output$plot_tendencia <- renderPlot({
    # Usamos data_filtrada() como función
    data_filtrada() %>%
      count(mes_egreso) %>%
      ggplot(aes(x = mes_egreso, y = n)) +
      geom_line(color = "#2c3e50", size = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = paste("Egresos en", input$provincia), y = "Cantidad")
  })
  
  output$tabla_resumen <- renderTable({
    data_filtrada() %>%
      summarise(
        Total = n(),
        Edad_Promedio = mean(edad, na.rm = TRUE),
        Dias_Estancia_Media = mean(dias_estadia, na.rm = TRUE)
      )
  })
}
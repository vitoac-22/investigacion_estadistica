# app/server.R

server <- function(input, output, session) {
  
  # --- 1. DATOS REACTIVOS (El corazón del dashboard) ---
  data_filtrada <- reactive({
    d <- egresos
    
    # Aplicar filtros secuenciales
    if (input$filtro_provincia != "Todas") {
      d <- d %>% filter(prov_ubi == input$filtro_provincia)
    }
    
    if (input$filtro_sector != "Todos") {
      d <- d %>% filter(sector == input$filtro_sector)
    }
    
    d %>% filter(edad_anios >= input$filtro_edad[1],
                 edad_anios <= input$filtro_edad[2])
  })
  
  # --- 2. CÁLCULO DE KPIs ---
  
  output$kpi_total <- renderText({
    format(nrow(data_filtrada()), big.mark = ",")
  })
  
  output$kpi_estancia <- renderText({
    media <- mean(data_filtrada()$dia_estad, na.rm = TRUE)
    paste(round(media, 1), "días")
  })
  
  output$kpi_mortalidad <- renderText({
    # Asumiendo que es_fallecido es 1 (Sí) o 0 (No)
    # Si viene como factor/etiqueta, habría que convertirlo a numérico primero
    tasa <- mean(as.numeric(as.character(data_filtrada()$es_fallecido)), na.rm = TRUE) * 100
    paste0(round(tasa, 2), "%")
  })
  
  # --- 3. GRÁFICOS ---
  
  # Gráfico de Eficiencia (Estancia Hospitalaria)
  output$plot_estancia_sector <- renderPlot({
    req(nrow(data_filtrada()) > 0)
    
    p <- data_filtrada() %>%
      ggplot(aes(x = sector, y = dia_estad, fill = sector)) +
      theme_minimal() +
      labs(x = "Sector", y = "Días de Estancia") +
      theme(legend.position = "none", text = element_text(size = 14))
    
    if (input$tipo_grafico_estancia == "box") {
      # Usamos escala logarítmica si hay outliers muy grandes
      p + geom_boxplot(alpha = 0.7) + scale_y_log10() + 
        labs(subtitle = "Escala Logarítmica (para visualizar mejor outliers)")
    } else {
      p + stat_summary(fun = "mean", geom = "bar", alpha = 0.7) +
        geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 1)), vjust = -0.5)
    }
  })
  
  # Tabla Top Causas
  output$tabla_causas <- renderTable({
    data_filtrada() %>%
      count(cau_cie10, sort = TRUE) %>%
      head(5) %>%
      rename("Código CIE-10" = cau_cie10, "Frecuencia" = n)
  })
  
  # Gráfico Temporal (Estacionalidad)
  output$plot_temporal <- renderPlot({
    data_filtrada() %>%
      count(mes_ingr) %>%
      # Asegurar orden de meses si es texto
      mutate(mes_ingr = factor(mes_ingr, levels = month.name)) %>% 
      ggplot(aes(x = mes_ingr, y = n, group = 1)) +
      geom_line(color = "#2c3e50", size = 1) +
      geom_point(size = 3, color = "#e74c3c") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "Cantidad de Egresos")
  })
}
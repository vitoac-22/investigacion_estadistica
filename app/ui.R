# app/ui.R
ui <- page_sidebar(
  title = "Dashboard de Egresos Hospitalarios 2024",
  theme = bs_theme(version = 5, bootswatch = "zephyr"), # Tema limpio y pro
  
  sidebar = sidebar(
    selectInput("provincia", "Seleccione Provincia:", 
                choices = unique(datos_hospitalarios$provincia_nombre), 
                selected = "PICHINCHA"),
    numericInput("edad_min", "Edad Mínima:", value = 18, min = 0, max = 100),
    actionButton("btn_calc", "Actualizar Análisis", class = "btn-primary")
  ),
  
  card(
    card_header("Tendencia de Egresos"),
    plotOutput("plot_tendencia")
  ),
  
  card(
    card_header("Resumen Estadístico"),
    tableOutput("tabla_resumen")
  )
)
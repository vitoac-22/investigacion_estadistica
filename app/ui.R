# app/ui.R

ui <- page_sidebar(
  title = "Dashboard de Eficiencia Hospitalaria 2024",
  theme = bs_theme(bootswatch = "zephyr"), # Tema profesional
  
  # --- BARRA LATERAL (Filtros Globales) ---
  sidebar = sidebar(
    title = "Filtros de Control",
    selectInput("filtro_provincia", "Provincia:", 
                choices = c("Todas", lista_provincias), 
                selected = "Todas"),
    selectInput("filtro_sector", "Sector Hospitalario:", 
                choices = c("Todos", lista_sectores), 
                selected = "Todos"),
    sliderInput("filtro_edad", "Rango de Edad (Años):",
                min = 0, max = 110, value = c(0, 110)),
    hr(),
    helpText("Metodología: Análisis basado en Egresos Hospitalarios INEC 2024.")
  ),
  
  # --- CUERPO PRINCIPAL ---
  
  # 1. KPIs (Indicadores Clave de Rendimiento)
  layout_columns(
    value_box(
      title = "Total Egresos",
      value = textOutput("kpi_total"),
      showcase = bsicons::bs_icon("hospital"),
      theme = "primary"
    ),
    value_box(
      title = "Estancia Media (Días)", 
      value = textOutput("kpi_estancia"),
      p("Eficiencia Operativa"),
      showcase = bsicons::bs_icon("clock-history"),
      theme = "secondary"
    ),
    value_box(
      title = "Tasa de Mortalidad",
      value = textOutput("kpi_mortalidad"),
      p("Calidad Asistencial"),
      showcase = bsicons::bs_icon("heart-pulse-fill"),
      theme = "danger" # Rojo para alertas
    )
  ),
  
  # 2. Fila de Análisis de Eficiencia (Estancia)
  card(
    card_header("Análisis de Eficiencia: Días de Estada por Sector"),
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("tipo_grafico_estancia", "Visualización:",
                     choices = c("Boxplot (Distribución)" = "box", 
                                 "Promedio Simple" = "bar"))
      ),
      plotOutput("plot_estancia_sector")
    ),
    full_screen = TRUE
  ),
  
  # 3. Fila de Mortalidad y Estacionalidad
  layout_columns(
    card(
      card_header("Top 5 Causas (CIE-10)"),
      tableOutput("tabla_causas")
    ),
    card(
      card_header("Estacionalidad de Ingresos"),
      plotOutput("plot_temporal")
    )
  )
)
# -----------------------------------------------------------------------------
# Script de Inicialización de Estructura de Proyecto
# Objetivo: Crear un entorno reproducible y ordenado para la investigación.
# -----------------------------------------------------------------------------

# Instalamos paquetes de gestión de archivos si no los tienes
if (!require("fs")) install.packages("fs")
if (!require("here")) install.packages("here")

library(fs)   # Manejo de sistema de archivos moderno
library(here) # Para que las rutas relativas nunca fallen

# 1. Definición de la estructura de carpetas
# La lógica es separar datos, código y resultados.
carpetas <- c(
  "data/raw",           # Aquí tiras el CSV del INEC tal cual te lo bajaste
  "data/processed",     # Aquí guardaremos el .rds o .csv limpio
  "data/metadata",      # Aquí va el Diccionario de Variables (PDF/Excel)
  "R",                  # Scripts con funciones auxiliares (limpieza, gráficos custom)
  "analysis",           # Aquí van tus RMarkdown/Quarto de exploración
  "output/plots",       # Gráficos exportados (PNG, PDF)
  "output/tables",      # Tablas resumen (CSV, Excel)
  "docs"                # Documentación del proyecto (tu propuesta PDF va aquí)
)

# 2. Creación de directorios
# El argumento 'recurse = TRUE' crea subcarpetas automáticamente
print("🚧 Construyendo la infraestructura del proyecto...")

for (dir in carpetas) {
  if (dir_exists(dir)) {
    print(paste("✅ El directorio ya existe:", dir))
  } else {
    dir_create(dir)
    print(paste("🔨 Directorio creado:", dir))
  }
}

# 3. Creación de archivos clave vacíos (Placeholders)
# Un README es obligatorio si quieres que alguien (o tu yo del futuro) entienda algo.
if (!file_exists("README.md")) {
  file_create("README.md")
  writeLines(c("# Investigación Egresos Hospitalarios 2024",
               "",
               "## Descripción",
               "Análisis de eficiencia y letalidad hospitalaria para la UNACH.",
               "",
               "## Estructura",
               "- `data/raw`: Datos crudos del INEC.",
               "- `analysis`: Scripts de RMarkdown.",
               ""), "README.md")
  print("📄 README.md creado.")
}

# Script placeholder para la limpieza
if (!file_exists("R/01_limpieza_datos.R")) {
  file_create("R/01_limpieza_datos.R")
  print("📄 Script de limpieza creado en R/01_limpieza_datos.R")
}

print("🚀 ¡Infraestructura lista! Copia tus datos en data/raw y empieza a trabajar.")
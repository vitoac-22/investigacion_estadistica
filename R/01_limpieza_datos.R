# -----------------------------------------------------------------------------
# PROYECTO: Investigación de Eficiencia y Letalidad Hospitalaria (Ecuador 2024)
# SCRIPT: 01_limpieza_datos.R
# AUTOR: Equipo de Ciencia de Datos / Estudiante UNACH
#
# OBJETIVO: 
# Transformar la data cruda (Raw) en un dataset analítico limpio (Processed).
# Aplica reglas de negocio derivadas de la auditoría de metadatos.
#
# INPUT: data/raw/egresos_hospitalarios_2024.rds
# OUTPUT: data/processed/egresos_limpios_2024.rds
# -----------------------------------------------------------------------------

# 1. LIBRERÍAS
library(tidyverse)
library(janitor)   # Para estandarizar nombres de columnas
library(haven)     # Para manejar las etiquetas (labels) del INEC

# 2. CARGA DE DATOS
# -----------------------------------------------------------------------------
ruta_raw <- "data/raw/egresos_hospitalarios_2024.rds"

if (!file.exists(ruta_raw)) {
  stop("❌ ERROR: No se encuentra el archivo en data/raw/. Ejecuta primero la carga.")
}

print(paste("⏳ Cargando dataset crudo desde:", ruta_raw))
egresos_raw <- read_rds(ruta_raw)

# 3. PIPELINE DE LIMPIEZA Y TRANSFORMACIÓN
# -----------------------------------------------------------------------------
print("⚙️ Aplicando reglas de negocio y limpieza...")

egresos_clean <- egresos_raw %>%
  
  # A. LIMPIEZA DE ESTRUCTURA
  clean_names() %>% # Convierte nombres a minusculas_y_guiones
  
  # B. SELECCIÓN QUIRÚRGICA DE VARIABLES
  # Seleccionamos solo lo auditado. Descartamos ruido.
  select(
    sector,       # 1, 2, 3
    sexo,         # 1, 2
    cod_edad,     # 1,2,3 (bebés), 4 (años)
    edad,         # Valor numérico
    dia_estad,    # Días de estada
    con_egrpa,    # Condición de egreso (Vivo/Muerto)
    cau_cie10,    # Causa médica (Código)
    mes_ingr,     # Mes (Estacionalidad)
    prov_ubi      # Ubicación geográfica
  ) %>%
  
  # C. INGENIERÍA DE VARIABLES (FEATURE ENGINEERING)
  mutate(
    # --- 1. LÓGICA DE EDAD ---
    # Convertimos todo a una escala única: "Años Cumplidos"
    # Regla: Si cod_edad es 4, es Años. Si es 1, 2, 3 (Horas, Días, Meses), es 0 años.
    edad_anios = case_when(
      as.numeric(cod_edad) == 4 ~ as.numeric(edad),
      as.numeric(cod_edad) %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # --- 2. LÓGICA DE SECTOR (Gestión) ---
    # Simplificación dicotómica para prueba de hipótesis.
    # Regla: 1 = Público (MSP/IESS). 2 y 3 = Privado (Con/Sin fines de lucro).
    tipo_gestion = case_when(
      as.numeric(sector) == 1 ~ "Público",
      as.numeric(sector) %in% c(2, 3) ~ "Privado",
      TRUE ~ NA_character_ # Por si aparece basura
    ),
    
    # --- 3. LÓGICA DE MORTALIDAD (Target) ---
    # Unificamos los tipos de muerte para calcular Letalidad Total.
    # Regla: 1 = Vivo. 2 (<48h) y 3 (>48h) = Fallecido.
    estado_salida = case_when(
      as.numeric(con_egrpa) == 1 ~ "Vivo",
      as.numeric(con_egrpa) %in% c(2, 3) ~ "Fallecido",
      TRUE ~ NA_character_
    ),
    
    # Variable Dummy (0/1) para modelos de regresión o cálculo de tasas
    es_fallecido = if_else(estado_salida == "Fallecido", 1, 0),
    
    # --- 4. CATEGORIZACIÓN DEMOGRÁFICA ---
    sexo_cat = case_when(
      as.numeric(sexo) == 1 ~ "Hombre",
      as.numeric(sexo) == 2 ~ "Mujer",
      TRUE ~ NA_character_
    ),
    
    # Aseguramos que días sea numérico puro
    dia_estad = as.numeric(dia_estad)
  ) %>%
  
  # D. FILTRADO DE CALIDAD (SANITY CHECKS)
  filter(
    !is.na(edad_anios),      # Eliminamos edades corruptas
    !is.na(tipo_gestion),    # Eliminamos sectores desconocidos
    !is.na(estado_salida),   # Eliminamos condiciones desconocidas
    dia_estad >= 0,          # Eliminamos errores negativos
    
    # CORTE DE CORDURA (OUTLIERS):
    # Excluimos estancias > 365 días (1 año).
    # Justificación: Evitar que errores de digitación (ej. 19000 días) 
    # o casos psiquiátricos crónicos distorsionen la media de eficiencia operativa.
    dia_estad <= 365         
  )

# 4. VALIDACIÓN DE RESULTADOS
# -----------------------------------------------------------------------------
print("✅ Proceso finalizado. Resumen del Dataset Limpio:")
print(paste("Filas Originales:", nrow(egresos_raw)))
print(paste("Filas Procesadas:", nrow(egresos_clean)))
print(paste("Columnas Finales:", ncol(egresos_clean)))

print("--- Distribución por Sector ---")
print(table(egresos_clean$tipo_gestion))

print("--- Distribución por Estado de Salida ---")
print(table(egresos_clean$estado_salida))

# 5. EXPORTACIÓN
# -----------------------------------------------------------------------------
ruta_out <- "data/processed/egresos_limpios_2024.rds"

# Aseguramos que la carpeta exista
if (!dir.exists("data/processed")) dir.create("data/processed")

write_rds(egresos_clean, ruta_out)
print(paste("💾 DATASET EXPORTADO EXITOSAMENTE A:", ruta_out))
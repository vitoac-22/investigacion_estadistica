# -----------------------------------------------------------------------------
# PROYECTO: Investigación de Eficiencia y Letalidad Hospitalaria (Ecuador 2024)
# SCRIPT: 00_auditoria_completa.R
# AUTOR: Equipo de Ciencia de Datos / Estudiante UNACH
# FECHA: Enero 2026
#
# OBJETIVO GENERAL: 
# Realizar un diagnóstico forense de la estructura, integridad y metadatos del 
# archivo fuente (.rds). Este paso es prerrequisito para la limpieza de datos.
#
# OBJETIVOS ESPECÍFICOS:
# 1. Decodificar variables categóricas con metadatos incrustados (haven_labelled).
# 2. Validar la consistencia de las definiciones del Diccionario de Variables.
# 3. Identificar valores atípicos (outliers) y códigos de error en variables numéricas.
# -----------------------------------------------------------------------------

# --- 1. CONFIGURACIÓN DEL ENTORNO ---
# Se cargan paquetes esenciales para manipulación de datos y lectura de metadatos SPSS/Stata.
library(tidyverse) # Manipulación y visualización
library(haven)     # CRÍTICO: Para leer etiquetas de valores (attributes) del INEC
library(skimr)     # Resúmenes estadísticos de alto nivel
library(janitor)   # Limpieza de nombres de cabecera

# --- 2. INGESTA DE DATOS (LECTURA ROBUSTA) ---
# Se utiliza tryCatch para gestionar errores de ruta sin detener abruptamente la ejecución.
ruta_archivo <- "data/raw/egresos_hospitalarios_2024.rds"

print(paste("⏳ Iniciando carga del dataset:", ruta_archivo))

egresos <- tryCatch({
  read_rds(ruta_archivo)
}, error = function(e) {
  stop("❌ ERROR CRÍTICO: No se encuentra el archivo en data/raw/. Verifique la ruta.")
})

print(paste("✅ Carga exitosa. Dimensiones:", nrow(egresos), "filas x", ncol(egresos), "columnas."))


# --- 3. DEFINICIÓN DE FUNCIONES AUXILIARES ---
# Función para auditar variables tipo 'haven_labelled'.
# Muestra el Código Numérico subyacente junto a su Etiqueta de Texto.
inspect_variable <- function(data, variable_name) {
  var_sym <- sym(variable_name)
  
  message(paste("\n--- AUDITORÍA DE VARIABLE:", toupper(variable_name), "---"))
  
  data %>%
    count(Codigo_Raw = !!var_sym, Etiqueta_Decodificada = as_factor(!!var_sym)) %>%
    mutate(
      Porcentaje = n / sum(n) * 100,
      Validacion = "--> Verificar contra Diccionario"
    ) %>%
    arrange(desc(n)) %>%
    print(n = 20) # Muestra hasta 20 categorías principales
}


# --- 4. AUDITORÍA DE VARIABLES CATEGÓRICAS (METADATOS) ---
# Se busca confirmar el significado real de los códigos numéricos para configurar la limpieza.

# 4.1. SECTOR (Público vs. Privado)
# Hipótesis a validar: El código 1 agrupa toda la red pública (MSP, IESS, FFAA).
inspect_variable(egresos, "sector")

# 4.2. CONDICIÓN DE EGRESO (Variable de Resultado)
# Hipótesis a validar: Distinción entre Alta (Vivo) y tipos de Mortalidad (<48h vs >48h).
inspect_variable(egresos, "con_egrpa")

# 4.3. SEXO
# Hipótesis a validar: Codificación binaria estándar.
inspect_variable(egresos, "sexo")


# --- 5. AUDITORÍA DE VARIABLES NUMÉRICAS Y LÓGICA DE NEGOCIO ---

# 5.1. VALIDACIÓN DE UNIDAD DE MEDIDA DE EDAD (cod_edad)
# El INEC suele separar la edad en dos columnas: magnitud y unidad temporal.
message("\n--- ANÁLISIS CRUZADO: EDAD vs CÓDIGO DE EDAD ---")
message("Objetivo: Determinar qué código corresponde a 'Años' para filtrar bebés.")

egresos %>%
  group_by(cod_edad) %>%
  summarise(
    n_casos = n(),
    edad_min = min(edad, na.rm = TRUE),
    edad_promedio = mean(edad, na.rm = TRUE),
    edad_max = max(edad, na.rm = TRUE),
    interpretacion_sugerida = case_when(
      mean(edad, na.rm = TRUE) > 15 ~ "AÑOS (Usar directo)",
      max(edad, na.rm = TRUE) <= 31 ~ "DÍAS/MESES (Convertir a 0 años)",
      TRUE ~ "INDEFINIDO"
    )
  ) %>%
  print()

# 5.2. DETECCIÓN DE OUTLIERS EN ESTANCIA HOSPITALARIA (dia_estad)
message("\n--- ESTADÍSTICAS DESCRIPTIVAS: DÍAS DE ESTADA ---")
# Se fuerza a numérico para asegurar cálculos correctos
summary(as.numeric(egresos$dia_estad))

message("\n--- TOP 10 VALORES EXTREMOS (DÍAS DE ESTADA) ---")
# Identificación de posibles errores de digitación (ej. estancias > 10 años)
egresos %>%
  select(dia_estad, edad, cau_cie10) %>%
  arrange(desc(dia_estad)) %>%
  head(10) %>%
  print()

# Verificación de integridad (valores negativos)
n_negativos <- sum(egresos$dia_estad < 0, na.rm = TRUE)
if(n_negativos > 0) {
  warning(paste("⚠️ ALERTA: Se encontraron", n_negativos, "registros con días negativos."))
} else {
  message("✅ Validación: No existen tiempos de estancia negativos.")
}


# --- 6. CRUCE DE VARIABLES CRÍTICAS (CONSISTENCIA) ---
message("\n--- MATRIZ DE CONSISTENCIA: SECTOR vs MORTALIDAD ---")
# Tabla cruzada para asegurar que existen datos de fallecidos en todos los sectores
# Se usa as_factor() para visualizar las etiquetas en lugar de los códigos
table(
  Sector = as_factor(egresos$sector), 
  Condicion = as_factor(egresos$con_egrpa)
)

# --- FIN DEL SCRIPT ---
message("\n✅ Auditoría finalizada. Proceder al script '01_limpieza_datos.R' aplicando las reglas descubiertas.")

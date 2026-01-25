# -----------------------------------------------------------------------------
# Script: R/00_auditoria_inicial.R
# Objetivo: INTERROGATORIO. Diagnóstico forense de las variables críticas.
# Nota: No modificamos nada. Solo miramos.
# -----------------------------------------------------------------------------

library(tidyverse)
library(skimr)

# 1. Carga de Datos (Formato RDS confirmado)
# -----------------------------------------------------------------------------
# Usamos tryCatch por si acaso la ruta varíe, para no romper el script
egresos <- tryCatch({
  read_rds("data/raw/egresos_hospitalarios_2024.rds")
}, error = function(e) {
  stop("Error: No encuentro el archivo .rds en data/raw/")
})

print(paste("Dimensiones del dataset:", nrow(egresos), "filas x", ncol(egresos), "columnas"))


# 2. El Misterio de la Edad (cod_edad vs edad)
# -----------------------------------------------------------------------------
# Necesitamos saber qué código corresponde a Años, Meses y Días.
# Lógica: Si el Código X tiene una media de edad de 40, son Años. 
# Si el Código Y tiene max de 30, son Días.

print("--- DESGLOSE: COD_EDAD vs EDAD ---")
egresos %>%
  group_by(cod_edad) %>%
  summarise(
    total_casos = n(),
    edad_min = min(edad, na.rm = TRUE),
    edad_promedio = mean(edad, na.rm = TRUE),
    edad_max = max(edad, na.rm = TRUE),
    ejemplo_visual = "--> ¿Años, Meses o Días?"
  ) %>%
  print()


# 3. Auditoría de Tiempos: Días de Estada (dia_estad)
# -----------------------------------------------------------------------------
# Buscamos valores negativos (errores) o estancias de 5000 días (outliers extremos).

print("--- ESTADÍSTICAS: DÍAS DE ESTADA ---")
summary(egresos$dia_estad)

print("--- Top 10 Estancias más largas (¿Errores o Realidad?) ---")
egresos %>%
  select(dia_estad, edad, cau_cie10) %>%
  arrange(desc(dia_estad)) %>%
  head(10) %>%
  print()

# Chequeo de negativos
negativos <- sum(egresos$dia_estad < 0, na.rm = TRUE)
print(paste("Registros con días negativos:", negativos))


# 4. Auditoría de Códigos: SECTOR y CONDICIÓN DE SALIDA
# -----------------------------------------------------------------------------
# Necesitamos mapear qué números existen para configurar el script de limpieza luego.

print("--- FRECUENCIA: SECTOR (¿Qué códigos existen?) ---")
table(egresos$sector, useNA = "ifany")

print("--- FRECUENCIA: CONDICIÓN DE EGRESO (con_egrpa) ---")
# 1 suele ser Alta, 2 Fallecido. Vamos a ver si hay 3, 4, 9, etc.
table(egresos$con_egrpa, useNA = "ifany")


# 5. Cruce Rápido de Mortalidad (Solo para ver si los datos tienen sentido)
# -----------------------------------------------------------------------------
print("--- MATRIZ: SECTOR vs CONDICIÓN EGRESO ---")
table(egresos$sector, egresos$con_egrpa)
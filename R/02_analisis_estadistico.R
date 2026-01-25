# -----------------------------------------------------------------------------
# PROYECTO: Eficiencia y Letalidad Hospitalaria
# SCRIPT: 02_analisis_estadistico.R
# OBJETIVO: Ejecutar la metodología estadística (Descriptiva, Probabilidad, Inferencia).
# INPUT: data/processed/egresos_limpios_2024.rds
# -----------------------------------------------------------------------------

library(tidyverse)
library(knitr)
library(broom) # Para ver los resultados de los tests estadísticos ordenados

# 1. CARGA DE DATOS (EL DATASET "GOLD STANDARD")
# -----------------------------------------------------------------------------
ruta_datos <- "data/processed/egresos_limpios_2024.rds"

print("⏳ Cargando datos procesados...")
egresos <- read_rds(ruta_datos)

# =============================================================================
# METODOLOGÍA 1: MEDIDAS DE TENDENCIA CENTRAL Y DISPERSIÓN
# Objetivo: Entender el comportamiento de la estancia (Eficiencia).
# =============================================================================
print("--- 1. ANÁLISIS DESCRIPTIVO: DÍAS DE ESTADA ---")

tabla_descriptiva <- egresos %>%
  group_by(tipo_gestion) %>%
  summarise(
    N = n(),
    Media_Estancia = mean(dia_estad, na.rm = TRUE),
    Mediana_Estancia = median(dia_estad, na.rm = TRUE),
    Desviacion_Estandar = sd(dia_estad, na.rm = TRUE),
    CV_Porcentaje = (sd(dia_estad, na.rm=TRUE) / mean(dia_estad, na.rm=TRUE)) * 100,
    Maximo = max(dia_estad)
  )

# Imprimimos bonito en consola
print(kable(tabla_descriptiva, digits = 2, caption = "Eficiencia Operativa por Sector"))

# INTERPRETACIÓN AUTOMÁTICA (Lógica simple):
# Si la Desviación Estándar es alta (CV > 100%), significa que el promedio es mentiroso
# porque hay mucha variabilidad (pacientes de 1 día y pacientes de 100 días mezclados).


# =============================================================================
# METODOLOGÍA 2: CÁLCULO DE PROBABILIDAD CONDICIONAL
# Objetivo: Calcular P(Fallecido | Sector).
# Fórmula: P(A|B) = P(A y B) / P(B)
# =============================================================================
print("--- 2. PROBABILIDAD CONDICIONAL DE LETALIDAD ---")

probabilidades <- egresos %>%
  count(tipo_gestion, estado_salida) %>%
  group_by(tipo_gestion) %>%
  mutate(
    Total_Sector = sum(n),
    Probabilidad = n / Total_Sector
  ) %>%
  filter(estado_salida == "Fallecido") %>%
  select(tipo_gestion, n_fallecidos = n, Total_Sector, Probabilidad)

print(kable(probabilidades, digits = 4, caption = "Probabilidad de Fallecer dado el Sector"))

# Verificación de Riesgo Relativo (Opcional pero brutal)
prob_publico <- probabilidades$Probabilidad[probabilidades$tipo_gestion == "Público"]
prob_privado <- probabilidades$Probabilidad[probabilidades$tipo_gestion == "Privado"]
rr <- prob_publico / prob_privado

print(paste("RIESGO RELATIVO: Es", round(rr, 2), "veces más probable fallecer en el Público que en el Privado (Datos crudos sin ajustar por severidad)."))


# =============================================================================
# METODOLOGÍA 3: PRUEBA DE HIPÓTESIS (INFERENCIA)
# Hipótesis Nula (H0): Media_Público = Media_Privado (No hay diferencia en eficiencia)
# Hipótesis Alt  (H1): Media_Público != Media_Privado (Sí hay diferencia)
# Prueba: t-Student de Welch (porque las varianzas NO son iguales ni de broma)
# =============================================================================
print("--- 3. PRUEBA DE HIPÓTESIS: DIFERENCIA DE MEDIAS (WELCH T-TEST) ---")

# Ejecutamos el test
test_estancia <- t.test(dia_estad ~ tipo_gestion, data = egresos)

# Mostramos resultados técnicos
print(test_estancia)

# Interpretación Cínica Automatizada
p_valor <- test_estancia$p.value

print("--- CONCLUSIÓN ESTADÍSTICA FINAL ---")
if (p_valor < 0.05) {
  print("✅ RECHAZAMOS H0. La diferencia en los tiempos de estancia es ESTADÍSTICAMENTE SIGNIFICATIVA.")
  print("Traducción: No es casualidad. Los sistemas operan a velocidades estructuralmente distintas.")
} else {
  print("❌ NO RECHAZAMOS H0. No hay evidencia suficiente para decir que son diferentes.")
  print("Traducción: Todo es ruido. Estadísticamente son igual de lentos (o rápidos).")
}

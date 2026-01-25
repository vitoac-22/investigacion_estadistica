# app/global.R
library(shiny)
library(tidyverse)
library(bslib)
library(haven) # Importante para manejar las etiquetas del .rds

# 1. Cargar Datos (Manejo robusto de errores)
egresos <- tryCatch({
  # Intentamos cargar tu archivo real
  df <- readRDS("../data/processed/egresos_limpios_2024.rds")
  
  # CONVERSIÓN CRÍTICA: Convertir etiquetas de haven a factores legibles
  # Esto hace que en lugar de salir "1", salga "Público" en los gráficos
  df %>%
    mutate(across(where(is.labelled), as_factor))
  
}, error = function(e) {
  # DATOS DUMMY (Solo para que veas la estructura si no encuentra el archivo)
  # Generados con las variables exactas que me diste
  message("ADVERTENCIA: Usando datos simulados. Verifica la ruta del .rds")
  tibble(
    sector = sample(c("Público", "Privado"), 1000, replace = T),
    sexo = sample(c("Hombre", "Mujer"), 1000, replace = T),
    edad_anios = sample(0:90, 1000, replace = T),
    dia_estad = rpois(1000, 4), # Distribución Poisson para días de estancia
    mes_ingr = sample(month.name[1:12], 1000, replace = T),
    prov_ubi = sample(c("PICHINCHA", "GUAYAS", "AZUAY"), 1000, replace = T),
    es_fallecido = sample(c(0, 1), 1000, prob = c(0.95, 0.05), replace = T),
    cau_cie10 = sample(c("A09", "J18", "K35", "I10"), 1000, replace = T)
  )
})

# 2. Listas para filtros (se calculan una vez al inicio para no recargar)
lista_provincias <- unique(as.character(egresos$prov_ubi))
lista_sectores <- unique(as.character(egresos$sector))
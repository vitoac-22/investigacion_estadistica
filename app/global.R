# app/global.R
library(shiny)
library(tidyverse)
library(bslib) # Para que no parezca una web de 1995

# CARGA DE DATOS
# Nota el "../" para subir un nivel y entrar a data
# Usa tryCatch por si alguien clona el repo sin tener la data local
datos_hospitalarios <- tryCatch({
  readRDS("../data/processed/egresos_limpios_2024.rds")
}, error = function(e) {
  # Datos dummy por si falla la carga (útil para debug)
  data.frame(Mes = month.name, Ingresos = runif(12, 100, 500))
})

# Si tienes funciones personalizadas en la carpeta R/, cárgalas así:
# source("../R/mis_calculos_financieros.R")
library(shiny)
library(tidyverse)
library(bslib)
library(haven)

# Definimos explícitamente el orden en ESPAÑOL (Principio de Orden Lógico de Wilke)
meses_orden <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

egresos <- tryCatch({
  df <- readRDS("../data/processed/egresos_limpios_2024.rds")
  
  df %>%
    mutate(across(where(is.labelled), as_factor)) %>%
    mutate(
      # CORRECCIÓN CLAVE:
      # 1. Aseguramos que sea caracter primero
      mes_ingr = as.character(mes_ingr),
      # 2. Convertimos a factor usando los niveles en ESPAÑOL
      # Si tus datos vienen como "01", "02", avísame, pero asumo texto por el gráfico anterior.
      mes_ingr = factor(mes_ingr, levels = meses_orden)
    )
  
}, error = function(e) {
  message("ADVERTENCIA: Usando datos simulados por error de carga.")
  tibble(
    sector = sample(c("Público", "Privado"), 1000, replace = T),
    sexo = sample(c("Hombre", "Mujer"), 1000, replace = T),
    edad_anios = sample(0:90, 1000, replace = T),
    dia_estad = rpois(1000, 4),
    # Simulamos datos en español para que coincida con la lógica
    mes_ingr = factor(sample(meses_orden, 1000, replace = T), levels = meses_orden),
    prov_ubi = sample(c("PICHINCHA", "GUAYAS", "AZUAY"), 1000, replace = T),
    es_fallecido = sample(c(0, 1), 1000, prob = c(0.95, 0.05), replace = T),
    cau_cie10 = sample(c("A09", "J18", "K35", "I10"), 1000, replace = T)
  )
})

lista_provincias <- unique(as.character(egresos$prov_ubi))
lista_sectores <- unique(as.character(egresos$sector))
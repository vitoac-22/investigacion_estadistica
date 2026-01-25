# -----------------------------------------------------------------------------
# Script: 00_auditoria_parte2.R
# Objetivo: DECODIFICAR METADATOS.
# Problema previo: Las variables son tipo <haven_labelled> y 'table()' falló.
# Solución: Usar tidyverse y haven para ver Código + Etiqueta simultáneamente.
# -----------------------------------------------------------------------------

library(tidyverse)
library(haven) # Necesario para entender los archivos del INEC

# 1. Carga Segura
# -----------------------------------------------------------------------------
print("⏳ Cargando datos para inspección de etiquetas...")
egresos <- read_rds("data/raw/egresos_hospitalarios_2024.rds")

# 2. Función Auxiliar de Inspección
# -----------------------------------------------------------------------------
# Esta función nos muestra: El Código Real (Raw) + La Etiqueta (Texto) + Cantidad
inspect_variable <- function(data, variable_name) {
  var_sym <- sym(variable_name)
  
  data %>%
    count(Codigo = !!var_sym, Etiqueta = as_factor(!!var_sym)) %>%
    mutate(Porcentaje = n / sum(n) * 100) %>%
    arrange(desc(n)) %>%
    print(n = 50) # Imprime hasta 50 categorías para ver todo
}

# 3. Auditoría: SECTOR (Variable Crítica 1)
# -----------------------------------------------------------------------------
print("==========================================")
print("AUDITORÍA DE VARIABLE: SECTOR")
print("Objetivo: Confirmar qué es Público y qué es Privado")
print("==========================================")
inspect_variable(egresos, "sector")

# NOTA PARA EL USUARIO:
# Mira la consola. 
# ¿El código '1' dice 'Ministerio de Salud'? 
# ¿El código '2' dice 'IESS'?
# Anota esto para la limpieza.


# 4. Auditoría: CONDICIÓN DE EGRESO (Variable Crítica 2)
# -----------------------------------------------------------------------------
print("==========================================")
print("AUDITORÍA DE VARIABLE: CON_EGRPA (Condición Salida)")
print("Objetivo: Identificar el código exacto de FALLECIDO")
print("==========================================")
inspect_variable(egresos, "con_egrpa")

# NOTA PARA EL USUARIO:
# Normalmente: 1 = Alta, 2 = Fallecido (o Defunción).
# Pero a veces hay código 3, 9 o NA. Verifícalo.


# 5. Auditoría: SEXO (Variable Demográfica)
# -----------------------------------------------------------------------------
print("==========================================")
print("AUDITORÍA DE VARIABLE: SEXO")
print("==========================================")
inspect_variable(egresos, "sexo")


# 6. Cruce de Seguridad (Matriz)
# -----------------------------------------------------------------------------
print("==========================================")
print("MATRIZ: SECTOR vs CONDICIÓN (Datos Crudos)")
print("==========================================")
# Usamos as_factor para que la tabla sea legible en consola
table(Sector = as_factor(egresos$sector), Condicion = as_factor(egresos$con_egrpa))
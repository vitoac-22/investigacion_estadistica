# Librerías
library(tidyverse)
library(knitr)
library(broom) # Para ver los resultados de los tests estadísticos ordenados
library(tidyverse)
library(ggplot2)
library(haven) 

ruta_datos <- "data/processed/egresos_limpios_2024.rds"
egresos <- read_rds(ruta_datos)

# --- 0. LIMPIEZA Y PREPARACIÓN (Mantenemos lo que ya funcionaba) ---
df_clean <- egresos %>%
  mutate(across(where(is.labelled), as_factor))

df_graficos <- df_clean %>%
  mutate(grupo_etario = cut(edad_anios, 
                            breaks = c(0, 14, 64, 120),
                            labels = c("Niñez (0-14)", "Adultez (15-64)", "Adulto Mayor (65+)"),
                            include.lowest = TRUE)) %>%
  filter(!is.na(grupo_etario), !is.na(dia_estad), dia_estad > 0) %>%
  mutate(sexo = factor(sexo), sector = factor(sector))

# --- GRÁFICA 1: Boxplot (Arreglado fondo y leyenda segura) ---
p1 <- ggplot(df_graficos, aes(x = grupo_etario, y = dia_estad, fill = sexo)) +
  geom_boxplot(alpha = 0.9, # Menos transparencia en el color
               outlier.shape = 21, outlier.size = 1.5, outlier.alpha = 0.6) +
  scale_y_log10(labels = scales::comma) + 
  scale_fill_manual(values = c("#3498db", "#e74c3c")) + 
  labs(
    title = "Distribución de la Estancia Hospitalaria",
    subtitle = "Comparativa por Edad y Sexo (Escala Log)",
    x = NULL, # Quitamos etiqueta redundante
    y = "Días de Estancia",
    fill = "Sexo",
    caption = "Fuente: Egresos Hospitalarios 2024"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", # Leyenda abajo para dar aire al gráfico
    text = element_text(family = "sans", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    panel.background = element_rect(fill = "white", color = NA), # Fondo blanco explícito
    plot.background = element_rect(fill = "white", color = NA)   # Fondo blanco explícito
  )

print(p1)
# CLAVE: bg = "white" evita que el PNG salga transparente
ggsave("output/boxplot_estancia_edad_sexo.png", plot = p1, width = 8, height = 6, dpi = 300, bg = "white")


# --- GRÁFICA 2: Barras (Leyenda fuera para no tapar valores) ---
df_resumen <- df_graficos %>%
  group_by(grupo_etario, sector) %>%
  summarise(media_estancia = mean(dia_estad, na.rm = TRUE), .groups = 'drop')

p2 <- ggplot(df_resumen, aes(x = grupo_etario, y = media_estancia, fill = sector)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1, width = 0.7) + # alpha 1 = color sólido
  geom_text(aes(label = round(media_estancia, 1)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Estancia Media por Sector",
    subtitle = "Promedio de días según grupo etario",
    x = NULL,
    y = "Promedio de Días",
    fill = "Sector"
  ) +
  theme_classic() +
  theme(
    legend.position = "top", # La sacamos del gráfico para evitar superposición
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.background = element_rect(fill = "white", color = NA), # Fondo blanco explícito
    plot.background = element_rect(fill = "white", color = NA)   # Fondo blanco explícito
  )

print(p2)
ggsave("output/barras_estancia_sector.png", plot = p2, width = 8, height = 5, dpi = 300, bg = "white")

# ---- Segunda Parte ----
library(tidyverse)
library(haven)
library(scales)

# --- 1. CARGA Y PREPARACIÓN (Mismo estándar que usamos antes) ---
# Asegúrate de ajustar la ruta si lo corres desde la carpeta R/ o desde la raíz
egresos <- readRDS("data/processed/egresos_limpios_2024.rds") %>% 
  mutate(across(where(is.labelled), as_factor)) %>% 
  mutate(
    sector = factor(sector),
    sexo = factor(sexo),
    # Creamos variable binaria numérica para letalidad (asumiendo 1=Si, 0=No en es_fallecido)
    # Ajusta los niveles según tu data real si es necesario
    fallecido_bin = as.numeric(es_fallecido) - 1 # Si factor es 1/2, esto lo hace 0/1. Verifica tu data.
  )

# --- 2. GRÁFICO DE DENSIDAD: PÚBLICO VS PRIVADO (Prueba de Hipótesis Visual) ---
# Este gráfico valida tu hallazgo de que el público tiene estancias más largas
p_densidad <- egresos %>% 
  filter(dia_estad <= 30) %>% # Hacemos zoom en el primer mes para ver mejor la curva
  ggplot(aes(x = dia_estad, fill = sector, color = sector)) +
  geom_density(alpha = 0.4, size = 1) +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) + # Azul y Rojo
  scale_color_manual(values = c("#2980b9", "#c0392b")) +
  labs(
    title = "Distribución de la Estancia Hospitalaria",
    subtitle = "Comparativa de densidad (Zoom en primeros 30 días)",
    x = "Días de Estancia",
    y = "Densidad",
    fill = "Sector", color = "Sector",
    caption = "Nota: Se truncó el eje X a 30 días para visualización."
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/densidad_estancia_sector.png", p_densidad, width = 8, height = 5, dpi = 300)


# --- 3. GRÁFICO DE LETALIDAD (Calidad Asistencial) ---
# Muestra la diferencia de 1.73% vs 1.25%
df_letalidad <- egresos %>% 
  group_by(sector) %>% 
  summarise(
    tasa_mortalidad = mean(fallecido_bin, na.rm = TRUE) * 100,
    error_est = (sd(fallecido_bin, na.rm = TRUE) / sqrt(n())) * 100 # Para barras de error
  )

p_letalidad <- ggplot(df_letalidad, aes(x = sector, y = tasa_mortalidad, fill = sector)) +
  geom_col(alpha = 0.9, width = 0.6) +
  # Agregamos barras de error para darle rigor científico (intervalo de confianza)
  geom_errorbar(aes(ymin = tasa_mortalidad - 1.96*error_est, 
                    ymax = tasa_mortalidad + 1.96*error_est), 
                width = 0.2, size = 0.8) +
  geom_text(aes(label = paste0(round(tasa_mortalidad, 2), "%")), 
            vjust = -2.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("#3498db", "#e74c3c")) +
  scale_y_continuous(limits = c(0, max(df_letalidad$tasa_mortalidad) * 1.2)) + # Dar aire arriba
  labs(
    title = "Tasa de Letalidad Hospitalaria por Sector",
    subtitle = "Probabilidad condicional de fallecimiento dado el ingreso",
    x = NULL,
    y = "Tasa de Letalidad (%)",
    fill = "Sector"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/barras_letalidad_sector.png", p_letalidad, width = 6, height = 5, dpi = 300)

# (Aquí puedes añadir los otros gráficos demográficos que generamos anteriormente)
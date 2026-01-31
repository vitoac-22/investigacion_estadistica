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

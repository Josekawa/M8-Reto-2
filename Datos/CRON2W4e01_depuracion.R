# ===============================================================
# Reto 2 — Importación y depuración de datos (CRONOS2 Wave 4)
# Autor/a: <tu_nombre>
# Fecha: <yyyy-mm-dd>
# Descripción:
#   Este script debe colocarse en la MISMA carpeta que el archivo de datos,
#   por ejemplo en la carpeta 'Datos' junto con:
#     - CRON2W4e01.csv
#   Lee el CSV, depura y guarda:
#     - CRON2W4e01_depurado.csv
# ===============================================================

# ----------------------------
# 0) Paquetes necesarios
# ----------------------------
required <- c('tidyverse','readr','janitor')
new <- required[!required %in% rownames(installed.packages())]
if(length(new)) install.packages(new, dependencies = TRUE)
invisible(lapply(required, library, character.only = TRUE))

# ----------------------------
# 1) Rutas (mismo directorio)
# ----------------------------
input_file  <- 'CRON2W4e01.csv'           # archivo original
output_file <- 'CRON2W4e01_depurado.csv'  # archivo depurado

if (!file.exists(input_file)) {
  stop('No se encuentra CRON2W4e01.csv en el directorio actual. Asegúrate de colocar este script y el CSV en la misma carpeta (p. ej., Datos).')
}

# ----------------------------
# 2) Lectura e inspección inicial
# ----------------------------
df_raw <- readr::read_csv(input_file, show_col_types = FALSE)
df <- janitor::clean_names(df_raw)  # nombres en snake_case

message('Registros: ', nrow(df), ' | Variables: ', ncol(df))

# ----------------------------
# 3) Depuración y recodificación
# ----------------------------

# 3.1) Recodificar ítems tipo Likert (w4q*) → 9 = NA
likert_cols <- names(df)[grepl("^w4q\\d+(?:\\.\\d+)?$", names(df))]
df <- df %>% mutate(across(all_of(likert_cols), ~ na_if(.x, 9)))

# 3.2) Crear etiquetas legibles
df <- df %>%
  mutate(
    # Género: 1 = Hombre, 2 = Mujer; otros/NA -> NS/NC
    gndr_lab = case_when(
      gndr == 1 ~ 'Hombre',
      gndr == 2 ~ 'Mujer',
      TRUE ~ 'NS/NC'
    ),
    # Modo de recogida (mode)
    mode_lab = case_when(
      mode == 1 ~ 'F2F (presencial)',
      mode == 2 ~ 'CATI (teléfono)',
      mode == 3 ~ 'CAWI (online)',
      mode == 4 ~ 'Otro',
      TRUE ~ 'NS/NC'
    ),
    # Cohortes de edad
    age_group = cut(agea,
                    breaks = c(0, 29, 44, 64, 120),
                    labels = c('18–29','30–44','45–64','65+'),
                    right = TRUE, include.lowest = TRUE)
  )

# 3.3) Índice de confianza institucional (w4q61–w4q73)
trust_cols <- names(df)[grepl("^w4q(6[1-9]|7[0-3])$", names(df))]
df <- df %>% mutate(trust_index = rowMeans(across(all_of(trust_cols)), na.rm = TRUE))

# ----------------------------
# 4) Guardar base depurada
# ----------------------------
readr::write_csv(df, output_file)
message('Archivo depurado guardado: ', normalizePath(output_file))

# Fin del script

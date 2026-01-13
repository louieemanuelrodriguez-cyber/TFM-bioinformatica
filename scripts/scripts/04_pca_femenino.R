# ============================================================
# 04_pca_femenino.R
# PCA cohorte femenina (completo + zoom) + tablas PCA
# ============================================================

library(dplyr)
library(factoextra)
library(tibble)
library(gt)

# Cargar datos limpios (crea datos_completos_m)
source("scripts/01_limpieza_datos.R")

# ------------------------------------------------------------
# Variables para PCA (mismas del análisis)
# ------------------------------------------------------------
vars_pca_m <- c(
  "glucosa",
  "colesterol_total",
  "colesterol_hdl",
  "colesterol_ldl",
  "trigliceridos",
  "AST",
  "ALT",
  "GGT",
  "fosfatasa_alcalina",
  "hemoglobina",
  "hematocrito",
  "leucocitos",
  "pcr_us"
)

# ------------------------------------------------------------
# Clases a incluir en el PCA (incluye Menopausia)
# ------------------------------------------------------------
clases_pca_m <- c("Control paciente sano", "HTA", "DM2", "Menopausia")

datos_pca_m <- datos_completos_m %>%
  filter(diagnostico %in% clases_pca_m) %>%
  mutate(diagnostico = factor(diagnostico, levels = clases_pca_m))

X_pca_m <- datos_pca_m[, vars_pca_m]

# PCA
pca_res_m <- prcomp(X_pca_m, center = TRUE, scale. = TRUE)

grupo_diag_m <- datos_pca_m$diagnostico

# ------------------------------------------------------------
# PCA completo (ANEXOS) – sin elipses
# ------------------------------------------------------------
pca_femenino_completo <- fviz_pca_ind(
  pca_res_m,
  geom.ind = "point",
  habillage = grupo_diag_m,
  palette = c(
    "Control paciente sano" = "#1B9E77",
    "HTA" = "#D95F02",
    "DM2" = "#7570B3",
    "Menopausia" = "#E6AB02"
  ),
  pointshape = 16,
  pointsize  = 3,
  alpha.ind  = 0.8,
  legend.title = "Diagnóstico",
  repel = TRUE
) +
  theme_minimal(base_size = 14) +
  ggtitle("PCA – Cohorte femenina (completo, Anexos)")

pca_femenino_completo

# ------------------------------------------------------------
# PCA con ajuste de ventana (zoom) – Resultados
# ------------------------------------------------------------
pca_femenino_zoom <- pca_femenino_completo +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  ggtitle("PCA – Cohorte femenina (zoom, mejora visualización)")

pca_femenino_zoom

# ------------------------------------------------------------
# Tabla varianza explicada (Mujeres)
# ------------------------------------------------------------
make_pca_var_table <- function(pca_obj, cohort = "Mujeres") {
  eig     <- pca_obj$sdev^2
  prop    <- eig / sum(eig)
  cumprop <- cumsum(prop)

  tibble(
    Componente = paste0("PC", seq_along(eig)),
    `Varianza explicada (%)` = round(prop * 100, 1),
    `Varianza acumulada (%)` = round(cumprop * 100, 1)
  ) %>%
    gt() %>%
    tab_header(title = paste0("PCA – Varianza explicada (", cohort, ")")) %>%
    fmt_number(columns = 2:3, decimals = 1)
}

tabla_pca_var_m <- make_pca_var_table(pca_res_m, "Mujeres")
tabla_pca_var_m

# ------------------------------------------------------------
# Cargas PC1 y PC2 (Mujeres)
# ------------------------------------------------------------
load_m <- as.data.frame(pca_res_m$rotation) %>%
  rownames_to_column("Biomarcador")

tabla_cargas_m <- load_m %>%
  dplyr::select(Biomarcador, PC1, PC2) %>%
  arrange(desc(abs(PC1))) %>%
  gt() %>%
  tab_header(title = "Cargas principales (PC1 y PC2) del PCA – Mujeres")

tabla_cargas_m


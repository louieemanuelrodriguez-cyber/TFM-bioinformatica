# ============================================================
# 03_pca_masculino.R
# PCA cohorte masculina (completo + zoom) + tablas PCA
# ============================================================

library(dplyr)
library(factoextra)
library(tibble)
library(gt)

source("scripts/01_limpieza_datos.R")

# Variables para PCA
vars_pca <- c(
  "glucosa", "colesterol_total", "colesterol_hdl", "colesterol_ldl",
  "trigliceridos", "AST", "ALT", "GGT",
  "fosfatasa_alcalina", "hemoglobina", "hematocrito",
  "leucocitos", "pcr_us"
)

# Clases a visualizar
clases_pca <- c("Control paciente sano", "HTA", "DM2")

datos_pca <- datos_completos_h %>%
  filter(diagnostico %in% clases_pca) %>%
  mutate(diagnostico = factor(diagnostico, levels = clases_pca))

X_pca <- datos_pca[, vars_pca]

pca_res <- prcomp(X_pca, center = TRUE, scale. = TRUE)

grupo_diag <- datos_pca$diagnostico

# PCA completo (Anexos)
pca_completo <- fviz_pca_ind(
  pca_res,
  geom.ind = "point",
  habillage = grupo_diag,
  palette = c(
    "Control paciente sano" = "#1B9E77",
    "HTA" = "#D95F02",
    "DM2" = "#7570B3"
  ),
  pointshape = 16,
  pointsize = 3,
  alpha.ind = 0.8,
  legend.title = "Diagnóstico",
  repel = TRUE
) +
  theme_minimal(base_size = 14) +
  ggtitle("PCA – Cohorte masculina (completo)")

pca_completo

# PCA con zoom (Resultados)
pca_zoom <- pca_completo +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  ggtitle("PCA – Cohorte masculina (zoom, mejora visualización)")

pca_zoom

# ---------------------------
# Tabla varianza explicada
# ---------------------------
make_pca_var_table <- function(pca_obj, cohort = "Hombres") {
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

tabla_pca_var_h <- make_pca_var_table(pca_res, "Hombres")
tabla_pca_var_h

# ---------------------------
# Cargas PC1 y PC2
# ---------------------------
load_h <- as.data.frame(pca_res$rotation) %>%
  rownames_to_column("Biomarcador")

tabla_cargas_h <- load_h %>%
  dplyr::select(Biomarcador, PC1, PC2) %>%
  arrange(desc(abs(PC1))) %>%
  gt() %>%
  tab_header(title = "Cargas principales (PC1 y PC2) del PCA – Hombres")

tabla_cargas_h

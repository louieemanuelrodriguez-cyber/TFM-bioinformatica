# ============================================================
# 02_analisis_descriptivo.R
# Tablas descriptivas (gtsummary) 
# ============================================================

library(dplyr)
library(gtsummary)

# Cargar datos limpios (crea datos_completos_h y datos_completos_m)
source("scripts/01_limpieza_datos.R")

# ============================================================
# Tabla descriptiva - HOMBRES
# ============================================================
tabla1_h <- datos_completos_h %>%
  dplyr::select(
    diagnostico, edad,
    glucosa, colesterol_total, colesterol_hdl, colesterol_ldl,
    trigliceridos, AST, ALT, GGT, fosfatasa_alcalina,
    hemoglobina, hematocrito, leucocitos, pcr_us,
    tg_hdl, aip, tyg, pcr_us_hdl
  ) %>%
  tbl_summary(
    by = diagnostico,
    statistic = everything() ~ "{median} ({p25}–{p75})",
    digits = everything() ~ 1,
    label = list(
      edad               ~ "Edad (años)",
      glucosa            ~ "Glucosa (mg/dL)",
      colesterol_total   ~ "Colesterol total (mg/dL)",
      colesterol_hdl     ~ "Colesterol HDL (mg/dL)",
      colesterol_ldl     ~ "Colesterol LDL (mg/dL)",
      trigliceridos      ~ "Triglicéridos (mg/dL)",
      AST                ~ "AST (U/L)",
      ALT                ~ "ALT (U/L)",
      GGT                ~ "GGT (U/L)",
      fosfatasa_alcalina ~ "Fosfatasa alcalina (U/L)",
      hemoglobina        ~ "Hemoglobina (g/dL)",
      hematocrito        ~ "Hematocrito (%)",
      leucocitos         ~ "Leucocitos (×10⁹/L)",
      pcr_us             ~ "PCR-us (mg/L)",
      tg_hdl             ~ "TG/HDL",
      aip                ~ "AIP",
      tyg                ~ "TyG",
      pcr_us_hdl         ~ "PCR-us/HDL"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Características basales de la cohorte masculina según diagnóstico**")

tabla1_h

# ============================================================
# Tabla descriptiva - MUJERES
# ============================================================
tabla1_m <- datos_completos_m %>%
  dplyr::select(
    diagnostico, edad,
    glucosa, colesterol_total, colesterol_hdl, colesterol_ldl,
    trigliceridos, AST, ALT, GGT, fosfatasa_alcalina,
    hemoglobina, hematocrito, leucocitos, pcr_us,
    tg_hdl, aip, tyg, pcr_us_hdl
  ) %>%
  tbl_summary(
    by = diagnostico,
    statistic = everything() ~ "{median} ({p25}–{p75})",
    digits = everything() ~ 1,
    label = list(
      edad               ~ "Edad (años)",
      glucosa            ~ "Glucosa (mg/dL)",
      colesterol_total   ~ "Colesterol total (mg/dL)",
      colesterol_hdl     ~ "Colesterol HDL (mg/dL)",
      colesterol_ldl     ~ "Colesterol LDL (mg/dL)",
      trigliceridos      ~ "Triglicéridos (mg/dL)",
      AST                ~ "AST (U/L)",
      ALT                ~ "ALT (U/L)",
      GGT                ~ "GGT (U/L)",
      fosfatasa_alcalina ~ "Fosfatasa alcalina (U/L)",
      hemoglobina        ~ "Hemoglobina (g/dL)",
      hematocrito        ~ "Hematocrito (%)",
      leucocitos         ~ "Leucocitos (×10⁹/L)",
      pcr_us             ~ "PCR-us (mg/L)",
      tg_hdl             ~ "TG/HDL",
      aip                ~ "AIP",
      tyg                ~ "TyG",
      pcr_us_hdl         ~ "PCR-us/HDL"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Características basales de la cohorte femenina según diagnóstico**")

tabla1_m

# ============================================================
# Resumen de balance de clases 
# ============================================================
table(datos_completos_h$diagnostico)
prop.table(table(datos_completos_h$diagnostico))

table(datos_completos_m$diagnostico)
prop.table(table(datos_completos_m$diagnostico))


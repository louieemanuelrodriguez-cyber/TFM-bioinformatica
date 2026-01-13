# ============================================================
# 06_score_clinico.R
# Aplicación ciega del score metabólico en 8 pacientes (archivos externos)
# ============================================================

library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(gt)

# ============================================================
# 1. Lectura de datos (prueba ciega)
# ============================================================

# Controles / diagnóstico real
ph3 <- read_excel("Datos prueba.xlsx") |> clean_names()
controles1 <- ph3

# Limpieza de duplicados (elimina "No aplica" cuando hay duplicados)
controles_filtrado3 <- controles1 %>%
  group_by(peticion_modulab) %>%
  mutate(n_duplicados = n()) %>%
  ungroup() %>%
  filter(!(n_duplicados > 1 & diagnostico == "No aplica")) %>%
  distinct(peticion_modulab, .keep_all = TRUE) %>%
  dplyr::select(-n_duplicados)

controles_final3 <- controles_filtrado3

# Biomarcadores
hbio3 <- read_excel("Platinum Hombre2.xls") |> clean_names()
hbio3 <- hbio3 |> rename(peticion_modulab = 1)

# Limpiar edad a número
hbio3 <- hbio3 |>
  mutate(
    edad_num = str_extract(edad_del_paciente, "\\d+"),
    edad_num = as.integer(edad_num)
  )

# Si hubiera duplicados, quedarse con una fila por ID
hbio_unica3 <- hbio3 |>
  group_by(peticion_modulab) |>
  dplyr::slice(1) |>
  ungroup()

# ============================================================
# 2. Unión controles + biomarcadores
# ============================================================

datos_ml3 <- controles_final3 |>
  mutate(peticion_modulab = as.character(peticion_modulab)) |>
  inner_join(
    hbio_unica3 |> mutate(peticion_modulab = as.character(peticion_modulab)),
    by = "peticion_modulab"
  )

# Eliminar columnas duplicadas de edad
datos_ml3 <- datos_ml3 |>
  dplyr::select(-edad_num, -edad_del_paciente)

# ============================================================
# 3. Selección y renombrado de columnas
# ============================================================

datos_filtrados3 <- datos_ml3 %>%
  dplyr::select(
    peticion_modulab,
    diagnostico,
    edad,
    sexo_del_paciente,
    glucosa_en_ayunas_suero,
    colesterol_total_suero,
    colesterol_hdl_suero,
    colesterol_ldl_calculado_suero_2,
    trigliceridos_suero,
    ast_aspartato_aminotransferasa_suero,
    alt_alanina_aminotransferasa_suero,
    ggt_gammaglutamil_transferasa_suero,
    fosfatasa_alcalina_alkp_suero,
    hemoglobina_hgb,
    hematocrito_hct,
    leucocitos_wbc,
    pcr_ultrasensible_suero
  ) %>%
  rename(
    glucosa            = glucosa_en_ayunas_suero,
    colesterol_total   = colesterol_total_suero,
    colesterol_hdl     = colesterol_hdl_suero,
    colesterol_ldl     = colesterol_ldl_calculado_suero_2,
    trigliceridos      = trigliceridos_suero,
    AST                = ast_aspartato_aminotransferasa_suero,
    ALT                = alt_alanina_aminotransferasa_suero,
    GGT                = ggt_gammaglutamil_transferasa_suero,
    fosfatasa_alcalina = fosfatasa_alcalina_alkp_suero,
    hemoglobina        = hemoglobina_hgb,
    hematocrito        = hematocrito_hct,
    leucocitos         = leucocitos_wbc,
    pcr_us             = pcr_ultrasensible_suero
  )

# ============================================================
# 4. Eliminar NA + anonimizar + PCR-us numérico + índices
# ============================================================

datos_completos_h3 <- datos_filtrados3 %>%
  drop_na() %>%
  mutate(id_anonimo = row_number()) %>%
  dplyr::select(id_anonimo, everything(), -peticion_modulab) %>%
  mutate(
    pcr_us = as.character(pcr_us),
    pcr_us = ifelse(pcr_us == "<0.4", "0.4", pcr_us),
    pcr_us = as.numeric(pcr_us)
  ) %>%
  mutate(
    tg_hdl     = trigliceridos / colesterol_hdl,
    aip        = log(trigliceridos / colesterol_hdl),
    tyg        = log((trigliceridos * glucosa) / 2),
    pcr_us_hdl = pcr_us / colesterol_hdl
  )

# ============================================================
# 5. Función score metabólico (igual a tu versión)
# ============================================================

calcular_score_metabolico <- function(df) {
  df %>%
    mutate(
      # Puntuación por edad
      puntos_edad = case_when(
        edad < 45               ~ 0,
        edad >= 45 & edad <= 60 ~ 1,
        edad > 60               ~ 2,
        TRUE                    ~ NA_real_
      ),
      # Puntuación por glucosa
      puntos_glucosa = case_when(
        glucosa < 100                  ~ 0,
        glucosa >= 100 & glucosa <=125 ~ 1,
        glucosa >= 126                 ~ 2,
        TRUE                           ~ NA_real_
      ),
      # Puntuación por TG/HDL
      puntos_tg_hdl = case_when(
        tg_hdl < 2.0                 ~ 0,
        tg_hdl >= 2.0 & tg_hdl <=3.5 ~ 1,
        tg_hdl > 3.5                 ~ 2,
        TRUE                         ~ NA_real_
      ),
      # Puntuación por TyG
      puntos_tyg = case_when(
        tyg < 8.5               ~ 0,
        tyg >= 8.5 & tyg <= 9.0 ~ 1,
        tyg > 9.0               ~ 2,
        TRUE                    ~ NA_real_
      ),
      # Puntuación por GGT
      puntos_ggt = case_when(
        GGT <= 40             ~ 0,
        GGT > 40 & GGT <= 80  ~ 1,
        GGT > 80              ~ 2,
        TRUE                  ~ NA_real_
      ),
      # Puntuación por PCR-us
      puntos_pcr = case_when(
        pcr_us < 1               ~ 0,
        pcr_us >= 1 & pcr_us <=3 ~ 1,
        pcr_us > 3               ~ 2,
        TRUE                     ~ NA_real_
      ),
      # Score total
      score_total = puntos_edad + puntos_glucosa + puntos_tg_hdl +
        puntos_tyg + puntos_ggt + puntos_pcr,
      # Categoría de riesgo
      categoria_riesgo = case_when(
        score_total <= 3                    ~ "Bajo riesgo",
        score_total >= 4 & score_total <= 7 ~ "Riesgo intermedio",
        score_total >= 8                    ~ "Alto riesgo",
        TRUE                                ~ NA_character_
      )
    )
}

# Aplicar score
datos_score <- calcular_score_metabolico(datos_completos_h3)

# ============================================================
# 6. Tabla final con concordancia clínica (igual a tu lógica)
# ============================================================

tabla_8_pacientes <- datos_score %>%
  mutate(
    concordancia_clinica = case_when(
      categoria_riesgo == "Alto riesgo"       & diagnostico == "Diabetico" ~ "Sí",
      categoria_riesgo == "Riesgo intermedio" & diagnostico %in% c("HTA", "Prediabetico") ~ "Sí",
      categoria_riesgo == "Bajo riesgo"       & diagnostico == "Paciente sano" ~ "Sí",
      TRUE ~ "No"
    )
  ) %>%
  dplyr::select(
    Paciente                        = id_anonimo,
    `Score total`                   = score_total,
    `Categoría de riesgo por score` = categoria_riesgo,
    `Diagnóstico real`              = diagnostico,
    `Concordancia clínica`          = concordancia_clinica
  )

tabla_8_pacientes

tabla_8_pacientes_gt <- tabla_8_pacientes %>%
  gt() %>%
  tab_header(title = "Aplicación ciega del score de riesgo metabólico a ocho pacientes")

tabla_8_pacientes_gt


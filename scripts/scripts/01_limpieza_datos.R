# ============================================================
# 01_limpieza_datos.R
# Limpieza y preparación de datos (Hombres y Mujeres)
# Salidas esperadas:
#   - datos_completos_h
#   - datos_completos_m
# ============================================================

library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)

# ============================================================
# 1A. HOMBRES: Controles + biomarcadores -> datos_completos_h
# ============================================================

# 1A.1 Controles hombres
ph1 <- read_excel("Control PH1.xlsx") |> clean_names()
ph2 <- read_excel("Control PH2.xlsx") |> clean_names()

controles_h <- bind_rows(ph1, ph2)

# Limpieza de duplicados (elimina "No aplica" cuando hay duplicados)
controles_h <- controles_h %>%
  group_by(peticion_modulab) %>%
  mutate(n_duplicados = n()) %>%
  ungroup() %>%
  filter(!(n_duplicados > 1 & diagnostico == "No aplica")) %>%
  distinct(peticion_modulab, .keep_all = TRUE) %>%
  dplyr::select(-n_duplicados)

controles_final_h <- controles_h

# 1A.2 Biomarcadores hombres
hbio <- read_excel("Tabla HBio.xlsx") |> clean_names()

hbio <- hbio |> rename(peticion_modulab = 1)

hbio <- hbio |>
  mutate(
    edad_num = str_extract(edad_del_paciente, "\\d+"),
    edad_num = as.integer(edad_num)
  )

hbio_unica <- hbio |>
  group_by(peticion_modulab) |>
  dplyr::slice(1) |>
  ungroup()

# 1A.3 Unión controles + biomarcadores (hombres)
datos_ml_h <- controles_final_h |>
  inner_join(hbio_unica, by = "peticion_modulab")

# Eliminar columnas duplicadas de edad
datos_ml_h <- datos_ml_h |> dplyr::select(-edad_num, -edad_del_paciente)

# 1A.4 Selección y renombrado de columnas (hombres)
datos_filtrados_h <- datos_ml_h %>%
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

# 1A.5 Eliminar NA + anonimizar + PCR-us numérico (hombres)
datos_completos_h <- datos_filtrados_h %>%
  drop_na() %>%
  mutate(id_anonimo = row_number()) %>%
  dplyr::select(id_anonimo, everything(), -peticion_modulab) %>%
  mutate(
    pcr_us = as.character(pcr_us),
    pcr_us = ifelse(pcr_us == "<0.4", "0.4", pcr_us),
    pcr_us = as.numeric(pcr_us)
  )

# 1A.6 Índices derivados (hombres)
datos_completos_h <- datos_completos_h %>%
  mutate(
    tg_hdl     = trigliceridos / colesterol_hdl,
    aip        = log(trigliceridos / colesterol_hdl),
    tyg        = log((trigliceridos * glucosa) / 2),
    pcr_us_hdl = pcr_us / colesterol_hdl
  )


# ============================================================
# 1B. MUJERES: Controles + biomarcadores -> datos_completos_m
# ============================================================

# 1B.1 Controles mujeres
pm1 <- read_excel("Control PM1.xlsx") |> clean_names()
pm2 <- read_excel("Control PM2.xlsx") |> clean_names()

controles_m <- bind_rows(pm1, pm2) |>
  mutate(peticion_modulab = as.character(peticion_modulab))

# Limpieza de duplicados (elimina "No aplica" cuando hay duplicados)
controles_m <- controles_m %>%
  group_by(peticion_modulab) %>%
  mutate(n_duplicados = n()) %>%
  ungroup() %>%
  filter(!(n_duplicados > 1 & diagnostico == "No aplica")) %>%
  distinct(peticion_modulab, .keep_all = TRUE) %>%
  dplyr::select(-n_duplicados)

controles_final_m <- controles_m

# 1B.2 Biomarcadores mujeres
mbio <- read_excel("Tabla MBio.xlsx") |> clean_names()

mbio <- mbio |>
  rename(peticion_modulab = 1) |>
  mutate(peticion_modulab = as.character(peticion_modulab))

mbio <- mbio |>
  mutate(
    edad_num = str_extract(edad_del_paciente, "\\d+"),
    edad_num = as.integer(edad_num)
  )

mbio_unica <- mbio |>
  group_by(peticion_modulab) |>
  dplyr::slice(1) |>
  ungroup()

# 1B.3 Unión controles + biomarcadores (mujeres)
datos_ml_m <- controles_final_m |>
  inner_join(mbio_unica, by = "peticion_modulab")

datos_ml_m <- datos_ml_m %>%
  mutate(edad = as.numeric(edad)) %>%
  dplyr::select(-edad_num, -edad_del_paciente)

# 1B.4 Selección y renombrado de columnas (mujeres)
datos_filtrados_m <- datos_ml_m %>%
  dplyr::select(
    peticion_modulab,
    diagnostico,
    edad,
    sexo_del_paciente,
    glucosa_en_ayunas_suero,
    colesterol_total_suero,
    colesterol_hdl_suero,
    colesterol_ldl_calculado_suero,
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
    colesterol_ldl     = colesterol_ldl_calculado_suero,
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

# 1B.5 Eliminar NA + anonimizar + PCR-us numérico (mujeres)
datos_completos_m <- datos_filtrados_m %>%
  drop_na() %>%
  mutate(id_anonimo = row_number() + 200) %>%
  dplyr::select(id_anonimo, everything(), -peticion_modulab) %>%
  mutate(
    pcr_us = as.character(pcr_us),
    pcr_us = ifelse(pcr_us == "<0.4", "0.4", pcr_us),
    pcr_us = as.numeric(pcr_us)
  )

# 1B.6 Índices derivados (mujeres)
datos_completos_m <- datos_completos_m %>%
  mutate(
    tg_hdl     = trigliceridos / colesterol_hdl,
    aip        = log(trigliceridos / colesterol_hdl),
    tyg        = log((trigliceridos * glucosa) / 2),
    pcr_us_hdl = pcr_us / colesterol_hdl
  )

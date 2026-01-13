library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(gtsummary)
library(factoextra)
library(caret)
library(gt)
library(MASS)
library(randomForest)
library(xgboost)
library(tibble)
library(janitor)

## ============================================================
## 1. LECTURA DE DATOS
## ============================================================


# Ajuste de nombres de  columnas #
ph1 <- read_excel("Control PH1.xlsx") |> 
  clean_names()
ph2 <- read_excel("Control PH2.xlsx") |> 
  clean_names()

names(ph1)
names(ph2)

# Unir las dos tablas de controles
controles <- bind_rows(ph1, ph2)

# Vista rápida
glimpse(controles)
View(controles)

controles |> 
  count(peticion_sistema) |>
  filter(n > 1)




controles_filtrado <- controles %>%
  group_by(peticion_sistema) %>%
  mutate(n_duplicados = n()) %>%
  ungroup() %>%
  filter(!(n_duplicados > 1 & diagnostico == "No aplica")) %>%
  distinct(peticion_sistema, .keep_all = TRUE) %>%
  dplyr::select(-n_duplicados)

controles_final <- controles_filtrado
View(controles_final)

# 1.2 Tabla de biomarcadores
hbio <- read_excel("Tabla HBio.xlsx") |>
  clean_names()

# Asumimos primera columna = peticion_sistema
hbio <- hbio |>
  rename(peticion_sistema = 1)

## ============================================================
## 2. LIMPIEZA, PREPARACION DE DATOS Y CREACION DE PCA
## ============================================================

# Limpiar edad a número
hbio <- hbio |>
  mutate(
    edad_num = str_extract(edad_del_paciente, "\\d+"),
    edad_num = as.integer(edad_num)
  )

# Revisar duplicados en biomarcadores
hbio |> count(peticion_sistema) |> filter(n > 1)

hbio_unica <- hbio |>
  group_by(peticion_sistema) |>
  dplyr::slice(1) |>
  ungroup()

# Union de Tablas
datos_ml <- controles_final |>
  inner_join(hbio_unica, by = "peticion_modulab")

glimpse(datos_ml)



# Comparar edades
datos_ml |>
  mutate(dif_edad = edad - edad_num) |>
  count(dif_edad)

# Eliminar columnas de edad duplicadas
datos_ml <- datos_ml |>
  dplyr::select(-edad_num, -edad_del_paciente)

View(datos_ml) 

colnames(datos_ml)

#Seleccion de columnas Para el trabajo a desarrollar

datos_filtrados <- datos_ml %>%
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
  )


# Renombrar columnas #
datos_filtrados <- datos_filtrados %>%
  rename(
    glucosa = glucosa_en_ayunas_suero,
    colesterol_total = colesterol_total_suero,
    colesterol_hdl = colesterol_hdl_suero,
    colesterol_ldl = colesterol_ldl_calculado_suero_2,
    trigliceridos = trigliceridos_suero,
    AST = ast_aspartato_aminotransferasa_suero,
    ALT = alt_alanina_aminotransferasa_suero,
    GGT = ggt_gammaglutamil_transferasa_suero,
    fosfatasa_alcalina = fosfatasa_alcalina_alkp_suero,
    hemoglobina = hemoglobina_hgb,
    hematocrito = hematocrito_hct,
    leucocitos = leucocitos_wbc,
    pcr_us = pcr_ultrasensible_suero
  )

View(datos_filtrados)

# 1.5 Eliminar casos incompletos
datos_completos_h <- datos_filtrados %>%
  drop_na()

# Anonimizar ID
datos_completos_h <- datos_completos_h %>%
  mutate(id_anonimo = row_number()) %>%
  dplyr::select(id_anonimo, everything(), -peticion_modulab)

# Asegurar pcr_us numérico y reemplazar "<0.4"
datos_completos_h <- datos_completos_h %>%
  mutate(
    pcr_us = as.character(pcr_us),
    pcr_us = ifelse(pcr_us == "<0.4", "0.4", pcr_us),
    pcr_us = as.numeric(pcr_us)
  )

str(datos_completos_h)


# 1.6 Crear índices derivados
datos_completos_h <- datos_completos_h %>%
  mutate(
    tg_hdl     = trigliceridos / colesterol_hdl,
    aip        = log(trigliceridos / colesterol_hdl),
    tyg        = log((trigliceridos * glucosa) / 2),
    pcr_us_hdl = pcr_us / colesterol_hdl
  )

View(datos_completos_h)

# Análisis descriptivo General#

tabla1_h <- datos_completos_h %>%
  dplyr::select(
    diagnostico, edad,
    glucosa, colesterol_total, colesterol_hdl, colesterol_ldl,
    trigliceridos, AST, ALT, GGT, fosfatasa_alcalina,
    hemoglobina, hematocrito, leucocitos, pcr_us, tg_hdl, aip,
    tyg, pcr_us_hdl
  ) %>%

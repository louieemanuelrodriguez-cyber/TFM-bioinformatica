# ============================================================
# 05_modelos_ml_hombres.R
# Modelos supervisados (cohorte masculina): LDA, SVM, RF, GBDT
# Incluye balanceo en train (upSample) + tablas de métricas
# ============================================================

library(dplyr)
library(caret)
library(MASS)
library(randomForest)
library(xgboost)
library(gtsummary)
library(gt)
library(tibble)

# Cargar datos limpios
source("scripts/01_limpieza_datos.R")

# ============================================================
# 1. Preparación del dataset para modelos (HOMBRES)
# ============================================================

clases_validas <- c("Control paciente sano", "HTA", "DM2")

datos_modelo <- datos_completos_h %>%
  filter(diagnostico %in% clases_validas) %>%
  dplyr::select(-sexo_del_paciente)

# Recodificar diagnóstico
datos_modelo <- datos_modelo %>%
  mutate(
    diagnostico = recode_factor(
      diagnostico,
      "Control paciente sano" = "Control",
      "HTA"                   = "HTA",
      "DM2"                   = "DM2"
    )
  )

# ============================================================
# 2. División train/test + balanceo SOLO en train
# ============================================================

set.seed(123)
idx <- createDataPartition(datos_modelo$diagnostico, p = 0.8, list = FALSE)
train <- datos_modelo[idx, ]
test  <- datos_modelo[-idx, ]

train_bal <- upSample(
  x     = train %>% dplyr::select(-diagnostico),
  y     = train$diagnostico,
  yname = "diagnostico"
)

# ============================================================
# 3. LDA
# ============================================================

modelo_lda <- lda(diagnostico ~ ., data = train_bal)
pred_lda <- predict(modelo_lda, newdata = test)$class
cm_lda <- confusionMatrix(pred_lda, test$diagnostico)
cm_lda

# ============================================================
# 4. SVM radial (CV)
# ============================================================

ctrl_cv <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

set.seed(123)
modelo_svm <- train(
  diagnostico ~ .,
  data = train_bal,
  method = "svmRadial",
  trControl = ctrl_cv,
  preProcess = c("center", "scale"),
  tuneLength = 5
)

pred_svm <- predict(modelo_svm, newdata = test)
cm_svm <- confusionMatrix(pred_svm, test$diagnostico)
cm_svm

# ============================================================
# 5. Random Forest (CV)
# ============================================================

set.seed(123)
modelo_rf <- train(
  diagnostico ~ .,
  data = train_bal,
  method = "rf",
  trControl = ctrl_cv,
  tuneLength = 5,
  importance = TRUE
)

pred_rf <- predict(modelo_rf, newdata = test)
cm_rf <- confusionMatrix(pred_rf, test$diagnostico)
cm_rf

var_imp_rf <- varImp(modelo_rf, scale = TRUE)
var_imp_rf

# ============================================================
# 6. GBDT (XGBoost) con validación cruzada (sin hold-out test)
# ============================================================

ctrl_cv_gbdt <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = "final",
  allowParallel = FALSE
)

xgb_grid <- expand.grid(
  nrounds = 200,
  max_depth = 4,
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

set.seed(123)
modelo_gbdt_cv <- train(
  diagnostico ~ .,
  data = train_bal,
  method = "xgbTree",
  trControl = ctrl_cv_gbdt,
  tuneGrid = xgb_grid,
  verbosity = 0
)

modelo_gbdt_cv

# Predicciones CV para la mejor configuración (la misma del grid)
pred_gbdt_df <- modelo_gbdt_cv$pred %>%
  filter(nrounds == 200, max_depth == 4, eta == 0.1)

cm_gbdt <- confusionMatrix(pred_gbdt_df$pred, pred_gbdt_df$obs)
cm_gbdt

importance_xgb <- varImp(modelo_gbdt_cv, scale = TRUE)
importance_xgb

# ============================================================
# 7. Tablas de métricas a partir de confusionMatrix (tu misma lógica)
# ============================================================

cm_to_tbl <- function(cm, titulo = "Modelo") {
  bc <- as.data.frame(cm$byClass)
  bc$Clase <- rownames(bc)
  rownames(bc) <- NULL

  bc_tabla <- bc[, c("Clase",
                     "Sensitivity",
                     "Specificity",
                     "Pos Pred Value",
                     "Neg Pred Value",
                     "Balanced Accuracy")]

  colnames(bc_tabla) <- c("Clase",
                          "Sensibilidad",
                          "Especificidad",
                          "VPP",
                          "VPN",
                          "Balanced_Accuracy")

  bc_tabla %>%
    tbl_summary(
      by = Clase,
      type = everything() ~ "continuous",
      statistic = everything() ~ "{mean}",
      digits = everything() ~ 3,
      include = c(Sensibilidad, Especificidad, VPP, VPN, Balanced_Accuracy),
      missing = "no"
    ) %>%
    bold_labels() %>%
    modify_caption(paste0("**Análisis de matriz de confusión – ", titulo, "**"))
}

tabla_cm_lda  <- cm_to_tbl(cm_lda,  "Modelo LDA")
tabla_cm_svm  <- cm_to_tbl(cm_svm,  "Modelo SVM radial")
tabla_cm_rf   <- cm_to_tbl(cm_rf,   "Modelo Random Forest")
tabla_cm_gbdt <- cm_to_tbl(cm_gbdt, "Modelo GBDT (CV)")

tabla_cm_lda
tabla_cm_svm
tabla_cm_rf
tabla_cm_gbdt

# ============================================================
# 8. Tabla comparativa final (merge)
# ============================================================

tabla_comparativa <- tbl_merge(
  tbls = list(tabla_cm_gbdt, tabla_cm_lda, tabla_cm_rf, tabla_cm_svm),
  tab_spanner = c("**GBDT**", "**LDA**", "**Random Forest**", "**SVM**")
) %>%
  modify_caption("**Comparación de métricas de rendimiento entre modelos (cohorte masculina)**")

tabla_comparativa

# ============================================================
# 9. Importancia de variables RF + XGBoost
# ============================================================

imp_rf <- var_imp_rf$importance %>%
  rownames_to_column("Variable") %>%
  as_tibble() %>%
  mutate(Importancia_RF = rowMeans(across(where(is.numeric)))) %>%
  dplyr::select(Variable, Importancia_RF)

imp_xgb <- varImp(modelo_gbdt_cv)$importance %>%
  rownames_to_column("Variable") %>%
  as_tibble() %>%
  mutate(Ganancia_XGBoost = rowMeans(across(where(is.numeric)))) %>%
  dplyr::select(Variable, Ganancia_XGBoost)

tabla_imp <- imp_rf %>%
  full_join(imp_xgb, by = "Variable") %>%
  arrange(desc(Importancia_RF)) %>%
  gt() %>%
  tab_header(title = "Importancia de variables en Random Forest y XGBoost (GBDT) – Hombres") %>%
  fmt_number(columns = c(Importancia_RF, Ganancia_XGBoost), decimals = 3)

tabla_imp


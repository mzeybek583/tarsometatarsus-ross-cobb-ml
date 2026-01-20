############################################################
#  Tarsometatarsus: Ross vs Cobb - Full ML Pipeline (ASCII ONLY)
#  - Save all figures to ./figures
#  - Save all outputs to ./outputs
#  - Normalize Turkish characters to ASCII (using Unicode escapes)
#  - NO PLOTLY: PCA saved as normal PNG (base R + ggplot2)
#  - BIGGER TEXT in figures
#  - PCA plots WITHOUT outlier marking
#  - ALL FIGURES: saved AND also shown on screen
############################################################

# -------------------------
# 0) Settings / Folders
# -------------------------
data_file <- "data.xlsx"
fig_dir   <- "figures"
out_dir   <- "outputs"

dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

options(stringsAsFactors = FALSE)

# -------------------------
# Plot text sizing (global knobs)
# -------------------------
BASE_CEX   <- 1.35
AXIS_CEX   <- 1.20
LAB_CEX    <- 1.35
MAIN_CEX   <- 1.45
LEGEND_CEX <- 1.15

GGP_BASE_SIZE  <- 15
GGP_TITLE_SIZE <- 17

# -------------------------
# Progress bar
# -------------------------
start_time <- Sys.time()
steps_total <- 7
pb <- txtProgressBar(min = 0, max = steps_total, style = 3)
step <- 0

bump_step <- function(msg) {
  cat("\n\n==============================\n", msg, "\n==============================\n")
  assign("step", get("step", envir = .GlobalEnv) + 1, envir = .GlobalEnv)
  setTxtProgressBar(get("pb", envir = .GlobalEnv), get("step", envir = .GlobalEnv))
}

# -------------------------
# Helpers: save base plots (SAVE + SHOW)
# -------------------------
save_png <- function(filename, expr, width = 2000, height = 1400, res = 150) {
  # 1) show on screen
  force(expr)
  
  # 2) save to file
  png(file.path(fig_dir, filename), width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  par(
    cex = BASE_CEX,
    cex.axis = AXIS_CEX,
    cex.lab = LAB_CEX,
    cex.main = MAIN_CEX,
    mar = c(5, 5, 4, 2) + 0.1
  )
  
  force(expr)
}

# -------------------------
# Helpers: normalize Turkish chars to ASCII (ASCII-only source via Unicode escapes)
# -------------------------
normalize_ascii <- function(x) {
  if (is.null(x)) return(x)
  x <- as.character(x)
  
  x <- gsub("\u011F", "g", x)
  x <- gsub("\u011E", "G", x)
  
  x <- gsub("\u0131", "i", x)
  x <- gsub("\u0130", "I", x)
  
  x <- gsub("\u015F", "s", x)
  x <- gsub("\u015E", "S", x)
  
  x <- gsub("\u00E7", "c", x)
  x <- gsub("\u00C7", "C", x)
  
  x <- gsub("\u00F6", "o", x)
  x <- gsub("\u00D6", "O", x)
  
  x <- gsub("\u00FC", "u", x)
  x <- gsub("\u00DC", "U", x)
  
  x
}

normalize_factor_levels <- function(f) {
  if (!is.factor(f)) return(f)
  levels(f) <- normalize_ascii(levels(f))
  f
}

# -------------------------
# ggplot base theme (bigger text)
# -------------------------
theme_base_big <- function() {
  ggplot2::theme_minimal(base_size = GGP_BASE_SIZE) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(size = GGP_TITLE_SIZE, face = "bold"),
      axis.title   = ggplot2::element_text(size = GGP_BASE_SIZE),
      axis.text    = ggplot2::element_text(size = GGP_BASE_SIZE - 1),
      legend.title = ggplot2::element_text(size = GGP_BASE_SIZE),
      legend.text  = ggplot2::element_text(size = GGP_BASE_SIZE - 1)
    )
}

############################################################
# 0. PACKAGES
############################################################
required_packages <- c(
  "readxl", "dplyr", "caret", "randomForest",
  "janitor", "ggplot2", "pROC", "corrplot",
  "rrcov", "car", "tibble"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
invisible(lapply(required_packages, library, character.only = TRUE))

set.seed(3578)

############################################################
# 1. LOAD & PREPROCESS
############################################################
bump_step("1) Load data and preprocess")

spec1 <- readxl::read_excel(data_file, sheet = "Ross") %>% dplyr::mutate(spec = "Ross")
spec2 <- readxl::read_excel(data_file, sheet = "Cobb") %>% dplyr::mutate(spec = "Cobb")

data <- dplyr::bind_rows(spec1, spec2) %>% janitor::clean_names()

# If "yon" exists, normalize and map to English "direction"
if ("yon" %in% names(data)) {
  data$yon <- normalize_ascii(data$yon)
  data <- data %>%
    dplyr::mutate(
      yon = tolower(as.character(yon)),
      yon = dplyr::case_when(
        yon %in% c("sag", "right", "r") ~ "right",
        yon %in% c("sol", "left", "l")  ~ "left",
        TRUE ~ yon
      ),
      yon = factor(yon, levels = c("right", "left"))
    )
}

# Target + ID
data <- data %>% dplyr::mutate(spec = factor(spec), id = seq_len(n()))

# Normalize ALL character columns to ASCII
char_cols <- sapply(data, is.character)
if (any(char_cols)) data[char_cols] <- lapply(data[char_cols], normalize_ascii)

# Convert remaining character columns (except spec/yon) to factor
char_cols <- sapply(data, is.character)
if ("spec" %in% names(char_cols)) char_cols["spec"] <- FALSE
if ("yon"  %in% names(char_cols)) char_cols["yon"]  <- FALSE
if (any(char_cols)) data[char_cols] <- lapply(data[char_cols], as.factor)

# Normalize factor levels (ASCII)
fac_cols <- sapply(data, is.factor)
if (any(fac_cols)) data[fac_cols] <- lapply(data[fac_cols], normalize_factor_levels)

# Report missing values BEFORE omitting
na_by_col <- colSums(is.na(data))
na_total  <- sum(is.na(data))

write.csv(
  data.frame(variable = names(na_by_col), n_missing = as.integer(na_by_col)),
  file.path(out_dir, "missing_values_by_column.csv"),
  row.names = FALSE
)

cat("\n--- MISSING VALUES (TOTAL CELLS) ---\n")
print(na_total)
cat("\n--- MISSING VALUES BY COLUMN ---\n")
print(na_by_col)

# Drop NA rows (complete-case)
n_before <- nrow(data)
data <- na.omit(data)
n_after <- nrow(data)
n_removed <- n_before - n_after

cat("\n--- COMPLETE-CASE N ---\n")
print(n_after)
cat("\n--- ROWS REMOVED (>=1 NA) ---\n")
print(n_removed)

cat("\n--- DATA STRUCTURE ---\n")
str(data)
cat("\n--- CLASS DISTRIBUTION (spec) ---\n")
print(table(data$spec))

############################################################
# 2. TRAIN/TEST + MODELS (RF, SVM, GLM)
############################################################
bump_step("2) Train/Test split and baseline models (RF, SVM, GLM)")

train_index <- caret::createDataPartition(data$spec, p = 0.7, list = FALSE)
train_data  <- data[train_index, ]
test_data   <- data[-train_index, ]

train_data$spec <- relevel(train_data$spec, ref = "Ross")
test_data$spec  <- factor(test_data$spec, levels = levels(train_data$spec))

train_control <- caret::trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter = FALSE
)

# make sure only predictors are used; keep "yon" if it exists
p <- ncol(train_data) - 2  # exclude spec + id
mtry_candidates <- unique(pmax(1, round(c(sqrt(p) - 1, sqrt(p), sqrt(p) + 1))))
tune_grid <- expand.grid(mtry = mtry_candidates)

rf_model <- caret::train(
  spec ~ . - id,
  data       = train_data,
  method     = "rf",
  trControl  = train_control,
  tuneGrid   = tune_grid,
  metric     = "ROC",
  importance = TRUE,
  ntree      = 500
)

svm_model <- caret::train(
  spec ~ . - id,
  data       = train_data,
  method     = "svmRadial",
  trControl  = train_control,
  metric     = "ROC",
  preProcess = c("center", "scale")
)

glm_model <- caret::train(
  spec ~ . - id,
  data      = train_data,
  method    = "glm",
  trControl = train_control,
  metric    = "ROC"
)

cat("\n--- RF MODEL ---\n");  print(rf_model)
cat("\n--- SVM MODEL ---\n"); print(svm_model)
cat("\n--- GLM MODEL ---\n"); print(glm_model)

############################################################
# 3. METRICS + CONFUSION MATRICES + ROC PLOTS
############################################################
bump_step("3) Evaluation (confusion matrices, ROC, metrics)")

get_metrics <- function(true, pred, model_name) {
  cm <- confusionMatrix(pred, true)
  
  acc   <- as.numeric(cm$overall["Accuracy"])
  kappa <- as.numeric(cm$overall["Kappa"])
  
  bc <- cm$byClass
  sens <- as.numeric(bc["Sensitivity"])
  spec <- as.numeric(bc["Specificity"])
  prec <- as.numeric(bc["Pos Pred Value"])
  f1   <- 2 * sens * prec / (sens + prec)
  
  data.frame(
    Model       = model_name,
    Accuracy    = round(acc, 3),
    Kappa       = round(kappa, 3),
    Sensitivity = round(sens, 3),
    Specificity = round(spec, 3),
    F1          = round(f1, 3)
  )
}

pred_rf   <- predict(rf_model,  newdata = test_data)
prob_rf   <- predict(rf_model,  newdata = test_data, type = "prob")
pred_svm  <- predict(svm_model, newdata = test_data)
prob_svm  <- predict(svm_model, newdata = test_data, type = "prob")
pred_glm  <- predict(glm_model, newdata = test_data)
prob_glm  <- predict(glm_model, newdata = test_data, type = "prob")

cat("\n--- CONFUSION MATRIX: RF ---\n");  print(confusionMatrix(pred_rf,  test_data$spec))
cat("\n--- CONFUSION MATRIX: SVM ---\n"); print(confusionMatrix(pred_svm, test_data$spec))
cat("\n--- CONFUSION MATRIX: GLM ---\n"); print(confusionMatrix(pred_glm, test_data$spec))

perf_tbl <- rbind(
  get_metrics(test_data$spec, pred_rf,  "RF_full"),
  get_metrics(test_data$spec, pred_svm, "SVM_full"),
  get_metrics(test_data$spec, pred_glm, "GLM_full")
)

cat("\n=== PERFORMANCE TABLE (FULL MODELS) ===\n")
print(perf_tbl)
write.csv(perf_tbl, file.path(out_dir, "performance_full_models.csv"), row.names = FALSE)

# ROC/AUC
positive_class <- levels(test_data$spec)[1]
roc_rf  <- pROC::roc(response = test_data$spec, predictor = prob_rf[[positive_class]])
roc_svm <- pROC::roc(response = test_data$spec, predictor = prob_svm[[positive_class]])
roc_glm <- pROC::roc(response = test_data$spec, predictor = prob_glm[[positive_class]])

save_png("roc_curve_comparison.png", {
  plot(roc_rf,  col = "blue",  main = "ROC Curve Comparison", legacy.axes = TRUE)
  lines(roc_svm, col = "red")
  lines(roc_glm, col = "green")
  legend(
    "bottomright",
    legend = c(
      sprintf("RF (AUC=%.3f)",  pROC::auc(roc_rf)),
      sprintf("SVM (AUC=%.3f)", pROC::auc(roc_svm)),
      sprintf("GLM (AUC=%.3f)", pROC::auc(roc_glm))
    ),
    col = c("blue", "red", "green"),
    lwd = 2,
    cex = LEGEND_CEX,
    bty = "n"
  )
})

############################################################
# 4. FEATURE IMPORTANCE (RF varImp + RFE)
############################################################
bump_step("4) Feature importance (RF varImp + RFE)")

rf_imp <- caret::varImp(rf_model, scale = TRUE)
rf_imp_df <- as.data.frame(rf_imp$importance) %>% tibble::rownames_to_column("variable")

if (!"Overall" %in% names(rf_imp_df)) {
  rf_imp_df <- rf_imp_df %>%
    dplyr::mutate(Overall = rowMeans(dplyr::across(where(is.numeric)), na.rm = TRUE))
}
rf_imp_df <- rf_imp_df %>% dplyr::arrange(dplyr::desc(Overall))

write.csv(rf_imp_df, file.path(out_dir, "variable_importance_rf.csv"), row.names = FALSE)

g_varimp <- ggplot(rf_imp_df, aes(x = reorder(variable, Overall), y = Overall)) +
  geom_point(size = 2.6) +
  coord_flip() +
  theme_base_big() +
  labs(title = "RF Variable Importance", x = "Predictor", y = "Importance")

print(g_varimp)
ggsave(filename = file.path(fig_dir, "rf_variable_importance.png"),
       plot = g_varimp, width = 10, height = 7, dpi = 150)

predictors <- train_data %>% dplyr::select(-spec, -id)
response   <- train_data$spec

n_features <- ncol(predictors)
sizes <- unique(2:n_features)

ctrl_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 10, verbose = FALSE)
rfe_res <- rfe(x = predictors, y = response, sizes = sizes, rfeControl = ctrl_rfe)

best_feats <- predictors(rfe_res)
cat("\n--- RFE SELECTED VARIABLES ---\n")
cat(paste(best_feats, collapse = ", "), "\n")
cat("\n--- RFE SELECTED VARIABLES ---\n")
cat(paste(best_feats, collapse = "\n"), "\n")

write.csv(data.frame(best_feats = best_feats),
          file.path(out_dir, "rfe_selected_predictors.csv"),
          row.names = FALSE)

save_png("rfe_performance.png", { plot(rfe_res, type = c("g", "o")) })

############################################################
# 5. CORRELATION + PCA (PNG) + ROBUST PCA (NO OUTLIERS)
############################################################
bump_step("5) Correlation, PCA (PNG), robust PCA (no outliers)")

# Correlation matrix (numeric only, excluding id)
rm_id <- data[, !(names(data) %in% c("id")), drop = FALSE]
num_cols_all <- sapply(rm_id, is.numeric)

corr_matrix <- cor(
  rm_id[, num_cols_all, drop = FALSE],
  use = "pairwise.complete.obs",
  method = "pearson"
)

save_png("correlation_matrix_corrplot.png", {
  corrplot::corrplot(
    corr_matrix,
    method = "color",
    tl.cex = 1.05,
    cl.cex = 1.05
  )
})

# PCA features (numeric only, exclude id)
features <- names(data)[sapply(data, is.numeric) & !(names(data) %in% c("id"))]
X <- data[, features, drop = FALSE]

pca_res <- prcomp(X, center = TRUE, scale. = TRUE)

scores <- as.data.frame(pca_res$x)
scores$id   <- data$id
scores$spec <- data$spec
if ("yon" %in% names(data)) scores$yon <- data$yon

# Save PCA loadings + variance explained
loadings <- as.data.frame(pca_res$rotation)
loadings$variable <- rownames(loadings)
write.csv(loadings, file.path(out_dir, "pca_loadings.csv"), row.names = FALSE)

ve <- pca_res$sdev^2 / sum(pca_res$sdev^2)
ve_df <- data.frame(
  PC = paste0("PC", seq_along(ve)),
  Variance = ve,
  CumVariance = cumsum(ve)
)
write.csv(ve_df, file.path(out_dir, "pca_variance_explained.csv"), row.names = FALSE)

# Base R PCA plot (PC1 vs PC2)
save_png("pca_pc1_pc2_baseR.png", {
  cols <- as.integer(scores$spec)
  
  plot(
    scores$PC1, scores$PC2,
    pch = 16,
    col = cols,
    xlab = "PC1",
    ylab = "PC2",
    main = "PCA: PC1 vs PC2"
  )
  
  legend(
    "topright",
    legend = levels(scores$spec),
    col = seq_along(levels(scores$spec)),
    pch = 16,
    bty = "n",
    cex = LEGEND_CEX
  )
})

# ggplot PCA + ellipse
g_pca <- ggplot(scores, aes(x = PC1, y = PC2, color = spec)) +
  geom_point(size = 2.8, alpha = 0.9) +
  stat_ellipse(level = 0.95, linewidth = 0.9) +
  theme_base_big() +
  labs(title = "PCA: PC1 vs PC2", x = "PC1", y = "PC2")

print(g_pca)
ggsave(
  filename = file.path(fig_dir, "pca_pc1_pc2_ggplot.png"),
  plot = g_pca,
  width = 10, height = 7, dpi = 150
)

# Scree plot (first 10 PCs)
g_scree <- ggplot(ve_df[1:min(10, nrow(ve_df)), ], aes(x = PC, y = Variance)) +
  geom_point(size = 2.8) +
  theme_base_big() +
  labs(title = "PCA Scree Plot (First 10 PCs)", x = "Principal Component", y = "Proportion of Variance")

print(g_scree)
ggsave(
  filename = file.path(fig_dir, "pca_scree_plot.png"),
  plot = g_scree,
  width = 10, height = 7, dpi = 150
)

# Robust PCA (Hubert) - plot only
rpca_res <- rrcov::PcaHubert(X, k = min(5, ncol(X)), scale = TRUE)
rpca_scores <- as.data.frame(rpca_res@scores)
rpca_scores$spec <- data$spec

g_rpca <- ggplot(rpca_scores, aes(x = PC1, y = PC2, color = spec)) +
  geom_point(size = 2.8, alpha = 0.9) +
  stat_ellipse(level = 0.95, linewidth = 0.9) +
  theme_base_big() +
  labs(title = "Robust PCA (Hubert): PC1 vs PC2", x = "PC1", y = "PC2")

print(g_rpca)
ggsave(
  filename = file.path(fig_dir, "robust_pca_hubert_pc1_pc2.png"),
  plot = g_rpca,
  width = 10, height = 7, dpi = 150
)

############################################################
# 6. REMOVE HIGH CORRELATION FEATURES AND RETRAIN
############################################################
bump_step("6) Remove highly correlated features and retrain")

num_cols_train <- sapply(train_data, is.numeric)
corr_mat_train <- cor(train_data[, num_cols_train, drop = FALSE], use = "pairwise.complete.obs")
high_corr_idx  <- caret::findCorrelation(corr_mat_train, cutoff = 0.9)
high_corr_names <- colnames(corr_mat_train)[high_corr_idx]

write.csv(data.frame(removed_features = high_corr_names),
          file.path(out_dir, "high_correlation_removed_features.csv"),
          row.names = FALSE)

train_red <- train_data %>% dplyr::select(-all_of(high_corr_names))
test_red  <- test_data  %>% dplyr::select(-all_of(high_corr_names))

rf_red <- caret::train(
  spec ~ . - id,
  data       = train_red,
  method     = "rf",
  trControl  = train_control,
  metric     = "ROC",
  importance = TRUE,
  ntree      = 500
)

glm_red <- caret::train(
  spec ~ . - id,
  data      = train_red,
  method    = "glm",
  trControl = train_control,
  metric    = "ROC"
)

pred_rf_red  <- predict(rf_red,  newdata = test_red)
pred_glm_red <- predict(glm_red, newdata = test_red)

############################################################
# 7. MODEL SUMMARY + BEST MODEL + VIF
############################################################
bump_step("7) Model summary, best model, VIF")

perf_tbl_all <- perf_tbl

# Feature-selected models
train_fs <- train_data %>% dplyr::select(all_of(c(best_feats, "spec", "id")))
test_fs  <- test_data  %>% dplyr::select(all_of(c(best_feats, "spec", "id")))

rf_fs <- caret::train(
  spec ~ . - id,
  data       = train_fs,
  method     = "rf",
  trControl  = train_control,
  metric     = "ROC",
  importance = TRUE,
  ntree      = 500
)

svm_fs <- caret::train(
  spec ~ . - id,
  data       = train_fs,
  method     = "svmRadial",
  trControl  = train_control,
  metric     = "ROC",
  preProcess = c("center", "scale")
)

glm_fs <- caret::train(
  spec ~ . - id,
  data      = train_fs,
  method    = "glm",
  trControl = train_control,
  metric    = "ROC"
)

pred_rf_fs  <- predict(rf_fs,  newdata = test_fs)
pred_svm_fs <- predict(svm_fs, newdata = test_fs)
pred_glm_fs <- predict(glm_fs, newdata = test_fs)

perf_tbl_all <- rbind(
  perf_tbl_all,
  get_metrics(test_fs$spec, pred_rf_fs,  "RF_FS"),
  get_metrics(test_fs$spec, pred_svm_fs, "SVM_FS"),
  get_metrics(test_fs$spec, pred_glm_fs, "GLM_FS"),
  get_metrics(test_red$spec, pred_rf_red,  "RF_Reduced"),
  get_metrics(test_red$spec, pred_glm_red, "GLM_Reduced")
)

perf_tbl_sorted <- perf_tbl_all[order(-perf_tbl_all$Accuracy), ]
write.csv(perf_tbl_sorted, file.path(out_dir, "model_performance_summary.csv"), row.names = FALSE)

best_model_name <- perf_tbl_sorted$Model[1]
cat("\nBest model by Accuracy:", best_model_name, "\n")

# VIF (glm may warn on separation; still export what is computed)
glm_for_vif <- suppressWarnings(glm(spec ~ . - id, data = train_data, family = binomial))
vif_vals <- car::vif(glm_for_vif)

write.csv(data.frame(variable = names(vif_vals), VIF = as.numeric(vif_vals)),
          file.path(out_dir, "glm_vif_full_features.csv"),
          row.names = FALSE)

# Finish
close(pb)
end_time <- Sys.time()

cat("\nTotal runtime:",
    round(difftime(end_time, start_time, units = "secs"), 1),
    "seconds\n")
cat("Figures folder:", normalizePath(fig_dir), "\n")
cat("Outputs folder:", normalizePath(out_dir), "\n")

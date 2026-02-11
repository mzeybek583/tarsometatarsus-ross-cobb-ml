# Tarsometatarsus: Ross vs Cobb — Reproducible ML Pipeline (R)

End-to-end, **fully reproducible** machine-learning workflow in **R** for classifying tarsometatarsus specimens (**Ross vs Cobb**) using morphometric measurements stored in an Excel file.

This repository is designed for **publication-ready** analysis: it automatically generates model outputs, diagnostic plots, feature-importance summaries, PCA/robust PCA figures, and exports all intermediate results as CSV files.

---

## Highlights

- **One-command execution**: run the entire workflow with `Rscript pipeline.R`
- **Automatic folder structure**:
  - figures saved to `./figures`
  - tables / model outputs saved to `./outputs`
- **ASCII-only safe**: Turkish characters are normalized to ASCII (Unicode escapes) for cross-platform consistency
- **Readable figures**: larger fonts for both base R and ggplot2
- **Plots are shown and saved**: all figures are printed to the screen and exported as PNG
- **No Plotly**: PCA and all visuals are generated as standard PNG (base R + ggplot2)
- **Model suite** (caret):
  - Random Forest (RF)
  - SVM (radial kernel)
  - Logistic regression (GLM)
- **Explainability and robustness**:
  - RF variable importance (class-averaged)
  - Recursive Feature Elimination (RFE)
  - Correlation analysis + correlation-based feature removal (`findCorrelation`)
  - Classical PCA + Robust PCA (Hubert; `PcaHubert`)
  - VIF diagnostics for multicollinearity (GLM)

---

## Repository Structure
.
├─ pipeline.R
├─ data/
│ └─ data.xlsx
├─ figures/
└─ outputs/


---

## Input Data

Place your Excel file at:

- `data/data.xlsx`  *(recommended)*  
  or rename and keep as `data.xlsx` in the repository root (default in the script).

### Required Sheets

The Excel file must include **two sheets**:

- `Ross`
- `Cobb`

### Required Columns (typical morphometrics)

Your sheets should include numeric measurements such as:

- `gl, ab, ac, aj, bp, bd, sc, bmet, bmit, blat`

Optional column:

- `yon` (direction/side), which will be normalized to:
  - `right`
  - `left`

> Column names are automatically standardized using `janitor::clean_names()`.

---

## How to Run

From the repository root:

```bash
Rscript pipeline.R


The script will:

Load and preprocess data (ASCII normalization, NA handling)

Split into train/test sets (stratified)

Train RF / SVM / GLM models (10-fold CV, ROC metric)

Evaluate performance (confusion matrices, ROC curves, metrics table)

Compute RF variable importance + run RFE

Generate correlation matrix + PCA + Robust PCA (Hubert)

Remove highly correlated predictors and retrain reduced models

Export VIF diagnostics for the full GLM model

Outputs
Figures (./figures)

Common outputs include:

roc_curve_comparison.png

rf_variable_importance.png

rfe_performance.png

correlation_matrix_corrplot.png

pca_pc1_pc2_baseR.png

pca_pc1_pc2_ggplot.png

pca_scree_plot.png

robust_pca_hubert_pc1_pc2.png


Tables / Artifacts (./outputs)

Common outputs include:

missing_values_by_column.csv

performance_full_models.csv

model_performance_summary.csv

variable_importance_rf.csv

rfe_selected_predictors.csv

high_correlation_removed_features.csv

pca_loadings.csv

pca_variance_explained.csv

glm_vif_full_features.csv


Reproducibility

Random seed is fixed in the script: set.seed(3578)

CV configuration:

10-fold cross-validation

ROC-based optimization (for probabilistic models)

All preprocessing and transformations are embedded within pipeline.R


Requirements
R packages (auto-installed if missing)

readxl, dplyr, caret, randomForest, janitor, ggplot2,
pROC, corrplot, rrcov, car, tibble

Suggested R version

R ≥ 4.1 recommended


Citation

If you use this codebase in academic work, please cite it as software:

Aydoğdu, S., Kök, R. R., Zeybek, M., & Eken, E. (2026). Determining Morphometric Differences in Domestic Fowl (Gallus gallus domesticus L. 1758) Tarsometatarsus Using Artificial Intelligence. Animals, 16(4), 530. https://doi.org/10.3390/ani16040530

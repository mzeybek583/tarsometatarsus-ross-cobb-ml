# Tarsometatarsus: Ross vs Cobb - Full ML Pipeline (ASCII ONLY)

This repository contains an end-to-end ML pipeline in R to classify specimens (Ross vs Cobb) using measurements from an Excel file (`data.xlsx`).

Key features:
- Saves all figures to `./figures`
- Saves all outputs (CSVs) to `./outputs`
- Normalizes Turkish characters to ASCII (Unicode escapes)
- No Plotly (PCA saved as PNG via base R + ggplot2)

## Input data
Place your Excel file here:

- `data/sampledata.xlsx`

Expected sheets:
- `Ross`
- `Cobb`

## How to run
From the repo root:

```bash
Rscript pipeline.R



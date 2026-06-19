# ML Project: Circular Economy & Taiwan's Economy

**A Machine Learning Approach for Investigating the Impact of Circular Economy on Taiwan's Economy and the Environment**

[繁體中文版](README.zh-TW.md)

Final project · Feb 2025 – Jun 2025

---

## Overview

This project examines how circular economy indicators affect Taiwan's economic performance and environmental outcomes using machine learning methods. The analysis covers county-level panel data and compares multiple ML models against traditional panel regression baselines.

---

## Methods

| Model | Use |
|---|---|
| LightGBM | Main ML model |
| XGBoost | Comparison |
| CatBoost | Comparison |
| Random Forest | Comparison |
| Panel Regression (demeaned) | Baseline |

---

## Repository Structure

```
├── notebooks/
│   ├── ML.ipynb          # Main analysis notebook
│   └── MMA.ipynb         # Additional analysis
├── scripts/
│   └── ML_Project.R      # R script for panel regression
├── data/
│   ├── raw/              # Original data sources
│   └── processed/        # Cleaned and merged datasets
├── results/
│   ├── Result.csv        # Model performance summary
│   └── figures/          # Output plots
└── docs/
    ├── Project.pdf        # Full report
    ├── ppt.pdf            # Mid-term presentation
    └── final_ppt.pdf      # Final presentation
```

---

## Data Sources

Raw data in `data/raw/` includes electricity generation, population, vacancy rates, food waste, tax revenue, and renewable energy capacity statistics sourced from Taiwan government open data portals.

---

## Author

Yuze Tsai · [GitHub](https://github.com/YuzeTsai)

---

## License

Code in this repository is licensed under the [MIT License](LICENSE).  
Data files are sourced from Taiwan government open data and subject to their respective terms.

# ML Project: Circular Economy & Taiwan

**A Machine Learning Approach for Investigating the Impact of Circular Economy on Taiwan's Economy and the Environment**

[繁體中文版](README.zh-TW.md) · Yuze Tsai · May 2025

---

## Abstract

This study investigates how circular economy practices affect Taiwan's economic development and environmental performance, combining traditional econometric methods with machine learning. Using a self-constructed panel dataset covering **22 cities/counties from 2014 to 2023** (220 observations, 19 variables), we examine three outcome variables — household income, per capita electricity usage, and housing vacancy rates — against key circular economy indicators. SHAP values are used for model interpretation and feature importance analysis. Results show that **ML models consistently outperform panel regression**, and that environmental budget allocation and renewable energy capacity are the most impactful features.

---

## Research Design

### Dependent Variables
| Variable | Unit | Description |
|---|---|---|
| Household Income | Thousand NTD | Overall economic output per administrative region |
| Per Capita Electricity Usage | kWh/person | Proxy for energy efficiency at the local level |
| Vacancy Rate | % | Proxy for housing market dynamics and regional conditions |

### Independent Variables (Circular Economy Indicators)
- General waste recycling rate (%)
- Budget structure ratio for environmental protection (%)
- Proportion of environmental fines (%)
- Per capita incinerated / landfilled waste (kg)
- Per capita resource recovery by local authorities (kg)
- Per capita generation of general waste (kg)
- Per capita installed capacity of renewable energy (MW)
- Per capita food waste (kg)

### Data Sources
- **Economic & energy data**: Taiwan Power Company
- **Environmental policy & pollution data**: Directorate-General of Budget, Accounting and Statistics (DGBAS), Executive Yuan, R.O.C.

---

## Methods

### Linear Baseline: Panel Data Regression
- Fixed Effects (FE) and Random Effects (RE) models
- Hausman test to select between FE and RE
- FE model preferred as primary specification

### Non-Linear ML Models
| Model | Notes |
|---|---|
| XGBoost | Sequential boosting, L1/L2 regularization |
| LightGBM | Leaf-wise tree growth, memory-efficient |
| CatBoost | Ordered boosting, handles categorical features |
| Random Forest | Bagging ensemble, parallel tree construction |
| SVM | Dropped — negative R², poor compatibility with this dataset |

All non-linear models use **10-fold cross-validation** to address overfitting on the small dataset.

### Interpretability: SHAP
SHAP (SHapley Additive exPlanations) values are computed for all four ensemble models to assess feature importance direction and magnitude across individual predictions.

---

## Key Results

### Model Performance (R²)

| Outcome | XGBoost | Random Forest | LightGBM | CatBoost | FE Regression |
|---|---|---|---|---|---|
| Household Income | 0.720 | 0.688 | 0.699 | **0.752** | 0.578 |
| Per Capita Electricity | 0.427 | **0.479** | 0.474 | 0.420 | 0.054 |
| Vacancy Rate | 0.736 | 0.659 | 0.600 | **0.743** | 0.380 |

- **CatBoost** leads on income and vacancy rate; FE regression is substantially weaker, especially for electricity usage
- Electricity usage is the hardest to predict across all models (R² < 0.5), likely due to unobserved behavioral and infrastructure factors

### Key Feature Findings (SHAP)
- **Budget structure ratio for environmental**: most consistently positive feature across all models for both income and electricity usage
- **Per capita food waste**: strong negative impact on household income, positive on electricity usage — contradicts initial expectations
- **Per capita renewable energy capacity**: slight positive effect on income but inconsistent for electricity efficiency

### Hypotheses Evaluation
| Hypothesis | Statement | Status |
|---|---|---|
| H1 | Environmental budgets & recycling → better electricity efficiency | **Inversely supported** |
| H2 | High landfill/incineration → lower household income | Partially supported |
| H3 | Food waste & general waste positively linked to income and electricity usage | Partially supported |
| H4 | Renewable energy capacity → higher income and electricity efficiency | Partially supported |

---

## Repository Structure

```
├── notebooks/
│   ├── ML.ipynb          # Main analysis (Python: all ML models + SHAP)
│   └── MMA.ipynb         # Additional analysis
├── scripts/
│   └── ML_Project.R      # Panel regression (Fixed Effects / Random Effects)
├── data/
│   ├── raw/              # Original county-level CSV data (2014–2023)
│   └── processed/        # Cleaned and merged panel dataset
├── results/
│   ├── Result.csv        # Model performance summary
│   └── figures/          # SHAP plots, feature importance, correlation heatmap
└── docs/
    ├── Project.pdf        # Full report (27 pages)
    ├── ppt.pdf            # Mid-term presentation
    └── final_ppt.pdf      # Final presentation
```

---

## Future Directions
- Expand CE indicators (Circular Material Use Rate, Secondary Raw Materials Usage)
- Extend outcome variables to CO₂ emissions, Air Quality Index, housing prices
- Apply Regression Discontinuity Design (RDD) on environmental budget threshold
- Explore Environmental Kuznets Curve implications

---

## Author

Yuze Tsai · National Taiwan University, Department of Economics  
[GitHub](https://github.com/YuzeTsai)

---

## License

Code in this repository is licensed under the [MIT License](LICENSE).  
Data files are sourced from Taiwan government open data portals and subject to their respective terms.

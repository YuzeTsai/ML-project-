# ML 專案：循環經濟與台灣經濟

**以機器學習方法探討循環經濟對台灣經濟與環境的影響**

[English](README.md)

期末專案 · 2025 年 2 月 – 6 月

---

## 研究概述

本研究利用機器學習方法，分析循環經濟相關指標對台灣各縣市經濟表現與環境成果的影響。以縣市層級的 panel data 進行分析，並以傳統 panel 迴歸作為 baseline 比較多個 ML 模型的預測效果。

---

## 使用方法

| 模型 | 用途 |
|---|---|
| LightGBM | 主要 ML 模型 |
| XGBoost | 比較模型 |
| CatBoost | 比較模型 |
| Random Forest | 比較模型 |
| Panel Regression（去均值） | Baseline |

---

## 資料夾結構

```
├── notebooks/
│   ├── ML.ipynb          # 主要分析 notebook
│   └── MMA.ipynb         # 補充分析
├── scripts/
│   └── ML_Project.R      # Panel 迴歸 R 腳本
├── data/
│   ├── raw/              # 原始資料
│   └── processed/        # 清理與合併後的資料集
├── results/
│   ├── Result.csv        # 模型效能比較
│   └── figures/          # 輸出圖表
└── docs/
    ├── Project.pdf        # 完整報告
    ├── ppt.pdf            # 期中簡報
    └── final_ppt.pdf      # 期末簡報
```

---

## 資料來源

`data/raw/` 中的原始資料包含電力生產、人口、空屋率、廚餘、稅收及再生能源裝置容量等統計資料，來源為台灣政府開放資料平台。

---

## 作者

蔡佑澤 (Yuze Tsai) · [GitHub](https://github.com/YuzeTsai)

---

## 授權

本 repo 中的程式碼採 [MIT License](LICENSE) 授權。  
資料檔案來自台灣政府開放資料，適用其各自之授權條款。

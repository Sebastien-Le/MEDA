# MEDA — Multivariate Exploratory Data Analysis

MEDA is a jamovi module for exploratory multivariate data analysis. It provides guided analyses, interpretable outputs, graphical representations, optional clustering, and reproducible R code intended for use in the jamovi Rj Editor.

## Analyses

### Multivariate Exploratory Analysis
- PCA — Principal Component Analysis for quantitative data
- CA — Correspondence Analysis for contingency tables
- MCA — Multiple Correspondence Analysis for categorical data
- MFA — Multiple Factor Analysis for data organized into groups of variables

### Clustering
- HCPC — Hierarchical Clustering on Principal Components, available within the main multivariate analyses to identify and characterize groups of observations

### Textual Data
- Textual Analysis — exploratory analysis of words associated with the categories of a qualitative variable

## Statistical foundations

MEDA relies primarily on the R package **FactoMineR**. The module is designed to make exploratory multivariate analysis accessible while keeping the statistical workflow transparent: analyses include methodological guidance and, where applicable, R code reproducing the calculations and graphics.

The different methods share a common framework for exploring multidimensional structures, interpreting dimensions, representing observations and variables, and complementing factorial analyses with clustering when appropriate.

## Example data

The module includes example datasets for PCA, CA, MCA, MFA, clustering, and textual analysis.

## Development

Source repository: https://github.com/Sebastien-Le/MEDA

Please report issues at: https://github.com/Sebastien-Le/MEDA/issues

## License

MEDA is distributed under **GPL (>= 2)**.

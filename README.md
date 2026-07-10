![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![GLM](https://img.shields.io/badge/Model-Poisson_GLM-blue?style=for-the-badge)
![Statistics](https://img.shields.io/badge/Statistics-Insurance-green?style=for-the-badge)
![GitHub](https://img.shields.io/badge/GitHub-Repository-black?style=for-the-badge&logo=github)# Motor Insurance Claim Frequency Prediction using Poisson GLM

## 📌 Project Overview
This project develops a Poisson Generalized Linear Model (GLM) in R to predict motor insurance claim frequency using real-world insurance policy data. The objective is to identify the key factors influencing claim frequency and demonstrate how statistical modelling can support insurance pricing and risk assessment. The project covers the complete analytical workflow, including data preprocessing, exploratory data analysis, feature selection, model development, evaluation, and interpretation of results.


## 🎯 Business Problem
Motor insurance companies need accurate estimates of claim frequency to price policies fairly and manage risk effectively. This project aims to develop a statistical model that predicts the expected number of insurance claims based on driver, vehicle, and policy characteristics, enabling more informed underwriting and pricing decisions.


## 📂 Dataset

This project uses the **French Motor Third-Party Liability Claims** dataset to model motor insurance claim frequency.

**Dataset Files**
- `freMTPL2freq.csv`
- `freMTPL2sev.csv`

**Source**
- OpenML – French Motor Third-Party Liability Claims Dataset
- Downloaded via Kaggle

### Variables

**Target Variable**
- ClaimNb

**Predictor Variables**
- DrivAge
- VehAge
- BonusMalus
- VehPower
- VehGas
- VehBrand
- Area
- Region
- Density

## 🛠️ Tools & Technologies
- **Programming Language:** R
- **Statistical Modelling:** Poisson Generalized Linear Model (GLM)
- **Libraries:** ggplot2, dplyr
- **Data Analysis:** Exploratory Data Analysis (EDA)
- **Model Evaluation:** Deviance Analysis, AIC Comparison, Residual Diagnostics
- **Version Control:** Git & GitHub


## 📈 Methodology
1. Imported and explored the insurance dataset.
2. Performed data cleaning and preprocessing.
3. Conducted Exploratory Data Analysis (EDA).
4. Visualized important variables using ggplot2.
5. Developed a Poisson Generalized Linear Model.
6. Evaluated predictor significance using Analysis of Deviance.
7. Compared full and reduced models using AIC and Likelihood Ratio Tests.
8. Performed residual diagnostics to assess model adequacy.
9. Interpreted results for insurance pricing and risk assessment.


## 🤖 Model Development
A Poisson GLM with a log link function was developed to model insurance claim frequency. Significant predictors such as Driver Age, Vehicle Age, Bonus-Malus, Fuel Type, Vehicle Brand, Area, and Region were retained after statistical testing. Model refinement was performed using Analysis of Deviance and AIC comparison to obtain a parsimonious model without sacrificing predictive performance.


## 📊 Model Evaluation
The model was evaluated using multiple statistical techniques including:

- Analysis of Deviance
- Akaike Information Criterion (AIC)
- Likelihood Ratio Tests
- Residual Deviance
- Pearson Residual Diagnostics

The reduced model achieved comparable predictive performance while using fewer explanatory variables, resulting in a simpler and more interpretable model.


## 🔍 Key Findings
- Bonus-Malus score was the strongest predictor of claim frequency.
- Driver Age and Vehicle Age significantly influenced insurance claims.
- Vehicle Brand, Region, and Geographic Area showed meaningful regional variations in claim frequency.
- Vehicle Power and Population Density did not significantly improve model performance.
- The reduced Poisson GLM provided an efficient balance between model simplicity and predictive accuracy.



## 📷 Visualizations

### Distribution of Vehicle Age
![Distribution of Vehicle Age](images/Distribution%20of%20Vehicle%20age.png)

---

### Distribution of BonusMalus
![Distribution of BonusMalus](images/Distribution%20of%20BonusMalus.png)

---

### Average Claim Frequency by Driver Age Group
![Average Claim Frequency by Driver Age Group](images/Average%20claim%20frequency%20by%20driver%20age%20grp.png)

---

### Average Claim Frequency by BonusMalus Group
![Average Claim Frequency by BonusMalus Group](images/Average%20claim%20frequency%20by%20BonusMalus.png)

---

### Residuals vs Fitted Plot
![Residuals vs Fitted Plot](images/Residuals%20vs%20Fitted%20plot.png)


## 📁 Repository Structure
```
Motor-Insurance-Pricing-GLM
│
├── README.md
├── Motor.insurance.project.R
├── Average claim frequency by BonusMalus.png
├── Average claim frequency by driver age grp.png
├── Distribution of BonusMalus.png
├── Distribution of Vehicle age.png
└── Residuals vs Fitted plot.png


## 🚀 Future Improvements
- Extend the analysis using Negative Binomial Regression to address overdispersion.
- Develop a complete insurance pricing framework by incorporating claim severity modelling.
- Compare multiple machine learning models for claim frequency prediction.
- Build an interactive dashboard to visualize claim risk across customer segments.
- Deploy the model as a web application for real-time prediction.

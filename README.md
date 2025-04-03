# **Ranking the Importance of Application Features in Case Success Prediction**

**[ENG]**

This repository contains the final project for a course organized by the **Institute of Mathematics and Informatics, University of Wrocław** in collaboration with **KRUK SA**. 

## **Project Description**

This project analyzes the importance of application features in predicting the likelihood of debt repayment for cases purchased by **KRUK SA**, using various statistical and machine learning models.

The **success rate** of a case is defined as a combination of two binary (0-1) success metrics, where **1** means the case repays the debt and **0** means it does not.

The first success metric is based on the percentage of the initial debt that has been repaid:

$$
Suc_1 = 
\begin{cases} 
1 & \text{if } \frac{\text{Sum of Payments}}{\text{Initial Debt}} \geq 0.05 \\ 
0 & \text{otherwise} 
\end{cases}
$$

The second success metric is based on the number of payments made:

$$
Suc_2 = 
\begin{cases} 
1 & \text{if Number of Payments} \geq 2 \\ 
0 & \text{otherwise} 
\end{cases}
$$

The final success rate of a case is determined as follows:

$$
Suc = 
\begin{cases} 
Suc_2 & \text{if } Suc_2 > Suc_1 \\ 
Suc_1 & \text{if } Suc_2 \leq Suc_1 
\end{cases}
$$

## **Project Objectives**

- Rank application features to identify the most important ones for predicting case success.
- Compare various approaches for analyzing feature importance.

## **Dataset Description**

The study is based on a dataset of **113,229 cases**, each described by **19 attributes**.

The dataset includes features categorized into several groups:

- **Debt-related variables**: `LoanAmount`, `TOA`, `Principal`, `Interest`, `Other`
- **Debt collection variables**: `ExternalAgency`, `Bailiff`, `ClosedExecution`
- **Demographic and economic features**: `PopulationInCity`, `MeanSalary`, `GDPPerCapita`, `Land`, `Age`, `Gender`
- **Case activity before purchase**: `DPD`, `D_ContractDateToImportDate`, `LastPaymentAmount`, `M_LastPaymentToImportDate`

### **Correlation Baskets**

Based on **Pearson correlation**, the variables were grouped into four correlation baskets:

![Image](https://github.com/user-attachments/assets/2dd7e4b9-2517-4d16-b681-8cc473dc1d1f)

## **Ranking Rules**

- The **most important** feature is ranked **1**, while the **least important** is ranked **19**.
- Feature importance is measured by how model **error** or **AUC (Area Under Curve)** changes when a feature is **permuted** or **removed** from the test set.
- Correlation basket importance is measured similarly, except that **all variables** in a basket are permuted or removed at once.
- Importance is evaluated based on:
  - **AUC changes**
  - **Model error changes**
  - **Summary statistics** from model outputs
- The **final ranking** is calculated as the **average of all ranks** across different evaluation methods.

##  **Analysis Methods**

Feature importance is determined using:

- **Pearson & Spearman correlation analysis** – assessing relationships between features and case success rate
- **Boosting model** – evaluating the impact of feature permutation on model performance
- **KNN (k-nearest neighbors)** – removing features from test data and measuring performance impact
- **Logistic regression model** – analyzing feature significance via permutation in test data and measuring performance impact

##  **Results**

### **Top 6 Most Important Features in Predicting Case Success**

1. `M_LastPaymentToImportDate`
2. `DPD`
3. `Age`
4. `LastPaymentAmount`
5. `Other`
6. `D_ContractDateToImportDate`

### **Ranking of Correlation Baskets**

1. **Case activity & debt collection** (Basket #2)
2. **Debt components & loan amount** (Basket #1)
3. **Weakly correlated variables** (Basket #3)
4. **Informational features** (Basket #4)

## **Possible Extensions**

- **Analyzing the impact** of stepwise methods in logistic regression.
- **Comparing the effect** of feature removal vs. permutation – which method provides better insights?
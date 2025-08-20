# Women’s Education Analysis  

## 📌 Project Overview  
This project investigates the relationship between **female educational attainment** and **national well-being** across multiple countries. Using **ARIMAX time series models** and structured **panel data analysis**, the study quantifies how women’s education influences economic and social outcomes over time.  

The analysis integrates robust preprocessing, imputation, and visualization techniques to ensure accuracy and interpretability.  

---

## 🛠️ Tools & Technologies  
- **R Programming**  
- **ARIMAX** for time series forecasting with exogenous variables  
- **missForest** for missing data imputation  
- **ggplot2, tidyr, dplyr** for data wrangling and visualization  
- **tseries, forecast** for stationarity testing and time series modeling  

---

## 📊 Methodology  

### 1. Data Preprocessing  
- **Missing Value Handling**: Applied *missForest* imputation to maintain data integrity  
- **Feature Engineering**: Transformation of predictors to enhance model interpretability  
- **Stationarity Testing**: Conducted Augmented Dickey-Fuller (ADF) tests for time series readiness  

### 2. Exploratory Data Analysis (EDA)  
- **Correlation Analysis**: Identified key relationships between education and well-being indicators  
- **t-Tests**: Measured statistical differences across countries and time  
- **Visualizations**: Created overlayed bar plots and trend lines with *ggplot2* for cross-country comparison  

### 3. Modeling  
- **ARIMAX Models**: Estimated the dynamic effect of female education levels on national well-being  
- **Panel Data Modeling**: Structured country-level analysis for cross-sectional time trends  

### 4. Insights & Findings  
- Women’s educational attainment showed a **statistically significant positive effect** on well-being measures  
- Identified **key drivers** behind the education–well-being dynamic, varying across regions  
- Visual insights highlighted **long-term upward trends** linked to higher female education participation  

---

## 📂 Repository Structure  
```bash
├── README.md         # Project overview and documentation
├── scripts/          # R scripts for preprocessing, EDA, and modeling


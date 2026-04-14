# Secondary Data Analysis: National Panel Survey (NPS)

## Project Overview
This repository contains the complete analytical pipeline for my dissertation, focusing on a **Secondary Data Analysis of the National Panel Survey (NPS)**. The project is designed for full reproducibility, automating the workflow from raw data ingestion to the production of publication-ready tables.

The analysis explores the socio-economic determinants and causal relationships within the NPS dataset, specifically focusing on [Catastrophic Health Expenditure].

---

## Key Features
* **Data Ingestion:** Automated loading and merging of NPS waves.
* **Data Wrangling:** Extensive cleaning, handling of missing values, and variable recoding.
* **Statistical Modeling:** Implementation of **Multivariate Regression** models (including diagnostic checks for multicollinearity and heteroscedasticity).
* **Automated Reporting:** Direct export of regression outputs and descriptive statistics into **Microsoft Word (.docx)** in professional tabular formats.

---

## Repository Structure
```text
├── Data/                                 # Raw and processed datasets
├── Code/                                 # Analysis source code
│   ├── Dissertation Data Analysis.R     
├── Output/                               # Final tables and Word documents
├── README.md                             # Project documentation
```
---
## Technical Implementation

The pipeline is built using a modular approach to ensure each stage of the dissertation can be audited:

* **Preprocessing:** Using tidyverse to handle complex survey weights and household identifiers.

* **Modeling:** Running multivariate regressions to control for confounding variables.

    ***Exporting:** Utilizing specialized libraries (e.g., gtsummary) to convert statistical objects into formatted Word tables that meet academic standards.

---

## Getting Started
### Prerequisites

    * R
    * Required packages: here, tidyverse, haven, labelled, survey, srvyr, sjPlot, gtsummary, gt, officer, flextable

---
## Usage

    * Clone the repository: git clone [https://github.com/amrikyaruzi/National-Panel-Survey-Data-Analysis.git](https://github.com/amrikyaruzi/National-Panel-Survey-Data-Analysis.git)
    * Open the main project file and run the script

---
## License
This project is licensed under the MIT License

---
## Contact
Dr. Amri Kyaruzi Ishengoma (MD, MPH)
Email: amrikyaruzi@gmail.com
LinkedIn: https://www.linkedin.com/in/amri-kyaruzi-ishengoma

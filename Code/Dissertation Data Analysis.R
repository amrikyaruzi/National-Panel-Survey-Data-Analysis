# Load required packages
library(here)
library(tidyverse)
library(haven)
library(labelled)
library(survey)
library(srvyr)
library(sjPlot)
library(gtsummary)
library(gt)
library(officer)
library(flextable)


# Setting survey options
options(survey.lonely.psu = "adjust")


data_path <- "./Data/National Panel Survey 2020-21, Wave 5/Extracted Data/TZA_2020_NPS-R5_v02_M_STATA14/"

hh_sec_a <- read_dta(paste0(data_path, "hh_sec_a.dta"))
hh_sec_e1 <- read_dta(paste0(data_path, "hh_sec_e1.dta"))
consumption_y5 <- read_dta(paste0(data_path, "cons.dta"))
hh_sec_b <- read_dta(paste0(data_path, "hh_sec_b.dta"))
hh_sec_c <- read_dta(paste0(data_path, "hh_sec_c.dta"))

###############################################################################
# RESEARCH OBJECTIVE 1: Employment-Based Insurance Coverage
###############################################################################

cat("=== RESEARCH OBJECTIVE 1: EMPLOYMENT-BASED INSURANCE COVERAGE ===\n")

# Create comprehensive employment and insurance dataset
hh_insurance_employed <- hh_sec_e1 %>%
  group_by(y5_hhid) %>%
  summarise(
    # Core insurance measures for Objective 1
    employment_insurance = if_else(any(hh_e44e == 1, na.rm = TRUE), 1, 0),
    n_employed = sum(!is.na(hh_e44e) & hh_e44e %in% c(1, 2), na.rm = TRUE),
    n_insured_employed = sum(hh_e44e == 1, na.rm = TRUE),
    
    # Employment characteristics for detailed analysis
    has_formal_sector = if_else(any(hh_e43 %in% c(1, 2, 3, 4), na.rm = TRUE), 1, 0),
    has_government = if_else(any(hh_e29 %in% c(1, 2, 3), na.rm = TRUE), 1, 0),
    # has_government = if_else(any(hh_e43 == 1, na.rm = TRUE), 1, 0),
    has_private = if_else(any(hh_e29 %in% c(4, 5, 6, 7, 8, 9), na.rm = TRUE), 1, 0),
    n_formal_sector = sum(hh_e43 %in% c(1, 2, 3, 4), na.rm = TRUE)#,
    
    # Industry composition
    # n_agriculture = sum(hh_e46 == 1, na.rm = TRUE),
    # n_services = sum(hh_e46 %in% c(5, 6, 7, 8), na.rm = TRUE)
  ) %>%
  # CRITICAL: Filter to households with employed members only
  filter(n_employed > 0) %>%
  mutate(
    # Primary insurance variable for Objective 1
    health_insurance = factor(employment_insurance, 
                              levels = c(0, 1), 
                              labels = c("No employment insurance", "Employment insurance")),
    
    # Detailed coverage measures
    insurance_intensity = case_when(
      n_insured_employed >= 2 ~ "Multiple insured members",
      n_insured_employed == 1 ~ "Single insured member", 
      TRUE ~ "No insured members"
    ),
    
    # Employment type categories
    employment_composition = case_when(
      has_government == 1 ~ "Government employment",
      has_formal_sector == 1 ~ "Other formal sector",
      n_employed > 0 ~ "Informal employment only",
      TRUE ~ "No employment"
    ),
    
    # Coverage rate among employed members
    coverage_rate_employed = if_else(n_employed > 0, n_insured_employed / n_employed, 0)
  )

# Calculate results for Objective 1
cat("1. EMPLOYMENT-BASED HEALTH INSURANCE COVERAGE:\n")
obj1_results <- hh_insurance_employed %>%
  summarise(
    total_households_with_employment = n(),
    households_with_insurance = sum(employment_insurance),
    insurance_coverage_rate = mean(employment_insurance),
    total_employed_members = sum(n_employed),
    total_insured_members = sum(n_insured_employed),
    population_coverage_rate = total_insured_members / total_employed_members
  )

print(obj1_results)

cat("\nDetailed Coverage Breakdown:\n")
coverage_breakdown <- hh_insurance_employed %>%
  count(insurance_intensity) %>%
  mutate(percentage = n / sum(n) * 100)
print(coverage_breakdown)


###############################################################################
# RESEARCH OBJECTIVE 2: CHE Prevalence
###############################################################################

cat("\n=== RESEARCH OBJECTIVE 2: CATASTROPHIC HEALTH EXPENDITURE PREVALENCE ===\n")

# Create CHE dataset (same as before)
hh_che <- consumption_y5 %>%
  mutate(
    total_consumption = expm,
    health_spending = health,
    health_share = health_spending / total_consumption,
    che_10 = if_else(health_share > 0.10, 1, 0),
    che_25 = if_else(health_share > 0.25, 1, 0),
    total_consumption_real_pae = expmR_pae * (365/28),
    health_spending_real_pae = healthR_pae * (365/28),
    health_share_real = health_spending_real_pae / total_consumption_real_pae,
    che_10_real = if_else(health_share_real > 0.10, 1, 0)
  ) %>%
  select(
    y5_hhid,
    total_consumption, health_spending, health_share,
    total_consumption_real_pae, health_spending_real_pae, health_share_real,
    che_10, che_25, che_10_real
  ) %>%
  mutate(
    che_10 = factor(che_10, levels = c(0, 1), labels = c("No CHE", "CHE")),
    che_25 = factor(che_25, levels = c(0, 1), labels = c("No CHE", "CHE")),
    che_10_real = factor(che_10_real, levels = c(0, 1), labels = c("No CHE", "CHE"))
  )

###############################################################################
# Create Covariates Dataset
###############################################################################

# Household covariates
hh_covariates <- hh_sec_b %>%
  group_by(y5_hhid) %>%
  summarise(
    household_size = n(),
    n_children = sum(hh_b04 < 18, na.rm = TRUE),
    n_elderly = sum(hh_b04 >= 60, na.rm = TRUE),
    n_adults = sum(hh_b04 >= 18 & hh_b04 < 60, na.rm = TRUE),
    head_sex = first(hh_b02[hh_b05 == 1]),
    head_age = first(hh_b04[hh_b05 == 1])
  ) %>%
  mutate(
    head_sex = factor(head_sex, levels = c(1, 2), labels = c("Male", "Female")),
    dependency_ratio = (n_children + n_elderly) / pmax(n_adults, 1)
  )

# Head education
head_education <- hh_sec_c %>%
  left_join(hh_sec_b %>% select(y5_hhid, indidy5, hh_b05), by = c("y5_hhid", "indidy5")) %>%
  filter(hh_b05 == 1) %>%
  mutate(
    education = case_when(
      hh_c07 %in% c(1) ~ "Preschool",
      hh_c07 %in% c(11) ~ "Adult",
      hh_c07 %in% c(12:20) ~ "Primary",
      hh_c07 %in% c(2) ~ "Post Middle school course",
      hh_c07 %in% c(21:32) ~ "Secondary",
      hh_c07 %in% c(33) ~ "Post A level course",
      hh_c07 %in% c(34, 41:45) ~ "University",
      TRUE ~ "Missing"
    )
  ) %>%
  select(y5_hhid, education)

# Location data
location_data <- hh_sec_a %>%
  select(y5_hhid, urban_rural = y5_rural) %>%
  mutate(urban_rural = factor(urban_rural, 
                              levels = c(0, 1), 
                              labels = c("Urban", "Rural")))

###############################################################################
# CREATE ANALYSIS DATASET FOR ALL OBJECTIVES
###############################################################################

analysis_data <- hh_sec_a %>%
  filter(!is.na(clusterid) & !is.na(strataid)) %>%
  select(y5_hhid, clusterid, strataid, y5_crossweight) %>%
  # INNER JOIN: Keep only households with employed members
  inner_join(hh_insurance_employed, by = "y5_hhid") %>%
  # Join with CHE data for Objective 2
  left_join(hh_che, by = "y5_hhid") %>%
  # Join with covariates for Objective 3
  left_join(hh_covariates, by = "y5_hhid") %>%
  left_join(head_education, by = "y5_hhid") %>%
  left_join(location_data, by = "y5_hhid") %>%
  # Remove households with missing key variables
  filter(!is.na(health_insurance) & !is.na(che_10) & !is.na(education)) %>%
  # Create analysis variables for Objective 3
  mutate(
    consumption_quintile = cut(total_consumption_real_pae,
                               breaks = quantile(total_consumption_real_pae, 
                                                 probs = seq(0, 1, 0.2), 
                                                 na.rm = TRUE),
                               labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    has_children = if_else(n_children > 0, 1, 0),
    has_elderly = if_else(n_elderly > 0, 1, 0),
    high_dependency = if_else(dependency_ratio > 1, 1, 0),
    # Fix education variable
    education_fixed = case_when(
      education %in% c("Missing") ~ NA_character_,
      TRUE ~ as.character(education)
    ),
    education_fixed = factor(education_fixed, 
                             levels = c("Preschool", "Adult", "Primary", "Post Middle school course",
                                        "Secondary", "Post A level course", "University")),
    # Age group for household head
    age_group = cut(head_age, 
                    breaks = c(0, 35, 50, 65, 100),
                    labels = c("18-35", "36-50", "51-65", "65+"))
  )

cat("Final Analysis Dataset for All Objectives:\n")
cat("• Households with employed members:", nrow(analysis_data), "\n")
cat("• Proportion of all households:", round(nrow(analysis_data) / nrow(hh_sec_a) * 100, 1), "%\n")

###############################################################################
# SURVEY DESIGN FOR ALL ANALYSES
###############################################################################

tnps_survey <- analysis_data %>%
  as_survey_design(
    ids = clusterid,
    strata = strataid,
    weights = y5_crossweight,
    nest = TRUE
  )

###############################################################################
# FORMAL ANALYSIS FOR EACH OBJECTIVE
###############################################################################

# OBJECTIVE 1: Insurance Coverage (Weighted)
cat("\n=== FORMAL ANALYSIS: OBJECTIVE 1 - INSURANCE COVERAGE ===\n")

obj1_weighted <- tnps_survey %>%
  summarise(
    insurance_coverage = survey_mean(employment_insurance, vartype = "ci"),
    n_households = unweighted(n())
  )

cat("Weighted Results - Employment-Based Health Insurance:\n")
cat("• Coverage rate:", round(obj1_weighted$insurance_coverage * 100, 1), "%\n")
cat("• 95% CI: [", round(obj1_weighted$insurance_coverage_low * 100, 1), "%, ", 
    round(obj1_weighted$insurance_coverage_upp * 100, 1), "%]\n")
cat("• Representative of", round(sum(analysis_data$y5_crossweight)), "households nationally\n")

# OBJECTIVE 2: CHE Prevalence (Weighted)
cat("\n=== FORMAL ANALYSIS: OBJECTIVE 2 - CHE PREVALENCE ===\n")

obj2_results <- tnps_survey %>%
  summarise(
    che_10_rate = survey_mean(che_10 == "CHE", vartype = "ci"),
    che_25_rate = survey_mean(che_25 == "CHE", vartype = "ci"),
    mean_health_share = survey_mean(health_share, vartype = "ci"),
    n_households = unweighted(n())
  )

cat("Weighted Results - Catastrophic Health Expenditure:\n")
cat("• CHE (10% threshold):", round(obj2_results$che_10_rate * 100, 1), "%\n")
cat("• 95% CI: [", round(obj2_results$che_10_rate_low * 100, 1), "%, ", 
    round(obj2_results$che_10_rate_upp * 100, 1), "%]\n")
cat("• CHE (25% threshold):", round(obj2_results$che_25_rate * 100, 1), "%\n")
cat("• Mean health share:", round(obj2_results$mean_health_share * 100, 1), "%\n")

# OBJECTIVE 3: Factors Influencing CHE
cat("\n=== FORMAL ANALYSIS: OBJECTIVE 3 - FACTORS INFLUENCING CHE ===\n")

# Primary multivariate model
obj3_model <- svyglm(
  che_10 == "CHE" ~ health_insurance + education_fixed + head_sex + 
    age_group + household_size + consumption_quintile + 
    has_children + has_elderly,
  design = tnps_survey,
  family = quasibinomial()
)

cat("Multivariate Model Summary:\n")
print(summary(obj3_model))

# Odds ratios for key variables
obj3_or <- exp(cbind(OR = coef(obj3_model), confint(obj3_model)))
cat("\nAdjusted Odds Ratios for CHE:\n")
print(round(obj3_or, 3))

###############################################################################
# SECONDARY ANALYSIS: INDIVIDUAL-LEVEL INSIGHTS
###############################################################################

cat("\n=== SECONDARY ANALYSIS: INDIVIDUAL-LEVEL INSIGHTS ===\n")

# Create individual-level dataset for employed persons
individual_analysis <- hh_sec_e1 %>%
  filter(!is.na(hh_e44e)) %>%  # Employed individuals only
  select(y5_hhid, indidy5, hh_e43, hh_e44e, hh_e46) %>%
  left_join(hh_sec_b %>% select(y5_hhid, indidy5, hh_b04, hh_b02, hh_b05), 
            by = c("y5_hhid", "indidy5")) %>%
  inner_join(analysis_data %>% select(y5_hhid, che_10, clusterid, strataid, y5_crossweight),
             by = "y5_hhid") %>%
  mutate(
    individual_insured = if_else(hh_e44e == 1, 1, 0),
    formal_sector = if_else(hh_e43 %in% c(1, 2, 3, 4), 1, 0),
    is_household_head = if_else(hh_b05 == 1, 1, 0)
  )

# Individual coverage analysis
cat("Individual-Level Insurance Coverage:\n")
individual_coverage <- individual_analysis %>%
  summarise(
    n_employed_individuals = n(),
    n_insured_individuals = sum(individual_insured),
    individual_coverage_rate = mean(individual_insured),
    formal_sector_rate = mean(formal_sector)
  )
print(individual_coverage)

###############################################################################
# PUBLICATION-READY OUTPUTS
###############################################################################

cat("\n=== PUBLICATION-READY OUTPUTS ===\n")

# Table 1: Study Population Characteristics
table1_final <- tnps_survey %>%
  tbl_svysummary(
    include = c("health_insurance", "insurance_intensity", "employment_composition",
                "che_10", "education_fixed", "head_sex", "age_group", 
                "household_size", "consumption_quintile", "has_children", 
                "has_elderly", "urban_rural", "health_share"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    label = list(
      health_insurance ~ "Employment-Based Health Insurance",
      insurance_intensity ~ "Insurance Coverage Intensity", 
      employment_composition ~ "Household Employment Type",
      che_10 ~ "Catastrophic Health Expenditure (10%)",
      education_fixed ~ "Education Level",
      head_sex ~ "Sex of Household Head",
      age_group ~ "Age Group",
      household_size ~ "Household Size",
      consumption_quintile ~ "Wealth Quintile",
      has_children ~ "Has Children",
      has_elderly ~ "Has Elderly",
      urban_rural ~ "Residence",
      health_share ~ "Health Share of Consumption (%)"
    )
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 1: Characteristics of Tanzanian Households with Employed Members (N = {N})**")

# Table 2: Multivariate Analysis
table2_final <- tbl_regression(
  obj3_model,
  # add_pairwise_contrasts = TRUE,
  pairwise_reverse = FALSE,
  exponentiate = TRUE,
  label = list(
    health_insurance ~ "Employment-Based Health Insurance",
    education_fixed ~ "Education Level",
    head_sex ~ "Sex of Household Head",
    age_group ~ "Age Group",
    household_size ~ "Household Size",
    consumption_quintile ~ "Wealth Quintile",
    has_children ~ "Has Children",
    has_elderly ~ "Has Elderly"#,
    # urban_rural ~ "Residence"
  )
) %>%
  
  # add_global_p() %>%
  
  add_significance_stars(hide_p = FALSE,
                         hide_ci = FALSE,
                         hide_se = FALSE) %>%
  
  bold_p() %>%
  
  modify_caption("**Table 2: Factors Associated with Catastrophic Health Expenditure in Tanzanian Households with Employed Members**")

# Display results
cat("Table 1 - Study Population Characteristics:\n")
print(table1_final)

table1_final %>%
  as_flex_table() %>%
  fontsize(size = 12, part = "all") %>%  # Change font size to 10 for all parts of the table
  font(fontname = "Times New Roman", part = "all") %>%  # Change font family to Arial for all parts of the table

flextable::save_as_docx(.,
                        path = here("./Output/Table 1 - Study Population Characteristics.docx"))

cat("\nTable 2 - Multivariate Analysis:\n")
print(table2_final)

table2_final %>%
  as_flex_table() %>%
  fontsize(size = 12, part = "all") %>%  # Change font size to 10 for all parts of the table
  font(fontname = "Times New Roman", part = "all") %>%  # Change font family to Arial for all parts of the table
  
  flextable::save_as_docx(.,
                          path = here("./Output/Table 2 - Multivariate Analysis.docx"))

###############################################################################
# SUMMARY OF KEY FINDINGS FOR MANUSCRIPT
###############################################################################

cat("\n=== KEY FINDINGS FOR MANUSCRIPT ===\n")
cat("RESEARCH OBJECTIVE 1: Employment-Based Insurance Coverage\n")
cat("• Among households with employed members:", round(obj1_weighted$insurance_coverage * 100, 1), "% have employment-based insurance\n")
cat("• Coverage intensity:", paste0(round(coverage_breakdown$percentage, 1), "% ", coverage_breakdown$insurance_intensity, collapse = "; "), "\n")

cat("\nRESEARCH OBJECTIVE 2: CHE Prevalence\n")
cat("• CHE (10% threshold):", round(obj2_results$che_10_rate * 100, 1), "% of households experience CHE\n")
cat("• Mean health expenditure share:", round(obj2_results$mean_health_share * 100, 1), "% of total consumption\n")

cat("\nRESEARCH OBJECTIVE 3: Factors Influencing CHE\n")
cat("• Employment insurance reduces odds of CHE by", round((1 - obj3_or["health_insuranceEmployment insurance", "OR"]) * 100, 1), "%\n")
cat("• Key protective factors: Employment insurance, higher education") #, urban residence\n
cat("• Key risk factors: Larger household size, presence of children or elderly\n")

cat("\n=== ANALYSIS COMPLETED FOR ALL THREE OBJECTIVES ===\n")

# ============================================================
# Scottish Climate Survey 2024 – Data Preparation Pipeline
# Purpose: Clean, transform, validate, and export survey data
# Author: Daniils Smirnovs
# Notes:
# - Raw data is treated as read-only.
# - Script is designed to run from start to finish.
# - Outputs can be safely deleted and regenerated.
# ============================================================


# ============================================================
# 1. Load Required Libraries
# ============================================================

# Only required libraries are loaded for transparency and 
# reproducibility.
# tidyverse is used for data wrangling, readxl for Excel import, 
# stringr for pattern matching.

library(readxl)
library(tidyverse)
library(stringr)


# ============================================================
# 2. Define Column Structure (Schema Control)
# ============================================================

# Explicitly defining column names ensures:
# - Consistent schema across sheets
# - Protection against changes in Excel formatting
# - Reproducible column ordering

variables <- c(
  "response_category", 
  "overall", 
  "sex_man", 
  "sex_woman", 
  "age_16_34",
  "age_35_54",
  "age_55_69",
  "age_70_and_above",
  "age_35_and_above",
  "age_under_55",
  "area_urban",
  "area_rural",
  "area_urban_large",
  "area_urban_other",
  "area_small_towns_accessible",
  "area_small_towns_remote",
  "area_rural_accessible",
  "area_rural_remote",
  "energyhub_yes",
  "energyhub_no",
  "floodrisk_yes",
  "floodrisk_no",
  "education_no_formal",
  "education_other",
  "education_graduate",
  "education_non_graduate",
  "work_employed",
  "work_unemployed",
  "work_retired",
  "income_less_26000",
  "income_26000_52000",
  "income_less_than_52000",
  "income_52000_and_above",
  "mode_online",
  "mode_postal",
  "mode_total_unweighted",
  "mode_online_unweighted",
  "mode_postal_unweighted"
)


# ============================================================
# 3. Define Data Import & Cleaning Function
# ============================================================

# This function:
# - Imports one sheet (question) at a time
# - Removes non-analytical columns
# - Removes summary rows
# - Tags rows with question identifier
# - Returns a clean tibble
#
# Encapsulating this logic in a function:
# - Reduces duplication
# - Improves maintainability
# - Supports reproducibility

clean_scs_sheet <- function(sheet_name) {
  
  # --- Import raw sheet (read-only) ---
  scs_data_24 <- read_excel(
    "data/scottish_climate_survey_2024.xlsx",
    col_names = variables,
    skip = 15,  # Skip metadata rows
    sheet = sheet_name
  )
  
  # --- Remove unweighted mode columns (not required for analysis) ---
  scs_data_24_clean <- scs_data_24 |>
    select(-starts_with("mode"))
  
  # --- Remove empty rows ---
  scs_data_24_clean <- scs_data_24_clean |>
    filter(!is.na(response_category))
  
  # --- Keep rows only up to "Prefer not to say"
  # Assumes this is the final valid response option ---
  scs_data_24_clean <- scs_data_24_clean |>
    slice(1:(which(response_category == "Prefer not to say")[1]))
  
  # --- Add question identifier for traceability ---
  scs_data_24_clean |>
    mutate(question_number = sheet_name) |>
    select(question_number, everything())
}


# ============================================================
# 4. Apply Cleaning Function to Relevant Sheets
# ============================================================

# These sheets correspond to selected climate-related questions.
# Each sheet becomes one node in the analysis DAG.

sheets <- c("T1", "T3", "T7", "T11")

scs_data <- sheets |>
  lapply(clean_scs_sheet) |>
  bind_rows()


# ============================================================
# 5. Data Transformation (Wide → Long)
# ============================================================

# Convert demographic columns into tidy format.
# Each row now represents:
# - One question
# - One response category
# - One demographic subgroup

scs_data_long <- scs_data |>
  pivot_longer(
    cols = overall:income_52000_and_above,
    names_to = "demographic_parameter_subgroup",
    values_to = "responders_n"
  )

# Split demographic parameter from subgroup
scs_data_long <- scs_data_long |>
  separate(
    demographic_parameter_subgroup,
    c("demographic_parameter", "subgroup"),
    "_",
    extra = "merge"
  )


# ============================================================
# 6. Separate Base/Total Rows
# ============================================================

# "Base" rows contain denominators for frequency calculations.
# These are stored separately and later joined back.

scs_data_base_sub_total <- scs_data_long |>
  filter(str_detect(response_category, "^Base")) |>
  select(-response_category) |>
  rename(base_sub_total = responders_n)

# Remove Base rows from main dataset
scs_data_long <- scs_data_long |>
  filter(!str_detect(response_category, "^Base"))

# Join denominator back into dataset
scs_data_cleaned <- scs_data_long |>
  left_join(
    scs_data_base_sub_total,
    join_by(question_number, demographic_parameter, subgroup)
  )

# Remove intermediate objects to keep environment clean
rm(list = c("scs_data_long", "scs_data_base_sub_total"))


# ============================================================
# 7. Data Validation & Cleaning
# ============================================================

# Convert suppression symbols to NA
scs_data_cleaned <- scs_data_cleaned |>
  mutate(
    responders_n = na_if(responders_n, '-'),
    responders_n = na_if(responders_n, '*'),
    responders_n = na_if(responders_n, '**')
  )

# Convert numeric columns safely
scs_data_cleaned <- scs_data_cleaned |>
  mutate(
    responders_n = parse_number(responders_n),
    base_sub_total = parse_number(base_sub_total)
  )

# Calculate percentage frequency
# Assumes base_sub_total > 0
scs_data_cleaned <- scs_data_cleaned |>
  mutate(
    freq = round(responders_n / base_sub_total, 2) * 100
  )


# ============================================================
# 8. Improve Readability (Labelling)
# ============================================================

# Recode demographic parameter names for reporting clarity
scs_data_cleaned <- scs_data_cleaned |>
  mutate(
    demographic_parameter = case_when(
      demographic_parameter == "overall" ~ "Overall Population",
      demographic_parameter == "sex" ~ "Biological Sex",
      demographic_parameter == "age" ~ "Age",
      demographic_parameter == "area" ~ "Area Type (Population Density)",
      demographic_parameter == "energyhub" ~ "Energy Hub Area",
      demographic_parameter == "floodrisk" ~ "Flood Risk Area",
      demographic_parameter == "education" ~ "Education",
      demographic_parameter == "work" ~ "Working Status",
      demographic_parameter == "income" ~ "Income",
      demographic_parameter == "mode" ~ "Survey Mode Completion",
      TRUE ~ demographic_parameter
    )
  )

# Recode subgroup labels for clarity in dashboard/reporting
scs_data_cleaned <- scs_data_cleaned |> 
  mutate(
    subgroup = case_when(
      is.na(subgroup) ~ "Overall Population",
      subgroup == "man" ~ "Men", 
      subgroup == "woman" ~ "Women", 
      subgroup == "16_34" ~ "From 16 to 34",
      subgroup == "35_54" ~ "From 35 to 54", 
      subgroup == "55_69" ~ "From 55 to 69", 
      subgroup == "70_and_above" ~ "70 and above", 
      subgroup == "35_and_above" ~ "35 and above", 
      subgroup == "under_55" ~ "Under 55", 
      subgroup == "urban" ~ "Urban (All)", 
      subgroup == "rural" ~ "Rural (All)", 
      subgroup == "urban_large" ~ "Urban (Large)", 
      subgroup == "urban_other" ~ "Urban (Other)", 
      subgroup == "small_towns_accessible" ~ "Small Towns (Accessible)", 
      subgroup == "small_towns_remote" ~ "Small Towns (Remote)", 
      subgroup == "rural_accessible" ~ "Rural (Accessible)", 
      subgroup == "rural_remote" ~ "Rural (Remote)", 
      subgroup == "yes" ~ "Yes/Applicable", 
      subgroup == "no" ~ "No/Not Applicable", 
      subgroup == "no_formal" ~ "No Formal Qualifications", 
      subgroup == "other" ~ "Other Qualifications", 
      subgroup == "graduate" ~ "Degree or Higher Qualifications", 
      subgroup == "non_graduate" ~ "Non-graduate", 
      subgroup == "employed" ~ "Employed (FT or PT)", 
      subgroup == "unemployed" ~ "Unemployed", 
      subgroup == "retired" ~ "Retired", 
      subgroup == "less_26000" ~ "Less than £26,000", 
      subgroup == "26000_52000" ~ "From £26,000 to less than £52,000", 
      subgroup == "less_than_52000" ~ "Less than £52,000", 
      subgroup == "52000_and_above" ~ "£52,000 and above", 
      subgroup == "online" ~ "Online", 
      subgroup == "postal" ~ "Postal", 
      TRUE ~ subgroup
    ) 
  )


# ============================================================
# 9. Export Clean Dataset
# ============================================================

# Export final dataset for dashboard use.
# This file can be deleted and regenerated at any time
# by re-running this script end-to-end.

write_csv(
  scs_data_cleaned,
  "R/dashboard/data/scs_data_cleaned.csv"
)

# End of script

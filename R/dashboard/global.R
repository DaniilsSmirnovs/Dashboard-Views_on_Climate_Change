
# ============================================================
# Shiny Dashboard – Global Configuration & Plot Functions
# Purpose:
#   - Load cleaned survey dataset
#   - Prepare reusable lists for UI controls
#   - Define plotting functions for dashboard visualisations
#
# QA Principles:
#   - Script runs from top to bottom reproducibly
#   - Clean data is loaded (raw data not touched here)
#   - Plot functions are modular and reusable
#   - Factor ordering is controlled for consistency
# ============================================================


# ============================================================
# 1. Load Required Libraries
# ============================================================

# Only required packages are loaded.
# tidyverse = data manipulation
# shiny / shinydashboard = application framework
# plotly = interactivity layer
# stringr = text formatting for display

library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(stringr)

message("global.R loaded")  # Confirms script execution in console logs


# ============================================================
# 2. Load Cleaned Dataset
# ============================================================

# This script intentionally loads a pre-cleaned dataset.
# Data cleaning is handled in a separate pipeline.
# This separation ensures:
#   - Raw data integrity
#   - Clear DAG structure
#   - Dashboard only depends on validated outputs

scs_data_cleaned <- read_csv("data/scs_data_cleaned.csv")


# ============================================================
# 3. Prepare Reference Lists for UI Controls
# ============================================================

# Extract available demographic parameters dynamically.
# This ensures dropdown menus remain robust to future updates.

parameters_list <- scs_data_cleaned |> 
  pull(demographic_parameter) |>
  unique()


# Define fixed subgroup ordering.
# Explicit ordering prevents:
#   - Alphabetical reordering
#   - Inconsistent plot appearance
#   - UI instability across sessions

subgroups_list <- c(
  "Overall Population",
  "Men",
  "Women",
  "From 16 to 34",
  "35 and above",
  "From 35 to 54",
  "Under 55",
  "From 55 to 69",
  "70 and above",
  "Urban (All)",
  "Rural (All)",
  "Urban (Large)",
  "Urban (Other)",
  "Small Towns (Accessible)",
  "Small Towns (Remote)",
  "Rural (Accessible)",
  "Rural (Remote)",
  "Yes/Applicable",
  "No/Not Applicable",
  "Degree or Higher Qualifications",
  "Non-graduate",
  "Other Qualifications",
  "No Formal Qualifications",
  "Employed (FT or PT)",
  "Unemployed",
  "Retired",
  "Less than £26,000",
  "From £26,000 to less than £52,000",
  "Less than £52,000",
  "£52,000 and above",
  "Online",
  "Postal"
)


# ============================================================
# 4. Enforce Factor Ordering
# ============================================================

# Converting subgroup into a factor ensures:
#   - Controlled display order
#   - Consistency across all plots
#   - No unintended reordering by ggplot

scs_data_cleaned <- scs_data_cleaned |> 
  mutate(subgroup = factor(subgroup, levels = subgroups_list))


# Generate parameter–subgroup combinations.
# Useful for dynamic UI filtering logic.

select_subgroups <- scs_data_cleaned|> 
  select(demographic_parameter, subgroup) |> 
  group_by(demographic_parameter, subgroup) |> 
  summarise() |> 
  ungroup()



# ============================================================
# 5. Plot 1 – Knowledge of Climate Change (T1)
# ============================================================

# Extract valid response options directly from dataset.
# Avoids hardcoding categories elsewhere in the app.

knowledge_response_list <- scs_data_cleaned |> 
  filter(question_number == "T1") |> 
  pull(response_category) |>
  unique()


# Function design:
#   - Modular and reusable
#   - Accepts filtering inputs
#   - Keeps plotting logic encapsulated
#   - Returns interactive Plotly object

plot_knowledge <- function(
    data, 
    parameter = "Overall Population",
    knowledge_responses = NULL,
    subgroup_values = NULL
){
  
  # Explicit ordering of Likert responses
  response_levels <- c(
    "A great deal",
    "A fair amount",
    "A little",
    "Nothing at all",
    "Don't know",
    "Prefer not to say"
  )
  
  # Controlled colour mapping ensures consistency
  clr_knowledge <- c(
    "A great deal"        = "#002d54",
    "A fair amount"       = "#2b9c93",
    "A little"            = "#de3163",
    "Nothing at all"      = "#0b4c0b",
    "Don't know"          = "#6a2063",
    "Prefer not to say"   = "#ca72a2"
  )
  
  # Optional response filtering
  if (!is.null(knowledge_responses)) {
    data <- data |>
      filter(response_category %in% knowledge_responses)
  }
  
  # Optional subgroup filtering
  if (!is.null(subgroup_values)) {
    data <- data |>
      filter(subgroup %in% subgroup_values)
  }
  
  # Plot construction:
  #   - Filter to correct question and parameter
  #   - Remove missing values
  #   - Enforce factor ordering
  #   - Use stacked bar format
  #   - Convert to interactive Plotly
  
  ggplotly(
    data |>
      filter(
        question_number == "T1",
        demographic_parameter == parameter
      ) |>
      na.omit() |>
      mutate(
        response_category = factor(
          response_category,
          levels = response_levels
        )
      ) |>
      ggplot(
        aes(
          x = factor(
            str_wrap(subgroup, width = 15), 
            levels = str_wrap(subgroups_list, width = 15)
          ), 
          y = freq, 
          fill = response_category,
          text = sprintf(
            "Demographic Subgroup: %s<br>Response: %s<br>Percentage: %.0f%%",
            subgroup,
            response_category,
            freq
          )
        )
      ) +
      geom_bar(stat = "identity") +
      coord_flip(ylim = c(0, 100)) +
      scale_fill_manual(
        values = clr_knowledge,
        drop = TRUE
      ) +
      labs(
        x = "Demographic Subgroups",
        y = "% State the knowledge",
        fill = "Response(s)"
      ) +
      theme_minimal(),
    tooltip = "text"
  ) |>
    style(
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "black",
        font = list(color = "black")
      )
    )
}



# ============================================================
# 6. Plot 2 – Perceived Urgency (T3)
# ============================================================

# Extract urgency response categories dynamically

urgency_response_list <- scs_data_cleaned |> 
  filter(question_number == "T3") |> 
  pull(response_category) |>
  unique()


# Design:
#   - Focused single-response visualisation
#   - Lollipop-style plot for clarity
#   - Supports subgroup filtering

plot_urgency <- function(
    data, 
    parameter = "Overall Population", 
    urgency_responses = "Climate change is an immediate and urgent problem",
    subgroup_values = NULL
) {
  
  if (!is.null(subgroup_values)) {
    data <- data |>
      filter(subgroup %in% subgroup_values)
  }
  
  ggplotly(
    data |> 
      filter(
        question_number == "T3" & 
          demographic_parameter == parameter &
          response_category == urgency_responses
      ) |> 
      na.omit() |> 
      ggplot(
        aes(
          x = factor(
            str_wrap(subgroup, width = 15), 
            levels = str_wrap(subgroups_list, width = 15)
          ), 
          y = freq,
          text = sprintf(
            "Demographic Subgroup: %s<br>Percentage: %.0f%%",
            subgroup,
            freq
          )
        )
      ) +
      geom_point() + 
      geom_segment(
        aes(
          x =  str_wrap(subgroup, width = 15), 
          xend =  str_wrap(subgroup, width = 15), 
          y = 0, 
          yend = freq
        )
      ) +
      coord_flip(ylim = c(0, 100)) +
      labs(
        x = "Demographic Subgroups",
        y = str_wrap(str_glue("% {urgency_responses}"), width = 50)
      ) +
      theme_minimal(),
    tooltip = "text"
  )|> 
    style(
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "black",
        font = list(color = "black")
      )
    )
}



# ============================================================
# 7. Plot 3 – Trusted Sources (T7)
# ============================================================

# Generate parameter–subgroup reference for sources

sources_subgroups_list <- scs_data_cleaned |>
  select(demographic_parameter, subgroup) |> 
  group_by(demographic_parameter, subgroup) |> 
  summarise() |> 
  ungroup()


# Design:
#   - Select top N trusted sources
#   - Clean labels (remove bracketed metadata)
#   - Horizontal ranking bar chart

plot_sources <- function(
    data, 
    parameter = "Overall Population", 
    ntop = 5, 
    subgroup_value = "Overall Population"
) {
  
  ggplotly(data |> 
             filter(question_number == "T7" &
                      demographic_parameter == parameter &
                      subgroup == subgroup_value) |> 
             slice_max(freq, n = ntop) |>
             select(subgroup, response_category, freq) |> 
             mutate(
               response_category = gsub("\\s*\\(.*\\)", "", response_category)
             ) |> 
             ggplot(
               aes(
                 x = reorder(str_wrap(response_category, width = 25), freq),
                 y = freq, 
                 text = sprintf(
                   "Source of Information: %s<br>Trusts the Source: %.0f%%",
                   response_category,
                   freq
                 )
               )
             ) +
             geom_col(fill = "#123499") +
             coord_flip(ylim = c(0, 100))+
             labs(
               x = str_glue("Top {ntop} Trusted  Sources"),
               y = "% Trusts the source"
             ) +
             theme_minimal(),
           tooltip = "text") |> 
    style(
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "black",
        font = list(color = "black")
      )
    )
}  



# ============================================================
# 8. Plot 4 – Negative Impact on Daily Life (T11)
# ============================================================

# Extract response categories dynamically

negative_impact_response_list <- scs_data_cleaned |> 
  filter(question_number == "T11") |> 
  pull(response_category) |>
  unique()


# Design mirrors Knowledge plot for visual consistency.
# Explicit response ordering ensures interpretability.

plot_negative_impact <- function(
    data, 
    parameter = "Overall Population",
    negative_impact_responses = NULL,
    subgroup_values = NULL
){
  
  response_levels <- c(
    "Constantly",
    "Often",
    "Sometimes",
    "Rarely",
    "Never",
    "Don't know",
    "Prefer not to say"
  )
  
  clr_knowledge <- c(
    "Constantly"          = "#002d54",
    "Often"               = "#2b9c93",
    "Sometimes"           = "#de3163",
    "Rarely"              = "#0b4c0b",
    "Never"               = "#e1d4c6",
    "Don't know"          = "#6a2063",
    "Prefer not to say"   = "#ca72a2"
  )
  
  if (!is.null(negative_impact_responses)) {
    data <- data |>
      filter(response_category %in% negative_impact_responses)
  }
  
  if (!is.null(subgroup_values)) {
    data <- data |>
      filter(subgroup %in% subgroup_values)
  }
  
  ggplotly(
    data |>
      filter(
        question_number == "T11",
        demographic_parameter == parameter
      ) |>
      na.omit() |>
      mutate(
        response_category = factor(
          response_category,
          levels = response_levels
        )
      ) |>
      ggplot(
        aes(
          x = factor(
            str_wrap(subgroup, width = 15), 
            levels = str_wrap(subgroups_list, width = 15)
          ), 
          y = freq, 
          fill = response_category,
          text = sprintf(
            "Demographic Subgroup: %s<br>Response: %s<br>Report: %.0f%%",
            subgroup,
            response_category,
            freq
          )
        )
      ) +
      geom_bar(stat = "identity") +
      coord_flip(ylim = c(0, 100)) +
      scale_fill_manual(
        values = clr_knowledge,
        drop = TRUE
      ) +
      labs(
        x = "Demographic Subgroups",
        y = "% Negative effect on daily life",
        fill = "Response(s)"
      ) +
      theme_minimal(),
    tooltip = "text"
  ) |>
    style(
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "black",
        font = list(color = "black")
      )
    )
}
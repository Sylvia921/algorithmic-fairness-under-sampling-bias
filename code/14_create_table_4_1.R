# =========================================================
# 14_create_table_4_1.R
# Create Table 4.1 from the balanced cross-seed results
# =========================================================


# -------------------------
# 1. Clear environment
# -------------------------

rm(list = ls())


# -------------------------
# 2. Load package
# -------------------------

library(dplyr)


# -------------------------
# 3. Define file paths
# -------------------------

sim_file <- paste0(
  "/Users/karry/Documents/GitHub/",
  "algorithmic-fairness-under-sampling-bias/",
  "outputs/simulated/07/",
  "sim_cross_seed_mean_results_report.csv"
)

adult_file <- paste0(
  "/Users/karry/Documents/GitHub/",
  "algorithmic-fairness-under-sampling-bias/",
  "outputs/adult/13/",
  "adult_cross_seed_mean_results_report.csv"
)

output_dir <- paste0(
  "/Users/karry/Documents/GitHub/",
  "algorithmic-fairness-under-sampling-bias/outputs"
)

output_file <- file.path(
  output_dir,
  "table_4_1_cross_seed_baseline_report.csv"
)


# -------------------------
# 4. Check paths
# -------------------------

if (!file.exists(sim_file)) {
  stop("Simulated results file was not found.")
}

if (!file.exists(adult_file)) {
  stop("Adult results file was not found.")
}

if (!dir.exists(output_dir)) {
  dir.create(
    output_dir,
    recursive = TRUE
  )
}


# -------------------------
# 5. Read results
# -------------------------

sim_results <- read.csv(
  sim_file,
  stringsAsFactors = FALSE
)

adult_results <- read.csv(
  adult_file,
  stringsAsFactors = FALSE
)


# -------------------------
# 6. Add n_seeds if missing
# -------------------------

# The simulated report does not contain an n_seeds column
if (!"n_seeds" %in% names(sim_results)) {
  sim_results$n_seeds <- 30
}

if (!"n_seeds" %in% names(adult_results)) {
  adult_results$n_seeds <- 30
}


# -------------------------
# 7. Standardise model names
# -------------------------

standardise_model_name <- function(model_name) {
  
  cleaned_name <- tolower(
    gsub(
      "[ _-]",
      "",
      trimws(as.character(model_name))
    )
  )
  
  case_when(
    cleaned_name %in% c(
      "logisticregression",
      "lr"
    ) ~ "Logistic Regression",
    
    cleaned_name %in% c(
      "decisiontree",
      "dt"
    ) ~ "Decision Tree",
    
    cleaned_name %in% c(
      "randomforest",
      "rf"
    ) ~ "Random Forest",
    
    TRUE ~ NA_character_
  )
}


# -------------------------
# 8. Extract balanced rows
# -------------------------

sim_baseline <- sim_results %>%
  filter(
    tolower(trimws(as.character(bias_level))) ==
      "balanced"
  ) %>%
  transmute(
    dataset = "Simulated",
    model = standardise_model_name(model),
    n_seeds = as.integer(n_seeds),
    
    mean_accuracy = mean_accuracy,
    sd_accuracy = sd_accuracy,
    
    mean_error_rate = mean_error_rate,
    sd_error_rate = sd_error_rate,
    
    mean_PR_gap = mean_PR_gap,
    sd_PR_gap = sd_PR_gap,
    
    mean_TPR_gap = mean_TPR_gap,
    sd_TPR_gap = sd_TPR_gap,
    
    mean_Error_gap = mean_Error_gap,
    sd_Error_gap = sd_Error_gap
  )


adult_baseline <- adult_results %>%
  filter(
    tolower(trimws(as.character(bias_level))) ==
      "balanced"
  ) %>%
  transmute(
    dataset = "Adult",
    model = standardise_model_name(model),
    n_seeds = as.integer(n_seeds),
    
    mean_accuracy = mean_accuracy,
    sd_accuracy = sd_accuracy,
    
    mean_error_rate = mean_error_rate,
    sd_error_rate = sd_error_rate,
    
    mean_PR_gap = mean_PR_gap,
    sd_PR_gap = sd_PR_gap,
    
    mean_TPR_gap = mean_TPR_gap,
    sd_TPR_gap = sd_TPR_gap,
    
    mean_Error_gap = mean_Error_gap,
    sd_Error_gap = sd_Error_gap
  )


# -------------------------
# 9. Combine results
# -------------------------

table_4_1 <- bind_rows(
  sim_baseline,
  adult_baseline
)


# -------------------------
# 10. Check model names
# -------------------------

if (any(is.na(table_4_1$model))) {
  stop(
    "One or more model names could not be recognised."
  )
}


# -------------------------
# 11. Order rows
# -------------------------

table_4_1 <- table_4_1 %>%
  mutate(
    dataset = factor(
      dataset,
      levels = c(
        "Simulated",
        "Adult"
      )
    ),
    
    model = factor(
      model,
      levels = c(
        "Logistic Regression",
        "Decision Tree",
        "Random Forest"
      )
    )
  ) %>%
  arrange(
    dataset,
    model
  ) %>%
  mutate(
    dataset = as.character(dataset),
    model = as.character(model)
  )


# -------------------------
# 12. Use three decimal places
# -------------------------

numeric_result_columns <- c(
  "mean_accuracy",
  "sd_accuracy",
  "mean_error_rate",
  "sd_error_rate",
  "mean_PR_gap",
  "sd_PR_gap",
  "mean_TPR_gap",
  "sd_TPR_gap",
  "mean_Error_gap",
  "sd_Error_gap"
)

table_4_1 <- table_4_1 %>%
  mutate(
    across(
      all_of(numeric_result_columns),
      ~ round(.x, 3)
    )
  )


# -------------------------
# 13. Final checks
# -------------------------

if (nrow(table_4_1) != 6) {
  warning(
    paste(
      "Table 4.1 should contain 6 rows.",
      "Current number of rows:",
      nrow(table_4_1)
    )
  )
}

if (any(is.na(table_4_1))) {
  warning("Table 4.1 contains missing values.")
}


# -------------------------
# 14. Save Table 4.1
# -------------------------

write.csv(
  table_4_1,
  output_file,
  row.names = FALSE
)


# -------------------------
# 15. Display result
# -------------------------

print(table_4_1)

cat(
  "\nTable 4.1 was saved to:\n",
  output_file,
  "\n"
)
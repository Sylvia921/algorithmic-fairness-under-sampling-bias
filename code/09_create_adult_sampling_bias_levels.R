# =========================================================
# 09_create_adult_sampling_bias_levels.R
# Adult dataset: Create fixed test set and sampling-bias training sets
# =========================================================

# -------------------------
# 1. Set up environment
# -------------------------

rm(list = ls())

# Identify the project root
here::i_am("code/09_create_adult_sampling_bias_levels.R")

library(dplyr)
library(readr)
library(stringr)

set.seed(123)


# -------------------------
# 2. Define input and output paths
# -------------------------

input_path <- here::here(
  "outputs",
  "adult",
  "08",
  "adult_clean.csv"
)

output_dir <- here::here(
  "outputs",
  "adult",
  "09"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# -------------------------
# 3. Read cleaned Adult data
# -------------------------

adult_clean <- read.csv(input_path, stringsAsFactors = FALSE)

# Required columns for sampling-bias experiment
required_columns <- c("group", "sex", "income", "y", "row_id")

missing_columns <- setdiff(required_columns, names(adult_clean))

if (length(missing_columns) > 0) {
  stop(
    paste(
      "The following required columns are missing from adult_clean.csv:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# Standardise key variables
adult_clean <- adult_clean %>%
  mutate(
    group = str_trim(as.character(group)),
    sex = str_trim(as.character(sex)),
    income = str_trim(as.character(income)),
    y = as.integer(y),
    row_id = as.integer(row_id)
  ) %>%
  filter(group %in% c("Male", "Female")) %>%
  filter(y %in% c(0, 1))


# -------------------------
# 4. Define experiment settings
# -------------------------

sensitive_attr <- "group"

reference_group <- "Male"
underrepresented_group <- "Female"

# Fixed test set design
test_total_n <- 5000
test_male_n <- 2500
test_female_n <- 2500

# Training set design
train_total_n <- 20000

bias_design <- data.frame(
  bias_level = c("balanced", "mild", "moderate", "severe"),
  male_n = c(10000, 12000, 15000, 18000),
  female_n = c(10000, 8000, 5000, 2000),
  stringsAsFactors = FALSE
) %>%
  mutate(
    total_n = male_n + female_n,
    male_rate = male_n / total_n,
    female_rate = female_n / total_n
  )

# Check training set sizes
if (any(bias_design$total_n != train_total_n)) {
  stop("At least one training set does not match the required train_total_n.")
}


# -------------------------
# 5. Check available sample sizes before sampling
# -------------------------

available_group_summary <- adult_clean %>%
  group_by(group) %>%
  summarise(
    available_n = n(),
    y_0_n = sum(y == 0),
    y_1_n = sum(y == 1),
    y_1_rate = round(mean(y == 1), 4),
    .groups = "drop"
  ) %>%
  arrange(group)

write.csv(
  available_group_summary,
  file.path(output_dir, "adult_available_group_summary_report.csv"),
  row.names = FALSE
)

available_male_n <- available_group_summary$available_n[
  available_group_summary$group == reference_group
]

available_female_n <- available_group_summary$available_n[
  available_group_summary$group == underrepresented_group
]

required_male_n <- test_male_n + max(bias_design$male_n)
required_female_n <- test_female_n + max(bias_design$female_n)

if (length(available_male_n) == 0 || is.na(available_male_n)) {
  stop("No Male samples found in adult_clean.csv.")
}

if (length(available_female_n) == 0 || is.na(available_female_n)) {
  stop("No Female samples found in adult_clean.csv.")
}

if (available_male_n < required_male_n) {
  stop(
    paste0(
      "Not enough Male samples. Required: ",
      required_male_n,
      ", available: ",
      available_male_n
    )
  )
}

if (available_female_n < required_female_n) {
  stop(
    paste0(
      "Not enough Female samples. Required: ",
      required_female_n,
      ", available: ",
      available_female_n
    )
  )
}


# -------------------------
# 6. Save sampling design report
# -------------------------

adult_sampling_design_report <- bias_design %>%
  mutate(
    design_description = case_when(
      bias_level == "balanced" ~ "Balanced baseline: Male/Female representation is 50/50.",
      bias_level == "mild" ~ "Mild bias: Female is mildly under-represented in the training data.",
      bias_level == "moderate" ~ "Moderate bias: Female is moderately under-represented in the training data.",
      bias_level == "severe" ~ "Severe bias: Female is strongly under-represented in the training data.",
      TRUE ~ NA_character_
    )
  )

write.csv(
  adult_sampling_design_report,
  file.path(output_dir, "adult_sampling_design_report.csv"),
  row.names = FALSE
)


# -------------------------
# 7. Create fixed balanced test set
# -------------------------
# The test set is balanced by group only:
# Male = 2500, Female = 2500.
# It is not stratified by y, in order to keep the sampling logic
# consistent with the simulated dataset.

male_all <- adult_clean %>%
  filter(group == reference_group)

female_all <- adult_clean %>%
  filter(group == underrepresented_group)

test_male <- male_all %>%
  sample_n(test_male_n)

test_female <- female_all %>%
  sample_n(test_female_n)

adult_test_fixed <- bind_rows(test_male, test_female) %>%
  sample_frac(1) %>%
  mutate(set_type = "test_fixed")

# Save test row ids so that the test set is removed from the training pool
test_row_ids <- adult_test_fixed$row_id


# -------------------------
# 8. Create train pool
# -------------------------

adult_train_pool <- adult_clean %>%
  filter(!(row_id %in% test_row_ids)) %>%
  mutate(set_type = "train_pool")

train_pool_male <- adult_train_pool %>%
  filter(group == reference_group)

train_pool_female <- adult_train_pool %>%
  filter(group == underrepresented_group)


# -------------------------
# 9. Function to create one biased training set
# -------------------------

create_training_set <- function(pool_male,
                                pool_female,
                                male_n,
                                female_n,
                                bias_level_name) {
  
  sampled_male <- pool_male %>%
    sample_n(male_n)
  
  sampled_female <- pool_female %>%
    sample_n(female_n)
  
  training_set <- bind_rows(sampled_male, sampled_female) %>%
    sample_frac(1) %>%
    mutate(
      bias_level = bias_level_name,
      set_type = paste0("train_", bias_level_name)
    )
  
  return(training_set)
}


# -------------------------
# 10. Create four training sets
# -------------------------

adult_train_balanced <- create_training_set(
  pool_male = train_pool_male,
  pool_female = train_pool_female,
  male_n = bias_design$male_n[bias_design$bias_level == "balanced"],
  female_n = bias_design$female_n[bias_design$bias_level == "balanced"],
  bias_level_name = "balanced"
)

adult_train_mild <- create_training_set(
  pool_male = train_pool_male,
  pool_female = train_pool_female,
  male_n = bias_design$male_n[bias_design$bias_level == "mild"],
  female_n = bias_design$female_n[bias_design$bias_level == "mild"],
  bias_level_name = "mild"
)

adult_train_moderate <- create_training_set(
  pool_male = train_pool_male,
  pool_female = train_pool_female,
  male_n = bias_design$male_n[bias_design$bias_level == "moderate"],
  female_n = bias_design$female_n[bias_design$bias_level == "moderate"],
  bias_level_name = "moderate"
)

adult_train_severe <- create_training_set(
  pool_male = train_pool_male,
  pool_female = train_pool_female,
  male_n = bias_design$male_n[bias_design$bias_level == "severe"],
  female_n = bias_design$female_n[bias_design$bias_level == "severe"],
  bias_level_name = "severe"
)


# -------------------------
# 11. Combine training sets for checking reports
# -------------------------

adult_train_all_levels <- bind_rows(
  adult_train_balanced,
  adult_train_mild,
  adult_train_moderate,
  adult_train_severe
)


# -------------------------
# 12. Test fixed summary report
# -------------------------

adult_test_fixed_summary_report <- adult_test_fixed %>%
  group_by(group) %>%
  summarise(
    n = n(),
    proportion = round(n / nrow(adult_test_fixed), 4),
    y_0_n = sum(y == 0),
    y_1_n = sum(y == 1),
    y_1_rate = round(mean(y == 1), 4),
    .groups = "drop"
  ) %>%
  mutate(
    test_total_n = nrow(adult_test_fixed),
    note = "Fixed test set is balanced by group only and is not stratified by y."
  ) %>%
  arrange(group)

write.csv(
  adult_test_fixed_summary_report,
  file.path(output_dir, "adult_test_fixed_summary_report.csv"),
  row.names = FALSE
)


# -------------------------
# 13. Training bias-level summary report
# -------------------------

adult_train_bias_level_summary_report <- adult_train_all_levels %>%
  group_by(bias_level) %>%
  summarise(
    total_n = n(),
    male_n = sum(group == reference_group),
    female_n = sum(group == underrepresented_group),
    male_rate = round(male_n / total_n, 4),
    female_rate = round(female_n / total_n, 4),
    income_leq_50K_n = sum(income == "<=50K"),
    income_gt_50K_n = sum(income == ">50K"),
    income_gt_50K_rate = round(income_gt_50K_n / total_n, 4),
    .groups = "drop"
  ) %>%
  mutate(
    bias_level = factor(
      bias_level,
      levels = c("balanced", "mild", "moderate", "severe")
    )
  ) %>%
  arrange(bias_level)

write.csv(
  adult_train_bias_level_summary_report,
  file.path(output_dir, "adult_train_bias_level_summary_report.csv"),
  row.names = FALSE
)


# -------------------------
# 14. Training income-by-group report
# -------------------------
# This report checks whether each training set has unusual outcome composition
# within Male and Female groups.
# Sampling itself is not stratified by y.

adult_train_income_by_group_report <- adult_train_all_levels %>%
  group_by(bias_level, group, income) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(bias_level, group) %>%
  mutate(
    group_total_n = sum(n),
    proportion_within_group = round(n / group_total_n, 4)
  ) %>%
  ungroup() %>%
  mutate(
    bias_level = factor(
      bias_level,
      levels = c("balanced", "mild", "moderate", "severe")
    )
  ) %>%
  arrange(bias_level, group, income)

write.csv(
  adult_train_income_by_group_report,
  file.path(output_dir, "adult_train_income_by_group_report.csv"),
  row.names = FALSE
)


# -------------------------
# 15. Save 6 main data outputs for Adult models
# -------------------------

write.csv(
  adult_test_fixed,
  file.path(output_dir, "adult_test_fixed.csv"),
  row.names = FALSE
)

write.csv(
  adult_train_pool,
  file.path(output_dir, "adult_train_pool.csv"),
  row.names = FALSE
)

write.csv(
  adult_train_balanced,
  file.path(output_dir, "adult_train_balanced.csv"),
  row.names = FALSE
)

write.csv(
  adult_train_mild,
  file.path(output_dir, "adult_train_mild.csv"),
  row.names = FALSE
)

write.csv(
  adult_train_moderate,
  file.path(output_dir, "adult_train_moderate.csv"),
  row.names = FALSE
)

write.csv(
  adult_train_severe,
  file.path(output_dir, "adult_train_severe.csv"),
  row.names = FALSE
)


# -------------------------
# 16. Print final checks
# -------------------------

cat("\n==============================\n")
cat("09 Adult sampling-bias levels completed.\n")
cat("==============================\n\n")

cat("Input file:\n")
cat(input_path, "\n\n")

cat("Output directory:\n")
cat(output_dir, "\n\n")

cat("Fixed test set group distribution:\n")
print(table(adult_test_fixed$group))
cat("\n")

cat("Training sets by group:\n")
print(
  adult_train_all_levels %>%
    group_by(bias_level, group) %>%
    summarise(n = n(), .groups = "drop")
)

cat("\nTraining outcome rate by bias level and group:\n")
print(
  adult_train_all_levels %>%
    group_by(bias_level, group) %>%
    summarise(
      n = n(),
      y_1_rate = round(mean(y == 1), 4),
      .groups = "drop"
    )
)

cat("\nFiles saved successfully.\n")
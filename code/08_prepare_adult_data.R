# =========================================================
# 08_prepare_adult_data.R
# Part 1: Raw EDA before cleaning
# =========================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

# Identify the project root
here::i_am("code/08_prepare_adult_data.R")

# -------------------------
# 0. Load packages
# -------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# -------------------------
# 1. Define paths
# -------------------------
input_dir <- here::here(
  "data",
  "adult",
  "raw"
)

output_dir <- here::here(
  "outputs",
  "adult",
  "08"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

adult_data_path <- file.path(input_dir, "adult.data")
adult_test_path <- file.path(input_dir, "adult.test")

# -------------------------
# 2. Column names
# -------------------------
adult_colnames <- c(
  "age", "workclass", "fnlwgt", "education", "education_num",
  "marital_status", "occupation", "relationship", "race", "sex",
  "capital_gain", "capital_loss", "hours_per_week",
  "native_country", "income"
)

# -------------------------
# 3. Read raw Adult data
# -------------------------
adult_data_raw <- read.csv(
  adult_data_path,
  header = FALSE,
  strip.white = TRUE
)

adult_test_raw <- read.csv(
  adult_test_path,
  header = FALSE,
  skip = 1,
  strip.white = TRUE
)

colnames(adult_data_raw) <- adult_colnames
colnames(adult_test_raw) <- adult_colnames

adult_data_raw$source_file <- "adult.data"
adult_test_raw$source_file <- "adult.test"

# -------------------------
# 4. Combine raw data
# -------------------------
adult_raw <- bind_rows(adult_data_raw, adult_test_raw)

adult_raw <- adult_raw %>%
  mutate(across(where(is.character), ~ str_trim(.x))) %>%
  mutate(income = str_replace(income, "\\.$", ""))

# -------------------------
# 5. Missing-value summary copy
# -------------------------
adult_raw_for_summary <- adult_raw %>%
  mutate(across(where(is.character), ~ na_if(.x, "?")))

numeric_vars <- c(
  "age", "fnlwgt", "education_num",
  "capital_gain", "capital_loss", "hours_per_week"
)

adult_raw_for_summary[numeric_vars] <- lapply(
  adult_raw_for_summary[numeric_vars],
  as.numeric
)

# -------------------------
# 6. Shared figure theme
# No gridlines, clean white background
# -------------------------
theme_erp_clean <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.line = element_line(colour = "black", linewidth = 0.4),
      axis.ticks = element_line(colour = "black", linewidth = 0.4),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none"
    )
}

# =========================================================
# Output 1:
# =========================================================

raw_overview_table <- data.frame(
  item = c(
    "total_rows_after_combining",
    "number_of_variables_raw",
    "adult_data_rows",
    "adult_test_rows"
  ),
  value = c(
    nrow(adult_raw),
    length(adult_colnames),
    nrow(adult_data_raw),
    nrow(adult_test_raw)
  )
)

write.csv(
  raw_overview_table,
  file.path(output_dir, "adult_raw_dataset_overview_report.csv"),
  row.names = FALSE
)

# =========================================================
# Output 2:
# =========================================================

income_plot_df <- adult_raw %>%
  count(income) %>%
  mutate(
    income = factor(income, levels = c("<=50K", ">50K")),
    proportion = n / sum(n),
    count_label = format(n, big.mark = ","),
    prop_label = percent(proportion, accuracy = 0.1)
  )

p_income <- ggplot(income_plot_df, aes(x = income, y = n, fill = income)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = count_label),
    vjust = -0.45,
    size = 5
  ) +
  geom_text(
    aes(y = n / 2, label = prop_label),
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(
    values = c("<=50K" = "#8E8E8E", ">50K" = "#2E8B57")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Raw income distribution",
    x = "Income",
    y = "Count"
  ) +
  theme_erp_clean()

ggsave(
  filename = file.path(output_dir, "fig_adult_raw_income_distribution.png"),
  plot = p_income,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

# =========================================================
# Output 3:
# =========================================================

sex_plot_df <- adult_raw %>%
  count(sex) %>%
  mutate(
    sex = factor(sex, levels = c("Male", "Female")),
    proportion = n / sum(n),
    count_label = format(n, big.mark = ","),
    prop_label = percent(proportion, accuracy = 0.1)
  )

p_sex <- ggplot(sex_plot_df, aes(x = sex, y = n, fill = sex)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = count_label),
    vjust = -0.45,
    size = 5
  ) +
  geom_text(
    aes(y = n / 2, label = prop_label),
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(
    values = c("Male" = "#4C78A8", "Female" = "#E76F8A")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Raw sex distribution",
    x = "Sex",
    y = "Count"
  ) +
  theme_erp_clean()

ggsave(
  filename = file.path(output_dir, "fig_adult_raw_sex_distribution.png"),
  plot = p_sex,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

# =========================================================
# Output 4:
# =========================================================

income_by_sex_df <- adult_raw %>%
  count(sex, income) %>%
  group_by(sex) %>%
  mutate(
    proportion_within_sex = n / sum(n),
    count_label = format(n, big.mark = ","),
    prop_label = percent(proportion_within_sex, accuracy = 0.1)
  ) %>%
  ungroup() %>%
  mutate(
    sex = factor(sex, levels = c("Male", "Female")),
    income = factor(income, levels = c("<=50K", ">50K"))
  )

p_income_by_sex <- ggplot(income_by_sex_df, aes(x = sex, y = n, fill = sex)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = count_label),
    vjust = -0.45,
    size = 4.8
  ) +
  geom_text(
    aes(y = n / 2, label = prop_label),
    color = "white",
    fontface = "bold",
    size = 4.8
  ) +
  facet_wrap(~ income, nrow = 1) +
  scale_fill_manual(
    values = c("Male" = "#4C78A8", "Female" = "#E76F8A")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Raw income distribution by sex",
    subtitle = "Top label = count; inside label = proportion within each sex",
    x = "Sex",
    y = "Count"
  ) +
  theme_erp_clean() +
  theme(
    strip.background = element_rect(fill = "white", colour = "black", linewidth = 0.4),
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = file.path(output_dir, "fig_adult_raw_income_by_sex.png"),
  plot = p_income_by_sex,
  width = 11,
  height = 6,
  dpi = 300,
  bg = "white"
)

# =========================================================
# Output 5:
# =========================================================

raw_variable_quality_table <- data.frame(
  variable = adult_colnames,
  data_type = sapply(adult_raw_for_summary[adult_colnames], function(x) class(x)[1]),
  unique_values = sapply(adult_raw_for_summary[adult_colnames], function(x) dplyr::n_distinct(x, na.rm = TRUE)),
  missing_count = sapply(adult_raw_for_summary[adult_colnames], function(x) sum(is.na(x))),
  stringsAsFactors = FALSE
)

raw_variable_quality_table$missing_rate <- round(
  raw_variable_quality_table$missing_count / nrow(adult_raw_for_summary),
  4
)

write.csv(
  raw_variable_quality_table,
  file.path(output_dir, "adult_raw_variable_quality_report.csv"),
  row.names = FALSE
)















# =========================================================
# 08_prepare_adult_data.R
# Part 2: Cleaning and variable selection
# =========================================================


# -------------------------
# 8. Formal cleaning
# -------------------------

adult_clean_full <- adult_raw %>%
  # remove extra spaces and standardise income labels
  mutate(across(where(is.character), ~ str_trim(.x))) %>%
  mutate(
    income = str_replace(income, "\\.$", "")
  ) %>%
  
  # convert "?" to NA
  mutate(across(where(is.character), ~ na_if(.x, "?"))) %>%
  
  # create binary target variable
  mutate(
    y = case_when(
      income == ">50K" ~ 1L,
      income == "<=50K" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # define sensitive attribute for fairness analysis
    group = sex
  )


# -------------------------
# 9. Record rows before cleaning
# -------------------------

raw_rows <- nrow(adult_clean_full)


# -------------------------
# 10. Handle missing values
# -------------------------
# Main decision:
# - workclass and occupation are main predictors, so rows missing these variables are removed.
# - income, y, sex and group are required for target definition and fairness evaluation.
# - native_country is retained for descriptive checks, but it is not used in the main model.
#   Therefore, missing native_country values are recoded as "Missing" rather than causing row removal.

adult_clean_full <- adult_clean_full %>%
  mutate(
    native_country = if_else(
      is.na(native_country),
      "Missing",
      native_country
    )
  )

required_complete_vars <- c(
  "age",
  "workclass",
  "education_num",
  "occupation",
  "capital_gain",
  "capital_loss",
  "hours_per_week",
  "sex",
  "group",
  "income",
  "y"
)

adult_clean_full <- adult_clean_full %>%
  filter(if_all(all_of(required_complete_vars), ~ !is.na(.x)))

cleaned_rows_before_group_filter <- nrow(adult_clean_full)
removed_rows <- raw_rows - cleaned_rows_before_group_filter
removed_rate <- removed_rows / raw_rows


# -------------------------
# 11. Keep only Male and Female groups
# -------------------------

adult_clean_full <- adult_clean_full %>%
  filter(group %in% c("Male", "Female"))


# -------------------------
# 12. Variable selection for main experiment
# -------------------------
# Main predictors:
#   age, workclass, education_num, occupation,
#   capital_gain, capital_loss, hours_per_week
#
# Kept but not used in the main model:
#   sex            : sensitive attribute
#   group          : same as sex, used for sampling bias and fairness metrics
#   race           : another sensitive attribute, retained for descriptive checks
#   relationship   : strong proxy for sex, retained but excluded from main model
#   marital_status : correlated with relationship and sex, retained but excluded from main model
#   native_country : many categories; retained but excluded from main model
#   income         : original target label
#   y              : binary target variable
#   source_file    : original data source indicator
#   row_id         : unique row identifier for later train/test splitting
#
# Removed:
#   fnlwgt         : census final weight, not a standard individual predictor
#   education      : duplicated by education_num

adult_clean <- adult_clean_full %>%
  mutate(
    row_id = row_number()
  ) %>%
  select(
    # main model predictors
    age,
    workclass,
    education_num,
    occupation,
    capital_gain,
    capital_loss,
    hours_per_week,
    
    # retained variables not used in main model
    sex,
    group,
    race,
    relationship,
    marital_status,
    native_country,
    
    # target and tracking variables
    income,
    y,
    source_file,
    row_id
  )


# -------------------------
# 13. Set variable types
# -------------------------

adult_clean <- adult_clean %>%
  mutate(
    age = as.numeric(age),
    education_num = as.numeric(education_num),
    capital_gain = as.numeric(capital_gain),
    capital_loss = as.numeric(capital_loss),
    hours_per_week = as.numeric(hours_per_week),
    
    workclass = as.factor(workclass),
    occupation = as.factor(occupation),
    
    sex = as.factor(sex),
    group = as.factor(group),
    race = as.factor(race),
    relationship = as.factor(relationship),
    marital_status = as.factor(marital_status),
    native_country = as.factor(native_country),
    
    income = as.factor(income),
    source_file = as.factor(source_file),
    
    y = as.integer(y),
    row_id = as.integer(row_id)
  )


# =========================================================
# Output 6:
# adult_clean.csv
# =========================================================

write.csv(
  adult_clean,
  file.path(output_dir, "adult_clean.csv"),
  row.names = FALSE
)


# =========================================================
# Output 7:
# adult_clean_dataset_summary_report.csv
# =========================================================

clean_income_counts <- adult_clean %>%
  count(income) %>%
  mutate(proportion = n / sum(n))

clean_sex_counts <- adult_clean %>%
  count(sex) %>%
  mutate(proportion = n / sum(n))

adult_clean_dataset_summary_report <- data.frame(
  item = c(
    "raw_rows_before_cleaning",
    "cleaned_rows_after_required_missing_removal",
    "final_rows_after_variable_selection",
    "removed_rows_due_to_required_missing_values",
    "removed_rate_due_to_required_missing_values",
    "number_of_variables_final",
    "income_leq_50K_n",
    "income_gt_50K_n",
    "income_leq_50K_rate",
    "income_gt_50K_rate",
    "male_n",
    "female_n",
    "male_rate",
    "female_rate"
  ),
  value = c(
    raw_rows,
    cleaned_rows_before_group_filter,
    nrow(adult_clean),
    removed_rows,
    round(removed_rate, 4),
    ncol(adult_clean),
    clean_income_counts$n[clean_income_counts$income == "<=50K"],
    clean_income_counts$n[clean_income_counts$income == ">50K"],
    round(clean_income_counts$proportion[clean_income_counts$income == "<=50K"], 4),
    round(clean_income_counts$proportion[clean_income_counts$income == ">50K"], 4),
    clean_sex_counts$n[clean_sex_counts$sex == "Male"],
    clean_sex_counts$n[clean_sex_counts$sex == "Female"],
    round(clean_sex_counts$proportion[clean_sex_counts$sex == "Male"], 4),
    round(clean_sex_counts$proportion[clean_sex_counts$sex == "Female"], 4)
  )
)

write.csv(
  adult_clean_dataset_summary_report,
  file.path(output_dir, "adult_clean_dataset_summary_report.csv"),
  row.names = FALSE
)


# =========================================================
# Output 8:
# adult_clean_income_by_sex_report.csv
# =========================================================

adult_clean_income_by_sex_report <- adult_clean %>%
  group_by(group, income) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(group) %>%
  mutate(
    group_total_n = sum(n),
    proportion_within_group = round(n / group_total_n, 4)
  ) %>%
  ungroup() %>%
  arrange(group, income)

write.csv(
  adult_clean_income_by_sex_report,
  file.path(output_dir, "adult_clean_income_by_sex_report.csv"),
  row.names = FALSE
)


# =========================================================
# Output 9:
# adult_clean_variable_quality_report.csv
# =========================================================

adult_clean_variable_quality_report <- data.frame(
  variable = names(adult_clean),
  data_type = sapply(adult_clean, function(x) class(x)[1]),
  unique_values = sapply(adult_clean, function(x) dplyr::n_distinct(x, na.rm = TRUE)),
  missing_count = sapply(adult_clean, function(x) sum(is.na(x))),
  stringsAsFactors = FALSE
)

adult_clean_variable_quality_report$missing_rate <- round(
  adult_clean_variable_quality_report$missing_count / nrow(adult_clean),
  4
)

write.csv(
  adult_clean_variable_quality_report,
  file.path(output_dir, "adult_clean_variable_quality_report.csv"),
  row.names = FALSE
)


# =========================================================
# Output 10:
# adult_feature_selection_report.csv
# =========================================================

adult_feature_selection_report <- data.frame(
  variable = c(
    "age",
    "workclass",
    "fnlwgt",
    "education",
    "education_num",
    "marital_status",
    "occupation",
    "relationship",
    "race",
    "sex",
    "group",
    "capital_gain",
    "capital_loss",
    "hours_per_week",
    "native_country",
    "income",
    "y",
    "source_file",
    "row_id"
  ),
  role = c(
    "main predictor",
    "main predictor",
    "removed variable",
    "removed variable",
    "main predictor",
    "retained but excluded from main model",
    "main predictor",
    "retained but excluded from main model",
    "retained sensitive attribute",
    "sensitive attribute",
    "sensitive group variable",
    "main predictor",
    "main predictor",
    "main predictor",
    "retained but excluded from main model",
    "target source",
    "binary target variable",
    "tracking variable",
    "tracking variable"
  ),
  used_in_main_model = c(
    "yes",
    "yes",
    "no",
    "no",
    "yes",
    "no",
    "yes",
    "no",
    "no",
    "no",
    "no",
    "yes",
    "yes",
    "yes",
    "no",
    "no",
    "no",
    "no",
    "no"
  ),
  kept_in_adult_clean = c(
    "yes",
    "yes",
    "no",
    "no",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes",
    "yes"
  ),
  reason = c(
    "Age is a standard individual-level predictor and may be related to income.",
    "Workclass is employment-related and may help predict income; rows with missing workclass are removed.",
    "fnlwgt is a census final weight rather than an ordinary individual-level predictor; it is removed to avoid introducing survey-weighting mechanisms into the sampling-bias experiment.",
    "education duplicates information contained in education_num; it is removed to reduce redundant information.",
    "education_num is a numeric representation of education level and is easier to model than the text education variable.",
    "marital_status is correlated with relationship, sex and income; it is retained for checks but excluded from the main predictor set.",
    "occupation is a core work-related predictor and may be strongly related to income; rows with missing occupation are removed.",
    "relationship is likely to act as a strong proxy for sex, for example through Husband and Wife categories; it is retained but excluded from the main model.",
    "race is another demographic sensitive attribute. The main experiment focuses on sex, so race is retained for descriptive checks but excluded from the main model.",
    "sex is the sensitive attribute used to construct Male/Female sampling-bias levels and calculate fairness metrics.",
    "group is defined as sex and is used for bias construction and group-level fairness evaluation.",
    "capital_gain is a financial predictor related to income.",
    "capital_loss is a financial predictor related to income.",
    "hours_per_week measures work intensity and may help predict income.",
    "native_country has many categories and may create sparse dummy variables; it is retained for descriptive checks but excluded from the main model.",
    "income is the original target label and is used to construct y.",
    "y is the binary target variable where >50K is coded as 1 and <=50K is coded as 0.",
    "source_file records whether the row came from adult.data or adult.test.",
    "row_id is a unique identifier used for later train/test splitting and reproducibility."
  ),
  stringsAsFactors = FALSE
)

write.csv(
  adult_feature_selection_report,
  file.path(output_dir, "adult_feature_selection_report.csv"),
  row.names = FALSE
)


# =========================================================
# Output 11:
# adult_proxy_check_relationship_by_sex.csv
# =========================================================

adult_proxy_check_relationship_by_sex <- adult_clean %>%
  group_by(sex, relationship) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(sex) %>%
  mutate(
    sex_total_n = sum(n),
    proportion_within_sex = round(n / sex_total_n, 4)
  ) %>%
  ungroup() %>%
  arrange(sex, desc(n))

write.csv(
  adult_proxy_check_relationship_by_sex,
  file.path(output_dir, "adult_proxy_check_relationship_by_sex.csv"),
  row.names = FALSE
)






# =========================================================
# Output 12:
# adult_main_predictors.csv
# Main predictors used consistently in Adult LR / DT / RF models
# =========================================================

adult_main_predictors <- data.frame(
  predictor = c(
    "age",
    "workclass",
    "education_num",
    "occupation",
    "capital_gain",
    "capital_loss",
    "hours_per_week"
  ),
  variable_type = c(
    "numeric",
    "categorical",
    "numeric",
    "categorical",
    "numeric",
    "numeric",
    "numeric"
  ),
  role = c(
    "main predictor",
    "main predictor",
    "main predictor",
    "main predictor",
    "main predictor",
    "main predictor",
    "main predictor"
  ),
  reason = c(
    "Age is a standard individual-level predictor and may be related to income.",
    "Workclass is an employment-related predictor and may be related to income.",
    "Education_num is a numeric measure of education level and avoids duplication with education.",
    "Occupation is a core work-related predictor and may be strongly related to income.",
    "Capital_gain is a financial predictor related to income.",
    "Capital_loss is a financial predictor related to income.",
    "Hours_per_week measures work intensity and may be related to income."
  ),
  stringsAsFactors = FALSE
)

write.csv(
  adult_main_predictors,
  file.path(output_dir, "adult_main_predictors.csv"),
  row.names = FALSE
)





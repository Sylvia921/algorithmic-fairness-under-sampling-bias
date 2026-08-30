# =========================================================
# 12_random_forest_adult.R
# Adult dataset: Random Forest under sampling bias
# =========================================================


# -------------------------
# 1. Set up environment
# -------------------------

rm(list = ls())
# Identify the project root
here::i_am("code/12_random_forest_adult.R")

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(randomForest)

set.seed(123)


# -------------------------
# 2. Define input and output paths
# -------------------------

sampling_input_dir <- here::here(
  "outputs",
  "adult",
  "09"
)

predictor_path <- here::here(
  "outputs",
  "adult",
  "08",
  "adult_main_predictors.csv"
)

output_dir <- here::here(
  "outputs",
  "adult",
  "12"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# -------------------------
# 3. Read fixed test set and four training sets
# -------------------------

adult_test_fixed <- read.csv(
  file.path(sampling_input_dir, "adult_test_fixed.csv"),
  stringsAsFactors = FALSE
)

adult_train_balanced <- read.csv(
  file.path(sampling_input_dir, "adult_train_balanced.csv"),
  stringsAsFactors = FALSE
)

adult_train_mild <- read.csv(
  file.path(sampling_input_dir, "adult_train_mild.csv"),
  stringsAsFactors = FALSE
)

adult_train_moderate <- read.csv(
  file.path(sampling_input_dir, "adult_train_moderate.csv"),
  stringsAsFactors = FALSE
)

adult_train_severe <- read.csv(
  file.path(sampling_input_dir, "adult_train_severe.csv"),
  stringsAsFactors = FALSE
)


# -------------------------
# 4. Read main predictors from 08
# -------------------------

adult_main_predictors <- read.csv(
  predictor_path,
  stringsAsFactors = FALSE
)

predictors <- adult_main_predictors$predictor

cat("\nMain predictors used in Adult Random Forest:\n")
print(predictors)


# -------------------------
# 5. Check required columns
# -------------------------

required_columns <- c(
  predictors,
  "y",
  "group",
  "sex",
  "income",
  "row_id"
)

check_required_columns <- function(data, data_name) {
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Missing columns in",
        data_name,
        ":",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
}

check_required_columns(adult_test_fixed, "adult_test_fixed")
check_required_columns(adult_train_balanced, "adult_train_balanced")
check_required_columns(adult_train_mild, "adult_train_mild")
check_required_columns(adult_train_moderate, "adult_train_moderate")
check_required_columns(adult_train_severe, "adult_train_severe")


# -------------------------
# 6. Prepare consistent factor levels
# -------------------------
# Random Forest can use factor predictors directly.
# However, train and test data must use consistent factor levels.

all_data_for_levels <- bind_rows(
  adult_test_fixed,
  adult_train_balanced,
  adult_train_mild,
  adult_train_moderate,
  adult_train_severe
)

workclass_levels <- sort(unique(all_data_for_levels$workclass))
occupation_levels <- sort(unique(all_data_for_levels$occupation))
group_levels <- c("Female", "Male")
income_levels <- c("<=50K", ">50K")


prepare_adult_rf_data <- function(data) {
  
  data <- data %>%
    mutate(
      y = as.integer(y),
      y_factor = factor(y, levels = c(0, 1)),
      
      group = factor(group, levels = group_levels),
      sex = factor(sex, levels = group_levels),
      income = factor(income, levels = income_levels),
      row_id = as.integer(row_id),
      
      age = as.numeric(age),
      education_num = as.numeric(education_num),
      capital_gain = as.numeric(capital_gain),
      capital_loss = as.numeric(capital_loss),
      hours_per_week = as.numeric(hours_per_week),
      
      workclass = factor(workclass, levels = workclass_levels),
      occupation = factor(occupation, levels = occupation_levels)
    )
  
  return(data)
}

adult_test_fixed <- prepare_adult_rf_data(adult_test_fixed)
adult_train_balanced <- prepare_adult_rf_data(adult_train_balanced)
adult_train_mild <- prepare_adult_rf_data(adult_train_mild)
adult_train_moderate <- prepare_adult_rf_data(adult_train_moderate)
adult_train_severe <- prepare_adult_rf_data(adult_train_severe)


# -------------------------
# 7. Define Random Forest formula and settings
# -------------------------
# Main model excludes:
# sex, group, race, relationship, marital_status, native_country

formula_text <- paste(
  "y_factor ~",
  paste(predictors, collapse = " + ")
)

rf_formula <- as.formula(formula_text)

# Random Forest settings
ntree_value <- 500
mtry_value <- 2
rf_seed_value <- 123

cat("\nRandom Forest formula:\n")
print(rf_formula)

cat("\nRandom Forest settings:\n")
cat("ntree =", ntree_value, "\n")
cat("mtry  =", mtry_value, "\n")
cat("rf seed =", rf_seed_value, "\n")


# -------------------------
# 8. Evaluation function
# -------------------------

evaluate_predictions <- function(test_data,
                                 predicted_probability,
                                 threshold = 0.5,
                                 bias_level_name,
                                 model_name = "Random Forest") {
  
  eval_data <- test_data %>%
    mutate(
      predicted_probability = predicted_probability,
      y_pred = if_else(predicted_probability >= threshold, 1L, 0L),
      correct = if_else(y_pred == y, 1L, 0L),
      error = if_else(y_pred != y, 1L, 0L)
    )
  
  accuracy <- mean(eval_data$correct)
  error_rate <- mean(eval_data$error)
  
  group_metrics <- eval_data %>%
    group_by(group) %>%
    summarise(
      n = n(),
      actual_positive_n = sum(y == 1),
      predicted_positive_n = sum(y_pred == 1),
      true_positive_n = sum(y == 1 & y_pred == 1),
      error_n = sum(error == 1),
      positive_rate = mean(y_pred == 1),
      TPR = if_else(
        actual_positive_n > 0,
        true_positive_n / actual_positive_n,
        NA_real_
      ),
      group_error_rate = mean(error == 1),
      .groups = "drop"
    ) %>%
    mutate(
      dataset = "Adult",
      model = model_name,
      bias_level = bias_level_name
    ) %>%
    select(
      dataset,
      model,
      bias_level,
      group,
      n,
      actual_positive_n,
      predicted_positive_n,
      true_positive_n,
      error_n,
      positive_rate,
      TPR,
      group_error_rate
    )
  
  male_metrics <- group_metrics %>%
    filter(group == "Male")
  
  female_metrics <- group_metrics %>%
    filter(group == "Female")
  
  if (nrow(male_metrics) != 1 || nrow(female_metrics) != 1) {
    stop("Both Male and Female groups must exist in the fixed test set.")
  }
  
  PR_gap <- abs(male_metrics$positive_rate - female_metrics$positive_rate)
  TPR_gap <- abs(male_metrics$TPR - female_metrics$TPR)
  Error_gap <- abs(male_metrics$group_error_rate - female_metrics$group_error_rate)
  
  result_row <- data.frame(
    dataset = "Adult",
    model = model_name,
    bias_level = bias_level_name,
    accuracy = accuracy,
    error_rate = error_rate,
    PR_gap = PR_gap,
    TPR_gap = TPR_gap,
    Error_gap = Error_gap,
    threshold = threshold,
    stringsAsFactors = FALSE
  )
  
  return(
    list(
      result_row = result_row,
      group_metrics = group_metrics,
      predictions = eval_data
    )
  )
}


# -------------------------
# 9. Train and evaluate one Random Forest model
# -------------------------

train_and_evaluate_rf <- function(train_data,
                                  test_data,
                                  rf_formula,
                                  bias_level_name,
                                  ntree_value = 500,
                                  mtry_value = 2,
                                  rf_seed_value = 123,
                                  threshold = 0.5) {
  
  set.seed(rf_seed_value)
  
  rf_model <- randomForest(
    formula = rf_formula,
    data = train_data,
    ntree = ntree_value,
    mtry = mtry_value,
    importance = TRUE
  )
  
  predicted_prob_matrix <- predict(
    rf_model,
    newdata = test_data,
    type = "prob"
  )
  
  if (!("1" %in% colnames(predicted_prob_matrix))) {
    stop("Predicted probability matrix does not contain class '1'.")
  }
  
  predicted_probability <- predicted_prob_matrix[, "1"]
  
  evaluation <- evaluate_predictions(
    test_data = test_data,
    predicted_probability = predicted_probability,
    threshold = threshold,
    bias_level_name = bias_level_name,
    model_name = "Random Forest"
  )
  
  # Model settings report
  model_settings_report <- data.frame(
    dataset = "Adult",
    model = "Random Forest",
    bias_level = bias_level_name,
    ntree = ntree_value,
    mtry = mtry_value,
    rf_seed = rf_seed_value,
    oob_error = round(
      rf_model$err.rate[ntree_value, "OOB"],
      4
    ),
    n_original_predictors = length(predictors),
    predictors_used = paste(predictors, collapse = ", "),
    stringsAsFactors = FALSE
  )
  
  # Variable importance report
  importance_matrix <- importance(rf_model)
  
  variable_importance_report <- data.frame(
    dataset = "Adult",
    model = "Random Forest",
    bias_level = bias_level_name,
    variable = rownames(importance_matrix),
    importance_matrix,
    row.names = NULL,
    check.names = FALSE
  ) %>%
    arrange(desc(MeanDecreaseGini))
  
  return(
    list(
      model = rf_model,
      result_row = evaluation$result_row,
      group_metrics = evaluation$group_metrics,
      predictions = evaluation$predictions,
      model_settings_report = model_settings_report,
      variable_importance_report = variable_importance_report
    )
  )
}


# -------------------------
# 10. Train Random Forest models for all bias levels
# -------------------------

rf_balanced <- train_and_evaluate_rf(
  train_data = adult_train_balanced,
  test_data = adult_test_fixed,
  rf_formula = rf_formula,
  bias_level_name = "balanced",
  ntree_value = ntree_value,
  mtry_value = mtry_value,
  rf_seed_value = rf_seed_value,
  threshold = 0.5
)

rf_mild <- train_and_evaluate_rf(
  train_data = adult_train_mild,
  test_data = adult_test_fixed,
  rf_formula = rf_formula,
  bias_level_name = "mild",
  ntree_value = ntree_value,
  mtry_value = mtry_value,
  rf_seed_value = rf_seed_value,
  threshold = 0.5
)

rf_moderate <- train_and_evaluate_rf(
  train_data = adult_train_moderate,
  test_data = adult_test_fixed,
  rf_formula = rf_formula,
  bias_level_name = "moderate",
  ntree_value = ntree_value,
  mtry_value = mtry_value,
  rf_seed_value = rf_seed_value,
  threshold = 0.5
)

rf_severe <- train_and_evaluate_rf(
  train_data = adult_train_severe,
  test_data = adult_test_fixed,
  rf_formula = rf_formula,
  bias_level_name = "severe",
  ntree_value = ntree_value,
  mtry_value = mtry_value,
  rf_seed_value = rf_seed_value,
  threshold = 0.5
)


# -------------------------
# 11. Combine Random Forest results
# -------------------------

bias_level_order <- c("balanced", "mild", "moderate", "severe")

adult_random_forest_results <- bind_rows(
  rf_balanced$result_row,
  rf_mild$result_row,
  rf_moderate$result_row,
  rf_severe$result_row
) %>%
  mutate(
    bias_level = factor(bias_level, levels = bias_level_order)
  ) %>%
  arrange(bias_level)

adult_random_forest_group_metrics <- bind_rows(
  rf_balanced$group_metrics,
  rf_mild$group_metrics,
  rf_moderate$group_metrics,
  rf_severe$group_metrics
) %>%
  mutate(
    bias_level = factor(bias_level, levels = bias_level_order)
  ) %>%
  arrange(bias_level, group)

adult_random_forest_model_settings_report <- bind_rows(
  rf_balanced$model_settings_report,
  rf_mild$model_settings_report,
  rf_moderate$model_settings_report,
  rf_severe$model_settings_report
) %>%
  mutate(
    bias_level = factor(bias_level, levels = bias_level_order)
  ) %>%
  arrange(bias_level)

adult_random_forest_variable_importance_report <- bind_rows(
  rf_balanced$variable_importance_report,
  rf_mild$variable_importance_report,
  rf_moderate$variable_importance_report,
  rf_severe$variable_importance_report
) %>%
  mutate(
    bias_level = factor(bias_level, levels = bias_level_order)
  ) %>%
  arrange(bias_level, desc(MeanDecreaseGini))


# -------------------------
# 12. Create report versions
# -------------------------

adult_random_forest_results_report <- adult_random_forest_results %>%
  mutate(
    accuracy = round(accuracy, 4),
    error_rate = round(error_rate, 4),
    PR_gap = round(PR_gap, 4),
    TPR_gap = round(TPR_gap, 4),
    Error_gap = round(Error_gap, 4)
  )

adult_random_forest_group_metrics_report <- adult_random_forest_group_metrics %>%
  mutate(
    positive_rate = round(positive_rate, 4),
    TPR = round(TPR, 4),
    group_error_rate = round(group_error_rate, 4)
  )

# Round numeric importance columns for report version
adult_random_forest_variable_importance_report_rounded <- adult_random_forest_variable_importance_report %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )


# -------------------------
# 13. Create delta table
# -------------------------

baseline_row <- adult_random_forest_results %>%
  filter(bias_level == "balanced")

adult_random_forest_delta_results <- adult_random_forest_results %>%
  filter(bias_level != "balanced") %>%
  mutate(
    delta_accuracy = accuracy - baseline_row$accuracy,
    delta_error_rate = error_rate - baseline_row$error_rate,
    delta_PR_gap = PR_gap - baseline_row$PR_gap,
    delta_TPR_gap = TPR_gap - baseline_row$TPR_gap,
    delta_Error_gap = Error_gap - baseline_row$Error_gap
  ) %>%
  select(
    dataset,
    model,
    bias_level,
    delta_accuracy,
    delta_error_rate,
    delta_PR_gap,
    delta_TPR_gap,
    delta_Error_gap
  )

adult_random_forest_delta_results_report <- adult_random_forest_delta_results %>%
  mutate(
    delta_accuracy = round(delta_accuracy, 4),
    delta_error_rate = round(delta_error_rate, 4),
    delta_PR_gap = round(delta_PR_gap, 4),
    delta_TPR_gap = round(delta_TPR_gap, 4),
    delta_Error_gap = round(delta_Error_gap, 4)
  )


# -------------------------
# 14. Save result tables
# -------------------------

write.csv(
  adult_random_forest_results,
  file.path(output_dir, "adult_random_forest_results.csv"),
  row.names = FALSE
)

write.csv(
  adult_random_forest_results_report,
  file.path(output_dir, "adult_random_forest_results_report.csv"),
  row.names = FALSE
)

write.csv(
  adult_random_forest_delta_results,
  file.path(output_dir, "adult_random_forest_delta_results.csv"),
  row.names = FALSE
)

write.csv(
  adult_random_forest_delta_results_report,
  file.path(output_dir, "adult_random_forest_delta_results_report.csv"),
  row.names = FALSE
)

write.csv(
  adult_random_forest_group_metrics,
  file.path(output_dir, "adult_random_forest_group_metrics.csv"),
  row.names = FALSE
)

write.csv(
  adult_random_forest_group_metrics_report,
  file.path(output_dir, "adult_random_forest_group_metrics_report.csv"),
  row.names = FALSE
)

write.csv(
  adult_random_forest_model_settings_report,
  file.path(output_dir, "adult_random_forest_model_settings_report.csv"),
  row.names = FALSE
)

write.csv(
  adult_random_forest_variable_importance_report,
  file.path(output_dir, "adult_random_forest_variable_importance.csv"),
  row.names = FALSE
)

write.csv(
  adult_random_forest_variable_importance_report_rounded,
  file.path(output_dir, "adult_random_forest_variable_importance_report.csv"),
  row.names = FALSE
)


# -------------------------
# 15. Plot raw fairness gaps
# -------------------------

adult_rf_fairness_long <- adult_random_forest_results %>%
  select(bias_level, PR_gap, TPR_gap, Error_gap) %>%
  pivot_longer(
    cols = c(PR_gap, TPR_gap, Error_gap),
    names_to = "fairness_metric",
    values_to = "gap_value"
  ) %>%
  mutate(
    bias_level = factor(bias_level, levels = bias_level_order),
    fairness_metric = factor(
      fairness_metric,
      levels = c("PR_gap", "TPR_gap", "Error_gap"),
      labels = c("PR gap", "TPR gap", "Error gap")
    )
  )

fig_adult_rf_fairness_gaps <- ggplot(
  adult_rf_fairness_long,
  aes(x = bias_level, y = gap_value, group = fairness_metric)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  facet_wrap(~ fairness_metric, nrow = 1, scales = "free_y") +
  labs(
    title = "Adult Random Forest fairness gaps across sampling-bias levels",
    x = "Sampling-bias level",
    y = "Fairness gap"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

ggsave(
  filename = file.path(output_dir, "fig_adult_random_forest_fairness_gaps.png"),
  plot = fig_adult_rf_fairness_gaps,
  width = 9,
  height = 4.5,
  dpi = 300
)


# -------------------------
# 16. Plot delta fairness gaps
# -------------------------

adult_rf_delta_long <- adult_random_forest_delta_results %>%
  select(bias_level, delta_PR_gap, delta_TPR_gap, delta_Error_gap) %>%
  pivot_longer(
    cols = c(delta_PR_gap, delta_TPR_gap, delta_Error_gap),
    names_to = "fairness_metric",
    values_to = "delta_gap_value"
  ) %>%
  mutate(
    bias_level = factor(
      bias_level,
      levels = c("mild", "moderate", "severe")
    ),
    fairness_metric = factor(
      fairness_metric,
      levels = c("delta_PR_gap", "delta_TPR_gap", "delta_Error_gap"),
      labels = c("Delta PR gap", "Delta TPR gap", "Delta Error gap")
    )
  )

fig_adult_rf_delta_fairness_gaps <- ggplot(
  adult_rf_delta_long,
  aes(x = bias_level, y = delta_gap_value, group = fairness_metric)
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  facet_wrap(~ fairness_metric, nrow = 1, scales = "free_y") +
  labs(
    title = "Adult Random Forest change in fairness gaps from balanced baseline",
    x = "Sampling-bias level",
    y = "Change from balanced baseline"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

ggsave(
  filename = file.path(output_dir, "fig_adult_random_forest_delta_fairness_gaps.png"),
  plot = fig_adult_rf_delta_fairness_gaps,
  width = 9,
  height = 4.5,
  dpi = 300
)


# -------------------------
# 17. Print final checks
# -------------------------

cat("\n=====================================\n")
cat("12 Adult Random Forest completed.\n")
cat("=====================================\n\n")

cat("Sampling input directory:\n")
cat(sampling_input_dir, "\n\n")

cat("Predictor file:\n")
cat(predictor_path, "\n\n")

cat("Output directory:\n")
cat(output_dir, "\n\n")

cat("Main predictors:\n")
print(predictors)

cat("\nRandom Forest formula:\n")
print(rf_formula)

cat("\nRandom Forest settings:\n")
cat("ntree   =", ntree_value, "\n")
cat("mtry    =", mtry_value, "\n")
cat("rf seed =", rf_seed_value, "\n\n")

cat("Main Random Forest results:\n")
print(adult_random_forest_results_report)

cat("\nDelta results from balanced baseline:\n")
print(adult_random_forest_delta_results_report)

cat("\nGroup-level metrics:\n")
print(adult_random_forest_group_metrics_report)

cat("\nRandom Forest model settings report:\n")
print(adult_random_forest_model_settings_report)

cat("\nFiles saved successfully.\n")
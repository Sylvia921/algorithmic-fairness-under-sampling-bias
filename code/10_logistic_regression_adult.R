# =========================================================
# 10_logistic_regression_adult.R
# Adult dataset: Logistic Regression under sampling bias
# =========================================================


# -------------------------
# 1. Set up environment
# -------------------------

rm(list = ls())
# Identify the project root
here::i_am("code/10_logistic_regression_adult.R")

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)

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
  "10"
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

cat("\nMain predictors used in Adult Logistic Regression:\n")
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
# 6. Prepare data types
# -------------------------

prepare_adult_model_data <- function(data) {
  
  data <- data %>%
    mutate(
      y = as.integer(y),
      group = as.factor(group),
      sex = as.factor(sex),
      income = as.factor(income),
      row_id = as.integer(row_id),
      
      age = as.numeric(age),
      education_num = as.numeric(education_num),
      capital_gain = as.numeric(capital_gain),
      capital_loss = as.numeric(capital_loss),
      hours_per_week = as.numeric(hours_per_week),
      
      workclass = as.factor(workclass),
      occupation = as.factor(occupation)
    )
  
  return(data)
}

adult_test_fixed <- prepare_adult_model_data(adult_test_fixed)
adult_train_balanced <- prepare_adult_model_data(adult_train_balanced)
adult_train_mild <- prepare_adult_model_data(adult_train_mild)
adult_train_moderate <- prepare_adult_model_data(adult_train_moderate)
adult_train_severe <- prepare_adult_model_data(adult_train_severe)


# -------------------------
# 7. Create model matrix with consistent dummy columns
# -------------------------
# Logistic Regression requires numeric model inputs.
# Categorical predictors are converted into dummy variables.
# For each bias level, training set and fixed test set are processed together
# so that they use exactly the same dummy columns.

create_lr_model_matrices <- function(train_data, test_data, predictors) {
  
  train_data$.dataset_split <- "train"
  test_data$.dataset_split <- "test"
  
  combined_data <- bind_rows(train_data, test_data)
  
  formula_text <- paste("~", paste(predictors, collapse = " + "))
  model_formula <- as.formula(formula_text)
  
  x_all <- model.matrix(model_formula, data = combined_data)
  
  # Remove intercept
  if ("(Intercept)" %in% colnames(x_all)) {
    x_all <- x_all[, colnames(x_all) != "(Intercept)", drop = FALSE]
  }
  
  train_index <- combined_data$.dataset_split == "train"
  test_index <- combined_data$.dataset_split == "test"
  
  x_train <- x_all[train_index, , drop = FALSE]
  x_test <- x_all[test_index, , drop = FALSE]
  
  # Remove columns with zero variance in the training data.
  # This avoids singular dummy variables caused by categories appearing only in the test set.
  non_constant_columns <- apply(x_train, 2, function(col) length(unique(col)) > 1)
  
  x_train <- x_train[, non_constant_columns, drop = FALSE]
  x_test <- x_test[, non_constant_columns, drop = FALSE]
  
  train_model_data <- data.frame(
    y = train_data$y,
    x_train,
    check.names = FALSE
  )
  
  test_model_data <- data.frame(
    x_test,
    check.names = FALSE
  )
  
  return(
    list(
      train_model_data = train_model_data,
      test_model_data = test_model_data,
      feature_names = colnames(x_train)
    )
  )
}


# -------------------------
# 8. Evaluation function
# -------------------------

evaluate_predictions <- function(test_data,
                                 predicted_probability,
                                 threshold = 0.5,
                                 bias_level_name,
                                 model_name = "Logistic Regression") {
  
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
# 9. Train and evaluate one Logistic Regression model
# -------------------------

train_and_evaluate_lr <- function(train_data,
                                  test_data,
                                  predictors,
                                  bias_level_name,
                                  threshold = 0.5) {
  
  matrix_objects <- create_lr_model_matrices(
    train_data = train_data,
    test_data = test_data,
    predictors = predictors
  )
  
  train_model_data <- matrix_objects$train_model_data
  test_model_data <- matrix_objects$test_model_data
  feature_names <- matrix_objects$feature_names
  
  lr_model <- glm(
    y ~ .,
    data = train_model_data,
    family = binomial()
  )
  
  predicted_probability <- predict(
    lr_model,
    newdata = test_model_data,
    type = "response"
  )
  
  evaluation <- evaluate_predictions(
    test_data = test_data,
    predicted_probability = predicted_probability,
    threshold = threshold,
    bias_level_name = bias_level_name,
    model_name = "Logistic Regression"
  )
  
  feature_report <- data.frame(
    dataset = "Adult",
    model = "Logistic Regression",
    bias_level = bias_level_name,
    feature_name = feature_names,
    n_model_features = length(feature_names),
    stringsAsFactors = FALSE
  )
  
  return(
    list(
      model = lr_model,
      result_row = evaluation$result_row,
      group_metrics = evaluation$group_metrics,
      predictions = evaluation$predictions,
      feature_report = feature_report
    )
  )
}


# -------------------------
# 10. Train LR models for all bias levels
# -------------------------

lr_balanced <- train_and_evaluate_lr(
  train_data = adult_train_balanced,
  test_data = adult_test_fixed,
  predictors = predictors,
  bias_level_name = "balanced",
  threshold = 0.5
)

lr_mild <- train_and_evaluate_lr(
  train_data = adult_train_mild,
  test_data = adult_test_fixed,
  predictors = predictors,
  bias_level_name = "mild",
  threshold = 0.5
)

lr_moderate <- train_and_evaluate_lr(
  train_data = adult_train_moderate,
  test_data = adult_test_fixed,
  predictors = predictors,
  bias_level_name = "moderate",
  threshold = 0.5
)

lr_severe <- train_and_evaluate_lr(
  train_data = adult_train_severe,
  test_data = adult_test_fixed,
  predictors = predictors,
  bias_level_name = "severe",
  threshold = 0.5
)


# -------------------------
# 11. Combine LR results
# -------------------------

bias_level_order <- c("balanced", "mild", "moderate", "severe")

adult_logistic_regression_results <- bind_rows(
  lr_balanced$result_row,
  lr_mild$result_row,
  lr_moderate$result_row,
  lr_severe$result_row
) %>%
  mutate(
    bias_level = factor(bias_level, levels = bias_level_order)
  ) %>%
  arrange(bias_level)

adult_logistic_regression_group_metrics <- bind_rows(
  lr_balanced$group_metrics,
  lr_mild$group_metrics,
  lr_moderate$group_metrics,
  lr_severe$group_metrics
) %>%
  mutate(
    bias_level = factor(bias_level, levels = bias_level_order)
  ) %>%
  arrange(bias_level, group)

adult_logistic_regression_model_features_report <- bind_rows(
  lr_balanced$feature_report,
  lr_mild$feature_report,
  lr_moderate$feature_report,
  lr_severe$feature_report
) %>%
  mutate(
    bias_level = factor(bias_level, levels = bias_level_order)
  ) %>%
  arrange(bias_level, feature_name)


# -------------------------
# 12. Create report versions
# -------------------------

adult_logistic_regression_results_report <- adult_logistic_regression_results %>%
  mutate(
    accuracy = round(accuracy, 4),
    error_rate = round(error_rate, 4),
    PR_gap = round(PR_gap, 4),
    TPR_gap = round(TPR_gap, 4),
    Error_gap = round(Error_gap, 4)
  )

adult_logistic_regression_group_metrics_report <- adult_logistic_regression_group_metrics %>%
  mutate(
    positive_rate = round(positive_rate, 4),
    TPR = round(TPR, 4),
    group_error_rate = round(group_error_rate, 4)
  )


# -------------------------
# 13. Create delta table
# -------------------------

baseline_row <- adult_logistic_regression_results %>%
  filter(bias_level == "balanced")

adult_logistic_regression_delta_results <- adult_logistic_regression_results %>%
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

adult_logistic_regression_delta_results_report <- adult_logistic_regression_delta_results %>%
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
  adult_logistic_regression_results,
  file.path(output_dir, "adult_logistic_regression_results.csv"),
  row.names = FALSE
)

write.csv(
  adult_logistic_regression_results_report,
  file.path(output_dir, "adult_logistic_regression_results_report.csv"),
  row.names = FALSE
)

write.csv(
  adult_logistic_regression_delta_results,
  file.path(output_dir, "adult_logistic_regression_delta_results.csv"),
  row.names = FALSE
)

write.csv(
  adult_logistic_regression_delta_results_report,
  file.path(output_dir, "adult_logistic_regression_delta_results_report.csv"),
  row.names = FALSE
)

write.csv(
  adult_logistic_regression_group_metrics,
  file.path(output_dir, "adult_logistic_regression_group_metrics.csv"),
  row.names = FALSE
)

write.csv(
  adult_logistic_regression_group_metrics_report,
  file.path(output_dir, "adult_logistic_regression_group_metrics_report.csv"),
  row.names = FALSE
)

write.csv(
  adult_logistic_regression_model_features_report,
  file.path(output_dir, "adult_logistic_regression_model_features_report.csv"),
  row.names = FALSE
)


# -------------------------
# 15. Plot raw fairness gaps
# -------------------------

adult_lr_fairness_long <- adult_logistic_regression_results %>%
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

fig_adult_lr_fairness_gaps <- ggplot(
  adult_lr_fairness_long,
  aes(x = bias_level, y = gap_value, group = fairness_metric)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  facet_wrap(~ fairness_metric, nrow = 1, scales = "free_y") +
  labs(
    title = "Adult Logistic Regression fairness gaps across sampling-bias levels",
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
  filename = file.path(output_dir, "fig_adult_logistic_regression_fairness_gaps.png"),
  plot = fig_adult_lr_fairness_gaps,
  width = 9,
  height = 4.5,
  dpi = 300
)


# -------------------------
# 16. Plot delta fairness gaps
# -------------------------

adult_lr_delta_long <- adult_logistic_regression_delta_results %>%
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

fig_adult_lr_delta_fairness_gaps <- ggplot(
  adult_lr_delta_long,
  aes(x = bias_level, y = delta_gap_value, group = fairness_metric)
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.2) +
  facet_wrap(~ fairness_metric, nrow = 1, scales = "free_y") +
  labs(
    title = "Adult Logistic Regression change in fairness gaps from balanced baseline",
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
  filename = file.path(output_dir, "fig_adult_logistic_regression_delta_fairness_gaps.png"),
  plot = fig_adult_lr_delta_fairness_gaps,
  width = 9,
  height = 4.5,
  dpi = 300
)


# -------------------------
# 17. Print final checks
# -------------------------

cat("\n========================================\n")
cat("10 Adult Logistic Regression completed.\n")
cat("========================================\n\n")

cat("Sampling input directory:\n")
cat(sampling_input_dir, "\n\n")

cat("Predictor file:\n")
cat(predictor_path, "\n\n")

cat("Output directory:\n")
cat(output_dir, "\n\n")

cat("Main predictors:\n")
print(predictors)

cat("\nMain Logistic Regression results:\n")
print(adult_logistic_regression_results_report)

cat("\nDelta results from balanced baseline:\n")
print(adult_logistic_regression_delta_results_report)

cat("\nGroup-level metrics:\n")
print(adult_logistic_regression_group_metrics_report)

cat("\nFiles saved successfully.\n")
# =========================================================
# 13_cross_seed_adult_experiment.R
# Adult dataset: Cross-seed repetition for LR / DT / RF
# Simplified version: only output mean results and mean delta results
# =========================================================


# -------------------------
# 1. Set up environment
# -------------------------

rm(list = ls())
# Identify the project root
here::i_am("code/13_cross_seed_adult_experiment.R")

library(dplyr)
library(readr)
library(stringr)
library(rpart)
library(randomForest)

set.seed(123)


# -------------------------
# 2. Define input and output paths
# -------------------------

adult_clean_path <- here::here(
  "outputs",
  "adult",
  "08",
  "adult_clean.csv"
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
  "13"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# -------------------------
# 3. Experiment settings
# -------------------------

# For testing the code quickly, use:
# seeds <- 1:3
# For final results, use:
seeds <- 1:30

reference_group <- "Male"
underrepresented_group <- "Female"

test_male_n <- 2500
test_female_n <- 2500

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

bias_level_order <- c("balanced", "mild", "moderate", "severe")
model_order <- c("Logistic Regression", "Decision Tree", "Random Forest")

threshold_value <- 0.5

# Decision Tree settings
dt_cp <- 0.001
dt_minsplit <- 30
dt_minbucket <- 10
dt_maxdepth <- 10
dt_xval <- 10

# Random Forest settings
rf_ntree <- 500
rf_mtry <- 2


# -------------------------
# 4. Read data and predictors
# -------------------------

adult_clean <- read.csv(
  adult_clean_path,
  stringsAsFactors = FALSE
)

adult_main_predictors <- read.csv(
  predictor_path,
  stringsAsFactors = FALSE
)

predictors <- adult_main_predictors$predictor

required_columns <- c(
  predictors,
  "y",
  "group",
  "sex",
  "income",
  "row_id"
)

missing_columns <- setdiff(required_columns, names(adult_clean))

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing columns in adult_clean.csv:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

adult_clean <- adult_clean %>%
  mutate(
    group = str_trim(as.character(group)),
    sex = str_trim(as.character(sex)),
    income = str_trim(as.character(income)),
    y = as.integer(y),
    row_id = as.integer(row_id)
  ) %>%
  filter(group %in% c(reference_group, underrepresented_group)) %>%
  filter(y %in% c(0, 1))


# -------------------------
# 5. Check sample availability
# -------------------------

available_summary <- adult_clean %>%
  group_by(group) %>%
  summarise(
    available_n = n(),
    y_0_n = sum(y == 0),
    y_1_n = sum(y == 1),
    y_1_rate = round(mean(y == 1), 4),
    .groups = "drop"
  )

print(available_summary)

available_male_n <- available_summary$available_n[
  available_summary$group == reference_group
]

available_female_n <- available_summary$available_n[
  available_summary$group == underrepresented_group
]

required_male_n <- test_male_n + max(bias_design$male_n)
required_female_n <- test_female_n + max(bias_design$female_n)

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
# 6. Prepare consistent factor levels
# -------------------------

workclass_levels <- sort(unique(adult_clean$workclass))
occupation_levels <- sort(unique(adult_clean$occupation))
group_levels <- c("Female", "Male")
income_levels <- c("<=50K", ">50K")

prepare_adult_data <- function(data) {
  
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


# -------------------------
# 7. Create fixed test set and train pool for one seed
# -------------------------

create_train_pool_and_test <- function(adult_data, experiment_seed) {
  
  set.seed(experiment_seed + 10000)
  
  male_all <- adult_data %>%
    filter(group == reference_group)
  
  female_all <- adult_data %>%
    filter(group == underrepresented_group)
  
  test_male <- male_all %>%
    slice_sample(n = test_male_n)
  
  test_female <- female_all %>%
    slice_sample(n = test_female_n)
  
  test_fixed <- bind_rows(test_male, test_female) %>%
    slice_sample(prop = 1) %>%
    mutate(set_type = "test_fixed")
  
  test_row_ids <- test_fixed$row_id
  
  train_pool <- adult_data %>%
    filter(!(row_id %in% test_row_ids)) %>%
    mutate(set_type = "train_pool")
  
  return(
    list(
      test_fixed = test_fixed,
      train_pool = train_pool
    )
  )
}


# -------------------------
# 8. Create one biased training set
# -------------------------

create_training_set <- function(train_pool,
                                male_n,
                                female_n,
                                bias_level_name,
                                sampling_seed) {
  
  set.seed(sampling_seed)
  
  pool_male <- train_pool %>%
    filter(group == reference_group)
  
  pool_female <- train_pool %>%
    filter(group == underrepresented_group)
  
  sampled_male <- pool_male %>%
    slice_sample(n = male_n)
  
  sampled_female <- pool_female %>%
    slice_sample(n = female_n)
  
  train_data <- bind_rows(sampled_male, sampled_female) %>%
    slice_sample(prop = 1) %>%
    mutate(
      bias_level = bias_level_name,
      set_type = paste0("train_", bias_level_name)
    )
  
  return(train_data)
}


# -------------------------
# 9. Evaluation function
# -------------------------

evaluate_predictions <- function(test_data,
                                 predicted_probability,
                                 experiment_seed,
                                 model_name,
                                 bias_level_name,
                                 threshold = 0.5) {
  
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
    seed = experiment_seed,
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
  
  return(result_row)
}


# -------------------------
# 10. Logistic Regression helper
# -------------------------

create_lr_model_matrices <- function(train_data, test_data, predictors) {
  
  train_data$.dataset_split <- "train"
  test_data$.dataset_split <- "test"
  
  combined_data <- bind_rows(train_data, test_data)
  
  formula_text <- paste("~", paste(predictors, collapse = " + "))
  model_formula <- as.formula(formula_text)
  
  x_all <- model.matrix(model_formula, data = combined_data)
  
  if ("(Intercept)" %in% colnames(x_all)) {
    x_all <- x_all[, colnames(x_all) != "(Intercept)", drop = FALSE]
  }
  
  colnames(x_all) <- make.names(colnames(x_all), unique = TRUE)
  
  train_index <- combined_data$.dataset_split == "train"
  test_index <- combined_data$.dataset_split == "test"
  
  x_train <- x_all[train_index, , drop = FALSE]
  x_test <- x_all[test_index, , drop = FALSE]
  
  non_constant_columns <- apply(
    x_train,
    2,
    function(col) length(unique(col)) > 1
  )
  
  x_train <- x_train[, non_constant_columns, drop = FALSE]
  x_test <- x_test[, non_constant_columns, drop = FALSE]
  
  train_model_data <- data.frame(
    y = train_data$y,
    x_train,
    check.names = TRUE
  )
  
  test_model_data <- data.frame(
    x_test,
    check.names = TRUE
  )
  
  return(
    list(
      train_model_data = train_model_data,
      test_model_data = test_model_data
    )
  )
}

run_logistic_regression <- function(train_data,
                                    test_data,
                                    experiment_seed,
                                    bias_level_name) {
  
  matrix_objects <- create_lr_model_matrices(
    train_data = train_data,
    test_data = test_data,
    predictors = predictors
  )
  
  lr_model <- glm(
    y ~ .,
    data = matrix_objects$train_model_data,
    family = binomial()
  )
  
  predicted_probability <- predict(
    lr_model,
    newdata = matrix_objects$test_model_data,
    type = "response"
  )
  
  result_row <- evaluate_predictions(
    test_data = test_data,
    predicted_probability = predicted_probability,
    experiment_seed = experiment_seed,
    model_name = "Logistic Regression",
    bias_level_name = bias_level_name,
    threshold = threshold_value
  )
  
  return(result_row)
}


# -------------------------
# 11. Decision Tree helper
# -------------------------

run_decision_tree <- function(train_data,
                              test_data,
                              experiment_seed,
                              bias_level_name) {
  
  formula_text <- paste(
    "y_factor ~",
    paste(predictors, collapse = " + ")
  )
  
  tree_formula <- as.formula(formula_text)
  
  tree_model <- rpart(
    formula = tree_formula,
    data = train_data,
    method = "class",
    control = rpart.control(
      cp = dt_cp,
      minsplit = dt_minsplit,
      minbucket = dt_minbucket,
      maxdepth = dt_maxdepth,
      xval = dt_xval
    )
  )
  
  predicted_prob_matrix <- predict(
    tree_model,
    newdata = test_data,
    type = "prob"
  )
  
  predicted_probability <- predicted_prob_matrix[, "1"]
  
  result_row <- evaluate_predictions(
    test_data = test_data,
    predicted_probability = predicted_probability,
    experiment_seed = experiment_seed,
    model_name = "Decision Tree",
    bias_level_name = bias_level_name,
    threshold = threshold_value
  )
  
  return(result_row)
}


# -------------------------
# 12. Random Forest helper
# -------------------------
# Important:
# experiment_seed identifies the repeated experiment.
# rf_seed controls Random Forest internal randomness.
# The same RF seed is used across bias levels within one experiment seed,
# while RF randomness changes across experiment seeds.

run_random_forest <- function(train_data,
                              test_data,
                              experiment_seed,
                              rf_seed,
                              bias_level_name) {
  
  set.seed(rf_seed)
  
  formula_text <- paste(
    "y_factor ~",
    paste(predictors, collapse = " + ")
  )
  
  rf_formula <- as.formula(formula_text)
  
  rf_model <- randomForest(
    formula = rf_formula,
    data = train_data,
    ntree = rf_ntree,
    mtry = rf_mtry,
    importance = TRUE
  )
  
  predicted_prob_matrix <- predict(
    rf_model,
    newdata = test_data,
    type = "prob"
  )
  
  predicted_probability <- predicted_prob_matrix[, "1"]
  
  result_row <- evaluate_predictions(
    test_data = test_data,
    predicted_probability = predicted_probability,
    experiment_seed = experiment_seed,
    model_name = "Random Forest",
    bias_level_name = bias_level_name,
    threshold = threshold_value
  )
  
  return(result_row)
}


# -------------------------
# 13. Run one full seed experiment
# -------------------------

run_one_seed <- function(experiment_seed) {
  
  split_data <- create_train_pool_and_test(
    adult_data = adult_clean,
    experiment_seed = experiment_seed
  )
  
  test_fixed <- prepare_adult_data(split_data$test_fixed)
  train_pool <- prepare_adult_data(split_data$train_pool)
  
  seed_results <- data.frame()
  
  for (i in 1:nrow(bias_design)) {
    
    current_bias_level <- bias_design$bias_level[i]
    current_male_n <- bias_design$male_n[i]
    current_female_n <- bias_design$female_n[i]
    
    train_data <- create_training_set(
      train_pool = train_pool,
      male_n = current_male_n,
      female_n = current_female_n,
      bias_level_name = current_bias_level,
      sampling_seed = experiment_seed * 100 + i
    )
    
    train_data <- prepare_adult_data(train_data)
    
    lr_result <- run_logistic_regression(
      train_data = train_data,
      test_data = test_fixed,
      experiment_seed = experiment_seed,
      bias_level_name = current_bias_level
    )
    
    dt_result <- run_decision_tree(
      train_data = train_data,
      test_data = test_fixed,
      experiment_seed = experiment_seed,
      bias_level_name = current_bias_level
    )
    
    rf_result <- run_random_forest(
      train_data = train_data,
      test_data = test_fixed,
      experiment_seed = experiment_seed,
      rf_seed = experiment_seed * 1000,
      bias_level_name = current_bias_level
    )
    
    seed_results <- bind_rows(
      seed_results,
      lr_result,
      dt_result,
      rf_result
    )
  }
  
  return(seed_results)
}


# -------------------------
# 14. Run all seeds
# -------------------------

adult_cross_seed_raw_results <- data.frame()

for (current_seed in seeds) {
  
  cat("Running Adult cross-seed experiment, seed:", current_seed, "\n")
  
  current_seed_results <- run_one_seed(
    experiment_seed = current_seed
  )
  
  adult_cross_seed_raw_results <- bind_rows(
    adult_cross_seed_raw_results,
    current_seed_results
  )
}

adult_cross_seed_raw_results <- adult_cross_seed_raw_results %>%
  mutate(
    model = factor(model, levels = model_order),
    bias_level = factor(bias_level, levels = bias_level_order)
  ) %>%
  arrange(seed, model, bias_level)


# -------------------------
# 15. Cross-seed mean results
# -------------------------

adult_cross_seed_mean_results <- adult_cross_seed_raw_results %>%
  group_by(model, bias_level) %>%
  summarise(
    n_seeds = n_distinct(seed),
    
    mean_accuracy = mean(accuracy),
    sd_accuracy = sd(accuracy),
    
    mean_error_rate = mean(error_rate),
    sd_error_rate = sd(error_rate),
    
    mean_PR_gap = mean(PR_gap),
    sd_PR_gap = sd(PR_gap),
    
    mean_TPR_gap = mean(TPR_gap),
    sd_TPR_gap = sd(TPR_gap),
    
    mean_Error_gap = mean(Error_gap),
    sd_Error_gap = sd(Error_gap),
    
    .groups = "drop"
  ) %>%
  mutate(
    model = factor(model, levels = model_order),
    bias_level = factor(bias_level, levels = bias_level_order)
  ) %>%
  arrange(model, bias_level)

adult_cross_seed_mean_results_report <- adult_cross_seed_mean_results %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )

write.csv(
  adult_cross_seed_mean_results_report,
  file.path(output_dir, "adult_cross_seed_mean_results_report.csv"),
  row.names = FALSE
)


# -------------------------
# 16. Seed-level delta from balanced baseline
# -------------------------
# Delta must be calculated within each seed and model:
# delta = biased condition - balanced condition

adult_balanced_results <- adult_cross_seed_raw_results %>%
  filter(bias_level == "balanced") %>%
  select(
    seed,
    model,
    balanced_accuracy = accuracy,
    balanced_error_rate = error_rate,
    balanced_PR_gap = PR_gap,
    balanced_TPR_gap = TPR_gap,
    balanced_Error_gap = Error_gap
  )

adult_biased_results <- adult_cross_seed_raw_results %>%
  filter(bias_level %in% c("mild", "moderate", "severe")) %>%
  select(
    seed,
    model,
    bias_level,
    accuracy,
    error_rate,
    PR_gap,
    TPR_gap,
    Error_gap
  )

adult_cross_seed_delta_results <- adult_biased_results %>%
  left_join(
    adult_balanced_results,
    by = c("seed", "model")
  ) %>%
  mutate(
    delta_accuracy = accuracy - balanced_accuracy,
    delta_error_rate = error_rate - balanced_error_rate,
    delta_PR_gap = PR_gap - balanced_PR_gap,
    delta_TPR_gap = TPR_gap - balanced_TPR_gap,
    delta_Error_gap = Error_gap - balanced_Error_gap
  ) %>%
  select(
    seed,
    model,
    bias_level,
    delta_accuracy,
    delta_error_rate,
    delta_PR_gap,
    delta_TPR_gap,
    delta_Error_gap
  ) %>%
  mutate(
    model = factor(model, levels = model_order),
    bias_level = factor(bias_level, levels = c("mild", "moderate", "severe"))
  ) %>%
  arrange(seed, model, bias_level)


# -------------------------
# 17. Cross-seed mean delta results
# -------------------------

adult_cross_seed_mean_delta_results <- adult_cross_seed_delta_results %>%
  group_by(model, bias_level) %>%
  summarise(
    n_seeds = n_distinct(seed),
    
    mean_delta_accuracy = mean(delta_accuracy),
    sd_delta_accuracy = sd(delta_accuracy),
    
    mean_delta_error_rate = mean(delta_error_rate),
    sd_delta_error_rate = sd(delta_error_rate),
    
    mean_delta_PR_gap = mean(delta_PR_gap),
    sd_delta_PR_gap = sd(delta_PR_gap),
    
    mean_delta_TPR_gap = mean(delta_TPR_gap),
    sd_delta_TPR_gap = sd(delta_TPR_gap),
    
    mean_delta_Error_gap = mean(delta_Error_gap),
    sd_delta_Error_gap = sd(delta_Error_gap),
    
    .groups = "drop"
  ) %>%
  mutate(
    model = factor(model, levels = model_order),
    bias_level = factor(bias_level, levels = c("mild", "moderate", "severe"))
  ) %>%
  arrange(model, bias_level)

adult_cross_seed_mean_delta_results_report <- adult_cross_seed_mean_delta_results %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )

write.csv(
  adult_cross_seed_mean_delta_results_report,
  file.path(output_dir, "adult_cross_seed_mean_delta_results_report.csv"),
  row.names = FALSE
)


# -------------------------
# 18. Print final results
# -------------------------

cat("\n========================================\n")
cat("13 Adult cross-seed experiment completed.\n")
cat("========================================\n\n")

cat("Seeds used:\n")
print(seeds)

cat("\nMain predictors:\n")
print(predictors)

cat("\nBias design:\n")
print(bias_design)

cat("\nCross-seed mean results:\n")
print(adult_cross_seed_mean_results_report)

cat("\nCross-seed mean delta results:\n")
print(adult_cross_seed_mean_delta_results_report)

cat("\nOutput files saved to:\n")
cat(output_dir, "\n\n")

cat("Files saved:\n")
cat("adult_cross_seed_mean_results_report.csv\n")
cat("adult_cross_seed_mean_delta_results_report.csv\n")
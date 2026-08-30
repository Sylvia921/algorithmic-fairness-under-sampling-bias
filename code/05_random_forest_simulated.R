# =========================
# 05_random_forest_simulated.R
# =========================

# Identify the project root
here::i_am("code/05_random_forest_simulated.R")

set.seed(123)


# =========================
# Load packages
# =========================

library(randomForest)


# =========================
# Load data
# =========================

train_dir <- here::here(
  "outputs",
  "simulated",
  "02"
)

test_dir <- here::here(
  "outputs",
  "simulated",
  "01"
)

train_balanced <- read.csv(
  file.path(train_dir, "sim_train_balanced.csv")
)

train_mild <- read.csv(
  file.path(train_dir, "sim_train_mild.csv")
)

train_moderate <- read.csv(
  file.path(train_dir, "sim_train_moderate.csv")
)

train_severe <- read.csv(
  file.path(train_dir, "sim_train_severe.csv")
)

test_fixed <- read.csv(
  file.path(test_dir, "sim_test_fixed.csv")
)

train_balanced$group <- factor(train_balanced$group)
train_mild$group <- factor(train_mild$group)
train_moderate$group <- factor(train_moderate$group)
train_severe$group <- factor(train_severe$group)
test_fixed$group <- factor(test_fixed$group)

train_balanced$y <- factor(train_balanced$y, levels = c(0, 1))
train_mild$y <- factor(train_mild$y, levels = c(0, 1))
train_moderate$y <- factor(train_moderate$y, levels = c(0, 1))
train_severe$y <- factor(train_severe$y, levels = c(0, 1))
test_fixed$y <- factor(test_fixed$y, levels = c(0, 1))



# =========================
# Basic checks
# =========================

str(train_balanced)
str(test_fixed)

table(train_balanced$y)
table(test_fixed$y)

table(train_balanced$group)
table(test_fixed$group)


# =========================
# Evaluation function
# =========================

evaluate_model <- function(actual, predicted, group) {
  
  actual <- as.numeric(as.character(actual))
  predicted <- as.numeric(as.character(predicted))
  
  accuracy <- mean(actual == predicted)
  error_rate <- mean(actual != predicted)
  
  positive_rate <- tapply(predicted, group, mean)
  pr_gap <- abs(positive_rate["A"] - positive_rate["B"])
  
  tpr_A <- mean(predicted[group == "A" & actual == 1] == 1)
  tpr_B <- mean(predicted[group == "B" & actual == 1] == 1)
  tpr_gap <- abs(tpr_A - tpr_B)
  
  error_A <- mean(predicted[group == "A"] != actual[group == "A"])
  error_B <- mean(predicted[group == "B"] != actual[group == "B"])
  error_gap <- abs(error_A - error_B)
  
  results <- data.frame(
    accuracy = accuracy,
    error_rate = error_rate,
    PR_gap = pr_gap,
    TPR_gap = tpr_gap,
    Error_gap = error_gap
  )
  
  return(results)
}


# =========================
# Random Forest function
# =========================

run_random_forest <- function(train_data, test_data, bias_level, mtry_value, ntree_value = 500) {
  
  set.seed(123)
  
  model <- randomForest(
    y ~ x1 + x2 + x3,
    data = train_data,
    ntree = ntree_value,
    mtry = mtry_value,
    importance = TRUE
  )
  
  pred_prob <- predict(
    model,
    newdata = test_data,
    type = "prob"
  )[, "1"]
  
  pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
  
  eval_results <- evaluate_model(
    actual = test_data$y,
    predicted = pred_class,
    group = test_data$group
  )
  
  eval_results$dataset <- "simulated"
  eval_results$model <- "random_forest"
  eval_results$bias_level <- bias_level
  eval_results$mtry <- mtry_value
  eval_results$ntree <- ntree_value
  eval_results$oob_error <- model$err.rate[ntree_value, "OOB"]
  
  eval_results <- eval_results[, c(
    "dataset",
    "model",
    "bias_level",
    "mtry",
    "ntree",
    "oob_error",
    "accuracy",
    "error_rate",
    "PR_gap",
    "TPR_gap",
    "Error_gap"
  )]
  
  return(eval_results)
}


# =========================
# Function to run all bias levels
# =========================

run_all_bias_levels <- function(mtry_value, ntree_value = 500) {
  
  result_balanced <- run_random_forest(
    train_data = train_balanced,
    test_data = test_fixed,
    bias_level = "balanced",
    mtry_value = mtry_value,
    ntree_value = ntree_value
  )
  
  result_mild <- run_random_forest(
    train_data = train_mild,
    test_data = test_fixed,
    bias_level = "mild",
    mtry_value = mtry_value,
    ntree_value = ntree_value
  )
  
  result_moderate <- run_random_forest(
    train_data = train_moderate,
    test_data = test_fixed,
    bias_level = "moderate",
    mtry_value = mtry_value,
    ntree_value = ntree_value
  )
  
  result_severe <- run_random_forest(
    train_data = train_severe,
    test_data = test_fixed,
    bias_level = "severe",
    mtry_value = mtry_value,
    ntree_value = ntree_value
  )
  
  rf_results <- rbind(
    result_balanced,
    result_mild,
    result_moderate,
    result_severe
  )
  
  balanced_PR_gap <- rf_results$PR_gap[rf_results$bias_level == "balanced"]
  balanced_TPR_gap <- rf_results$TPR_gap[rf_results$bias_level == "balanced"]
  balanced_Error_gap <- rf_results$Error_gap[rf_results$bias_level == "balanced"]
  
  rf_results$delta_PR_gap <- rf_results$PR_gap - balanced_PR_gap
  rf_results$delta_TPR_gap <- rf_results$TPR_gap - balanced_TPR_gap
  rf_results$delta_Error_gap <- rf_results$Error_gap - balanced_Error_gap
  
  return(rf_results)
}


# =========================
# Main analysis: Random Forest with mtry = 1
# =========================

rf_results <- run_all_bias_levels(
  mtry_value = 1,
  ntree_value = 500
)

rf_results

output_dir <- here::here(
  "outputs",
  "simulated",
  "05"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  rf_results,
  file.path(output_dir, "sim_random_forest_results.csv"),
  row.names = FALSE
)


# =========================
# Appendix / robustness check: mtry = 2 and mtry = 3
# =========================

rf_results_mtry2 <- run_all_bias_levels(
  mtry_value = 2,
  ntree_value = 500
)

rf_results_mtry3 <- run_all_bias_levels(
  mtry_value = 3,
  ntree_value = 500
)

rf_results_appendix <- rbind(
  rf_results_mtry2,
  rf_results_mtry3
)

rf_results_mtry2
rf_results_mtry3
rf_results_appendix

write.csv(
  rf_results_appendix,
  file.path(
    output_dir,
    "sim_random_forest_results_appendix_mtry2_mtry3.csv"
  ),
  row.names = FALSE
)


# =========================
# Reproducibility information
# =========================

sessionInfo()
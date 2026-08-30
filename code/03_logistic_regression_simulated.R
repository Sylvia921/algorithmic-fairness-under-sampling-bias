# =========================
# 03_logistic_regression_simulated.R
# Train and evaluate Logistic Regression on simulated data
# =========================


# =========================
# 1. Load data
# =========================

# Identify the project root
here::i_am("code/03_logistic_regression_simulated.R")

set.seed(123)

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


# =========================
# Evaluation function
# =========================

evaluate_model <- function(actual, predicted, group) {
  
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
# Logistic Regression function
# =========================

run_logistic_regression <- function(train_data, test_data, bias_level) {
  
  model <- glm(
    y ~ x1 + x2 + x3,
    data = train_data,
    family = binomial
  )
  
  pred_prob <- predict(
    model,
    newdata = test_data,
    type = "response"
  )
  
  pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
  
  eval_results <- evaluate_model(
    actual = test_data$y,
    predicted = pred_class,
    group = test_data$group
  )
  
  eval_results$dataset <- "simulated"
  eval_results$model <- "logistic_regression"
  eval_results$bias_level <- bias_level
  
  eval_results <- eval_results[, c(
    "dataset",
    "model",
    "bias_level",
    "accuracy",
    "error_rate",
    "PR_gap",
    "TPR_gap",
    "Error_gap"
  )]
  
  return(eval_results)
}


# =========================
# Run Logistic Regression across bias levels
# =========================

result_balanced <- run_logistic_regression(
  train_data = train_balanced,
  test_data = test_fixed,
  bias_level = "balanced"
)

result_mild <- run_logistic_regression(
  train_data = train_mild,
  test_data = test_fixed,
  bias_level = "mild"
)

result_moderate <- run_logistic_regression(
  train_data = train_moderate,
  test_data = test_fixed,
  bias_level = "moderate"
)

result_severe <- run_logistic_regression(
  train_data = train_severe,
  test_data = test_fixed,
  bias_level = "severe"
)


# =========================
# Combine results
# =========================

lr_results <- rbind(
  result_balanced,
  result_mild,
  result_moderate,
  result_severe
)

lr_results


# =========================
# Add change from balanced condition
# =========================

balanced_PR_gap <- lr_results$PR_gap[lr_results$bias_level == "balanced"]
balanced_TPR_gap <- lr_results$TPR_gap[lr_results$bias_level == "balanced"]
balanced_Error_gap <- lr_results$Error_gap[lr_results$bias_level == "balanced"]

lr_results$delta_PR_gap <- lr_results$PR_gap - balanced_PR_gap
lr_results$delta_TPR_gap <- lr_results$TPR_gap - balanced_TPR_gap
lr_results$delta_Error_gap <- lr_results$Error_gap - balanced_Error_gap

lr_results


# =========================
# Save results
# =========================

output_dir <- here::here(
  "outputs",
  "simulated",
  "03"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  lr_results,
  file.path(output_dir, "sim_logistic_regression_results.csv"),
  row.names = FALSE
)


# =========================
# Reproducibility information
# =========================

sessionInfo()
# =========================
# 04_decision_tree_simulated.R
# =========================

# Identify the project root
here::i_am("code/04_decision_tree_simulated.R")

set.seed(123)

# =========================
# Load packages
# =========================

library(rpart)


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
# Decision Tree function
# =========================

run_decision_tree <- function(train_data, test_data, bias_level) {
  
  model <- rpart(
    y ~ x1 + x2 + x3,
    data = train_data,
    method = "class",
    control = rpart.control(
      cp = 0.01,
      minsplit = 20
    )
  )
  
  pred_class <- predict(
    model,
    newdata = test_data,
    type = "class"
  )
  
  eval_results <- evaluate_model(
    actual = test_data$y,
    predicted = pred_class,
    group = test_data$group
  )
  
  eval_results$dataset <- "simulated"
  eval_results$model <- "decision_tree"
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
# Run Decision Tree across bias levels
# =========================

result_balanced <- run_decision_tree(
  train_data = train_balanced,
  test_data = test_fixed,
  bias_level = "balanced"
)

result_mild <- run_decision_tree(
  train_data = train_mild,
  test_data = test_fixed,
  bias_level = "mild"
)

result_moderate <- run_decision_tree(
  train_data = train_moderate,
  test_data = test_fixed,
  bias_level = "moderate"
)

result_severe <- run_decision_tree(
  train_data = train_severe,
  test_data = test_fixed,
  bias_level = "severe"
)


# =========================
# Combine results
# =========================

dt_results <- rbind(
  result_balanced,
  result_mild,
  result_moderate,
  result_severe
)

dt_results


# =========================
# Add change from balanced condition
# =========================

balanced_PR_gap <- dt_results$PR_gap[dt_results$bias_level == "balanced"]
balanced_TPR_gap <- dt_results$TPR_gap[dt_results$bias_level == "balanced"]
balanced_Error_gap <- dt_results$Error_gap[dt_results$bias_level == "balanced"]

dt_results$delta_PR_gap <- dt_results$PR_gap - balanced_PR_gap
dt_results$delta_TPR_gap <- dt_results$TPR_gap - balanced_TPR_gap
dt_results$delta_Error_gap <- dt_results$Error_gap - balanced_Error_gap

dt_results


# =========================
# Save results
# =========================

output_dir <- here::here(
  "outputs",
  "simulated",
  "04"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  dt_results,
  file.path(output_dir, "sim_decision_tree_results.csv"),
  row.names = FALSE
)


# =========================
# Reproducibility information
# =========================

sessionInfo()
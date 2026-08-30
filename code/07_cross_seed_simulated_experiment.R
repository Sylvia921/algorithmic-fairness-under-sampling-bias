# =========================
# 07_cross_seed_simulated_experiment.R
# Cross-seed repetition for simulated experiment
# =========================

# Identify the project root
here::i_am("code/07_cross_seed_simulated_experiment.R")

# =========================
# 1. Load packages
# =========================

library(rpart)
library(randomForest)
library(ggplot2)


# =========================
# 2. Global settings
# =========================

output_folder <- here::here(
  "outputs",
  "simulated",
  "07"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

seeds <- 1:30

n_population <- 20000
test_size <- 1000
train_size <- 4000

bias_levels <- data.frame(
  bias_level = c("balanced", "mild", "moderate", "severe"),
  prop_A = c(0.50, 0.60, 0.75, 0.90)
)

bias_levels$prop_B <- 1 - bias_levels$prop_A
bias_levels$n_A <- train_size * bias_levels$prop_A
bias_levels$n_B <- train_size * bias_levels$prop_B


# =========================
# 3. Generate simulated population
# =========================

generate_simulated_population <- function(seed_value) {
  
  set.seed(seed_value)
  
  group <- rep(
    c("A", "B"),
    each = n_population / 2
  )
  
  group <- sample(
    group,
    size = n_population,
    replace = FALSE
  )
  
  x1 <- ifelse(
    group == "A",
    rnorm(n_population, mean = 0.3, sd = 1),
    rnorm(n_population, mean = -0.5, sd = 1)
  )
  
  x2 <- ifelse(
    group == "A",
    rnorm(n_population, mean = -0.4, sd = 1),
    rnorm(n_population, mean = 0.2, sd = 1)
  )
  
  x3 <- rbinom(
    n_population,
    size = 1,
    prob = ifelse(group == "A", 0.55, 0.45)
  )
  
  linear_score <- -0.3 +
    0.8 * x1 +
    0.6 * x2 +
    0.5 * x3 +
    0.25 * ifelse(group == "B", 1, 0)
  
  prob_y <- 1 / (1 + exp(-linear_score))
  
  y <- rbinom(
    n_population,
    size = 1,
    prob = prob_y
  )
  
  sim_data <- data.frame(
    y = y,
    group = group,
    x1 = x1,
    x2 = x2,
    x3 = x3
  )
  
  return(sim_data)
}


# =========================
# 4. Create fixed balanced test set and train pool
# =========================

create_train_pool_and_test <- function(sim_data, seed_value) {
  
  set.seed(seed_value + 10000)
  
  test_size_per_group <- test_size / 2
  
  group_A_index <- which(sim_data$group == "A")
  group_B_index <- which(sim_data$group == "B")
  
  test_A_index <- sample(
    group_A_index,
    size = test_size_per_group,
    replace = FALSE
  )
  
  test_B_index <- sample(
    group_B_index,
    size = test_size_per_group,
    replace = FALSE
  )
  
  test_index <- c(
    test_A_index,
    test_B_index
  )
  
  test_fixed <- sim_data[test_index, ]
  train_pool <- sim_data[-test_index, ]
  
  test_fixed <- test_fixed[
    sample(1:nrow(test_fixed)),
  ]
  
  train_pool <- train_pool[
    sample(1:nrow(train_pool)),
  ]
  
  return(
    list(
      train_pool = train_pool,
      test_fixed = test_fixed
    )
  )
}


# =========================
# 5. Create fixed-size biased training set
# =========================

create_fixed_size_train <- function(train_pool, n_A, n_B, seed_value) {
  
  set.seed(seed_value)
  
  pool_A <- train_pool[train_pool$group == "A", ]
  pool_B <- train_pool[train_pool$group == "B", ]
  
  sample_A <- pool_A[
    sample(1:nrow(pool_A), size = n_A, replace = FALSE),
  ]
  
  sample_B <- pool_B[
    sample(1:nrow(pool_B), size = n_B, replace = FALSE),
  ]
  
  train_data <- rbind(
    sample_A,
    sample_B
  )
  
  train_data <- train_data[
    sample(1:nrow(train_data)),
  ]
  
  return(train_data)
}


# =========================
# 6. Evaluation function
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
# 7. Run Logistic Regression
# =========================

run_logistic_regression <- function(train_data, test_data) {
  
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
  
  pred_class <- ifelse(
    pred_prob >= 0.5,
    1,
    0
  )
  
  eval_results <- evaluate_model(
    actual = test_data$y,
    predicted = pred_class,
    group = test_data$group
  )
  
  return(eval_results)
}


# =========================
# 8. Run Decision Tree
# =========================

run_decision_tree <- function(train_data, test_data) {
  
  train_data$y <- factor(
    train_data$y,
    levels = c(0, 1)
  )
  
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
  
  return(eval_results)
}


# =========================
# 9. Run Random Forest
# =========================

run_random_forest <- function(train_data, test_data, seed_value) {
  
  set.seed(seed_value)
  
  train_data$y <- factor(
    train_data$y,
    levels = c(0, 1)
  )
  
  model <- randomForest(
    y ~ x1 + x2 + x3,
    data = train_data,
    ntree = 500,
    mtry = 1,
    importance = TRUE
  )
  
  pred_prob <- predict(
    model,
    newdata = test_data,
    type = "prob"
  )[, "1"]
  
  pred_class <- ifelse(
    pred_prob >= 0.5,
    1,
    0
  )
  
  eval_results <- evaluate_model(
    actual = test_data$y,
    predicted = pred_class,
    group = test_data$group
  )
  
  return(eval_results)
}


# =========================
# 10. Run one full seed experiment
# =========================

run_one_seed <- function(seed_value) {
  
  sim_data <- generate_simulated_population(
    seed_value = seed_value
  )
  
  split_data <- create_train_pool_and_test(
    sim_data = sim_data,
    seed_value = seed_value
  )
  
  train_pool <- split_data$train_pool
  test_fixed <- split_data$test_fixed
  
  test_fixed$group <- factor(
    test_fixed$group,
    levels = c("A", "B")
  )
  
  test_fixed$y <- as.numeric(test_fixed$y)
  
  seed_results <- data.frame()
  
  for (i in 1:nrow(bias_levels)) {
    
    current_bias_level <- bias_levels$bias_level[i]
    current_n_A <- bias_levels$n_A[i]
    current_n_B <- bias_levels$n_B[i]
    
    train_data <- create_fixed_size_train(
      train_pool = train_pool,
      n_A = current_n_A,
      n_B = current_n_B,
      seed_value = seed_value * 100 + i
    )
    
    train_data$group <- factor(
      train_data$group,
      levels = c("A", "B")
    )
    
    train_data$y <- as.numeric(train_data$y)
    
    # Logistic Regression
    lr_eval <- run_logistic_regression(
      train_data = train_data,
      test_data = test_fixed
    )
    
    lr_eval$seed <- seed_value
    lr_eval$dataset <- "simulated"
    lr_eval$model <- "logistic_regression"
    lr_eval$bias_level <- current_bias_level
    
    # Decision Tree
    dt_eval <- run_decision_tree(
      train_data = train_data,
      test_data = test_fixed
    )
    
    dt_eval$seed <- seed_value
    dt_eval$dataset <- "simulated"
    dt_eval$model <- "decision_tree"
    dt_eval$bias_level <- current_bias_level
    
    # Random Forest
    rf_eval <- run_random_forest(
      train_data = train_data,
      test_data = test_fixed,
      seed_value = seed_value * 1000 + i
    )
    
    rf_eval$seed <- seed_value
    rf_eval$dataset <- "simulated"
    rf_eval$model <- "random_forest"
    rf_eval$bias_level <- current_bias_level
    
    seed_results <- rbind(
      seed_results,
      lr_eval,
      dt_eval,
      rf_eval
    )
  }
  
  seed_results <- seed_results[, c(
    "seed",
    "dataset",
    "model",
    "bias_level",
    "accuracy",
    "error_rate",
    "PR_gap",
    "TPR_gap",
    "Error_gap"
  )]
  
  return(seed_results)
}


# =========================
# 11. Run cross-seed experiment
# =========================

cross_seed_raw_results <- data.frame()

for (current_seed in seeds) {
  
  cat("Running seed:", current_seed, "\n")
  
  current_results <- run_one_seed(
    seed_value = current_seed
  )
  
  cross_seed_raw_results <- rbind(
    cross_seed_raw_results,
    current_results
  )
}

cross_seed_raw_results$model <- factor(
  cross_seed_raw_results$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

cross_seed_raw_results$bias_level <- factor(
  cross_seed_raw_results$bias_level,
  levels = c(
    "balanced",
    "mild",
    "moderate",
    "severe"
  )
)

cross_seed_raw_results <- cross_seed_raw_results[
  order(
    cross_seed_raw_results$seed,
    cross_seed_raw_results$model,
    cross_seed_raw_results$bias_level
  ),
]


# =========================
# 12. Save raw cross-seed results
# =========================

write.csv(
  cross_seed_raw_results,
  file.path(output_folder, "sim_cross_seed_raw_results.csv"),
  row.names = FALSE
)


# =========================
# 13. Cross-seed mean results
# =========================

cross_seed_mean_results <- aggregate(
  cbind(
    accuracy,
    error_rate,
    PR_gap,
    TPR_gap,
    Error_gap
  ) ~ model + bias_level,
  data = cross_seed_raw_results,
  FUN = mean
)

cross_seed_sd_results <- aggregate(
  cbind(
    accuracy,
    error_rate,
    PR_gap,
    TPR_gap,
    Error_gap
  ) ~ model + bias_level,
  data = cross_seed_raw_results,
  FUN = sd
)

names(cross_seed_mean_results)[3:7] <- c(
  "mean_accuracy",
  "mean_error_rate",
  "mean_PR_gap",
  "mean_TPR_gap",
  "mean_Error_gap"
)

names(cross_seed_sd_results)[3:7] <- c(
  "sd_accuracy",
  "sd_error_rate",
  "sd_PR_gap",
  "sd_TPR_gap",
  "sd_Error_gap"
)

cross_seed_mean_summary <- merge(
  cross_seed_mean_results,
  cross_seed_sd_results,
  by = c("model", "bias_level")
)

cross_seed_mean_summary$model <- factor(
  cross_seed_mean_summary$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

cross_seed_mean_summary$bias_level <- factor(
  cross_seed_mean_summary$bias_level,
  levels = c(
    "balanced",
    "mild",
    "moderate",
    "severe"
  )
)

cross_seed_mean_summary <- cross_seed_mean_summary[
  order(
    cross_seed_mean_summary$model,
    cross_seed_mean_summary$bias_level
  ),
]

cross_seed_mean_summary_report <- cross_seed_mean_summary
cross_seed_mean_summary_report[, 3:ncol(cross_seed_mean_summary_report)] <- round(
  cross_seed_mean_summary_report[, 3:ncol(cross_seed_mean_summary_report)],
  3
)


write.csv(
  cross_seed_mean_summary,
  file.path(output_folder, "sim_cross_seed_mean_results.csv"),
  row.names = FALSE
)


write.csv(
  cross_seed_mean_summary_report,
  file.path(output_folder, "sim_cross_seed_mean_results_report.csv"),
  row.names = FALSE
)


# =========================
# 14. Cross-seed baseline mean results
# =========================

cross_seed_baseline_mean_results <- cross_seed_mean_summary[
  cross_seed_mean_summary$bias_level == "balanced",
  c(
    "model",
    "mean_accuracy",
    "sd_accuracy",
    "mean_PR_gap",
    "sd_PR_gap",
    "mean_TPR_gap",
    "sd_TPR_gap",
    "mean_Error_gap",
    "sd_Error_gap"
  )
]

cross_seed_baseline_mean_results_report <- cross_seed_baseline_mean_results
cross_seed_baseline_mean_results_report[, 2:ncol(cross_seed_baseline_mean_results_report)] <- round(
  cross_seed_baseline_mean_results_report[, 2:ncol(cross_seed_baseline_mean_results_report)],
  3
)


write.csv(
  cross_seed_baseline_mean_results,
  file.path(output_folder, "sim_cross_seed_baseline_mean_results.csv"),
  row.names = FALSE
)


write.csv(
  cross_seed_baseline_mean_results_report,
  file.path(output_folder, "sim_cross_seed_baseline_mean_results_report.csv"),
  row.names = FALSE
)


# =========================
# 15. Calculate seed-level deltas from balanced condition
# =========================

balanced_seed_results <- cross_seed_raw_results[
  cross_seed_raw_results$bias_level == "balanced",
  c(
    "seed",
    "model",
    "accuracy",
    "PR_gap",
    "TPR_gap",
    "Error_gap"
  )
]

names(balanced_seed_results) <- c(
  "seed",
  "model",
  "balanced_accuracy",
  "balanced_PR_gap",
  "balanced_TPR_gap",
  "balanced_Error_gap"
)

biased_seed_results <- cross_seed_raw_results[
  cross_seed_raw_results$bias_level %in% c("mild", "moderate", "severe"),
  c(
    "seed",
    "model",
    "bias_level",
    "accuracy",
    "PR_gap",
    "TPR_gap",
    "Error_gap"
  )
]

cross_seed_delta_results <- merge(
  biased_seed_results,
  balanced_seed_results,
  by = c("seed", "model")
)

cross_seed_delta_results$delta_accuracy <-
  cross_seed_delta_results$accuracy -
  cross_seed_delta_results$balanced_accuracy

cross_seed_delta_results$delta_PR_gap <-
  cross_seed_delta_results$PR_gap -
  cross_seed_delta_results$balanced_PR_gap

cross_seed_delta_results$delta_TPR_gap <-
  cross_seed_delta_results$TPR_gap -
  cross_seed_delta_results$balanced_TPR_gap

cross_seed_delta_results$delta_Error_gap <-
  cross_seed_delta_results$Error_gap -
  cross_seed_delta_results$balanced_Error_gap

cross_seed_delta_results <- cross_seed_delta_results[, c(
  "seed",
  "model",
  "bias_level",
  "delta_accuracy",
  "delta_PR_gap",
  "delta_TPR_gap",
  "delta_Error_gap"
)]

cross_seed_delta_results$model <- factor(
  cross_seed_delta_results$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

cross_seed_delta_results$bias_level <- factor(
  cross_seed_delta_results$bias_level,
  levels = c(
    "mild",
    "moderate",
    "severe"
  )
)

cross_seed_delta_results <- cross_seed_delta_results[
  order(
    cross_seed_delta_results$seed,
    cross_seed_delta_results$model,
    cross_seed_delta_results$bias_level
  ),
]


write.csv(
  cross_seed_delta_results,
  file.path(output_folder, "sim_cross_seed_delta_results.csv"),
  row.names = FALSE
)


# =========================
# 16. Cross-seed mean delta table
# =========================

cross_seed_mean_delta <- aggregate(
  cbind(
    delta_PR_gap,
    delta_TPR_gap,
    delta_Error_gap
  ) ~ model + bias_level,
  data = cross_seed_delta_results,
  FUN = mean
)

cross_seed_sd_delta <- aggregate(
  cbind(
    delta_PR_gap,
    delta_TPR_gap,
    delta_Error_gap
  ) ~ model + bias_level,
  data = cross_seed_delta_results,
  FUN = sd
)

names(cross_seed_mean_delta)[3:5] <- c(
  "mean_delta_PR_gap",
  "mean_delta_TPR_gap",
  "mean_delta_Error_gap"
)

names(cross_seed_sd_delta)[3:5] <- c(
  "sd_delta_PR_gap",
  "sd_delta_TPR_gap",
  "sd_delta_Error_gap"
)

cross_seed_mean_delta_summary <- merge(
  cross_seed_mean_delta,
  cross_seed_sd_delta,
  by = c("model", "bias_level")
)

cross_seed_mean_delta_summary$model <- factor(
  cross_seed_mean_delta_summary$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

cross_seed_mean_delta_summary$bias_level <- factor(
  cross_seed_mean_delta_summary$bias_level,
  levels = c(
    "mild",
    "moderate",
    "severe"
  )
)

cross_seed_mean_delta_summary <- cross_seed_mean_delta_summary[
  order(
    cross_seed_mean_delta_summary$model,
    cross_seed_mean_delta_summary$bias_level
  ),
]

cross_seed_mean_delta_summary_report <- cross_seed_mean_delta_summary
cross_seed_mean_delta_summary_report[, 3:ncol(cross_seed_mean_delta_summary_report)] <- round(
  cross_seed_mean_delta_summary_report[, 3:ncol(cross_seed_mean_delta_summary_report)],
  3
)


write.csv(
  cross_seed_mean_delta_summary,
  file.path(output_folder, "sim_cross_seed_mean_delta_results.csv"),
  row.names = FALSE
)


write.csv(
  cross_seed_mean_delta_summary_report,
  file.path(output_folder, "sim_cross_seed_mean_delta_results_report.csv"),
  row.names = FALSE
)


# =========================
# 17. Cross-seed mean fairness gaps faceted plot
# =========================

cross_seed_pr_long <- data.frame(
  model = cross_seed_mean_summary$model,
  bias_level = cross_seed_mean_summary$bias_level,
  metric = "Mean PR gap",
  fairness_gap = cross_seed_mean_summary$mean_PR_gap
)

cross_seed_tpr_long <- data.frame(
  model = cross_seed_mean_summary$model,
  bias_level = cross_seed_mean_summary$bias_level,
  metric = "Mean TPR gap",
  fairness_gap = cross_seed_mean_summary$mean_TPR_gap
)

cross_seed_error_long <- data.frame(
  model = cross_seed_mean_summary$model,
  bias_level = cross_seed_mean_summary$bias_level,
  metric = "Mean Error gap",
  fairness_gap = cross_seed_mean_summary$mean_Error_gap
)

cross_seed_fairness_long <- rbind(
  cross_seed_pr_long,
  cross_seed_tpr_long,
  cross_seed_error_long
)

cross_seed_fairness_long$model_label <- as.character(
  cross_seed_fairness_long$model
)

cross_seed_fairness_long$model_label[
  cross_seed_fairness_long$model == "logistic_regression"
] <- "Logistic Regression"

cross_seed_fairness_long$model_label[
  cross_seed_fairness_long$model == "decision_tree"
] <- "Decision Tree"

cross_seed_fairness_long$model_label[
  cross_seed_fairness_long$model == "random_forest"
] <- "Random Forest"

cross_seed_fairness_long$model_label <- factor(
  cross_seed_fairness_long$model_label,
  levels = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest"
  )
)

cross_seed_fairness_long$metric <- factor(
  cross_seed_fairness_long$metric,
  levels = c(
    "Mean PR gap",
    "Mean TPR gap",
    "Mean Error gap"
  )
)

fig_cross_seed_mean_fairness_gaps <- ggplot(
  cross_seed_fairness_long,
  aes(
    x = bias_level,
    y = fairness_gap,
    group = model_label,
    colour = model_label
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title = "Cross-seed mean fairness gaps across sampling-bias levels",
    x = "Sampling-bias level",
    y = "Mean fairness gap",
    colour = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

fig_cross_seed_mean_fairness_gaps


ggsave(
  filename = file.path(output_folder, "fig_sim_cross_seed_mean_fairness_gaps_faceted.png"),
  plot = fig_cross_seed_mean_fairness_gaps,
  width = 10,
  height = 6,
  dpi = 300
)


write.csv(
  cross_seed_fairness_long,
  file.path(output_folder, "sim_cross_seed_fairness_gaps_long.csv"),
  row.names = FALSE
)


# =========================
# 18. Classify trend patterns based on cross-seed means
# =========================

classify_trend <- function(values) {
  
  tolerance <- 1e-10
  
  balanced_value <- values[1]
  step_changes <- diff(values)
  
  all_non_decreasing <- all(step_changes >= -tolerance)
  strictly_increasing_somewhere <- any(step_changes > tolerance)
  
  biased_values <- values[2:4]
  all_biased_above_balanced <- all(biased_values > balanced_value + tolerance)
  severe_above_balanced <- values[4] > balanced_value + tolerance
  
  if (all_non_decreasing && strictly_increasing_somewhere) {
    
    if (any(abs(step_changes) <= tolerance)) {
      pattern <- "Stable or flat until later increase"
      support <- "Partial"
    } else {
      pattern <- "Increasing"
      support <- "Strong"
    }
    
  } else if (all_biased_above_balanced) {
    
    pattern <- "Mostly above balanced but non-monotonic"
    support <- "Partial"
    
  } else if (severe_above_balanced) {
    
    pattern <- "Weak or non-monotonic increase"
    support <- "Limited"
    
  } else {
    
    pattern <- "Non-monotonic / no increasing trend"
    support <- "Limited"
  }
  
  return(
    data.frame(
      pattern = pattern,
      support = support
    )
  )
}


# =========================
# 19. Cross-seed trend summary
# =========================

trend_models <- levels(cross_seed_mean_summary$model)

trend_metrics <- c(
  "mean_PR_gap",
  "mean_TPR_gap",
  "mean_Error_gap"
)

trend_metric_labels <- c(
  "PR gap",
  "TPR gap",
  "Error gap"
)

cross_seed_trend_summary <- data.frame()

for (current_model in trend_models) {
  
  model_data <- cross_seed_mean_summary[
    cross_seed_mean_summary$model == current_model,
  ]
  
  model_data <- model_data[
    order(model_data$bias_level),
  ]
  
  for (i in seq_along(trend_metrics)) {
    
    current_metric <- trend_metrics[i]
    current_metric_label <- trend_metric_labels[i]
    
    values <- model_data[[current_metric]]
    
    trend_result <- classify_trend(values)
    
    temp_result <- data.frame(
      model = current_model,
      metric = current_metric_label,
      cross_seed_pattern = trend_result$pattern,
      cross_seed_support = trend_result$support
    )
    
    cross_seed_trend_summary <- rbind(
      cross_seed_trend_summary,
      temp_result
    )
  }
}

cross_seed_trend_summary$model_label <- as.character(
  cross_seed_trend_summary$model
)

cross_seed_trend_summary$model_label[
  cross_seed_trend_summary$model == "logistic_regression"
] <- "Logistic Regression"

cross_seed_trend_summary$model_label[
  cross_seed_trend_summary$model == "decision_tree"
] <- "Decision Tree"

cross_seed_trend_summary$model_label[
  cross_seed_trend_summary$model == "random_forest"
] <- "Random Forest"

cross_seed_trend_summary_report <- cross_seed_trend_summary[, c(
  "model_label",
  "metric",
  "cross_seed_pattern",
  "cross_seed_support"
)]

names(cross_seed_trend_summary_report) <- c(
  "Model",
  "Metric",
  "Cross-seed mean pattern",
  "Cross-seed support"
)


write.csv(
  cross_seed_trend_summary,
  file.path(output_folder, "sim_cross_seed_trend_summary.csv"),
  row.names = FALSE
)


write.csv(
  cross_seed_trend_summary_report,
  file.path(output_folder, "sim_cross_seed_trend_summary_report.csv"),
  row.names = FALSE
)


# =========================
# 20. Single-seed vs cross-seed trend comparison
# =========================

single_seed_file <- here::here(
  "outputs",
  "simulated",
  "06",
  "sim_rq4_monotonic_trend_summary_report.csv"
)

if (file.exists(single_seed_file)) {
  
  single_seed_trend <- read.csv(
    single_seed_file
  )
  
} else {
  
  single_seed_trend <- NULL
  
  warning(
    "Single-seed RQ4 trend summary file was not found. Trend comparison table was not created."
  )
}

if (!is.null(single_seed_trend)) {
  
  names(single_seed_trend) <- c(
    "Model",
    "Metric",
    "Single-seed pattern",
    "Single-seed support"
  )
  
  trend_comparison <- merge(
    single_seed_trend,
    cross_seed_trend_summary_report,
    by = c("Model", "Metric")
  )
  
  classify_consistency <- function(single_support, cross_support) {
    
    if (single_support == cross_support) {
      return("Consistent")
    }
    
    if (
      single_support %in% c("Strong", "Partial") &&
      cross_support %in% c("Strong", "Partial")
    ) {
      return("Partially consistent")
    }
    
    if (
      single_support %in% c("Partial", "Limited") &&
      cross_support %in% c("Partial", "Limited")
    ) {
      return("Partially consistent")
    }
    
    return("Not consistent")
  }
  
  trend_comparison$Consistency <- mapply(
    classify_consistency,
    trend_comparison$`Single-seed support`,
    trend_comparison$`Cross-seed support`
  )
  
  trend_comparison$Model <- factor(
    trend_comparison$Model,
    levels = c(
      "Logistic Regression",
      "Decision Tree",
      "Random Forest"
    )
  )
  
  trend_comparison$Metric <- factor(
    trend_comparison$Metric,
    levels = c(
      "PR gap",
      "TPR gap",
      "Error gap"
    )
  )
  
  trend_comparison <- trend_comparison[
    order(
      trend_comparison$Model,
      trend_comparison$Metric
    ),
  ]
  

  write.csv(
    trend_comparison,
    file.path(output_folder, "sim_single_seed_vs_cross_seed_trend_comparison.csv"),
    row.names = FALSE
  )
  
  trend_comparison
}


# =========================
# 21. Print key outputs
# =========================

cross_seed_baseline_mean_results_report
cross_seed_mean_summary_report
cross_seed_mean_delta_summary_report
cross_seed_trend_summary_report

if (exists("trend_comparison")) {
  trend_comparison
}


# =========================
# 22. Reproducibility information
# =========================

sessionInfo()
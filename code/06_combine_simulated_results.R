# =========================
# 06_combine_simulated_results.R
# Part 1: Combine simulated model results
# =========================

# Identify the project root
here::i_am("code/06_combine_simulated_results.R")

# Define output folder
output_folder <- here::here(
  "outputs",
  "simulated",
  "06"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

# =========================
# 1. Load model results
# =========================

lr_results <- read.csv(
  here::here(
    "outputs",
    "simulated",
    "03",
    "sim_logistic_regression_results.csv"
  )
)
dt_results <- read.csv(
  here::here(
    "outputs",
    "simulated",
    "04",
    "sim_decision_tree_results.csv"
  )
)
rf_results <- read.csv(
  here::here(
    "outputs",
    "simulated",
    "05",
    "sim_random_forest_results.csv"
  )
)

# =========================
# 2. Keep main columns
# =========================

main_columns <- c(
  "dataset",
  "model",
  "bias_level",
  "accuracy",
  "error_rate",
  "PR_gap",
  "TPR_gap",
  "Error_gap"
)

lr_results_main <- lr_results[, main_columns]
dt_results_main <- dt_results[, main_columns]
rf_results_main <- rf_results[, main_columns]


# =========================
# 3. Combine all model results
# =========================

sim_all_model_results <- rbind(
  lr_results_main,
  dt_results_main,
  rf_results_main
)


# =========================
# 4. Set model and bias-level order
# =========================

sim_all_model_results$model <- factor(
  sim_all_model_results$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

sim_all_model_results$bias_level <- factor(
  sim_all_model_results$bias_level,
  levels = c(
    "balanced",
    "mild",
    "moderate",
    "severe"
  )
)


# =========================
# 5. Sort results
# =========================

sim_all_model_results <- sim_all_model_results[
  order(
    sim_all_model_results$model,
    sim_all_model_results$bias_level
  ),
]


# =========================
# 6. Round numeric columns for reporting
# =========================

sim_all_model_results_report <- sim_all_model_results

numeric_columns <- c(
  "accuracy",
  "error_rate",
  "PR_gap",
  "TPR_gap",
  "Error_gap"
)

sim_all_model_results_report[, numeric_columns] <- round(
  sim_all_model_results_report[, numeric_columns],
  3
)


# =========================
# 7. Print main table
# =========================

sim_all_model_results_report


# =========================
# 8. Save main table
# =========================

write.csv(
  sim_all_model_results,
  file.path(output_folder, "sim_all_model_results.csv"),
  row.names = FALSE
)

write.csv(
  sim_all_model_results_report,
  file.path(output_folder, "sim_all_model_results_report.csv"),
  row.names = FALSE
)











# =========================
# Baseline model performance
# Simulated dataset baseline results
# =========================


# =========================
# 2. Extract balanced condition
# =========================

sim_baseline_results <- sim_all_model_results[
  sim_all_model_results$bias_level == "balanced",
  c(
    "dataset",
    "model",
    "accuracy",
    "error_rate",
    "PR_gap",
    "TPR_gap",
    "Error_gap"
  )
]


# =========================
# 3. Set model order
# =========================

sim_baseline_results$model <- factor(
  sim_baseline_results$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

sim_baseline_results <- sim_baseline_results[
  order(sim_baseline_results$model),
]


# =========================
# 4. Create model labels for thesis table
# =========================

sim_baseline_results_report <- sim_baseline_results

sim_baseline_results_report$model <- as.character(
  sim_baseline_results_report$model
)

sim_baseline_results_report$model[
  sim_baseline_results_report$model == "logistic_regression"
] <- "Logistic Regression"

sim_baseline_results_report$model[
  sim_baseline_results_report$model == "decision_tree"
] <- "Decision Tree"

sim_baseline_results_report$model[
  sim_baseline_results_report$model == "random_forest"
] <- "Random Forest"


# =========================
# 5. Round numeric columns for reporting
# =========================

numeric_columns <- c(
  "accuracy",
  "error_rate",
  "PR_gap",
  "TPR_gap",
  "Error_gap"
)

sim_baseline_results_report[, numeric_columns] <- round(
  sim_baseline_results_report[, numeric_columns],
  3
)


# =========================
# 6. Rename columns for thesis table
# =========================

names(sim_baseline_results_report) <- c(
  "Dataset",
  "Model",
  "Accuracy",
  "Error rate",
  "PR gap",
  "TPR gap",
  "Error gap"
)


# =========================
# 7. Print baseline table
#Table：Baseline model performance on the simulated dataset
# =========================

sim_baseline_results_report


# =========================
# 8. Save baseline table to output folder
# =========================

write.csv(
  sim_baseline_results,
  file.path(output_folder, "sim_baseline_results.csv"),
  row.names = FALSE
)

write.csv(
  sim_baseline_results_report,
  file.path(output_folder, "sim_baseline_results_report.csv"),
  row.names = FALSE
)








# =========================
# Baseline data group differences
# Table 4.2: Group differences in the simulated baseline data
# =========================

# =========================
# 2. Load simulated baseline training data
# =========================
sim_baseline_train <- read.csv(
  here::here(
    "outputs",
    "simulated",
    "02",
    "sim_train_balanced.csv"
  )
)


# =========================
# 3. Make sure group and outcome variables are correctly treated
# =========================

sim_baseline_train$group <- factor(
  sim_baseline_train$group,
  levels = c("A", "B")
)

sim_baseline_train$y <- as.numeric(sim_baseline_train$y)


# =========================
# 4. Create group-level baseline data summary
# =========================

baseline_group_summary <- aggregate(
  cbind(
    y,
    x1,
    x2,
    x3
  ) ~ group,
  data = sim_baseline_train,
  FUN = mean
)

names(baseline_group_summary) <- c(
  "group",
  "positive_outcome_rate",
  "mean_x1",
  "mean_x2",
  "mean_x3"
)


# =========================
# 5. Add sample size by group
# =========================

group_size <- as.data.frame(
  table(sim_baseline_train$group)
)

names(group_size) <- c(
  "group",
  "sample_size"
)

baseline_group_summary <- merge(
  group_size,
  baseline_group_summary,
  by = "group"
)


# =========================
# 6. Order groups
# =========================

baseline_group_summary$group <- factor(
  baseline_group_summary$group,
  levels = c("A", "B")
)

baseline_group_summary <- baseline_group_summary[
  order(baseline_group_summary$group),
]


# =========================
# 7. Create rounded version for thesis table
# =========================

baseline_group_summary_report <- baseline_group_summary

baseline_group_summary_report[, c(
  "positive_outcome_rate",
  "mean_x1",
  "mean_x2",
  "mean_x3"
)] <- round(
  baseline_group_summary_report[, c(
    "positive_outcome_rate",
    "mean_x1",
    "mean_x2",
    "mean_x3"
  )],
  3
)


# =========================
# 8. Rename columns for thesis table
# =========================

names(baseline_group_summary_report) <- c(
  "Group",
  "Sample size",
  "Positive outcome rate",
  "Mean x1",
  "Mean x2",
  "Mean x3"
)


# =========================
# 9. Print table
# =========================

baseline_group_summary_report


# =========================
# 10. Save baseline group-difference table to output folder
#Table: Group differences in the simulated baseline data
# =========================

write.csv(
  baseline_group_summary,
  file.path(output_folder, "sim_baseline_group_differences.csv"),
  row.names = FALSE
)

write.csv(
  baseline_group_summary_report,
  file.path(output_folder, "sim_baseline_group_differences_report.csv"),
  row.names = FALSE
)


































# =========================
# Part 2: RQ1
# Does sampling bias lead to unfair outcomes?
# =========================


# =========================
# 2. Use original combined results
# =========================

# Important:
# Use sim_all_model_results, not sim_all_model_results_report,
# because delta values should be calculated from unrounded results.

rq1_data <- sim_all_model_results


# =========================
# 3. Extract balanced condition for each model
# =========================

balanced_results <- rq1_data[
  rq1_data$bias_level == "balanced",
  c("model", "PR_gap", "TPR_gap", "Error_gap")
]

names(balanced_results) <- c(
  "model",
  "balanced_PR_gap",
  "balanced_TPR_gap",
  "balanced_Error_gap"
)


# =========================
# 4. Keep only biased conditions
# =========================

biased_results <- rq1_data[
  rq1_data$bias_level %in% c("mild", "moderate", "severe"),
  c("model", "bias_level", "PR_gap", "TPR_gap", "Error_gap")
]


# =========================
# 5. Merge biased results with balanced baseline
# =========================

rq1_change_from_balanced <- merge(
  biased_results,
  balanced_results,
  by = "model"
)


# =========================
# 6. Calculate changes from balanced condition
# =========================

rq1_change_from_balanced$delta_PR_gap <- 
  rq1_change_from_balanced$PR_gap - rq1_change_from_balanced$balanced_PR_gap

rq1_change_from_balanced$delta_TPR_gap <- 
  rq1_change_from_balanced$TPR_gap - rq1_change_from_balanced$balanced_TPR_gap

rq1_change_from_balanced$delta_Error_gap <- 
  rq1_change_from_balanced$Error_gap - rq1_change_from_balanced$balanced_Error_gap


# =========================
# 7. Keep reporting columns
# =========================

rq1_change_from_balanced <- rq1_change_from_balanced[, c(
  "model",
  "bias_level",
  "delta_PR_gap",
  "delta_TPR_gap",
  "delta_Error_gap"
)]


# =========================
# 8. Set model and bias-level order
# =========================

rq1_change_from_balanced$model <- factor(
  rq1_change_from_balanced$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

rq1_change_from_balanced$bias_level <- factor(
  rq1_change_from_balanced$bias_level,
  levels = c(
    "mild",
    "moderate",
    "severe"
  )
)

rq1_change_from_balanced <- rq1_change_from_balanced[
  order(
    rq1_change_from_balanced$model,
    rq1_change_from_balanced$bias_level
  ),
]


# =========================
# 9. Create rounded version for thesis table
# =========================

rq1_change_from_balanced_report <- rq1_change_from_balanced

rq1_change_from_balanced_report[, c(
  "delta_PR_gap",
  "delta_TPR_gap",
  "delta_Error_gap"
)] <- round(
  rq1_change_from_balanced_report[, c(
    "delta_PR_gap",
    "delta_TPR_gap",
    "delta_Error_gap"
  )],
  3
)


# =========================
# 10. Print RQ1 main table
# =========================

rq1_change_from_balanced_report


# =========================
# 11. Save RQ1 main table to output folder
# =========================

write.csv(
  rq1_change_from_balanced,
  file.path(output_folder, "sim_rq1_change_from_balanced.csv"),
  row.names = FALSE
)

write.csv(
  rq1_change_from_balanced_report,
  file.path(output_folder, "sim_rq1_change_from_balanced_report.csv"),
  row.names = FALSE
)


# =========================
# 12. Create RQ1 auxiliary table:
# Average change across three biased conditions
# =========================

rq1_mean_change <- aggregate(
  cbind(
    delta_PR_gap,
    delta_TPR_gap,
    delta_Error_gap
  ) ~ model,
  data = rq1_change_from_balanced,
  FUN = mean
)

names(rq1_mean_change) <- c(
  "model",
  "mean_delta_PR_gap",
  "mean_delta_TPR_gap",
  "mean_delta_Error_gap"
)


# =========================
# 13. Set model order
# =========================

rq1_mean_change$model <- factor(
  rq1_mean_change$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

rq1_mean_change <- rq1_mean_change[
  order(rq1_mean_change$model),
]


# =========================
# 14. Create rounded version for thesis table
# =========================

rq1_mean_change_report <- rq1_mean_change

rq1_mean_change_report[, c(
  "mean_delta_PR_gap",
  "mean_delta_TPR_gap",
  "mean_delta_Error_gap"
)] <- round(
  rq1_mean_change_report[, c(
    "mean_delta_PR_gap",
    "mean_delta_TPR_gap",
    "mean_delta_Error_gap"
  )],
  3
)


# =========================
# 15. Print RQ1 auxiliary table
# =========================

rq1_mean_change_report


# =========================
# 16. Save RQ1 auxiliary table to output folder
# =========================

write.csv(
  rq1_mean_change,
  file.path(output_folder, "sim_rq1_mean_change_across_biased_levels.csv"),
  row.names = FALSE
)

write.csv(
  rq1_mean_change_report,
  file.path(output_folder, "sim_rq1_mean_change_across_biased_levels_report.csv"),
  row.names = FALSE
)





























# =========================
# Part 3: RQ2
# Are conclusions consistent across different fairness definitions?
# =========================


# =========================
# 1. Load packages
# =========================

library(ggplot2)


# =========================
# 3. Use combined simulated results
# =========================

rq2_data <- sim_all_model_results


# =========================
# 4. Create model labels for plotting
# =========================

rq2_data$model_label <- as.character(rq2_data$model)

rq2_data$model_label[rq2_data$model == "logistic_regression"] <- "Logistic Regression"
rq2_data$model_label[rq2_data$model == "decision_tree"] <- "Decision Tree"
rq2_data$model_label[rq2_data$model == "random_forest"] <- "Random Forest"

rq2_data$model_label <- factor(
  rq2_data$model_label,
  levels = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest"
  )
)


# =========================
# 5. Set bias-level order
# =========================

rq2_data$bias_level <- factor(
  rq2_data$bias_level,
  levels = c(
    "balanced",
    "mild",
    "moderate",
    "severe"
  )
)


# =========================
# 6. Reshape fairness gaps into long format
# =========================

rq2_pr_gap <- data.frame(
  model_label = rq2_data$model_label,
  bias_level = rq2_data$bias_level,
  metric = "PR gap",
  fairness_gap = rq2_data$PR_gap
)

rq2_tpr_gap <- data.frame(
  model_label = rq2_data$model_label,
  bias_level = rq2_data$bias_level,
  metric = "TPR gap",
  fairness_gap = rq2_data$TPR_gap
)

rq2_error_gap <- data.frame(
  model_label = rq2_data$model_label,
  bias_level = rq2_data$bias_level,
  metric = "Error gap",
  fairness_gap = rq2_data$Error_gap
)

rq2_fairness_long <- rbind(
  rq2_pr_gap,
  rq2_tpr_gap,
  rq2_error_gap
)


# =========================
# 7. Set metric order
# =========================

rq2_fairness_long$metric <- factor(
  rq2_fairness_long$metric,
  levels = c(
    "PR gap",
    "TPR gap",
    "Error gap"
  )
)


# =========================
# 8. Create faceted fairness-gap plot
# =========================

fig_rq2_fairness_gaps <- ggplot(
  rq2_fairness_long,
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
    title = "Fairness gaps across sampling-bias levels in the simulated dataset",
    x = "Sampling-bias level",
    y = "Fairness gap",
    colour = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )


# =========================
# 9. Print figure
# =========================

fig_rq2_fairness_gaps


# =========================
# 10. Save figure to output folder
# =========================

ggsave(
  filename = file.path(output_folder, "fig_sim_rq2_fairness_gaps_faceted.png"),
  plot = fig_rq2_fairness_gaps,
  width = 10,
  height = 6,
  dpi = 300
)


# =========================
# 11. Save long-format data used for the figure
# =========================

write.csv(
  rq2_fairness_long,
  file.path(output_folder, "sim_rq2_fairness_gaps_long.csv"),
  row.names = FALSE
)































# =========================
# Part 4: RQ3
# Which models are more sensitive to sampling bias?
# =========================


# =========================
# 3. Use original combined results
# =========================

# Important:
# Use sim_all_model_results, not sim_all_model_results_report,
# because delta values should be calculated from unrounded results.

rq3_data <- sim_all_model_results


# =========================
# 4. Extract balanced condition for each model
# =========================

rq3_balanced_results <- rq3_data[
  rq3_data$bias_level == "balanced",
  c("model", "accuracy", "PR_gap", "TPR_gap", "Error_gap")
]

names(rq3_balanced_results) <- c(
  "model",
  "balanced_accuracy",
  "balanced_PR_gap",
  "balanced_TPR_gap",
  "balanced_Error_gap"
)


# =========================
# 5. Keep only biased conditions
# =========================

rq3_biased_results <- rq3_data[
  rq3_data$bias_level %in% c("mild", "moderate", "severe"),
  c("model", "bias_level", "accuracy", "PR_gap", "TPR_gap", "Error_gap")
]


# =========================
# 6. Merge biased results with balanced baseline
# =========================

rq3_model_sensitivity <- merge(
  rq3_biased_results,
  rq3_balanced_results,
  by = "model"
)


# =========================
# 7. Calculate changes from balanced condition
# =========================

rq3_model_sensitivity$delta_accuracy <- 
  rq3_model_sensitivity$accuracy - rq3_model_sensitivity$balanced_accuracy

rq3_model_sensitivity$delta_PR_gap <- 
  rq3_model_sensitivity$PR_gap - rq3_model_sensitivity$balanced_PR_gap

rq3_model_sensitivity$delta_TPR_gap <- 
  rq3_model_sensitivity$TPR_gap - rq3_model_sensitivity$balanced_TPR_gap

rq3_model_sensitivity$delta_Error_gap <- 
  rq3_model_sensitivity$Error_gap - rq3_model_sensitivity$balanced_Error_gap


# =========================
# 8. Keep reporting columns
# =========================

rq3_model_sensitivity <- rq3_model_sensitivity[, c(
  "model",
  "bias_level",
  "delta_accuracy",
  "delta_PR_gap",
  "delta_TPR_gap",
  "delta_Error_gap"
)]


# =========================
# 9. Set model and bias-level order
# =========================

rq3_model_sensitivity$model <- factor(
  rq3_model_sensitivity$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

rq3_model_sensitivity$bias_level <- factor(
  rq3_model_sensitivity$bias_level,
  levels = c(
    "mild",
    "moderate",
    "severe"
  )
)

rq3_model_sensitivity <- rq3_model_sensitivity[
  order(
    rq3_model_sensitivity$model,
    rq3_model_sensitivity$bias_level
  ),
]


# =========================
# 10. Create rounded version for thesis table
# =========================

rq3_model_sensitivity_report <- rq3_model_sensitivity

rq3_model_sensitivity_report[, c(
  "delta_accuracy",
  "delta_PR_gap",
  "delta_TPR_gap",
  "delta_Error_gap"
)] <- round(
  rq3_model_sensitivity_report[, c(
    "delta_accuracy",
    "delta_PR_gap",
    "delta_TPR_gap",
    "delta_Error_gap"
  )],
  3
)


# =========================
# 11. Print RQ3 main table
#主表Table: Model sensitivity to sampling bias in the simulated dataset
# =========================

rq3_model_sensitivity_report


# =========================
# 12. Save RQ3 main table to output folder
# =========================

write.csv(
  rq3_model_sensitivity,
  file.path(output_folder, "sim_rq3_model_sensitivity.csv"),
  row.names = FALSE
)

write.csv(
  rq3_model_sensitivity_report,
  file.path(output_folder, "sim_rq3_model_sensitivity_report.csv"),
  row.names = FALSE
)


# =========================
# 13. Create RQ3 auxiliary table:
# Average absolute sensitivity across biased levels
# =========================

rq3_abs_sensitivity <- rq3_model_sensitivity

rq3_abs_sensitivity$abs_delta_accuracy <- abs(rq3_abs_sensitivity$delta_accuracy)
rq3_abs_sensitivity$abs_delta_PR_gap <- abs(rq3_abs_sensitivity$delta_PR_gap)
rq3_abs_sensitivity$abs_delta_TPR_gap <- abs(rq3_abs_sensitivity$delta_TPR_gap)
rq3_abs_sensitivity$abs_delta_Error_gap <- abs(rq3_abs_sensitivity$delta_Error_gap)

rq3_average_abs_sensitivity <- aggregate(
  cbind(
    abs_delta_accuracy,
    abs_delta_PR_gap,
    abs_delta_TPR_gap,
    abs_delta_Error_gap
  ) ~ model,
  data = rq3_abs_sensitivity,
  FUN = mean
)

names(rq3_average_abs_sensitivity) <- c(
  "model",
  "mean_abs_delta_accuracy",
  "mean_abs_delta_PR_gap",
  "mean_abs_delta_TPR_gap",
  "mean_abs_delta_Error_gap"
)


# =========================
# 14. Calculate overall fairness sensitivity score
# =========================

# The overall sensitivity score only uses fairness-gap changes.
# Accuracy is reported separately because it measures predictive performance,
# not fairness.

rq3_average_abs_sensitivity$overall_fairness_sensitivity_score <- (
  rq3_average_abs_sensitivity$mean_abs_delta_PR_gap +
    rq3_average_abs_sensitivity$mean_abs_delta_TPR_gap +
    rq3_average_abs_sensitivity$mean_abs_delta_Error_gap
) / 3


# =========================
# 15. Set model order
# =========================

rq3_average_abs_sensitivity$model <- factor(
  rq3_average_abs_sensitivity$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

rq3_average_abs_sensitivity <- rq3_average_abs_sensitivity[
  order(rq3_average_abs_sensitivity$model),
]


# =========================
# 16. Create rounded version for thesis table
# =========================

rq3_average_abs_sensitivity_report <- rq3_average_abs_sensitivity

rq3_average_abs_sensitivity_report[, c(
  "mean_abs_delta_accuracy",
  "mean_abs_delta_PR_gap",
  "mean_abs_delta_TPR_gap",
  "mean_abs_delta_Error_gap",
  "overall_fairness_sensitivity_score"
)] <- round(
  rq3_average_abs_sensitivity_report[, c(
    "mean_abs_delta_accuracy",
    "mean_abs_delta_PR_gap",
    "mean_abs_delta_TPR_gap",
    "mean_abs_delta_Error_gap",
    "overall_fairness_sensitivity_score"
  )],
  3
)


# =========================
# 17. Print RQ3 auxiliary table
#辅助表Table: Average absolute sensitivity across biased levels
# =========================

rq3_average_abs_sensitivity_report


# =========================
# 18. Save RQ3 auxiliary table to output folder
# =========================

write.csv(
  rq3_average_abs_sensitivity,
  file.path(output_folder, "sim_rq3_average_absolute_sensitivity.csv"),
  row.names = FALSE
)

write.csv(
  rq3_average_abs_sensitivity_report,
  file.path(output_folder, "sim_rq3_average_absolute_sensitivity_report.csv"),
  row.names = FALSE
)


# =========================
# 19. Prepare data for RQ3 auxiliary figure
# Change in fairness gaps from balanced condition by model
# =========================

rq3_delta_PR <- data.frame(
  model = rq3_model_sensitivity$model,
  bias_level = rq3_model_sensitivity$bias_level,
  metric = "Delta PR gap",
  delta_gap = rq3_model_sensitivity$delta_PR_gap
)

rq3_delta_TPR <- data.frame(
  model = rq3_model_sensitivity$model,
  bias_level = rq3_model_sensitivity$bias_level,
  metric = "Delta TPR gap",
  delta_gap = rq3_model_sensitivity$delta_TPR_gap
)

rq3_delta_Error <- data.frame(
  model = rq3_model_sensitivity$model,
  bias_level = rq3_model_sensitivity$bias_level,
  metric = "Delta Error gap",
  delta_gap = rq3_model_sensitivity$delta_Error_gap
)

rq3_delta_long <- rbind(
  rq3_delta_PR,
  rq3_delta_TPR,
  rq3_delta_Error
)


# =========================
# 20. Create model labels for plotting
# =========================

rq3_delta_long$model_label <- as.character(rq3_delta_long$model)

rq3_delta_long$model_label[rq3_delta_long$model == "logistic_regression"] <- "Logistic Regression"
rq3_delta_long$model_label[rq3_delta_long$model == "decision_tree"] <- "Decision Tree"
rq3_delta_long$model_label[rq3_delta_long$model == "random_forest"] <- "Random Forest"

rq3_delta_long$model_label <- factor(
  rq3_delta_long$model_label,
  levels = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest"
  )
)


# =========================
# 21. Set metric and bias-level order
# =========================

rq3_delta_long$metric <- factor(
  rq3_delta_long$metric,
  levels = c(
    "Delta PR gap",
    "Delta TPR gap",
    "Delta Error gap"
  )
)

rq3_delta_long$bias_level <- factor(
  rq3_delta_long$bias_level,
  levels = c(
    "mild",
    "moderate",
    "severe"
  )
)


# =========================
# 22. Create RQ3 auxiliary faceted plot
# =========================

fig_rq3_delta_fairness_gaps <- ggplot(
  rq3_delta_long,
  aes(
    x = bias_level,
    y = delta_gap,
    group = model_label,
    colour = model_label
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title = "Change in fairness gaps from balanced condition by model",
    x = "Sampling-bias level",
    y = "Change from balanced condition",
    colour = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )


# =========================
# 23. Print RQ3 auxiliary figure
#辅助Figure: Change in fairness gaps from balanced condition by model
# =========================

fig_rq3_delta_fairness_gaps


# =========================
# 24. Save RQ3 auxiliary figure to output folder
# =========================

ggsave(
  filename = file.path(output_folder, "fig_sim_rq3_delta_fairness_gaps_faceted.png"),
  plot = fig_rq3_delta_fairness_gaps,
  width = 10,
  height = 6,
  dpi = 300
)


# =========================
# 25. Save long-format data used for RQ3 figure
# =========================

write.csv(
  rq3_delta_long,
  file.path(output_folder, "sim_rq3_delta_fairness_gaps_long.csv"),
  row.names = FALSE
)































# =========================
# Part 5: RQ4
# Does increasing sampling bias lead to increasing unfairness?
# =========================

# =========================
# 2. Use original combined results
# =========================

rq4_data <- sim_all_model_results


# =========================
# 3. Set model and bias-level order
# =========================

rq4_data$model <- factor(
  rq4_data$model,
  levels = c(
    "logistic_regression",
    "decision_tree",
    "random_forest"
  )
)

rq4_data$bias_level <- factor(
  rq4_data$bias_level,
  levels = c(
    "balanced",
    "mild",
    "moderate",
    "severe"
  )
)

rq4_data <- rq4_data[
  order(
    rq4_data$model,
    rq4_data$bias_level
  ),
]


# =========================
# 4. Function to classify trend pattern
# =========================

classify_trend <- function(values) {
  
  # values must follow this order:
  # balanced, mild, moderate, severe
  
  tolerance <- 1e-10
  
  balanced_value <- values[1]
  mild_value <- values[2]
  moderate_value <- values[3]
  severe_value <- values[4]
  
  step_changes <- diff(values)
  
  all_non_decreasing <- all(step_changes >= -tolerance)
  strictly_increasing_somewhere <- any(step_changes > tolerance)
  
  biased_values <- values[2:4]
  number_above_balanced <- sum(biased_values > balanced_value + tolerance)
  
  if (all_non_decreasing && strictly_increasing_somewhere) {
    
    if (any(abs(step_changes) <= tolerance)) {
      pattern <- "Stable or flat until later increase"
      support <- "Partial"
    } else {
      pattern <- "Increasing"
      support <- "Strong"
    }
    
  } else if (
    severe_value > balanced_value + tolerance &&
    number_above_balanced >= 2
  ) {
    
    pattern <- "Mostly above balanced but non-monotonic"
    support <- "Partial"
    
  } else if (
    severe_value > balanced_value + tolerance &&
    number_above_balanced == 1
  ) {
    
    pattern <- "Weak or non-monotonic increase"
    support <- "Limited"
    
  } else {
    
    pattern <- "Non-monotonic / no increasing trend"
    support <- "Limited"
    
  }
  
  return(
    data.frame(
      pattern_across_bias_levels = pattern,
      supports_increasing_trend = support
    )
  )
}


# =========================
# 5. Create trend summary table
# =========================

models <- levels(rq4_data$model)

metrics <- c(
  "PR_gap",
  "TPR_gap",
  "Error_gap"
)

metric_labels <- c(
  "PR gap",
  "TPR gap",
  "Error gap"
)

rq4_trend_summary <- data.frame()

for (current_model in models) {
  
  model_data <- rq4_data[rq4_data$model == current_model, ]
  
  model_data <- model_data[
    order(model_data$bias_level),
  ]
  
  for (i in seq_along(metrics)) {
    
    current_metric <- metrics[i]
    current_metric_label <- metric_labels[i]
    
    metric_values <- model_data[[current_metric]]
    
    trend_result <- classify_trend(metric_values)
    
    temp_result <- data.frame(
      model = current_model,
      metric = current_metric_label,
      pattern_across_bias_levels = trend_result$pattern_across_bias_levels,
      supports_increasing_trend = trend_result$supports_increasing_trend
    )
    
    rq4_trend_summary <- rbind(
      rq4_trend_summary,
      temp_result
    )
  }
}


# =========================
# 6. Create model labels for thesis table
# =========================

rq4_trend_summary$model_label <- as.character(rq4_trend_summary$model)

rq4_trend_summary$model_label[
  rq4_trend_summary$model == "logistic_regression"
] <- "Logistic Regression"

rq4_trend_summary$model_label[
  rq4_trend_summary$model == "decision_tree"
] <- "Decision Tree"

rq4_trend_summary$model_label[
  rq4_trend_summary$model == "random_forest"
] <- "Random Forest"


# =========================
# 7. Reorder columns for report
# =========================

rq4_trend_summary_report <- rq4_trend_summary[, c(
  "model_label",
  "metric",
  "pattern_across_bias_levels",
  "supports_increasing_trend"
)]

names(rq4_trend_summary_report) <- c(
  "Model",
  "Metric",
  "Pattern across bias levels",
  "Supports increasing trend?"
)


# =========================
# 8. Set model order in report table
# =========================

rq4_trend_summary_report$Model <- factor(
  rq4_trend_summary_report$Model,
  levels = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest"
  )
)

rq4_trend_summary_report$Metric <- factor(
  rq4_trend_summary_report$Metric,
  levels = c(
    "PR gap",
    "TPR gap",
    "Error gap"
  )
)

rq4_trend_summary_report <- rq4_trend_summary_report[
  order(
    rq4_trend_summary_report$Model,
    rq4_trend_summary_report$Metric
  ),
]


# =========================
# 9. Print RQ4 trend summary table
# =========================

rq4_trend_summary_report


# =========================
# 10. Save RQ4 trend summary table to output folder
# =========================

write.csv(
  rq4_trend_summary,
  file.path(output_folder, "sim_rq4_monotonic_trend_summary.csv"),
  row.names = FALSE
)

write.csv(
  rq4_trend_summary_report,
  file.path(output_folder, "sim_rq4_monotonic_trend_summary_report.csv"),
  row.names = FALSE
)




















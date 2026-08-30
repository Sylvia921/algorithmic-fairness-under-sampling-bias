# =========================
# 02_create_sampling_bias_levels.R
# =========================

# Identify the project root
here::i_am("code/02_create_sampling_bias_levels.R")

set.seed(123)


# =========================
# Load data
# =========================

input_dir <- here::here(
  "outputs",
  "simulated",
  "01"
)

train_pool <- read.csv(
  file.path(input_dir, "sim_train_pool.csv")
)

test_fixed <- read.csv(
  file.path(input_dir, "sim_test_fixed.csv")
)

train_pool$group <- factor(train_pool$group)
test_fixed$group <- factor(test_fixed$group)


# =========================
# Check available samples
# =========================

table(train_pool$group)
prop.table(table(train_pool$group))


# =========================
# Define training size and bias levels
# =========================

train_size <- 4000

bias_levels <- data.frame(
  bias_level = c("balanced", "mild", "moderate", "severe"),
  prop_A = c(0.50, 0.60, 0.75, 0.90)
)

bias_levels$prop_B <- 1 - bias_levels$prop_A

bias_levels$n_A <- train_size * bias_levels$prop_A
bias_levels$n_B <- train_size * bias_levels$prop_B

bias_levels


# =========================
# Function to create fixed-size training sets
# =========================

create_fixed_size_train <- function(train_pool, n_A, n_B, seed = 123) {
  
  set.seed(seed)
  
  pool_A <- subset(train_pool, group == "A")
  pool_B <- subset(train_pool, group == "B")
  
  sampled_A <- pool_A[
    sample(1:nrow(pool_A), size = n_A, replace = FALSE),
  ]
  
  sampled_B <- pool_B[
    sample(1:nrow(pool_B), size = n_B, replace = FALSE),
  ]
  
  train_data <- rbind(sampled_A, sampled_B)
  
  train_data <- train_data[sample(1:nrow(train_data)), ]
  
  return(train_data)
}


# =========================
# Create training sets under different bias levels
# =========================

train_balanced <- create_fixed_size_train(
  train_pool = train_pool,
  n_A = 2000,
  n_B = 2000,
  seed = 101
)

train_mild <- create_fixed_size_train(
  train_pool = train_pool,
  n_A = 2400,
  n_B = 1600,
  seed = 102
)

train_moderate <- create_fixed_size_train(
  train_pool = train_pool,
  n_A = 3000,
  n_B = 1000,
  seed = 103
)

train_severe <- create_fixed_size_train(
  train_pool = train_pool,
  n_A = 3600,
  n_B = 400,
  seed = 104
)


# =========================
# Check group distribution
# =========================

table(train_balanced$group)
prop.table(table(train_balanced$group))

table(train_mild$group)
prop.table(table(train_mild$group))

table(train_moderate$group)
prop.table(table(train_moderate$group))

table(train_severe$group)
prop.table(table(train_severe$group))


# =========================
# Check outcome distribution
# =========================

table(train_balanced$y)
prop.table(table(train_balanced$y))

table(train_mild$y)
prop.table(table(train_mild$y))

table(train_moderate$y)
prop.table(table(train_moderate$y))

table(train_severe$y)
prop.table(table(train_severe$y))


# =========================
# Check outcome rate by group
# =========================

prop.table(table(train_balanced$group, train_balanced$y), margin = 1)
prop.table(table(train_mild$group, train_mild$y), margin = 1)
prop.table(table(train_moderate$group, train_moderate$y), margin = 1)
prop.table(table(train_severe$group, train_severe$y), margin = 1)


# =========================
# Check feature means by group
# =========================

aggregate(x1 ~ group, data = train_balanced, mean)
aggregate(x2 ~ group, data = train_balanced, mean)
aggregate(x3 ~ group, data = train_balanced, mean)

aggregate(x1 ~ group, data = train_mild, mean)
aggregate(x2 ~ group, data = train_mild, mean)
aggregate(x3 ~ group, data = train_mild, mean)

aggregate(x1 ~ group, data = train_moderate, mean)
aggregate(x2 ~ group, data = train_moderate, mean)
aggregate(x3 ~ group, data = train_moderate, mean)

aggregate(x1 ~ group, data = train_severe, mean)
aggregate(x2 ~ group, data = train_severe, mean)
aggregate(x3 ~ group, data = train_severe, mean)


# =========================
# Confirm fixed test set
# =========================

table(test_fixed$group)
prop.table(table(test_fixed$group))

table(test_fixed$y)
prop.table(table(test_fixed$y))

prop.table(table(test_fixed$group, test_fixed$y), margin = 1)

aggregate(x1 ~ group, data = test_fixed, mean)
aggregate(x2 ~ group, data = test_fixed, mean)
aggregate(x3 ~ group, data = test_fixed, mean)




# =========================
# 9. Save biased training sets
# =========================

output_dir <- here::here(
  "outputs",
  "simulated",
  "02"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  train_balanced,
  file.path(output_dir, "sim_train_balanced.csv"),
  row.names = FALSE
)

write.csv(
  train_mild,
  file.path(output_dir, "sim_train_mild.csv"),
  row.names = FALSE
)

write.csv(
  train_moderate,
  file.path(output_dir, "sim_train_moderate.csv"),
  row.names = FALSE
)

write.csv(
  train_severe,
  file.path(output_dir, "sim_train_severe.csv"),
  row.names = FALSE
)








# =========================
# 01_generate_simulated_data.R
# =========================

# Identify the project root
here::i_am("code/01_generate_simulated_data.R")

set.seed(123)

n_population <- 20000

test_size <- 1000
test_size_A <- test_size/2
test_size_B <- test_size/2


# =========================
# Generate group
# =========================

group <- rep(c("A", "B"), each = n_population /2)
group <- sample(group, size = n_population, replace = FALSE)


# =========================
# Generate features
# =========================

x1 <- ifelse(
  group == "A",
  rnorm(n_population, mean = 0.3, sd =1),
  rnorm(n_population, mean =-0.5, sd = 1)
)
x2 <- ifelse(
  group == "A",
  rnorm(n_population, mean = -0.4, sd = 1),
  rnorm(n_population, mean = 0.2, sd = 1)
)
x3 <- ifelse(
  group == "A",
  rbinom(n_population, size = 1, prob = 0.55),
  rbinom(n_population, size = 1, prob = 0.45)
)


# =========================
# Generate outcome
# =========================

linear_score <- -0.3 + 0.8 * x1 + 0.6 * x2 + 0.5 * x3 + 0.25 * ifelse(group == "B", 1, 0)
prob_y <- 1/ (1 + exp(-linear_score))

y <- rbinom(n_population, size = 1, prob = prob_y)


# =========================
# Combine data
# =========================

sim_data <- data.frame(y = y, group = factor(group), 
  x1 = x1,
  x2 = x2,
  x3 = x3
)
head(sim_data)


# =========================
# Check full simulated data
# =========================

table(sim_data$group)
prop.table(table(sim_data$group))

table(sim_data$y)
prop.table(table(sim_data$y))

prop.table(table(sim_data$group, sim_data$y), margin = 1)

aggregate(x1 ~ group, data = sim_data, mean)
aggregate(x2 ~ group, data = sim_data, mean)
aggregate(x3 ~ group, data = sim_data, mean)


# =========================
# Create fixed test set and training pool
# =========================

set.seed(123)

idx_A <- which(sim_data$group == "A")
idx_B <- which(sim_data$group == "B")

test_idx_A <- sample(idx_A, size = test_size_A, replace = FALSE)
test_idx_B <- sample(idx_B, size = test_size_B, replace = FALSE)

test_idx <- c(test_idx_A, test_idx_B)

test_fixed <- sim_data[test_idx, ]
train_pool <- sim_data[-test_idx, ]

set.seed(123)
test_fixed <- test_fixed[sample(1:nrow(test_fixed)), ]

set.seed(123)
train_pool <- train_pool[sample(1:nrow(train_pool)), ]


# =========================
# Check train pool and fixed test set
# =========================

table(train_pool$group)
prop.table(table(train_pool$group))

table(test_fixed$group)
prop.table(table(test_fixed$group))

table(train_pool$y)
prop.table(table(train_pool$y))

table(test_fixed$y)
prop.table(table(test_fixed$y))

prop.table(table(train_pool$group, train_pool$y), margin = 1)
prop.table(table(test_fixed$group, test_fixed$y), margin = 1)

aggregate(x1 ~ group, data = train_pool, mean)
aggregate(x2 ~ group, data = train_pool, mean)
aggregate(x3 ~ group, data = train_pool, mean)

aggregate(x1 ~ group, data = test_fixed, mean)
aggregate(x2 ~ group, data = test_fixed, mean)
aggregate(x3 ~ group, data = test_fixed, mean)





# =========================
# 9. Save datasets
# =========================
output_dir <- here::here(
  "outputs",
  "simulated",
  "01"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  sim_data,
  file.path(output_dir, "sim_data_full.csv"),
  row.names = FALSE
)

write.csv(
  train_pool,
  file.path(output_dir, "sim_train_pool.csv"),
  row.names = FALSE
)

write.csv(
  test_fixed,
  file.path(output_dir, "sim_test_fixed.csv"),
  row.names = FALSE
)

# =========================
# 10. Optional reproducibility information
# =========================
sessionInfo()

library(tidymodels)
library(doMC)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)
registerDoMC(cores = parallel::detectCores())

# ------------------------------------------------------------------------------

set.seed(9383)
sim_train <- sim_regression(10000, method = "hooker_2004", keep_truth = FALSE)
sim_new <- sim_regression(1000, method = "hooker_2004", keep_truth = TRUE)
sim_rs <- vfold_cv(sim_train)

mlp_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = 1000) %>% 
  set_mode("regression")

mlp_grid <- 
  crossing(
    hidden_units = c(2, (1:10) * 5),
    penalty = c(0, 10^(-4:-1))
  )

rec <-
  recipe(outcome ~ ., data = sim_train) %>%
  step_normalize(all_predictors())

ctrl <- control_grid(parallel_over = "everything")

set.seed(2083)
mlp_res <- 
  mlp_spec %>% 
  tune_grid(rec, sim_rs, grid = mlp_grid, control = ctrl)

autoplot(mlp_res, metric = "rmse")

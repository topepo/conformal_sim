library(tidymodels)
library(rules)
library(doMC)

# ------------------------------------------------------------------------------

tidymodels_prefer()
registerDoMC(cores = parallel::detectCores(TRUE))

# ------------------------------------------------------------------------------

resampling <- "cv-1"
n <- 10000
n_new <- 1000
seed <- 20

# ------------------------------------------------------------------------------

set.seed(seed)
sim_train <- sim_regression(n, method = "hooker_2004", keep_truth = FALSE)
sim_cal   <- sim_regression(500, method = "hooker_2004", keep_truth = FALSE)
sim_new   <- sim_regression(n_new, method = "hooker_2004", keep_truth = TRUE)
sim_new_pred <- sim_new %>% select(-outcome, -.truth)

# ------------------------------------------------------------------------------

rs_split <- strsplit(resampling, "-")[[1]]
rs_type <- rs_split[1]
rs_num <- as.numeric(rs_split[2])

set.seed(seed + 1)
if (rs_type == "cv") {
  if (rs_num == 1) {
    sim_rs <- vfold_cv(sim_train)
  } else {
    sim_rs <- vfold_cv(sim_train, repeats = rs_num)
  }
} else if (rs_type == "boot") {
  sim_rs <- bootstraps(sim_train, times = rs_num)
}

# ------------------------------------------------------------------------------

rec <-
  recipe(outcome ~ ., data = sim_train) %>%
  step_normalize(all_predictors())

cubist_wflow <-
  workflow() %>%
  add_formula(outcome ~ .) %>%
  add_model(cubist_rules(committees = 20, neighbors = 7))

nnet_wflow <-
  workflow() %>%
  add_recipe(rec) %>%
  add_model(mlp(hidden_units = 7, penalty = 0) %>% set_mode("regression"))

nnet_spec <-
  mlp(hidden_units = 50, penalty = 0) %>%
  set_mode("regression")

nnet_over_wflow <-
  workflow() %>%
  add_recipe(rec) %>%
  add_model(nnet_spec)

cart_wflow <-
  workflow() %>%
  add_formula(outcome ~ .) %>%
  add_model(decision_tree() %>% set_mode("regression"))

lm_wflow <-
  workflow() %>%
  add_formula(outcome ~ .) %>%
  add_model(linear_reg())

cv_res <-
  as_workflow_set(CART = cart_wflow, Cubist = cubist_wflow, lm = lm_wflow,
                  `nnet (overfit)` = nnet_over_wflow, nnet = nnet_wflow) %>%
  workflow_map("fit_resamples", resamples = sim_rs, seed = 1)

cv_results <-
  rank_results(cv_res) %>%
  select(model = wflow_id, metric = .metric, mean, n, std_err)

save(cv_results, file = "cv_results.RData")

# ------------------------------------------------------------------------------

sessioninfo::session_info()

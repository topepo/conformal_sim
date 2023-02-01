library(tidymodels)
library(conformity)
library(future)
library(glue)

# ------------------------------------------------------------------------------

tidymodels_prefer()
plan("multisession")

# ------------------------------------------------------------------------------

model <- "mars"
n <- 1000
n_new <- 500
seed <- 7976
conf_level <- 0.9
conf_method <- "full"
file_name <- glue("{conf_method}_{model}_{n}_{conf_level}_{seed}.RData")

# ------------------------------------------------------------------------------

set.seed(seed)
sim_train <- sim_regression(n, method = "hooker_2004", keep_truth = FALSE)
sim_new <- sim_regression(n_new, method = "hooker_2004", keep_truth = TRUE)
sim_new_pred <- sim_new %>% select(-outcome, -.truth)

# ------------------------------------------------------------------------------

if (model == "mars") {

  mars_wflow <-
    workflow() %>%
    add_formula(outcome ~ .) %>%
    add_model(mars() %>% set_mode("regression"))

  mod_fit <- fit(mars_wflow, sim_train)

} else if (model == "svm") {

  rec <-
    recipe(outcome ~ ., data = sim_train) %>%
    step_normalize(all_predictors())

  svm_wflow <-
    workflow() %>%
    add_recipe(rec) %>%
    add_model(svm_rbf() %>% set_mode("regression"))

  mod_fit <- fit(svm_wflow, sim_train)

} else if (model == "cart") {

  cart_wflow <-
    workflow() %>%
    add_formula(outcome ~ .) %>%
    add_model(decision_tree() %>% set_mode("regression"))

  mod_fit <- fit(cart_wflow, sim_train)

}

# ------------------------------------------------------------------------------

time_bisect <-
  system.time({
    res_bisect <- int_conformal_infer(mod_fit, sim_new_pred, level = conf_level)
  })

res_bisect <-
  res_bisect %>%
  bind_cols(sim_new) %>%
  mutate(
    out_bound = .pred_lower > .truth | .pred_upper < .truth,
    no_result = is.na(.pred_lower) | is.na(.pred_upper)
  )

sim_res <- tibble(
  seed = seed,
  training_size = n,
  eval_size = n_new,
  model = model,
  conf_level = conf_level,
  workers = unname(future::nbrOfWorkers()),
  bisect_time = time_bisect[3],
  bisect_cov = mean(!res_bisect$out_bound, na.rm = TRUE),
  bisect_fail = sum(res_bisect$no_result)
)

# ------------------------------------------------------------------------------

ctrl_grid <- control_conformal_infer(method = "grid")

time_grid <-
  system.time({
    res_grid <- int_conformal_infer(mod_fit, sim_new_pred, level = conf_level, control = ctrl_grid)
  })

res_grid <-
  res_grid %>%
  bind_cols(sim_new) %>%
  mutate(
    out_bound = .pred_lower > .truth | .pred_upper < .truth,
    no_result = is.na(.pred_lower) | is.na(.pred_upper)
  )

sim_res$grid_time <- time_grid[3]
sim_res$grid_cov <- mean(!res_grid$out_bound, na.rm = TRUE)
sim_res$grid_fail <- sum(res_grid$no_result)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

# ------------------------------------------------------------------------------

save(sim_res, file = file_name)

if (!interactive()) {
  q("no")
}

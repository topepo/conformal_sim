library(tidymodels)
library(conformity)
library(future)
library(glue)

# ------------------------------------------------------------------------------

tidymodels_prefer()
plan("multisession")

# ------------------------------------------------------------------------------

model <- "lm"
n <- 5000
n_new <- 1000
seed <- 19242
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
  
} else if (model == "lm") {
  
  lm_wflow <-
    workflow() %>%
    add_formula(outcome ~ .) %>%
    add_model(linear_reg())
  
  mod_fit <- fit(lm_wflow, sim_train)
  
}

# ------------------------------------------------------------------------------

time_search <-
  system.time({
    res_search <-
      int_conformal_infer(mod_fit,
                          sim_new_pred,
                          level = conf_level,
                          train_data = sim_train)
  })

res_search <-
  res_search %>%
  bind_cols(sim_new %>% select(.truth, outcome)) %>% 
  add_rowindex() %>% 
  mutate(
    method = "search",
    seed = seed,
    training_size = n,
    eval_size = n_new,
    model = model,
    conf_level = conf_level,
    workers = unname(future::nbrOfWorkers()),
    time = time_search[3] 
  )

# ------------------------------------------------------------------------------

ctrl_grid <- control_conformal_infer(method = "grid")

time_grid <-
  system.time({
    res_grid <-
      int_conformal_infer(
        mod_fit,
        sim_new_pred,
        level = conf_level,
        control = ctrl_grid,
        train_data = sim_train
      )
  })

res_grid <-
  res_grid %>%
  bind_cols(sim_new %>% select(.truth, outcome)) %>% 
  add_rowindex() %>% 
  mutate(
    method = "grid",
    seed = seed,
    training_size = n,
    eval_size = n_new,
    model = model,
    conf_level = conf_level,
    workers = unname(future::nbrOfWorkers()),
    time = time_grid[3] 
  )

# ------------------------------------------------------------------------------

sim_res <- bind_rows(res_search, res_grid)

# ------------------------------------------------------------------------------

if (model == "lm") {
  res_lm <- 
    predict(mod_fit, sim_new, type = "pred_int", level = conf_level) %>% 
    bind_cols(predict(mod_fit, sim_new)) %>% 
    select(1, 3, 2) %>% 
    bind_cols(sim_new %>% select(.truth, outcome)) %>% 
    add_rowindex() %>% 
    mutate(
      method = "lm_native",
      seed = seed,
      training_size = n,
      eval_size = n_new,
      model = model,
      conf_level = conf_level,
      workers = NA_integer_,
      time = NA_real_ 
    )
  sim_res <- bind_rows(sim_res, res_lm)
} 

# ------------------------------------------------------------------------------

sessioninfo::session_info()

# ------------------------------------------------------------------------------

save(sim_res, file = file_name)

if (!interactive()) {
  q("no")
}

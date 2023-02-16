library(tidymodels)
library(probably)
library(future)
library(glue)

# ------------------------------------------------------------------------------

tidymodels_prefer()
plan("multisession")

# ------------------------------------------------------------------------------

model <- "mars"
n <- 1000
n_new <- 1000
seed <- 69773
conf_level <- 0.9
conf_method <- "full"
hidden_units <- 20
file_name <- glue("{conf_method}_{model}_{n}_{conf_level}_{seed}.RData")

# ------------------------------------------------------------------------------

set.seed(seed)
sim_train <- sim_regression(n, method = "hooker_2004", keep_truth = FALSE)
sim_new <- sim_regression(n_new, method = "hooker_2004", keep_truth = TRUE)
sim_new_pred <- sim_new %>% select(-outcome, -.truth)

# ------------------------------------------------------------------------------

if (model == "mars") {
  
  mod_wflow <-
    workflow() %>%
    add_formula(outcome ~ .) %>%
    add_model(mars() %>% set_mode("regression"))
  
  mod_fit <- fit(mod_wflow, sim_train)
  
} else if (model == "mlp") {
  
  rec <-
    recipe(outcome ~ ., data = sim_train) %>%
    step_normalize(all_predictors())
  
  mod_wflow <-
    workflow() %>%
    add_recipe(rec) %>%
    add_model(
      # >50 hidden units is over-fit
      mlp(hidden_units = hidden_units) %>% 
        set_mode("regression") %>% 
        set_engine("nnet", MaxNWts = 1500)
    )
  
  mod_fit <- fit(mod_wflow, sim_train)
  chr_units <- format(1:100)[hidden_units]
  model <- paste0("mlp (", chr_units, ")")
  
} else if (model == "lm") {
  
  mod_wflow <-
    workflow() %>%
    add_formula(outcome ~ .) %>%
    add_model(linear_reg())
  
  mod_fit <- fit(mod_wflow, sim_train)
  
}

# ------------------------------------------------------------------------------

time_search <-
  system.time({
    obj_search <- int_conformal_infer(mod_fit, train_data = sim_train)
    res_search <-predict(obj_search, sim_new_pred, level = conf_level)
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
    obj_grid <- int_conformal_infer(mod_fit, train_data = sim_train, control = ctrl_grid)
    res_grid <-predict(obj_grid, sim_new_pred, level = conf_level)
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

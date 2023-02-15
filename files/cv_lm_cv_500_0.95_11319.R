library(tidymodels)
library(probably)
library(glue)
library(doMC)

# ------------------------------------------------------------------------------

tidymodels_prefer()
cores <- parallelly::availableCores()
registerDoMC(cores = cores)

# ------------------------------------------------------------------------------

model <- "lm"
n <- 500
n_new <- 1000
seed <- 11319
conf_level <- 0.95
resample_type <- "cv"
conf_method <- "cv"
hidden_units <- 20
file_name <- 
  glue("{conf_method}_{model}_{resample_type}_{n}_{conf_level}_{seed}.RData")

# ------------------------------------------------------------------------------

set.seed(seed)
sim_train <- sim_regression(n, method = "hooker_2004", keep_truth = FALSE)
sim_new <- sim_regression(n_new, method = "hooker_2004", keep_truth = TRUE)
sim_new_pred <- sim_new %>% select(-outcome, -.truth)

if (resample_type == "cv") {
  sim_rs <- vfold_cv(sim_train)
} else {
  sim_rs <- bootstraps(sim_train, times = 25)
}

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

set.seed(838)
rs_res <- 
  mod_wflow %>% 
  fit_resamples(sim_rs, control = control_resamples(save_pred = TRUE, extract = I))

# ------------------------------------------------------------------------------

time_cv_plus <-
  system.time({
    obj_cv_plus <- int_conformal_infer_cv(rs_res)
    res_cv_plus <- predict(obj_cv_plus, sim_new_pred, level = conf_level)
  })

sim_res <-
  res_cv_plus %>%
  bind_cols(
    predict(mod_fit, sim_new_pred),
    sim_new %>% select(.truth, outcome)
  ) %>% 
  add_rowindex() %>% 
  mutate(
    method = "cv+",
    resample = resample_type,
    seed = seed,
    training_size = n,
    eval_size = n_new,
    model = model,
    conf_level = conf_level,
    workers = cores,
    time = time_cv_plus[3] 
  )

# ------------------------------------------------------------------------------

if (model == "lm") {
  res_lm <- 
    predict(mod_fit, sim_new, type = "pred_int", level = conf_level) %>% 
    bind_cols(predict(mod_fit, sim_new)) %>% 
    bind_cols(sim_new %>% select(.truth, outcome)) %>% 
    add_rowindex() %>% 
    mutate(
      method = "lm_native",
      resample = resample_type,
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

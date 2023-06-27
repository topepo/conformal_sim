library(tidymodels)
library(probably)
library(glue)

# ------------------------------------------------------------------------------

tidymodels_prefer()

# ------------------------------------------------------------------------------

model <- "cart"
resampling <- "boot-30"
n <- 10000
n_new <- 1000
seed <- 19
conf_level <- 0.95
file_name <- glue("{model}_{n}_{conf_level}_{resampling}_{seed}.RData")

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

if (model == "cubist") {
  
  library(rules)
  model_wflow <-
    workflow() %>%
    add_formula(outcome ~ .) %>%
    add_model(cubist_rules(committees = 20, neighbors = 7))
  
} else if (model == "nnet") {
  
  model_wflow <-
    workflow() %>%
    add_recipe(rec) %>%
    add_model(mlp(hidden_units = 7, penalty = 0) %>% set_mode("regression"))
  
} else if (model == "nnet_overfit") {
  
  nnet_spec <- 
    mlp(hidden_units = 50, penalty = 0) %>% 
    set_mode("regression") 
  
  model_wflow <-
    workflow() %>%
    add_recipe(rec) %>%
    add_model(nnet_spec)
  
} else if (model == "cart") {
  
  model_wflow <-
    workflow() %>%
    add_formula(outcome ~ .) %>%
    add_model(decision_tree() %>% set_mode("regression"))
  
} else if (model == "lm") {
  
  model_wflow <-
    workflow() %>%
    add_formula(outcome ~ .) %>%
    add_model(linear_reg())
}

# ------------------------------------------------------------------------------

mod_fit <- fit(model_wflow, sim_train)

# ------------------------------------------------------------------------------
# split sample inference

time_split <-
  system.time({
    int_split <-
      int_conformal_split(mod_fit, cal = sim_cal)
  })

pred_split <- 
  predict(int_split, sim_new, level = conf_level) %>%
  bind_cols(sim_new %>% select(.truth, outcome)) %>% 
  add_rowindex()

res_split <-
  pred_split %>% 
  mutate(
    method = "split",
    seed = seed,
    training_size = n,
    eval_size = n_new,
    cal_size = nrow(sim_cal),
    resampling = "none",
    resamples = nrow(sim_rs),
    model = model,
    conf_level = conf_level,
    time = time_split[3] 
  )

# ------------------------------------------------------------------------------
# quantile inference

time_quant <-
  system.time({
    int_quant <-
      int_conformal_quantile(
        mod_fit, 
        train_data = sim_train,  
        cal_data = sim_cal, 
        level = conf_level
      )
  })

pred_quant <- 
  predict(int_quant, sim_new) %>%
  bind_cols(sim_new %>% select(.truth, outcome)) %>% 
  add_rowindex()

res_quant <-
  pred_quant %>% 
  mutate(
    method = "quantile",
    seed = seed,
    training_size = n,
    eval_size = n_new,
    cal_size = nrow(sim_cal),
    resampling = "none",
    resamples = nrow(sim_rs),
    model = model,
    conf_level = conf_level,
    time = time_quant[3] 
  )

# ------------------------------------------------------------------------------
# cv+ inference

ctrl <- control_resamples(save_pred = TRUE, save_workflow = TRUE, extract = I)

time_cv <-
  system.time({
    mod_rs <- fit_resamples(model_wflow, sim_rs, control = ctrl)
    int_cv <- int_conformal_cv(mod_rs)
  })

pred_cv <- 
  predict(int_cv, sim_new, level = conf_level) %>%
  bind_cols(sim_new %>% select(.truth, outcome)) %>% 
  add_rowindex()

res_cv <-
  pred_cv %>% 
  mutate(
    method = "cv+",
    seed = seed,
    training_size = n,
    eval_size = n_new,
    cal_size = nrow(sim_cal),
    resampling = "boot-30",
    resamples = nrow(sim_rs),
    model = model,
    conf_level = conf_level,
    time = time_cv[3] 
  )

# ------------------------------------------------------------------------------

sim_res <- bind_rows(res_split, res_quant, res_cv)

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
      resampling = "boot-30",
      resamples = nrow(sim_rs),
      model = model,
      conf_level = conf_level,
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

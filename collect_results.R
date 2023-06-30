
library(tidymodels)
library(glue)
library(stringr)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

get_res <- function(x) {
  load(x)
  sim_res$file <- basename(x)
  sim_res
}

rdata_files <- list.files(path = "files", pattern = "RData$", full.names = TRUE)
r_files <- list.files(path = "files", pattern = "\\.R$", full.names = TRUE)
pct_done <- round(length(rdata_files)/length(r_files) * 100, 1)
cat(pct_done, "% of the simulations are finished\n", sep = "")

# ------------------------------------------------------------------------------

res_raw <- 
  list.files(path = "files", pattern = "RData$", full.names = TRUE) %>% 
  map_dfr(get_res) %>% 
  mutate(
    out_of_bound = .pred_lower > outcome | .pred_upper < outcome,
    no_result = is.na(.pred_lower) | is.na(.pred_upper),
    int_width = .pred_upper - .pred_lower,
    model = ifelse(model == "nnet_overfit", "nnet (overfit)", model)
    # conf_level = format(conf_level),
    # conf_level = paste0(conf_level, "%")
  )

# ------------------------------------------------------------------------------
# TODO the non-cv+ results are replicated so get the unique values (or slice on seed)

non_resampled_coverage <- 
  res_raw %>% 
  filter(resampling == "none" & grepl("cv-1", file)) %>% 
  group_by(training_size, model, conf_level, method, resampling, resamples) %>% 
  summarize(
    coverage = mean(!out_of_bound, na.rm = TRUE),
    failure = mean(no_result, na.rm = TRUE),
    num_sims = length(unique(file)),
    time = median(time),
    width = mean(.pred_upper - .pred_lower),
    num_res = n(),
    .groups = "drop"
  ) %>% 
  mutate(
    training_size = format(training_size),
    resamples = 0
  ) 

resampled_coverage <- 
  res_raw %>% 
  filter(resampling != "none") %>% 
  group_by(training_size, model, conf_level, method, resampling, resamples) %>% 
  summarize(
    coverage = mean(!out_of_bound, na.rm = TRUE),
    failure = mean(no_result, na.rm = TRUE),
    num_sims = length(unique(file)),
    time = median(time),
    width = mean(.pred_upper - .pred_lower),
    num_res = n(),
    .groups = "drop"
  ) %>% 
  mutate(
    training_size = format(training_size),
    resampling = ifelse(grepl("^boot", resampling), "Bootstrap", "Cross-Validation")
  ) 

basic_coverage <- 
  resampled_coverage %>% 
  filter(resampling == "Cross-Validation" & resamples == 10) %>% 
  bind_rows(non_resampled_coverage)

# ------------------------------------------------------------------------------

parametric_coverage <- 
  basic_coverage %>% 
  filter(method == "lm_native") %>% 
  rename(parametric = coverage, parametric_width = width) %>% 
  select(-method, -failure, -num_sims, -time, -num_res, -resampling, -resamples, -model) 

lm_comparison <-
  basic_coverage %>%
  filter(model == "lm" & method != "lm_native") %>%
  select(-model,-failure,-num_sims,-time,-num_res) %>%
  full_join(parametric_coverage, by = c("training_size", "conf_level")) %>% 
  mutate(
    coverage = (coverage - parametric),
    width = (width - parametric_width) / parametric_width
  )

# ------------------------------------------------------------------------------

save(lm_comparison, basic_coverage, file = "simulation_results.RData")

# ------------------------------------------------------------------------------

sessioninfo::session_info()


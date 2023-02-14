
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

rdata_files <- list.files(path = "files", pattern = "^cv.*RData$", full.names = TRUE)
r_files <- list.files(path = "files", pattern = "^cv.*R$", full.names = TRUE)

res_raw <- 
  list.files(path = "files", pattern = "RData$", full.names = TRUE) %>% 
  map_dfr(get_res) %>% 
  mutate(
    out_of_bound = .pred_lower > outcome | .pred_upper < outcome,
    no_result = is.na(.pred_lower) | is.na(.pred_upper),
    int_width = .pred_upper - .pred_lower
  )

res_coverage <- 
  res_raw %>% 
  group_by(training_size, model, resample, conf_level, method, file) %>% 
  summarize(
    coverage = mean(!out_of_bound, na.rm = TRUE),
    failure = mean(no_result, na.rm = TRUE),
    num_sims = length(unique(file)),
    .groups = "drop"
  ) %>% 
  mutate(
    training_size = format(training_size)
  ) 

res_coverage %>%
  filter(method != "lm_native") %>% 
  select(training_size, model, resample, conf_level, method, coverage) %>%
  ggplot(aes(x = training_size, y = coverage, col = resample)) +
  geom_boxplot(position = position_dodge(width = 1)) +
  facet_grid(model ~ conf_level)

res_coverage %>%
  filter(method != "lm_native") %>% 
  group_by(training_size, model, resample, conf_level, method) %>% 
  count() %>% 
  arrange(n)

# ------------------------------------------------------------------------------


res_width_mean <- 
  res_raw %>% 
  group_by(method, training_size, model, resample, conf_level) %>% 
  summarize(
    mean_width = mean(int_width, na.rm = TRUE),
    .groups = "drop"
  )

res_width_diff <- 
  res_raw %>%
  select(method, training_size, model, resample, conf_level, int_width, .row, file) %>%
  pivot_wider(
    id_cols = c(training_size, model, resample, conf_level, .row, file),
    names_from = "method",
    values_from = "int_width"
  ) %>% 
  mutate(
    lm_vs_lm_search = lm_native - `cv+`
  ) 

res_width_diff %>%
  select(training_size, conf_level, 
         search = lm_vs_lm_search, grid = lm_vs_lm_grid) %>%
  pivot_longer(c(search, grid), names_to = "method", values_to = "difference") %>% 
  filter(!is.na(difference)) %>% 
  mutate(
    training_size = format(training_size)
  ) %>% 
  ggplot(aes(x = training_size, y = difference)) +
  geom_boxplot(position = position_dodge()) +
  geom_hline(yintercept = 0, col = "green", lty = 2) +
  facet_grid(method ~ conf_level) +
  labs(y = "parametric - conformal")

# ------------------------------------------------------------------------------



library(tidymodels)
library(glue)
library(stringr)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)


get_res <- function(x) {
  load(x)
  sim_res$file <- basename(x)
  sim_res
}

# ------------------------------------------------------------------------------
# CV results

rdata_cv_files <- list.files(path = "files", pattern = "^cv.*RData$", full.names = TRUE)
r_cv_files <- list.files(path = "files", pattern = "^cv.*R$", full.names = TRUE)
cat(round(length(rdata_cv_files) / length(r_cv_files) * 100, 2), "% finished\n")

cv_raw <- 
  rdata_cv_files %>% 
  map_dfr(get_res) %>% 
  mutate(
    out_of_bound = .pred_lower > outcome | .pred_upper < outcome,
    no_result = is.na(.pred_lower) | is.na(.pred_upper),
    int_width = .pred_upper - .pred_lower
  )

# ------------------------------------------------------------------------------
# full results

rdata_full_files <- list.files(path = "files", pattern = "^full.*RData$", full.names = TRUE)
r_full_files <- list.files(path = "files", pattern = "^full.*R$", full.names = TRUE)
cat(round(length(rdata_full_files) / length(r_full_files) * 100, 2), "% finished\n")

full_raw <- 
  rdata_full_files %>% 
  map_dfr(get_res) %>% 
  mutate(
    out_of_bound = .pred_lower > outcome | .pred_upper < outcome,
    no_result = is.na(.pred_lower) | is.na(.pred_upper),
    int_width = .pred_upper - .pred_lower
  ) %>% 
  mutate(resample = "none")


# ------------------------------------------------------------------------------

coverage_dat <- 
  bind_rows(cv_raw, full_raw) %>% 
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

coverage_dat %>%
  filter(method != "lm_native" & conf_level == 0.9) %>% 
  select(training_size, model, resample, method, coverage) %>%
  ggplot(aes(x = training_size, y = coverage, col = resample)) + 
  geom_hline(yintercept = 0.9) +
  geom_boxplot(position = position_dodge(width = 2 / 3), width = 1 / 2) +
  facet_grid(model ~ method)

coverage_dat %>%
  filter(method != "lm_native" & conf_level == 0.95) %>% 
  select(training_size, model, resample, method, coverage) %>%
  ggplot(aes(x = training_size, y = coverage, col = resample)) + 
  geom_hline(yintercept = 0.95) +
  geom_boxplot(position = position_dodge(width = 2 / 3), width = 1 / 2) +
  facet_grid(model ~ method)


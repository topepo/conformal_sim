
library(tidymodels)
library(glue)
library(stringr)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

rs_cols <- RColorBrewer::brewer.pal(9, "YlOrRd")[-(1:2)]
mth_cols <- RColorBrewer::brewer.pal(3, "Dark2")
mth_lvl <- c("split", "quantile", "cv+")

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

basic_coverage %>% 
  filter(method != "lm_native") %>% 
  ggplot(aes(x = training_size, y = coverage, col = method, pch = method)) + 
  geom_hline(aes(yintercept = conf_level), lty = 3) +
  geom_point(cex = 3 / 4) +
  geom_line(aes(group = method)) +
  facet_grid(model ~ conf_level) +
  lims(y = 0:1) +
  scale_color_manual(values = mth_cols)

basic_coverage %>% 
  filter(method != "lm_native") %>% 
  ggplot(aes(x = training_size, y = coverage, col = method, pch = method)) + 
  geom_hline(aes(yintercept = conf_level), lty = 3) +
  geom_point(cex = 3 / 4) +
  geom_line(aes(group = method)) +
  facet_grid(model ~ conf_level) +
  scale_color_manual(values = mth_cols)

# ------------------------------------------------------------------------------


resampled_coverage %>% 
  filter(resampling == "Cross-Validation" & method != "lm_native") %>% 
  mutate(`number of resamples` = format(resamples)) %>% 
  ggplot(
    aes(
      x = training_size,
      y = coverage,
      col = `number of resamples`,
      group = `number of resamples`,
      pch = `number of resamples`
    )
  ) +
  geom_hline(aes(yintercept = conf_level), lty = 3) +
  geom_point(cex = 3 / 4) +
  geom_line() +
  facet_grid(model ~ conf_level) +
  scale_color_manual(values = rs_cols) 

resampled_coverage %>% 
  filter(resampling == "Bootstrap" & method != "lm_native") %>% 
  mutate(`number of resamples` = format(resamples)) %>% 
  ggplot(
    aes(
      x = training_size,
      y = coverage,
      col = `number of resamples`,
      group = `number of resamples`,
      pch = `number of resamples`
    )
  ) +
  geom_hline(aes(yintercept = conf_level), lty = 3) +
  geom_point(cex = 3 / 4) +
  geom_line() +
  facet_grid(model ~ conf_level) +
  scale_color_manual(values = rs_cols)

# ------------------------------------------------------------------------------


lm_comparison %>% 
  ggplot(aes(x = training_size, y = coverage, col = method)) + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_point() +
  geom_line(aes(group = method)) +
  facet_wrap( ~ conf_level) +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "conformal - parametric", title = "coverage") +
  scale_color_manual(values = mth_cols)

lm_comparison %>% 
  ggplot(aes(x = training_size, y = width, col = method)) + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_point() +
  geom_line(aes(group = method)) +
  facet_wrap( ~ conf_level) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "conformal - parametric", title = "interval width") +
  scale_color_manual(values = mth_cols)



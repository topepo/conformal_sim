library(tidymodels)
library(glue)
library(stringr)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

template <- readLines("template_cv.R")

# ------------------------------------------------------------------------------

num_sim <- 25

# Seed 5071 all models x 25 sims, mlp's use 20 HU 
# Seed 4403 mlp x 25 sims only with 100 HU
set.seed(4403)

combinations <- 
  crossing(
    model = "mlp",
    training = c(500, 1000, 5000),
    eval = 1000,
    seed = sample.int(10^5, num_sim),
    conf = c(0.90, 0.95),
    resamp = c("cv", "boot")
  ) %>% 
  mutate(
    file = glue("files/cv_{model}_{resamp}_{training}_{conf}_{seed}.R")
  )

new_file <- function(x, template) {
  template <- gsub("MODEL", x$model, template)
  template <- gsub("RESAMP", x$resamp, template)  
  template <- gsub("NTRAIN", x$training, template)  
  template <- gsub("NEVAL", x$eval, template)  
  template <- gsub("SEED", x$seed, template)   
  template <- gsub("CONF", x$conf, template)  
  cat(template, sep = "\n", file = x$file)
  invisible(NULL)
}

for (i in 1:nrow(combinations)) {
  new_file(combinations[i,], template)
}



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
set.seed(5071)

combinations <- 
  crossing(
    model = c("mars", "mlp", "lm"),
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

# ------------------------------------------------------------------------------

src_files <- list.files(path = "files", pattern = "^cv.*R$")
src_files <- sample(src_files)
rda_files <- gsub("R$", "RData", src_files)

# target_list <- 
#   paste(rda_files, collapse = " ") %>% 
#   str_wrap() %>% 
#   str_split("\n") 

# target_list <- paste0("\t", target_list[[1]], collapse = " \\ \n")

target_list <- paste0(rda_files, collapse = " ")

target_list <- paste0("all: ", target_list, "\n\n")

instruct <- function(src_file) {
  glue(
"
{src_file}Data: {src_file} 
\t@date '+ %Y-%m-%d %H:%M:%S: + {src_file}'
\t@$(RCMD) BATCH --vanilla {src_file}
\t@date '+ %Y-%m-%d %H:%M:%S: - {src_file}'

"
  )
}

instructions <- map_chr(src_files, instruct)
instructions <- paste0(instructions, collapse = "\n")

header <- 
"SHELL = /bin/bash
R    ?= R 
RCMD  =@$(R) CMD
TIMESTAMP = $(shell  date '+%Y-%m-%d-%H-%M')
here=${PWD}/..
"

cat(header, file = "files/makefile", sep = "")

cat(target_list, file = "files/makefile", append = TRUE, sep = "")
cat(instructions, file = "files/makefile", append = TRUE, sep = "")


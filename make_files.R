
library(tidymodels)
library(glue)
library(stringr)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

template <- readLines("template.R")

# ------------------------------------------------------------------------------

num_sim <- 50

set.seed(1)

combinations <- 
  crossing(
    model = c("cubist", "cart", "lm", "nnet", "nnet_overfit"),
    resample = c("cv-1", "cv-2", "cv-3", "cv-4", "cv-5", 
                 "boot-10", "boot-20", "boot-30", "boot-40", "boot-50"),
    training = c(500, 1000, 5000, 10000),
    eval = 1000,
    seed = sample.int(10^5, num_sim),
    conf = c(0.90, 0.95)
  ) %>% 
  mutate(
    file = glue("files/{model}_{training}_{conf}_{resample}_{seed}.R")
  )

new_file <- function(x, template) {
  template <- gsub("MODEL", x$model, template)
  template <- gsub("NTRAIN", x$training, template)  
  template <- gsub("NEVAL", x$eval, template)  
  template <- gsub("SEED", x$seed, template)   
  template <- gsub("RESAMPLE", x$resample, template)     
  template <- gsub("CONF", x$conf, template)  
  cat(template, sep = "\n", file = x$file)
  invisible(NULL)
}

for (i in 1:nrow(combinations)) {
  new_file(combinations[i,], template)
}

# ------------------------------------------------------------------------------

src_files <- list.files(path = "files", pattern = "*.*R$")
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


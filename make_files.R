
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

num_sim <- 100

set.seed(1)

combinations <- 
  crossing(
    model = c("mars", "cart"),
    training = c(100, 1000),
    eval = 500,
    seed = sample.int(10^5, num_sim),
    conf = c(0.90, 0.95)
  ) %>% 
  mutate(
    file = glue("files/full_{model}_{training}_{conf}_{seed}.R")
  )

new_file <- function(x, template) {
  template <- gsub("MODEL", x$model, template)
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

src_files <- list.files(path = "files", pattern = "^full.*R$")
rda_files <- gsub("R$", "RData", src_files)
target_list <- 
  paste(rda_files, collapse = " ") %>% 
  str_wrap() %>% 
  str_split("\n") 

target_list <- paste0("\t", target_list[[1]], collapse = " \\ \n")

target_list <- paste("all: ", target_list, "\n\n")

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

cat(header, file = "files/makefile")

cat(target_list, file = "files/makefile", append = TRUE)
cat(instructions, file = "files/makefile", append = TRUE)


make_file <- paste(header, target_list, instructions, collapse = "\n")
cat(make_file)


src_files <- list.files(path = "files", pattern = "^(cv)|(full).*R$")
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


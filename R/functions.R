if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(officer, officedown, knitr, rmarkdown, tidyverse, qdapRegex, readxl)



# set output

add_output <- function(text, child){
  childdoc <- readLines(child, encoding = "UTF-8")
  yaml_position <- text %>% str_detect("---") %>% which()
  start <- yaml_position[2]
  newtext <- c(text[1:(start-1)], childdoc, text[(start):length(text)])
  return(newtext)
}



# 定制开头

add_opening <- function(text, child){
  childdoc <- str_c("```{r, child=c('", child, "')}")
  text <- text %>% str_replace("<!-- opening -->",childdoc)
  text <- text %>% str_replace("<!-- openingending -->","```")

  return(text)
}

# 查有几个作者
author_line <- function(text){
  out <- text
  bib <- out %>% str_extract("author_names.*c\\(.*")
  bib <- bib[bib %>% is.na() == F]
  n <- str_count(bib, ",") + 1
  return(n)
}

add_ending <- function(text, child){
  n <- author_line(text)
  childdoc <- readLines(child, encoding = "UTF-8")

  added <- str_c("`r ftext(institute_str[", seq(1,n), "], ft)`")
  insti <- character()
  for (i in 1:n) {
    insti <- c(insti, added[i], "")
  }
  text <- c(text, childdoc, insti)
  return(text)
}

build_rmd <- function(file, name){
  build_parameters <- load("sysdata.rda")
  dt <- subset(build_parameters, name == name)
  output <- dt$output
  opening	<- dt$opening
  ending <- dt$ending

  text <- readLines(file, encoding = "UTF-8")

  text_out <- add_output(text, output)

  text_out <- add_opening(text_out, opening)

  if(ending %>% is.na() == F){
    text_out <- add_ending(text_out, ending)
  }
  return(text_out)
}


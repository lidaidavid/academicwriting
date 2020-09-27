# set output

add_output <- function(text, child){
  childdoc <- readLines(child, encoding = "UTF-8")
  yaml_position <-  which(stringr::str_detect(text, "---"))
  start <- yaml_position[2]
  newtext <- c(text[1:(start-1)], childdoc, text[(start):length(text)])
  return(newtext)
}



# 定制开头

add_opening <- function(text, child){
  childdoc <- readLines(child, encoding = "UTF-8")
  yaml_position <-  which(stringr::str_detect(text, "<!-- opening -->"))
  start <- yaml_position[1]
  newtext <- c(text[1:(start-1)], childdoc, text[(start):length(text)])
  return(newtext)

}

# 查有几个作者

author_line <- function(text){
  out <- text
  bib <- stringr::str_extract(out, "author_names.*c\\(.*")
  bib <- bib[is.na(bib) == F]
  n <- stringr::str_count(bib, ",") + 1
  return(n)
}

add_ending <- function(text, child){
  n <- author_line(text)
  childdoc <- readLines(child, encoding = "UTF-8")

  added <- stringr::str_c("`r ftext(institute_str[", seq(1,n), "], ft)`")
  insti <- character()
  for (i in 1:n) {
    insti <- c(insti, added[i], "")
  }
  text <- c(text, childdoc, insti)
  return(text)
}

#' @export
build_rmd <- function(file, name){
  dt <- subset(build_parameters, name == name)
  output <- system.file("template-rmd", stringr::str_c(dt$output,".Rmd"), package = "academicwriting")
  opening <- system.file("template-rmd", stringr::str_c(dt$opening,".Rmd"), package = "academicwriting")
  ending <- system.file("template-rmd", stringr::str_c(dt$ending,".Rmd"), package = "academicwriting")

  text <- readLines(file, encoding = "UTF-8")

  text_out <- add_output(text, output)

  text_out <- add_opening(text_out, opening)

  if(is.na(ending) == F){
    text_out <- add_ending(text_out, ending)
  }
  return(text_out)
}


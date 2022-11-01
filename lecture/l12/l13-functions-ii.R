## ----child = "setup.Rmd"---------------------------

## ----setup, include=FALSE--------------------------
# R options
options(
  htmltools.dir.version = FALSE, # for blogdown
  show.signif.stars = FALSE,     # for regression output
  warn = 1,
  pillar.print_max = 8,
  pillar.print_min = 4,
  pillar.max_footer_lines = 3,
  pillar.width = 65,
  width = 65)
 
# ggplot2 color palette with gray
color_palette <- list(gray = "#999999", 
                      salmon = "#E69F00", 
                      lightblue = "#56B4E9", 
                      green = "#009E73", 
                      yellow = "#F0E442", 
                      darkblue = "#0072B2", 
                      red = "#D55E00", 
                      purple = "#CC79A7")
# For nonsese...
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
# For magick
dev.off <- function(){
  invisible(grDevices::dev.off())
}

# figure height, width, dpi
knitr::opts_chunk$set(echo = TRUE, 
                      fig.width = 8, 
                      fig.asp = 0.618,
                      out.width = "60%",
                      fig.align = "center",
                      dpi = 300,
                      message = FALSE)
# ggplot2
ggplot2::theme_set(ggplot2::theme_gray(base_size = 16))
# set seed
set.seed(1234)
# fontawesome
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
# magick
dev.off <- function(){
  invisible(grDevices::dev.off())
}
# conflicted
library(conflicted)
conflict_prefer("filter", "dplyr")
# xaringanExtra
library(xaringanExtra)
xaringanExtra::use_panelset()
# output number of lines
hook_output <- knitr::knit_hooks$get("output")
knitr::knit_hooks$set(output = function(x, options) {
  lines <- options$output.lines
  if (is.null(lines)) {
    return(hook_output(x, options))  # pass to default hook
  }
  x <- unlist(strsplit(x, "\n"))
  more <- "..."
  if (length(lines)==1) {        # first n lines
    if (length(x) > lines) {
      # truncate the output, but add ....
      x <- c(head(x, lines), more)
    }
  } else {
    x <- c(more, x[lines], more)
  }
  # paste these lines together
  x <- paste(c(x, ""), collapse = "\n")
  hook_output(x, options)
})



## ---- include = FALSE------------------------------
library(tidyverse)
library(gapminder)


## --------------------------------------------------
qdiff3 = function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles = quantile(x, probs)
  max(the_quantiles) - min(the_quantiles)
}


## ----error = TRUE----------------------------------
z = gapminder$lifeExp
z[3] = NA
quantile(z)
quantile(z, na.rm = TRUE)


## --------------------------------------------------
qdiff4 = function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles = quantile(x, probs, na.rm = TRUE)
  max(the_quantiles) - min(the_quantiles)
}
qdiff4(gapminder$lifeExp)
qdiff4(z)


## ----error = TRUE----------------------------------
qdiff5 = function(x, probs = c(0, 1), na.rm = TRUE) {
  stopifnot(is.numeric(x))
  the_quantiles = quantile(x, probs, na.rm = na.rm)
  max(the_quantiles) - min(the_quantiles)
}
qdiff5(gapminder$lifeExp)
qdiff5(z)
qdiff5(z, na.rm = FALSE)


## --------------------------------------------------
set.seed(1234)
z = rnorm(10)
quantile(z, type = 1)
quantile(z, type = 4)


## --------------------------------------------------
qdiff6 = function(x, probs = c(0, 1), na.rm = TRUE, ...) {
  the_quantiles = quantile(x = x, probs = probs, na.rm = na.rm, ...)
  max(the_quantiles) - min(the_quantiles)
}


## --------------------------------------------------
qdiff6(z, probs = c(0.25, 0.75), type = 1)
qdiff6(z, probs = c(0.25, 0.75), type = 4)


## --------------------------------------------------
a = 10
my_function = function(a, x){
  a = 5
  a*x
}


## --------------------------------------------------
my_function(1, 2)
print(a)


## --------------------------------------------------
my_function(a, 2)
print(a)


## --------------------------------------------------
a = -5
my_function2 = function(x){
  a*x
}

my_function2(2)



## --------------------------------------------------
data = dplyr::starwars

process_data = function(dta){
  region_summary = filter(dta, !is.na(result)) %>% 
    group_by(region) %>%
    summarize(mean = mean(result))
  # do some other stuff
  left_join(data, region_summary) #Uhoh!  This should have been dta! #<<
}


## --------------------------------------------------
a = -5
my_function3 = function(x){
  a = 1e9
  a*x
}
my_function3(2)
print(a)


## --------------------------------------------------
quartic_eq = function(x) 3*x^4 - 10*x^3 - 20*x^2 + 10*x - 5
class(quartic_eq)
body(quartic_eq)
formals(quartic_eq)


## --------------------------------------------------
curve(quartic_eq, from = -5, to = 5)
optimize(quartic_eq, interval = c(-5, 5))


## --------------------------------------------------
n = 5
log.vec = 0
for (i in seq_len(n)) {
  log.vec[i] = log(i)
}
log.vec


## --------------------------------------------------
x = sort(runif(5))
n_negative = sum(x<0)
for (i in 1:n_negative){
  print(x[i])
}
cat("Exited loop.")


## --------------------------------------------------
x = sort(runif(5))
n_negative = sum(x<0)
for (i in seq_len(n_negative)){
  print(x[i])
}
cat("Exited loop.")


## --------------------------------------------------
n = 5
log.vec = 0
for (i in 1:n) {
  if (log(i) > 1) {
    cat("I'm outta here. I don't like numbers bigger than 1\n")
    break
  }
  log.vec[i] = log(i)
}
log.vec


## --------------------------------------------------
for (str in c("Tatoosh", "Infimum", "McGrindleCat")) {
  cat(glue::glue("Free (OBO): {str}, one gently used cat."))
}


## --------------------------------------------------
for (i in seq_len(4)) {
  for (j in i:4) {
    cat(paste(j,""))
  }
  cat("\n")
}


## --------------------------------------------------
X = model.matrix(~ scale(mass), data = dplyr::starwars)
y = filter(starwars, !is.na(height) & !is.na(mass)) %>% pull(height)
beta = matrix(0, nrow = ncol(X))
beta_new = matrix(1, nrow = ncol(X))
epsilon = 1e-4
i = 0
while (mean((beta - beta_new)^2) > epsilon) {
 print(glue::glue("At i = {i}, beta = ({beta[1]}, {beta[2]})"))
 beta = beta_new
 beta_new = beta + 0.02 * crossprod(X, y -  X%*%beta)
 i = i + 1
}


## --------------------------------------------------
lm(height ~ scale(mass), data = dplyr::starwars)


## --------------------------------------------------
weird_list = list(rnorm(5), stringr::fruit, TRUE, starwars)
lapply(weird_list, length)


## --------------------------------------------------
lapply(weird_list, class)


## --------------------------------------------------
purrr::map_dfr(weird_list, function(x){
  tibble(length = length(x), class = class(x)[1])
}, .id = 'item')


## --------------------------------------------------
(A = matrix(1:18, nrow = 3))
count_odd = function(x) sum(x %% 2 > 0)
apply(A, 1, count_odd)
apply(A, 2, count_odd)


## --------------------------------------------------
collatz = function(n){
  cat(n)
  if(n == 1) return(invisible(1))
  cat("->")
  if(n %% 2 == 1){ # odd
    return(collatz(3*n + 1))
  } else{ # even
    return(collatz(n /2))
  }
}


## --------------------------------------------------
collatz(10)


## --------------------------------------------------
collatz(101)


## --------------------------------------------------
collatz(837799)


## ----eval = TRUE, error = TRUE---------------------
library(testthat)
test_that('invalid args are detected', {
  expect_error(qdiff6("eggplants are purple"))
  expect_error(qdiff6(iris))
})
test_that('NA handling works', {
  expect_error(qdiff6(c(1:5, NA), na.rm = FALSE))
  expect_equal(qdiff6(c(1:5, NA)), 4)
})


## ----end_func3, eval = TRUE, error = TRUE----------
qdiff_no_NA = function(x, probs = c(0, 1)) {
  the_quantiles = quantile(x = x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}
test_that('NA handling works', {
  expect_that(qdiff_no_NA(c(1:5, NA)), equals(4))
})


## ----links, child="l12/links.md"-------------------




## ----child = "setup.Rmd"-------------------

## ----setup, include=FALSE------------------
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



## ----links, child="l12/links.md"-----------




## ---- include=FALSE------------------------
library(tidyverse)
library(rvest)
library(lubridate)
theme_set(theme_minimal())


## ----start_func1---------------------------
library(gapminder)
glimpse(gapminder)


## ------------------------------------------
ggplot(gapminder, aes(x = year, y = lifeExp, color = continent))  + 
  geom_line(aes(group = country), alpha = .5) + scale_color_brewer(type = 'qual') + 
  geom_boxplot(data = filter(gapminder, year %in% seq(from=1952, to = 2002, by = 20)), 
               aes(group = interaction(year,continent)), 
               width = 8, outlier.shape = NA, position = 'dodge') 


## ------------------------------------------
min(gapminder$lifeExp)
max(gapminder$lifeExp)
range(gapminder$lifeExp)


## ------------------------------------------
max(gapminder$lifeExp) - min(gapminder$lifeExp)
range(gapminder$lifeExp)[2] - range(gapminder$lifeExp)[1]
range(gapminder[['lifeExp']])[2] - range(gapminder[['lifeExp']])[1]
diff(range(gapminder$lifeExp))


## ----spotify-howtobuildmvp, echo = FALSE, out.width = "100%"----
knitr::include_graphics("l12/img/spotify-howtobuildmvp.jpg")


## ------------------------------------------
max_minus_min = function(x){
  max(x) - min(x)
}
max_minus_min(gapminder$lifeExp)


## ------------------------------------------
max_minus_min(1:10)
max_minus_min(runif(1000))


## ------------------------------------------
max_minus_min(gapminder$gdpPercap)
max_minus_min(gapminder$pop)


## ----error = TRUE--------------------------
max_minus_min(gapminder) ## hey sometimes things "just work" on data.frames!
max_minus_min(gapminder$country) ## factors are kind of like integer vectors, no?
max_minus_min("eggplants are purple") ## i have no excuse for this one


## ------------------------------------------
max_minus_min(gapminder[c('lifeExp', 'gdpPercap', 'pop')])
max_minus_min(c(TRUE, TRUE, FALSE, TRUE, TRUE))


## ----error = TRUE--------------------------
max_minus_min = function(x) {
  stopifnot(is.numeric(x))
  max(x) - min(x)
}
max_minus_min(gapminder)
max_minus_min(gapminder$country)
max_minus_min("eggplants are purple")


## ----error = TRUE--------------------------
max_minus_min(gapminder[c('lifeExp', 'gdpPercap', 'pop')])
max_minus_min(c(TRUE, TRUE, FALSE, TRUE, TRUE))


## ----error = TRUE--------------------------
max_minus_min = function(x) {
  if(!is.numeric(x)) {
    stop('I am so sorry, but this function only works for numeric input!\n',
         'You have provided an object of class: ', class(x)[1])
  }
  max(x) - min(x)
}
max_minus_min(gapminder)


## ------------------------------------------
work_without_question = function(condition){
  print(str_c("Good capitalistic worker bee ", emo::ji("honey_pot")))
}

life_crisis = function(condition){
  warning("Joining a commune.")
}


## ------------------------------------------
life = function(condition) {
  good_things = c("skiing", "cats", "health",
                  "wilderness", "coffee")
  if (sum(condition %in% good_things) >= 3) {
    condition = work_without_question(condition)
  } else{
    condition = life_crisis(condition)
  }
}


## ------------------------------------------
life(c("cats", "health"))
life(c("cats", "skiing", "health"))


## ----eval=FALSE----------------------------
## url = "https://www.gov.scot/publications/coronavirus-covid-19-update-first-ministers-speech-23-october/"
## speech_page = read_html(url)


## ----include=FALSE-------------------------
url = "https://www.gov.scot/publications/coronavirus-covid-19-update-first-ministers-speech-23-october/"
speech_page = read_html("l12/data/oct-23.html")


## ------------------------------------------
speech_page


## ------------------------------------------
title = speech_page %>%
  html_node(".article-header__title") %>%
  html_text()

date = speech_page %>%
  html_node(".content-data__list:nth-child(1) strong") %>%
  html_text() %>%
  dmy()

location = speech_page %>%
  html_node(".content-data__list+ .content-data__list strong") %>%
  html_text()

abstract = speech_page %>%
  html_node(".leader--first-para p") %>%
  html_text()

text = speech_page %>%
  html_nodes("#preamble p") %>%
  html_text() %>%
  list()


## ------------------------------------------
oct_23_speech = tibble(
  title    = title,
  date     = date,
  location = location,
  abstract = abstract,
  text     = text,
  url      = url
)

oct_23_speech


## ----echo=FALSE, out.width="75%"-----------
knitr::include_graphics("l12/img/fm-speech-oct-23.png")


## ----echo=FALSE, out.width="100%"----------
knitr::include_graphics("l12/img/funct-all-things.png")


## ----eval=FALSE----------------------------
## scrape_speech =
## 
## 
## 
## 
## 
## 


## ----eval=FALSE----------------------------
## scrape_speech = function(x){
## 
## 
## 
## 
## 
## }


## ----eval=FALSE----------------------------
## scrape_speech = function(url){
## 
##   # code we developed earlier to scrape info
##   # on single art piece goes here
## 
## }


## ------------------------------------------
scrape_speech = function(url) {
  
  speech_page = read_html(url)

  title = speech_page %>%
    html_node(".article-header__title") %>%
    html_text()

  date = speech_page %>%
    html_node(".content-data__list:nth-child(1) strong") %>%
    html_text() %>%
    dmy()

  location = speech_page %>%
    html_node(".content-data__list+ .content-data__list strong") %>%
    html_text()

  abstract = speech_page %>%
    html_node(".leader--first-para p") %>%
    html_text()

  text = speech_page %>%
    html_nodes("#preamble p") %>%
    html_text() %>%
    list()

  tibble(
    title = title, date = date, location = location,
    abstract = abstract, text = text, url = url
  )
}


## ----eval=FALSE----------------------------
## function([inputs separated by commas]){
##   # what to do with those inputs
## }


## ----eval=FALSE----------------------------
## scrape_page = function(x){
##   # do bunch of stuff with the input...
## 
##   # return a tibble
##   tibble(...)
## }


## ------------------------------------------
add_2 = function(x){
  x + 2
  1000
}


## ------------------------------------------
add_2(3)
add_2(10)


## ----eval=FALSE----------------------------
## # JUST DON'T
## mean = function(x){
##   x * 3
##   }


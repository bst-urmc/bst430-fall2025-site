## ----child = "setup.Rmd"---------------------------------------------------------------------------

## ----setup, include=FALSE--------------------------------------------------------------------------
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



## ----include = FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(cache = TRUE, autodep = TRUE)


## ----message=FALSE---------------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(broom)
data("mtcars")
glimpse(mtcars)


## ----pairs1, fig.show="hide"-----------------------------------------------------------------------
library(GGally)
GGally::ggpairs(mtcars)


## ----ref.label = "pairs1", echo = FALSE, warning = FALSE, out.width = "70%"------------------------


## ----pairs2, fig.show="hide"-----------------------------------------------------------------------
mtcars = mtcars %>% mutate(across(c(vs, am, gear, cyl), factor))
GGally::ggpairs(mtcars)


## ----ref.label = "pairs2", echo = FALSE, warning = FALSE, out.width = "70%"------------------------


## --------------------------------------------------------------------------------------------------
ggplot(mtcars, aes(x = wt, mpg)) +
  geom_point() +
  scale_x_continuous(transform = "log") + xlab("Weight") + ylab("Miles Per Gallon")


## --------------------------------------------------------------------------------------------------
submt = mtcars %>% mutate(logwt = log(wt))


## --------------------------------------------------------------------------------------------------
lmout = lm(mpg ~ logwt, data = submt)
lmtide = tidy(lmout)
select(lmtide, term, estimate)


## --------------------------------------------------------------------------------------------------
tidy(lm(mpg ~ ., data = mtcars))


## --------------------------------------------------------------------------------------------------
glance(lmout)


## ----echo = FALSE----------------------------------------------------------------------------------
ggplot(data = submt, mapping = aes(x = logwt, y = mpg)) +
  geom_point() +
  geom_abline(slope = coef(lmout)[2], intercept = coef(lmout)[1], lwd = 1, col = "blue", alpha = 1/2) +
  geom_segment(data = data.frame(x    = 1, 
                                 xend = 1,
                                 y    = 0,
                                 yend = coef(lmout)[1] + 1 * coef(lmout)[2]),
               mapping = aes(x = x, xend = xend, y = y, yend = yend), 
               lty = 2, color = "red", lwd = 1) +
  geom_hline(yintercept = 0, lty = 2, alpha = 1/2) +
  ylab("MPG") +
  xlab("Log-weight")


## ----echo = FALSE----------------------------------------------------------------------------------
ggplot(data = submt, mapping = aes(x = logwt, y = mpg)) +
  geom_point() +
  geom_abline(slope = coef(lmout)[2], intercept = coef(lmout)[1], lwd = 1, col = "blue", alpha = 1/2) +
  geom_segment(data = data.frame(x    = 3, 
                                 xend = 3,
                                 y    = 0,
                                 yend = coef(lmout)[1] + 3 * coef(lmout)[2]),
               mapping = aes(x = x, xend = xend, y = y, yend = yend), 
               lty = 2, color = "red", lwd = 1) +
  geom_hline(yintercept = 0, lty = 2, alpha = 1/2) +
  ylab("MPG") +
  xlab("Log-weight")


## --------------------------------------------------------------------------------------------------
newdf = tribble(~logwt,
                1, 
                1.5)


## --------------------------------------------------------------------------------------------------
newdf = newdf %>%
  mutate(predictions = predict(object = lmout, newdata = newdf))


## ----echo = FALSE----------------------------------------------------------------------------------
knitr::include_graphics("l16/img/slope_hypothesis_testing_2x.png")



## --------------------------------------------------------------------------------------------------
aout = augment(lmout)
glimpse(aout)


## ----echo = FALSE, message = FALSE, warning = FALSE------------------------------------------------
set.seed(1)
x = rnorm(100, sd = 1)
y = x + rnorm(100)
lmout = lm(y ~ x)
res_vec = resid(lmout)
fit_vec = fitted(lmout)
qplot(x, y, xlab = "X", ylab = "Y", main = "Raw Data") + 
  geom_smooth(se = FALSE, method = "lm")


## ----echo = FALSE, message = FALSE, warning = FALSE------------------------------------------------
qplot(fit_vec, res_vec, xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot") +
  geom_hline(yintercept = 0)


## ----output.lines = 7------------------------------------------------------------------------------
set.seed(1)
x = rexp(100)
x = x - min(x) + 0.5
y = log(x) * 20 + rnorm(100, sd = 4)
(df_fake = tibble(x, y))


## ----echo = FALSE----------------------------------------------------------------------------------
lmout = lm(y ~ x)
res_vec = resid(lmout)
fit_vec = fitted(lmout)
qplot(x, y, xlab = "X", ylab = "Y", main = "Raw Data")+ geom_smooth(se = FALSE, method = "lm")


## ----echo = FALSE----------------------------------------------------------------------------------
qplot(fit_vec, res_vec, xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot") + geom_hline(yintercept = 0)


## --------------------------------------------------------------------------------------------------
df_fake %>%
  mutate(logx = log(x)) ->
  df_fake
lm_fake = lm(y ~ logx, data = df_fake)


## --------------------------------------------------------------------------------------------------
set.seed(1)
x = rnorm(100)
y = -x^2 + rnorm(100)
df_fake = tibble(x, y)


## ----echo = FALSE----------------------------------------------------------------------------------
lmout = lm(y ~ x)
res_vec = resid(lmout)
fit_vec = fitted(lmout)
qplot(x, y, xlab = "X", ylab = "Y", main = "Raw Data")+ geom_smooth(se = FALSE, method = "lm")


## ----echo = FALSE----------------------------------------------------------------------------------
qplot(fit_vec, res_vec, xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot") + geom_hline(yintercept = 0)


## --------------------------------------------------------------------------------------------------
lmout = lm(y ~ I(x^2), data = df_fake)


## --------------------------------------------------------------------------------------------------
set.seed(1)
x = rnorm(100)
y = exp(x + rnorm(100, sd = 1/2))
df_fake = tibble(x, y)


## ----echo = FALSE----------------------------------------------------------------------------------
lmout = lm(y ~ x)
res_vec = resid(lmout)
fit_vec = fitted(lmout)
qplot(x, y, xlab = "X", ylab = "Y", main = "Raw Data")+ geom_smooth(se = FALSE, method = "lm")


## ----echo = FALSE----------------------------------------------------------------------------------
qplot(fit_vec, res_vec, xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot") + geom_hline(yintercept = 0)


## --------------------------------------------------------------------------------------------------
df_fake %>%
  mutate(logy = log(y)) ->
  df_fake
lm_fake = lm(logy ~ x, data = df_fake)


## --------------------------------------------------------------------------------------------------
set.seed(1)
x = runif(200)
y = 15 * x + rexp(200, 0.2)


## ----echo = FALSE----------------------------------------------------------------------------------
lmout = lm(y ~ x)
res_vec = resid(lmout)
fit_vec = fitted(lmout)
qplot(x, y, xlab = "X", ylab = "Y", main = "Raw Data")+ geom_smooth(se = FALSE, method = "lm")


## ----echo = FALSE----------------------------------------------------------------------------------
qplot(fit_vec, res_vec, xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot") + geom_hline(yintercept = 0)


## --------------------------------------------------------------------------------------------------
set.seed(1)
x = runif(100) * 10
y = 0.85 * x + rnorm(100, sd = (x - 5) ^ 2)
df_fake = tibble(x, y)


## ----echo = FALSE----------------------------------------------------------------------------------
lmout = lm(y ~ x)
res_vec = resid(lmout)
fit_vec = fitted(lmout)
qplot(x, y, xlab = "X", ylab = "Y", main = "Raw Data")+ geom_smooth(se = FALSE, method = "lm")


## ----echo = FALSE----------------------------------------------------------------------------------
qplot(fit_vec, res_vec, xlab = "Fitted Values", ylab = "Residuals", main = "Residual Plot") + geom_hline(yintercept = 0)


## --------------------------------------------------------------------------------------------------
rob_fit = estimatr::lm_robust(y ~ x, data = df_fake)
tidy(rob_fit)


## --------------------------------------------------------------------------------------------------
only_wt =  lm(mpg ~ log(wt), mtcars)
only_disp =  lm(mpg ~ log(disp), mtcars)
only_cyl =  lm(mpg ~ cyl, mtcars)
complicated_model = lm(mpg ~ (log(wt) + log(disp))*cyl, mtcars)


## ----out.width="80%"-------------------------------------------------------------------------------
GGally::ggcoef_compare(list(only_wt = only_wt, only_disp = only_disp, only_cyl = only_cyl, complicated=complicated_model))


## ----pred-plot, fig.show='hide', warning = FALSE---------------------------------------------------
(newdf = expand.grid(wt = mean(mtcars$wt),
                   disp = mean(mtcars$disp),
                   cyl = factor(c(4, 6, 8))))

newdf = newdf %>% mutate(mpg = predict(complicated_model, across()))
ggplot(mtcars, aes(x = cyl, y = mpg)) + 
  geom_boxplot() +
  geom_line(data =newdf, aes(group = 1), color = 'red')


## ----ref.label = 'pred-plot', echo = FALSE, out.width = '70%', results = 'hide', warning = FALSE----


## --------------------------------------------------------------------------------------------------

ggplot(mtcars, aes(x = log(disp), y = mpg)) + 
  geom_point() + facet_wrap(~cyl) + 
  geom_smooth(method = 'lm') + 
  geom_point(data =newdf, aes(group = 1), color = 'red')



## ----message = FALSE, warning = FALSE--------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(gapminder)
data("gapminder")
glimpse(gapminder)


## --------------------------------------------------------------------------------------------------
gapminder %>%
  ggplot(aes(x = year, y = lifeExp, group = country)) +
  geom_line(alpha = 1/3) +
  xlab("Year") +
  ylab("Life Expectancy")


## --------------------------------------------------------------------------------------------------
usdf = gapminder %>% filter(country == "United States")


## ----echo = FALSE, out.width = "50%"---------------------------------------------------------------
 ggplot(usdf, aes(x = year, y = lifeExp)) +
 geom_line() +
 geom_smooth(method = "lm", se = FALSE) +
 geom_line(alpha = 1/3) +
 xlab("Year") +
 ylab("Life Expectancy")


## ----echo = FALSE----------------------------------------------------------------------------------
us_lmout = lm(lifeExp ~ year, data = usdf)
tidy_uslm = tidy(us_lmout)
tidy_uslm


## ----eval = FALSE----------------------------------------------------------------------------------
## many_fits = gapminder %>%
##   group_by(country) %>%
##   summarize(fit = fit_model(across())) #<<
## 
## tidied_output = many_fits %>%
##   rowwise()  %>% #<<
##   mutate(df = list( #<<
##     post_process(fit) #<<
##     ))
## 
## unnested_output = tidied_output %>%
##   tidyr::unnest(cols = c(df)) #<<


## ----output.lines = 7, warning = FALSE, message = FALSE--------------------------------------------
fit_model = function(data) {
  linear_reg() %>%
    set_engine("lm") %>%
    fit(lifeExp ~ I(year-1990), data = data) %>%
    list()
# Base R version for lm
#  fit = lm(lifeExp ~ I(year-1990), data)
#  list(fit)
}

many_fits = gapminder %>% 
  group_by(country, continent) %>%
  summarize(fit = fit_model(across(.col = 1:4))) %>% 
  ungroup()
many_fits


## --------------------------------------------------------------------------------------------------
dplyr::filter(many_fits, country == 'Japan')$fit[[1]] %>% tidy()


## --------------------------------------------------------------------------------------------------
dplyr::filter(many_fits, country == 'Senegal')$fit[[1]] %>% tidy()



## --------------------------------------------------------------------------------------------------
dplyr::filter(many_fits, country == 'Senegal')$fit 


## ----error=TRUE------------------------------------------------------------------------------------
fit_not_list = function(data) {
  linear_reg() %>%
    set_engine("lm") %>%
    fit(lifeExp ~ I(year-1990), data = data)
# Base R version for lm also doesn't work
# fit = lm(lifeExp ~ I(year-1990), data)
# fit
}

many_fits = gapminder %>% 
 group_by(country, continent) %>%
 summarize(fit = fit_not_list(across()))


## --------------------------------------------------------------------------------------------------
(df = tibble( x = 1:3, 
             y = list('a', c('bb', 'cc'), 'ddd')))

str(df$y)


## --------------------------------------------------------------------------------------------------
nchar(df$y)


## --------------------------------------------------------------------------------------------------
purrr::map(df$y, nchar)


## ----purrr-----------------------------------------------------------------------------------------
(df = df %>% mutate(nchar = purrr::map(y, nchar)))


## ----map-------------------------------------------------------------------------------------------
many_fits = many_fits %>% 
  mutate(tidyout = map(fit, tidy))
many_fits


## --------------------------------------------------------------------------------------------------
(df %>% unnest(cols = c(y, nchar)))


## --------------------------------------------------------------------------------------------------
many_fits_coef = many_fits %>% unnest(cols = c(tidyout))
many_fits_coef


## ----country-intercept, fig.show='hide'------------------------------------------------------------
intercept = many_fits_coef %>% 
  dplyr::filter(term == '(Intercept)') %>% 
  arrange(desc(estimate)) %>%
  mutate(country = fct_inorder(country))

to_show = intercept %>% group_by(continent) %>% 
  reframe(country = country[seq(from = 1, to = length(country), by = 5)]) 

intercept %>%
ggplot(aes(y = country, x = estimate, xmin = estimate - std.error, xmax = estimate + std.error)) + 
  geom_pointrange() + 
  scale_y_discrete(breaks = to_show$country) + 
  facet_grid(continent ~ ., scales = 'free_y', space = 'free')


## ----ref.label = 'country-intercept', echo = FALSE, out.width='70%'--------------------------------



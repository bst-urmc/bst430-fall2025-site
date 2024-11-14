## ----child = "setup.Rmd"------------------------------------

## ----setup, include=FALSE-----------------------------------
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



## ----include = FALSE----------------------------------------
knitr::opts_chunk$set(cache = TRUE, autodep = TRUE, warnings=FALSE)
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(mlbench)
set.seed(1234)
options(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE, 
  warnPartialMatchDollar = FALSE,
  width = 100
)


## ----echo=FALSE, out.width="100%", fig.align="right"--------
knitr::include_graphics("l20/Leo_Breiman.jpg")


## ----echo=FALSE, out.width="100%", fig.align="right"--------
knitr::include_graphics("l20/fig_boston_partitions.png")


## -----------------------------------------------------------
library(mlbench) #you probably need to install thsi library
data("BostonHousing")
names(BostonHousing)

ggplot(BostonHousing, aes(x=lstat, y=rm, color=medv)) +
geom_point()


## ----echo=FALSE, out.width="100%", fig.align="right"--------
knitr::include_graphics("l20/fig_boston_cart_tree.png")


## ----echo=FALSE, out.width="100%", fig.align="right"--------
knitr::include_graphics("l20/fig_boston_skeleton_partitions.png")


## ----warning = FALSE----------------------------------------
library(rpart) #you probably need to install this library
#rpart(medv ~., data = BostonHousing, control = rpart.control(maxdepth = 2))
rpart(medv ~., data = BostonHousing, control = rpart.control(cp = .1))
set.seed(1234)
BostonSplit = initial_split(BostonHousing, prop=0.80)
trained1 = rpart(medv ~., data = training(BostonSplit), control = rpart.control(cp = .1))
cat("\n Sum Squared Error for one Tree", sum((predict(trained1, testing(BostonSplit)) - testing(BostonSplit)$medv)^2),"\n")


## ----warning = FALSE----------------------------------------
library(ipred) #you probably need to install this library
bagging(medv ~., data = BostonHousing, nbagg=100)
#bagging(medv ~., data = BostonHousing, nbagg=100)$mtrees[[25]]$btree

trained2 = bagging(medv ~., data = training(BostonSplit), nbagg=100)
cat("\n Sum Squared Error for Bagging", sum((predict(trained2, testing(BostonSplit)) - testing(BostonSplit)$medv)^2),"\n")



## ----warning = FALSE----------------------------------------
library(randomForest) #you probably need to install this library
randomForest(medv ~., data = BostonHousing, mtry=4)

trained3 = randomForest(medv ~., data = training(BostonSplit), mtry=3)
cat("\n Sum Squared Error for randomForest", sum((predict(trained3, testing(BostonSplit)) - testing(BostonSplit)$medv)^2),"\n")


## ----warning = FALSE----------------------------------------
library(mboost) #you probably need to install this library
blackboost(medv ~., data = BostonHousing, control = boost_control(mstop = 50))

trained4 = blackboost(medv ~., data = training(BostonSplit))
cat("Sum Squared Error for Boosting", sum((predict(trained4, testing(BostonSplit)) - testing(BostonSplit)$medv)^2),"\n")


## ----echo=FALSE, out.width="100%", fig.align="right"--------
knitr::include_graphics("l20/Algorithm_Map.PNG")


## ----warning = FALSE----------------------------------------
library(dbarts) #you probably need to install this library
set.seed(99)
bartFit <- dbarts::bart(BostonHousing[-14], BostonHousing$medv)


## -----------------------------------------------------------
plot(bartFit)

trained5 = dbarts::bart(training(BostonSplit)[-14], training(BostonSplit)$medv, 
              keeptrees = TRUE) # must keep trees to do predictions
cat("\n Sum Squared Error for BART", sum((colMeans(predict(trained5, testing(BostonSplit))) - testing(BostonSplit)$medv)^2),"\n")



## ----echo = FALSE-------------------------------------------
cat("\n Sum Squared Error for one Tree", sum((predict(trained1, testing(BostonSplit)) - testing(BostonSplit)$medv)^2),"\n")
cat("\n Sum Squared Error for Bagging", sum((predict(trained2, testing(BostonSplit)) - testing(BostonSplit)$medv)^2),"\n")
cat("\n Sum Squared Error for randomForest", sum((predict(trained3, testing(BostonSplit)) - testing(BostonSplit)$medv)^2),"\n")
cat("\n Sum Squared Error for Boosting", sum((predict(trained4, testing(BostonSplit)) - testing(BostonSplit)$medv)^2),"\n")
cat("\n Sum Squared Error for BART", sum((colMeans(predict(trained5, testing(BostonSplit))) - testing(BostonSplit)$medv)^2),"\n")


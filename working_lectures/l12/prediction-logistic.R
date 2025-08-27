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



## ----packages, echo = FALSE, message=FALSE, warning=FALSE------------------------------------------
library(tidyverse)
library(tidymodels)
library(openintro)
library(patchwork)
library(skimr)
library(conflicted)
set.seed(1234)
options(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE, 
  warnPartialMatchDollar = FALSE,
  width = 100
)
knitr::opts_chunk$set(cache = TRUE, autodep = TRUE)
conflict_prefer("step", "recipes")


## ----echo=FALSE------------------------------------------------------------------------------------
d = tibble(p = seq(0.001, 0.999, length.out = 1000)) %>%
  mutate(logit_p = log(p/(1-p)))

ggplot(d, aes(x = p, y = logit_p)) + 
  geom_line() + 
  ylab("logit(p)") +
  labs(title = "logit(p) vs. p") + 
  scale_x_continuous(labels = c(min(d$p), .25, .5, .75, max(d$p)))


## ----echo=FALSE, out.width="70%", warning = FALSE--------------------------------------------------
lm_fit = linear_reg() %>%
  set_engine("lm") %>%
  fit(y4 ~ x2, data = association)

loess_fit = loess(y4 ~ x2, data = association)

loess_overfit = loess(y4 ~ x2, span = 0.05, data = association)

association %>%
  select(x2, y4) %>%
  mutate(
    Underfit = augment(lm_fit$fit) %>% select(.fitted) %>% pull(),
    OK       = augment(loess_fit) %>% select(.fitted) %>% pull(),
    Overfit  = augment(loess_overfit) %>% select(.fitted) %>% pull(),
  ) %>%
  pivot_longer(
    cols      = Underfit:Overfit,
    names_to  = "fit",
    values_to = "y_hat"
  ) %>%
  mutate(fit = fct_relevel(fit, "Underfit", "OK", "Overfit")) %>%
  ggplot(aes(x = x2)) +
  geom_point(aes(y = y4), color = "darkgray") +
  geom_line(aes(y = y_hat, group = fit, color = fit), size = 1) +
  labs(x = NULL, y = NULL, color = NULL) +
  scale_color_viridis_d(option = "plasma", end = 0.7)


## --------------------------------------------------------------------------------------------------
# Fix random numbers by setting the seed 
# Enables analysis to be reproducible when random numbers are used 
set.seed(20211115)

# Put 80% of the data into the training set 
email_split = initial_split(email, prop = 0.80)

# Create data frames for the two sets:
train_data = training(email_split)
test_data  = testing(email_split)


## --------------------------------------------------------------------------------------------------
glimpse(train_data)


## --------------------------------------------------------------------------------------------------
glimpse(test_data)


## --------------------------------------------------------------------------------------------------
email_fit = logistic_reg() %>%
  set_engine("glm") %>%
  fit(spam ~ ., data = train_data, family = "binomial")


## ----echo=FALSE, out.width="75%", fig.width=10-----------------------------------------------------
factor_predictors = train_data %>%
  select(where(is.factor), -spam) %>%
  names()

p_to_multiple = ggplot(train_data, aes(x = to_multiple, fill = spam)) +
  geom_bar() +
  scale_fill_manual(values = c("#E48957", "#CA235F"))

p_from = ggplot(train_data, aes(x = from, fill = spam)) +
  geom_bar() +
  scale_fill_manual(values = c("#E48957", "#CA235F"))

p_sent_email = ggplot(train_data, aes(x = sent_email, fill = spam)) +
  geom_bar() +
  scale_fill_manual(values = c("#E48957", "#CA235F"))

p_winner = ggplot(train_data, aes(x = winner, fill = spam)) +
  geom_bar() +
  scale_fill_manual(values = c("#E48957", "#CA235F"))

p_format = ggplot(train_data, aes(x = format, fill = spam)) +
  geom_bar() +
  scale_fill_manual(values = c("#E48957", "#CA235F"))

p_re_subj = ggplot(train_data, aes(x = re_subj, fill = spam)) +
  geom_bar() +
  scale_fill_manual(values = c("#E48957", "#CA235F"))

p_urgent_subj = ggplot(train_data, aes(x = urgent_subj, fill = spam)) +
  geom_bar() +
  scale_fill_manual(values = c("#E48957", "#CA235F"))

p_number = ggplot(train_data, aes(x = number, fill = spam)) +
  geom_bar() +
  scale_fill_manual(values = c("#E48957", "#CA235F"))

p_to_multiple + p_from + p_sent_email + p_winner + p_format + p_re_subj + p_urgent_subj + p_number +
  plot_layout(ncol = 4, guides = "collect") & 
  theme(axis.title.y = element_blank())


## --------------------------------------------------------------------------------------------------
email_fit = logistic_reg() %>%
  set_engine("glm") %>%
  fit(spam ~ . - from - sent_email - viagra, data = train_data, family = "binomial") #<<


## --------------------------------------------------------------------------------------------------
email_fit


## --------------------------------------------------------------------------------------------------
predict(email_fit, test_data)


## --------------------------------------------------------------------------------------------------
email_pred = predict(email_fit, test_data, type = "prob") %>%
  bind_cols(test_data %>% select(spam, time))

email_pred


## ----highlight.output=c(6, 10)---------------------------------------------------------------------
email_pred %>%
  arrange(desc(.pred_1)) %>%
  print(n = 10)


## ----roc, fig.show="hide"--------------------------------------------------------------------------
email_pred %>%
  roc_curve(
    truth = spam,
    .pred_1,
    event_level = "second"
  ) %>%
  autoplot()


## ----ref.label="roc", echo=FALSE, out.width="100%"-------------------------------------------------


## --------------------------------------------------------------------------------------------------
email_pred %>%
  roc_auc(
    truth = spam,
    .pred_1,
    event_level = "second"
  )


## ----ref.label="roc", echo=FALSE, out.width="100%"-------------------------------------------------


## --------------------------------------------------------------------------------------------------
library(lubridate)
train_data %>%
  mutate(
    date = date(time),
    dow  = wday(time),
    month = month(time)
    ) %>%
  select(time, date, dow, month) %>%
  sample_n(size = 5) # shuffle to show a variety


## ----initiate-recipe, results="hide"---------------------------------------------------------------
email_rec = recipe(
  spam ~ .,          # formula
  data = train_data  # data to use for cataloguing names and types of variables
  )

summary(email_rec)


## ----echo=FALSE------------------------------------------------------------------------------------
summary(email_rec) %>% print(n = 21)


## --------------------------------------------------------------------------------------------------
email_rec = email_rec %>%
  step_rm(from, sent_email)


## ----echo=FALSE------------------------------------------------------------------------------------
email_rec


## --------------------------------------------------------------------------------------------------
email_rec = email_rec %>%
  step_date(time, features = c("dow", "month")) %>%
  step_rm(time)


## ----echo=FALSE------------------------------------------------------------------------------------
email_rec


## --------------------------------------------------------------------------------------------------
email_rec = email_rec %>%
  step_cut(cc, attach, dollar, breaks = c(0, 1)) %>%
  step_cut(inherit, password, breaks = c(0, 1, 5, 10, 20))


## ----echo=FALSE------------------------------------------------------------------------------------
email_rec


## --------------------------------------------------------------------------------------------------
email_rec = email_rec %>%
  step_dummy(all_nominal(), -all_outcomes())


## ----echo=FALSE------------------------------------------------------------------------------------
email_rec


## --------------------------------------------------------------------------------------------------
email_rec = email_rec %>%
  step_zv(all_predictors())


## ----echo=FALSE------------------------------------------------------------------------------------
email_rec


## --------------------------------------------------------------------------------------------------
email_rec = recipe(spam ~ ., data = email) %>%
  step_rm(from, sent_email) %>%
  step_date(time, features = c("dow", "month")) %>%               
  step_rm(time) %>%
  step_cut(cc, attach, dollar, breaks = c(0, 1)) %>%
  step_cut(inherit, password, breaks = c(0, 1, 5, 10, 20)) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors())


## --------------------------------------------------------------------------------------------------
email_mod = logistic_reg() %>% 
  set_engine("glm")

email_mod


## --------------------------------------------------------------------------------------------------
email_wflow = workflow() %>% 
  add_model(email_mod) %>% 
  add_recipe(email_rec)


## ----echo=FALSE------------------------------------------------------------------------------------
email_wflow


## --------------------------------------------------------------------------------------------------
email_fit = email_wflow %>% 
  fit(data = train_data)


## ----output.lines=30-------------------------------------------------------------------------------
tidy(email_fit)


## ----R.options=list(width = 60), output.lines = 10-------------------------------------------------
email_pred = predict(email_fit, test_data, type = "prob") %>% 
  bind_cols(test_data) 

email_pred


## ----roc-2, fig.show="hide"------------------------------------------------------------------------
email_pred %>%
  roc_curve(
    truth = spam,
    .pred_1,
    event_level = "second"
  ) %>%
  autoplot()


## ----ref.label="roc-2", echo=FALSE, out.width="100%"-----------------------------------------------


## --------------------------------------------------------------------------------------------------
email_pred %>%
  roc_auc(
    truth = spam,
    .pred_1,
    event_level = "second"
  )


## ----ref.label="roc-2", echo=FALSE, out.width="100%"-----------------------------------------------


## ----ref.label = "confusion-50,", echo = FALSE-----------------------------------------------------


## ----confusion-50, results = "hide"----------------------------------------------------------------
cutoff_prob = 0.5
email_pred %>%
  mutate(
    spam      = if_else(spam == 1, "Email is spam", "Email is not spam"),
    spam_pred = if_else(.pred_1 > cutoff_prob, "Email labelled spam", "Email labelled not spam")
    ) %>%
  count(spam_pred, spam) %>%
  pivot_wider(names_from = spam, values_from = n) %>%
  knitr::kable(col.names = c("", "Email is not spam", "Email is spam"))


## ----ref.label = "confusion-25,", echo = FALSE-----------------------------------------------------


## ----confusion-25, results = "hide"----------------------------------------------------------------
cutoff_prob = 0.25
email_pred %>%
  mutate(
    spam      = if_else(spam == 1, "Email is spam", "Email is not spam"),
    spam_pred = if_else(.pred_1 > cutoff_prob, "Email labelled spam", "Email labelled not spam")
    ) %>%
  count(spam_pred, spam) %>%
  pivot_wider(names_from = spam, values_from = n) %>%
  knitr::kable(col.names = c("", "Email is not spam", "Email is spam"))


## ----ref.label = "confusion-75,", echo = FALSE-----------------------------------------------------


## ----confusion-75, results = "hide"----------------------------------------------------------------
cutoff_prob = 0.75
email_pred %>%
  mutate(
    spam      = if_else(spam == 1, "Email is spam", "Email is not spam"),
    spam_pred = if_else(.pred_1 > cutoff_prob, "Email labelled spam", "Email labelled not spam")
    ) %>%
  count(spam_pred, spam) %>%
  pivot_wider(names_from = spam, values_from = n) %>%
  knitr::kable(col.names = c("", "Email is not spam", "Email is spam"))


## ----echo=FALSE, out.width="100%"------------------------------------------------------------------
knitr::include_graphics("l17/img/cross-validation.png")


## --------------------------------------------------------------------------------------------------
set.seed(345)

folds = vfold_cv(train_data, v = 5)
folds


## ----echo=FALSE, out.width="100%", fig.align="right"-----------------------------------------------
knitr::include_graphics("l17/img/cross-validation.png")


## ----warning = FALSE-------------------------------------------------------------------------------
set.seed(456)

email_fit_rs = email_wflow %>%
  fit_resamples(folds, metrics = metric_set(accuracy, roc_auc))

email_fit_rs


## ----echo=FALSE, out.width="100%", fig.align="right"-----------------------------------------------
knitr::include_graphics("l17/img/cross-validation-animated.gif")


## --------------------------------------------------------------------------------------------------
collect_metrics(email_fit_rs)


## --------------------------------------------------------------------------------------------------
collect_metrics(email_fit_rs, summarize = FALSE) %>%
  print(n = 10)


## ----echo=FALSE------------------------------------------------------------------------------------
collect_metrics(email_fit_rs, summarize = FALSE) %>%
  select(id, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  knitr::kable(col.names = c("Fold", "accuracy", "roc_auc"), digits = 3)


## ----warning=FALSE---------------------------------------------------------------------------------
set.seed(123)
train_data_small = train_data %>% sample_n(size = 100)
overfit = fit(email_wflow, data = train_data_small)
overpredict = predict(overfit, train_data_small, type = "prob") %>% 
  bind_cols(train_data_small)


## ----echo =FALSE, out.width="90%"------------------------------------------------------------------
 overpredict %>% roc_curve(
    truth = spam,
    .pred_1,
    event_level = "second"
  ) %>%
  autoplot()

overpredict %>% roc_auc(
    truth = spam,
    .pred_1,
    event_level = "second"
  )


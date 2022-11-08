## ----child = "setup.Rmd"----------------------

## ----setup, include=FALSE---------------------
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



## ----packages, echo = FALSE, message=FALSE, warning=FALSE----
library(tidyverse)
library(tidymodels)
set.seed(1234)
knitr::opts_chunk$set(cache = TRUE, autodep = TRUE)


## ----echo = FALSE, out.width = "100%"---------
df1 = tibble(x = 1:100, y = x + rnorm(100, mean = 0, sd = 5))
ggplot(df1, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#E48957", se = FALSE) +
  labs(title = "Linear", x = NULL, y = NULL) +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank()
    )


## ----echo = FALSE, out.width = "100%"---------
df2 = tibble(x = seq(-6, 5.9, 0.1), y = (1 / (1+exp(-2*x))) + rnorm(120, mean = 0, sd = 0.1))
ggplot(df2, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "loess", color = "#8E2C90", se = FALSE) +
  labs(title = "Non-linear", x = NULL, y = NULL, subtitle = "(But could be approximated with a linear model after a basis expansion...)") +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank()
    )


## ----message=FALSE----------------------------
pp = read_csv("l15/data/paris-paintings.csv", na = c("n/a", "", "NA"))


## ----out.width="65%", echo=FALSE--------------
knitr::include_graphics("l15/img/old-auction.png")


## ----out.width="60%", echo=FALSE--------------
knitr::include_graphics("l15/img/auction-trend-paris.png")


## ----out.width="65%", echo=FALSE--------------
knitr::include_graphics("l15/img/depart-pour-la-chasse.png")


## ----out.width="60%", echo=FALSE--------------
knitr::include_graphics("l15/img/auction-catalogue.png")


## ----out.width="60%", echo=FALSE--------------
knitr::include_graphics("l15/img/painting1.png")
knitr::include_graphics("l15/img/painting2.png")
knitr::include_graphics("l15/img/painting3.png")


## ----results="hide"---------------------------
pp %>%
  filter(name == "R1777-89a") %>%
  glimpse()


## ----output.lines=23, echo=FALSE--------------
pp %>%
  filter(name == "R1777-89a") %>%
  glimpse()


## ----output.lines=24:44, echo=FALSE-----------
pp %>%
  filter(name == "R1777-89a") %>%
  glimpse()


## ----height-dist, out.width="60%", warning=FALSE----
ggplot(data = pp, aes(x = Height_in)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Height, in inches", y = NULL)


## ----width-dist, out.width="60%", warning=FALSE----
ggplot(data = pp, aes(x = Width_in)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Width, in inches", y = NULL)


## ----ref.label = "height-width-plot", echo = FALSE, warning = FALSE, out.width = "60%"----


## ----height-width-plot, fig.show="hide"-------
ggplot(data = pp, aes(x = Width_in, y = Height_in)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Height vs. width of paintings",
    subtitle = "Paris auctions, 1764 - 1780",
    x = "Width (inches)",
    y = "Height (inches)"
  )


## ----ref.label = "height-width-plot-no-se", echo = FALSE, warning = FALSE, out.width = "60%"----


## ----height-width-plot-no-se, fig.show="hide", warning=FALSE----
ggplot(data = pp, aes(x = Width_in, y = Height_in)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE) + #<<
  labs(
    title = "Height vs. width of paintings",
    subtitle = "Paris auctions, 1764 - 1780",
    x = "Width (inches)",
    y = "Height (inches)"
  )


## ----ref.label = "height-width-plot-cosmetics", echo = FALSE, warning = FALSE, out.width = "60%"----


## ----height-width-plot-cosmetics, fig.show="hide", warning=FALSE----
ggplot(data = pp, aes(x = Width_in, y = Height_in)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,
              color = "#8E2C90", linetype = "dashed", size = 3) + #<<
  labs(
    title = "Height vs. width of paintings",
    subtitle = "Paris auctions, 1764 - 1780",
    x = "Width (inches)",
    y = "Height (inches)"
  )


## ----ref.label = "height-width-plot-gam", echo = FALSE, warning = FALSE, out.width = "60%"----


## ----height-width-plot-gam, fig.show="hide", warning=FALSE----
ggplot(data = pp, aes(x = Width_in, y = Height_in)) +
  geom_point() +
  geom_smooth(method = "gam", #<<
              se = FALSE, color = "#8E2C90") + 
  labs(
    title = "Height vs. width of paintings",
    subtitle = "Paris auctions, 1764 - 1780",
    x = "Width (inches)",
    y = "Height (inches)"
  )


## ----ref.label = "height-width-plot-loess", echo = FALSE, warning = FALSE, out.width = "60%"----


## ----height-width-plot-loess, fig.show="hide", warning=FALSE----
ggplot(data = pp, aes(x = Width_in, y = Height_in)) +
  geom_point() +
  geom_smooth(method = "loess", #<<
              se = FALSE, color = "#8E2C90") + 
  labs(
    title = "Height vs. width of paintings",
    subtitle = "Paris auctions, 1764 - 1780",
    x = "Width (inches)",
    y = "Height (inches)"
  )


## ----ref.label = "height-width-plot-residuals", echo = FALSE, warning = FALSE, out.width = "60%"----


## ----height-width-plot-residuals, fig.show="hide", warning=FALSE----
ht_wt_fit = linear_reg() %>%
  set_engine("lm") %>%
  fit(Height_in ~ Width_in, data = pp)

ht_wt_fit_tidy = tidy(ht_wt_fit$fit) 
ht_wt_fit_aug  = augment(ht_wt_fit$fit) %>%
  mutate(res_cat = ifelse(.resid > 0, TRUE, FALSE))

ggplot(data = ht_wt_fit_aug) +
  geom_point(aes(x = Width_in, y = Height_in, color = res_cat)) +
  geom_line(aes(x = Width_in, y = .fitted), size = 0.75, color = "#8E2C90") + 
  labs(
    title = "Height vs. width of paintings",
    subtitle = "Paris auctions, 1764 - 1780",
    x = "Width (inches)",
    y = "Height (inches)"
  ) +
  guides(color = 'none') +
  scale_color_manual(values = c("#260b27", "#e6b0e7")) +
  geom_text(aes(x = 0, y = 150), label = "Positive residual", color = "#e6b0e7", hjust = 0, size = 8) +
  geom_text(aes(x = 150, y = 25), label = "Negative residual", color = "#260b27", hjust = 0, size = 8)


## ----height-width-plot-alpha, warning = FALSE, echo=FALSE, out.width="60%"----
ggplot(data = pp, aes(x = Width_in, y = Height_in)) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Height vs. width of paintings",
    subtitle = "Paris auctions, 1764 - 1780",
    x = "Width (inches)",
    y = "Height (inches)"
  )


## ----ref.label = "height-width-landscape", echo = FALSE, warning = FALSE, out.width = "80%"----


## ----height-width-landscape, fig.show="hide", warning=FALSE----
ggplot(data = pp, aes(x = Width_in, y = Height_in, color = factor(landsALL))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Height vs. width of paintings, by landscape features",
    subtitle = "Paris auctions, 1764 - 1780",
    x = "Width (inches)",
    y = "Height (inches)",
    color = "landscape"
  ) +
  scale_color_manual(values = c("#E48957", "#071381"))


## ----ref.label = "extrapolation", echo = FALSE, warning = FALSE, out.width = "65%"----


## ----extrapolation, fig.show="hide", warning=FALSE----
ggplot(data = pp, aes(x = Width_in, y = Height_in, color = factor(landsALL))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE,
              fullrange = TRUE) + #<<
  labs(
    title = "Height vs. width of paintings, by landscape features",
    subtitle = "Paris auctions, 1764 - 1780",
    x = "Width (inches)",
    y = "Height (inches)",
    color = "landscape"
  ) +
  scale_color_manual(values = c("#E48957", "#071381"))


## ----echo = FALSE, message=FALSE, warning=FALSE----
library(tidyverse)
library(tidymodels)
library(ggtext)
library(knitr)
library(kableExtra)
set.seed(1234)
options(dplyr.print_min = 10, dplyr.print_max = 6)


## ----ref.label = "height-width-plot", echo=FALSE, warning=FALSE----


## ----out.width="98%", echo=FALSE--------------
knitr::include_graphics("l15/img/tidymodels.png")


## ---------------------------------------------
linear_reg()


## ---------------------------------------------
linear_reg() %>%
  set_engine("lm") # lm: linear model


## ----fit-model--------------------------------
linear_reg() %>%
  set_engine("lm") %>%
  fit(Height_in ~ Width_in, data = pp)


## ----ref.label="fit-model", echo=FALSE--------


## ---------------------------------------------
linear_reg() %>%
  set_engine("lm") %>%
  fit(Height_in ~ Width_in, data = pp) %>%
  tidy()


## ---------------------------------------------
linear_reg() %>%
  set_engine("lm") %>%
  fit(Height_in ~ Width_in, data = pp) %>%
  glance()


## ----echo=FALSE, out.width="90%"--------------
knitr::include_graphics("l15/img/cell_phones.png")


## ----vis-res-1, echo=FALSE, out.width="70%"----
ht_wt_fit = linear_reg() %>%
  set_engine("lm") %>%
  fit(Height_in ~ Width_in, data = pp)

ht_wt_fit_tidy = tidy(ht_wt_fit$fit) 
ht_wt_fit_aug  = augment(ht_wt_fit$fit) %>%
  mutate(res_cat = ifelse(.resid > 0, TRUE, FALSE))

p = ggplot(data = ht_wt_fit_aug, 
            aes(x = Width_in, y = Height_in)) +
  geom_point(alpha = 0.2) + 
  labs(
    title = "Height vs. width of paintings",
    x = "Width (inches)",
    y = "Height (inches)"
    ) +
  coord_cartesian(xlim = c(0, 250), ylim = c(0, 200)) +
  theme(plot.subtitle = element_text(colour = "#E48957", face = "bold", size = rel(1.5)))

p + 
  geom_smooth(method = "lm", color = "#8E2C90", se = FALSE) +
  geom_point(mapping = aes(y = .fitted), color = "#E48957") +
  geom_segment(mapping = aes(xend = Width_in, yend = .fitted), color = "#E48957", alpha = 0.4) +
  labs(subtitle = "Data + least squares line + residuals")


## ----echo=FALSE-------------------------------
pp %>% 
  select(name, Height_in, landsALL) %>%
  print(n = 20)


## ----ht-lands-fit-----------------------------
linear_reg() %>%
  set_engine("lm") %>%
  fit(Height_in ~ factor(landsALL), data = pp) %>%
  tidy()


## ----ht-school--------------------------------
linear_reg() %>%
  set_engine("lm") %>%
  fit(Height_in ~ school_pntg, data = pp) %>%
  tidy()


## ----ref.label="ht-school", echo = FALSE------


## ----echo=FALSE-------------------------------
dummy_df = pp %>% 
  select(school_pntg) %>% 
  group_by(school_pntg) %>% 
  sample_n(1) %>%
  mutate(
    D_FL = as.integer(ifelse(school_pntg == "D/FL", 1L, 0)),
    F    = as.integer(ifelse(school_pntg == "F", 1L, 0)),
    G    = as.integer(ifelse(school_pntg == "G", 1L, 0)),
    I    = as.integer(ifelse(school_pntg == "I", 1L, 0)),
    S    = as.integer(ifelse(school_pntg == "S", 1L, 0)),
    X    = as.integer(ifelse(school_pntg == "X", 1L, 0))
  )

dummy_df %>%
  kable(align = "lcccccc") %>%
  kable_styling() %>%
  column_spec(2, width = "10em", background = spec_color(dummy_df$D_FL[1:7], end = 0.8), color = "white") %>%
  column_spec(3, width = "10em", background = spec_color(dummy_df$F[1:7], end = 0.8), color = "white") %>%
  column_spec(4, width = "10em", background = spec_color(dummy_df$G[1:7], end = 0.8), color = "white") %>%
  column_spec(5, width = "10em", background = spec_color(dummy_df$I[1:7], end = 0.8), color = "white") %>%
  column_spec(6, width = "10em", background = spec_color(dummy_df$S[1:7], end = 0.8), color = "white") %>%
  column_spec(7, width = "10em", background = spec_color(dummy_df$X[1:7], end = 0.8), color = "white")


## ----echo=FALSE-------------------------------
pp %>% 
  select(name, Height_in, school_pntg) %>%
  print(n = 20)


## ----ref.label="ht-school", echo=FALSE--------


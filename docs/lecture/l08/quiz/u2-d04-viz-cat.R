library(tidyverse)
library(openintro)
loans_full_schema <- loans_full_schema %>%
  mutate(grade = factor(grade, ordered = TRUE))


## ----------------------------------------------------------------------------------------------------------------------------------
library(openintro)
loans <- loans_full_schema %>%
  select(loan_amount, interest_rate, term, grade, 
         state, annual_income, homeownership, debt_to_income)
glimpse(loans)


## ----------------------------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = homeownership)) +
  geom_bar()


## ----------------------------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = homeownership, fill = grade)) + geom_bar()


## ----------------------------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = homeownership, fill = grade)) +geom_bar(position = "fill") 



## ----bar-custom, fig.show = "hide", warning = FALSE--------------------------------------------------------------------------------
ggplot(loans, aes(y = homeownership, fill = grade)) + geom_bar(position = "fill") + labs(x = "Proportion", y = "Homeownership", fill = "Grade", title = "Grades of Lending Club loans", subtitle = "and homeownership of lendee")


## ----warning = FALSE---------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = homeownership, y = loan_amount)) +
  geom_violin()


## ----warning = FALSE---------------------------------------------------------------------------------------------------------------
library(ggridges)
ggplot(loans, aes(x = loan_amount, y = grade, fill = grade, color = grade)) + 
  geom_density_ridges(alpha = 0.5)


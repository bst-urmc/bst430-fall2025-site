library(tidyverse)
library(openintro)
loans_full_schema <- loans_full_schema %>%
  mutate(grade = factor(grade, ordered = TRUE))


## ----echo=FALSE, out.width = "100%"------------------------------------------------------------------------------------------------
knitr::include_graphics("img/lending-club.png")


## ----output.lines=18---------------------------------------------------------------------------------------------------------------
library(openintro)
glimpse(loans_full_schema)


## ----------------------------------------------------------------------------------------------------------------------------------
loans <- loans_full_schema %>%
  select(loan_amount, interest_rate, term, grade, 
         state, annual_income, homeownership, debt_to_income)
glimpse(loans)


## ----message = TRUE, out.width = "50%"---------------------------------------------------------------------------------------------
ggplot(loans, aes(x = loan_amount)) + geom_histogram()


## ----out.width = "50%"-------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = loan_amount)) + geom_histogram(binwidth = 1000)


## ----out.width = "50%"-------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = loan_amount)) + geom_histogram(binwidth = 5000)


## ----out.width = "50%"-------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = loan_amount)) + geom_histogram(binwidth = 20000)



## ----hist-fill, fig.show = "hide", warning = FALSE---------------------------------------------------------------------------------
ggplot(loans, aes(x = loan_amount, 
                  fill = homeownership)) + #<<
  geom_histogram(binwidth = 5000,
                 alpha = 0.5) + #<<
  labs(
    x = "Loan amount ($)",
    y = "Frequency",
    title = "Amounts of Lending Club loans"
  )


## ----ref.label = "hist-facet", echo = FALSE, warning = FALSE-----------------------------------------------------------------------


## ----hist-facet, fig.show = "hide", warning = FALSE--------------------------------------------------------------------------------
ggplot(loans, aes(x = loan_amount, fill = homeownership)) + 
  geom_histogram(binwidth = 5000) +
  labs(
    x = "Loan amount ($)",
    y = "Frequency",
    title = "Amounts of Lending Club loans"
  ) +
  facet_wrap(~ homeownership, nrow = 3) #<<


## ----------------------------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = loan_amount)) + geom_density()


## ----out.width = "50%"-------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = loan_amount)) + geom_density(adjust = 0.5)


## ----out.width = "50%"-------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = loan_amount)) + geom_density(adjust = 2)

ggplot(loans, aes(x = loan_amount)) + geom_density(adjust = 2) + labs(x = "Loan amount ($)", y = "Density", title = "Amounts of Lending Club loans")


ggplot(loans, aes(x = loan_amount, fill = homeownership)) + geom_density(adjust = 2,  alpha = 0.5) + labs(x = "Loan amount ($)", y = "Density", title = "Amounts of Lending Club loans", fill = "Homeownership")


## ----------------------------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = interest_rate)) + geom_boxplot()


## ----box-custom, fig.show = "hide", warning = FALSE--------------------------------------------------------------------------------
ggplot(loans, aes(x = interest_rate)) +
  geom_boxplot() +
  labs(
    x = "Interest rate (%)",
    y = NULL,
    title = "Interest rates of Lending Club loans"
  ) +
  theme( #<<
    axis.ticks.y = element_blank(), #<<
    axis.text.y = element_blank() #<<
  ) #<<


ggplot(loans, aes(x = interest_rate, y = grade)) + geom_boxplot() + labs(x = "Interest rate (%)", y = "Grade", title = "Interest rates of Lending Club loans", subtitle = "by grade of loan")


## ----warning = FALSE---------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = debt_to_income, y = interest_rate)) + geom_point()


## ----warning = FALSE---------------------------------------------------------------------------------------------------------------
ggplot(loans, aes(x = debt_to_income, y = interest_rate)) + geom_hex()


## ----warning = FALSE---------------------------------------------------------------------------------------------------------------
ggplot(loans %>% filter(debt_to_income < 100), 
       aes(x = debt_to_income, y = interest_rate)) +
  geom_hex()


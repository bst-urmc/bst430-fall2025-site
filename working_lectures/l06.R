
setwd("/scratch/user/u.sj319380/bst430-fall2025-site/")

library(tidyverse)
library(gapminder)

customers <- read_csv("working_lectures/l05/data/sales/customers.csv")
prices <- read_csv("working_lectures/l05/data/sales/prices.csv")
purchases <- customers %>% pivot_longer(item_1:item_3, names_to = "item_no", values_to = "item")
left_join(purchases, prices)

# Going back to wide form
purchases %>% pivot_wider(names_from = item_no, values_from = item)


biden <- read_csv("working_lectures/l05/data/trump/biden.csv")
trump <- read_csv("working_lectures/l05/data/trump/trump.csv")
summary(biden)
ratings <- biden %>% pivot_longer(cols = c(approval, disapproval), names_to = "rating_type", values_to = "rating")
ggplot(ratings, aes(date, rating, col=rating_type)) + 
  geom_line() + 
  facet_wrap(~ subgroup) + 
  scale_color_manual(values = c("dark green", "orange")) + 
  labs(x = "Date", y = "Rating", color = NULL, title = "How (un)popular is Joe Biden?",
       subtitle = "Estimates based on polls of all adults and pools of registered voters.",
       caption = "Source: FiveThirtyEight modeling estimates") +
  theme_minimal() + 
  theme(legend.position = "bottom")

tidyr::table2 %>%
  pivot_wider(names_from = type, values_from = count)

table4a %>%
  pivot_longer(cols = c("1999", "2000"), names_to = "year", values_to = "cases") %>%
  ggplot(aes(year, cases)) + 
  geom_point() + 
  facet_wrap(~country)

dim(gapminder)
names(gapminder)

gapminder |> nest(data = -continent)



#via https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv
#
counties = read_csv("~/Downloads/us-counties.csv")  %>% 
  filter(date > as.Date("2020-03-01"), state %in% c("Alabama", "Arizona", 'California', 'Florida', 'New York', 'Texas')) 

states = counties %>% 
  group_by(date, state) %>% summarise(across(c(cases, deaths), sum))
write_csv(counties, here::here('hw_lab_instruction/lab06-covid-times/', 'data', 'daily-selected-counties.csv'))
write_csv(states, here::here('hw_lab_instruction/lab06-covid-times/', 'data', 'daily-selected-states.csv'))

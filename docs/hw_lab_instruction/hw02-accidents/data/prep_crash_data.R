# From https://data.ny.gov/Transportation/Motor-Vehicle-Crashes-Case-Information-Three-Year-/e8ky-4vqe

library(tidyverse)
cases = read_csv('~/Downloads/Motor_Vehicle_Crashes_-_Case_Information__Three_Year_Window.csv') %>% select(-`Lighting Conditions`, -`Municipality`, -`Road Descriptor`, 
 -`Traffic Control Device`, -`Road Surface Conditions`,
 -`DOT Reference Marker Location`)

write_csv(cases, file = 'ny_collisions_2018_2019.csv')
system2('xz', "ny_collisions_2018_2019.csv")

#vehicle = read_csv('~/Downloads/Motor_Vehicle_Crashes_-_Vehicle_Information__Three_Year_Window.csv') %>% semi_join(ped_cases %>% select(`Event Type`=`Event Descriptor`))

#violations = read_csv('~/Downloads/Motor_Vehicle_Crashes_-_Violation_Information__Three_Year_Window.csv')

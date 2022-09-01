# via http://insideairbnb.com/get-the-data.html
listings_raw = data.table::fread('http://data.insideairbnb.com/united-states/ny/new-york-city/2021-08-04/data/listings.csv.gz')
vars_keep = c("id", "name", "host_id",
              "price", "neighbourhood" = "neighbourhood_cleansed", "accommodates", "room_type", 'neighbourhood_group_cleansed',"minimum_nights",
              "maximum_nights","availability_90",
               "beds", "review_scores_rating", "number_of_reviews", "number_of_reviews_ltm",
              "listing_url")
listings_raw2 = as_tibble(listings_raw) %>% filter(availability_365>0, number_of_reviews>0)
  
  listings_raw2 = listings_raw2[vars_keep] %>% rename(neighborhood = neighbourhood_cleansed, borough = neighbourhood_group_cleansed) %>%
  mutate(price = as.numeric(gsub('\\$|,', '', price)), price = pmin(price, 1000)) %>% group_by(neighborhood) %>%  filter(n()>100)
write_csv(listings_raw2, file = 'data/nylistings.csv')

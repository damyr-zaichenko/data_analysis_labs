knitr::opts_chunk$set(echo = TRUE)
library(tidyverse) 
library(kableExtra)
library(GGally)
library(qqplotr)
library(Hmisc)

hotel <- read.csv("Hotel Reservations.csv")

hotel_corr <- read.csv("Hotel Reservations.csv")

hotel <- hotel %>% mutate(repeated_guest = as.factor(repeated_guest), 
                          required_car_parking_space = as.factor(required_car_parking_space),
                          booking_status = as.factor(booking_status),
                          room_type_reserved = as.factor(room_type_reserved))

hotel <- hotel %>% mutate(arrival_year_and_month = paste(arrival_year, arrival_month, arrival_date, sep = "-"))


hotel_no_people = hotel %>% mutate(no_of_people = no_of_adults + no_of_children)

hotel_with_nights <- hotel %>% mutate(no_of_nights = no_of_weekend_nights + no_of_week_nights)

hotel_avg_prices <- hotel %>% filter((avg_price_per_room > 9))

hotel_sliced <- slice_sample(hotel_avg_prices, n = 5000)

hotel_grouped_by_room <- hotel %>% 
  dplyr::group_by(no_of_adults, no_of_children, room_type_reserved) %>% 
  dplyr::summarize(total = n(), .groups = "drop")

hotel_grouped_by_room <- hotel_grouped_by_room %>%
  mutate(pair = paste("(", no_of_adults, ",", no_of_children, ")", sep = "")) %>%
  arrange(desc(no_of_adults + no_of_children))

hotel_grouped_by_meal <- hotel %>% 
  dplyr::group_by(no_of_adults, no_of_children, type_of_meal_plan) %>% 
  dplyr::summarize(total = n(), .groups = "drop")

hotel_grouped_by_meal <- hotel_grouped_by_meal %>%
  mutate(pair = paste("(", no_of_adults, ",", no_of_children, ")", sep = "")) %>%
  arrange(desc(no_of_adults + no_of_children))

hotel_grouped_by_family <- hotel %>% 
  dplyr::group_by(no_of_adults, no_of_children) %>% 
  dplyr::summarize(total = n(), .groups = "drop")

hotel_grouped_by_bs <- hotel %>% 
  dplyr::group_by(no_of_adults, no_of_children, booking_status) %>% 
  dplyr::summarize(total = n(), .groups = "drop")

hotel_grouped_by_bs <- hotel_grouped_by_bs %>%
  mutate(pair = paste("(", no_of_adults, ",", no_of_children, ")", sep = "")) %>%
  arrange(desc(no_of_adults + no_of_children))

hotel_lead_check <- hotel_with_nights %>% filter(avg_price_per_room > 9)

hotel_lead_check <- hotel_lead_check %>%
  mutate(lead_time = ifelse(lead_time == 0, 1, lead_time))

hotel_zero_nights <- hotel_avg_prices %>% filter(no_of_weekend_nights == 0 | no_of_week_nights == 0)

room_type_vector <- c("Room_Type 1" = "#ff746c", "Room_Type 2" = "#c89c04", "Room_Type 3" = "#58b404", "Room_Type 4" = "#08c494", "Room_Type 5" = "#08b4ec", "Room_Type 6" = "#a88cfc", "Room_Type 7" = "#ff64d4")
room_label_vector <- c("?????? ?????????????? ???1", "?????? ?????????????? ???2", "?????? ?????????????? ???3", "?????? ?????????????? ???4", "?????? ?????????????? ???5", "?????? ?????????????? ???6", "?????? ?????????????? ???7")

meal_type_vector <- c("Meal Plan 1" = "#ff746c", "Meal Plan 2" = "#80ac04", "Meal Plan 3" = "#08bcc4", "Not Selected" = "#c87cfc")
meal_label_vector <- c("BB - Bed and Breakfast", "HB - Half Board", "FB - Full Board", "Not Selected") 
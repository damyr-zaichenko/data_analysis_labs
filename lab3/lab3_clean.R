library(tidyverse) 

original_hotel <- read.csv("Hotel Reservations.csv")

kinda_iriginal_hotel <- original_hotel %>% mutate(no_of_people = no_of_adults + no_of_children)

hotel_corr <- original_hotel

hotel <- original_hotel %>% filter((avg_price_per_room > 9),
                                   (no_of_children < 8))

hotel <- hotel %>% mutate(repeated_guest = as.factor(repeated_guest), 
                          required_car_parking_space = as.factor(required_car_parking_space),
                          booking_status = as.factor(booking_status),
                          room_type_reserved = as.factor(room_type_reserved),
                          no_of_special_requests = ifelse(no_of_special_requests == 0, 0, 1),
                          arrival_date_in_total = paste(arrival_year, arrival_month, arrival_date, sep = "-"),
                          arrival_year_and_month = paste(arrival_year, arrival_month, sep = "-"),
                          no_of_people = no_of_adults + no_of_children, 
                          no_of_nights = no_of_weekend_nights + no_of_week_nights,
                          no_of_people = no_of_adults + no_of_children)

hotel_reverse <- hotel
hotel_reverse$booking_status_binary <- ifelse(hotel$booking_status == "Canceled", 1, 0)

room_type_vector <- c("Room_Type 1" = "#ff746c", "Room_Type 2" = "#c89c04", "Room_Type 3" = "#58b404", "Room_Type 4" = "#08c494", "Room_Type 5" = "#08b4ec", "Room_Type 6" = "#a88cfc", "Room_Type 7" = "#ff64d4")
room_label_vector <- c("Тип кімнати №1", "Тип кімнати №2", "Тип кімнати №3", "Тип кімнати №4", "Тип кімнати №5", "Тип кімнати №6", "Тип кімнати №7")

meal_type_vector <- c("Meal Plan 1" = "#ff746c", "Meal Plan 2" = "#80ac04", "Meal Plan 3" = "#08bcc4", "Not Selected" = "#c87cfc")
meal_label_vector <- c("BB - Bed and Breakfast", "HB - Half Board", "FB - Full Board", "Not Selected")
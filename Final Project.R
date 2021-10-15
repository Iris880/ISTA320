# Weici Cao Final Project
library(tidyverse)
library(ggplot2)
library(sf)
library(geobr)
library(ggrepel)
library(effects)

# Read Brazilian map data, from `geobr` library
states <- read_state(year=2016)

# Read datasets
#All of those data from kaggle
customer <- read_csv("data/olist_customers_dataset.csv")
order <- read_csv("data/olist_orders_dataset.csv")
payment <- read_csv("data/olist_order_payments_dataset.csv")
review <- read_csv("data/olist_order_reviews_dataset.csv")

# Q1: Which state has the largest amount of orders?
# Answer: São Paulo. See the map plot below.

# Left join two datasets to get customer info of each order
order_custormer <- merge(order, customer, by = "customer_id", all.x = TRUE)

# Filter out canceled and unavailable orders, then count orders by state
order_count <- order_custormer %>%
  filter(!(order_status %in% c("canceled", "unavailable"))) %>%
  group_by(customer_state) %>%
  summarise(count = length(unique(order_id)))

# Prepare for drawing map
order_count_states <- merge(states, order_count, by.x = "abbrev_state", by.y = "customer_state")

centroid_labels <- order_count_states %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# Draw the map using `Blues` palette, which is colorblind-friendly
ggplot() +
  geom_sf(data = order_count_states, aes(fill = count), color = NA, size = .15) +
  labs(title = "Which state has the largest amount of orders?") +
  scale_fill_distiller(palette = "Blues", name = "Number of Orders", direction = 1) + 
  theme_minimal() +
  no_axis +
  geom_label_repel(data = centroid_labels,
                   aes(x = lon, y = lat, label = paste(abbrev_state, count)),
                   size = 3)


# Q2: What time in a day do people purchase most? (By order count and by purchase amount)
# Answer: 16:00-17:00 has the most order count, and 14:00-15:00 has the most purchase amount.
# Check the histogram and line plot below.

# Filter out canceled and unavailable orders, then extract purchase hour information
valid_order <- order %>% 
  filter(!(order_status %in% c("canceled", "unavailable"))) %>%
  mutate(hour = substr(order_purchase_timestamp, 12, 13))

# Summarise order count by hour
byhour_order_count <- valid_order %>%
  group_by(hour) %>%
  summarise(count = length(unique(order_id)))

# Inner join two dataframes to get payment info of each valid order
order_payment <- merge(valid_order, payment, by = "order_id")

# Summarise purchase amount by hour
byhour_purchase_amount <- order_payment %>%
  group_by(hour) %>%
  summarise(total_payment = sum(payment_value))

# Merge and plot
byhour <- merge(byhour_order_count, byhour_purchase_amount, by = "hour")
byhour %>%
  ggplot() +
  labs(title = "What time in a day do people purchase most?", x = "Hour") + 
  geom_col(aes(x = hour, y = count), color = "darkblue", fill = "white") +
  geom_line(aes(x = hour, y = total_payment/100), color="red", group = 1) +
  scale_y_continuous(name="Order Count", sec.axis = sec_axis(~ .*100, name="Total Payment"))

# Q3: Is review score affected by customer state? 
# Answer: Review scores vary across different states, but their std errors are also big.
# Controls' p-value shows great significance, which means we miss important variables.
# See the errorbar figure below.

# Inner join two dataframes to get reviews and customer info
review_state <- merge(review, order_custormer, by = "order_id")

# Run a linear regression model on the effect of customer state on the review scores
review_state_model <- review_state %>%
  lm(formula = review_score ~ customer_state) 

summary(review_state_model)

effect("customer_state", review_state_model) %>%
  data.frame() %>%
  ggplot(aes(y = reorder(customer_state, fit),
             x = fit,
             label = round(fit, digits = 2))) +
  geom_errorbar(aes(xmin = lower,
                    xmax = upper),
                width = .1) +
  geom_label() +
  labs(title = "Review scores across different regions",
       x = "Review Score",
       y = "Customer State")
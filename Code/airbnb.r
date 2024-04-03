
# Print neighbourhood_counts
print(neighbourhood_counts)

# Print the shape of Airbnb_NYC_2019 dataset
cat("The shape of Airbnb Dataset is", dim(Airbnb_NYC_2019), "\n")


cat("The names of the features present in the dataset are: \n")
cat(names(Airbnb_NYC_2019), "\n")


cat_cols <- colnames(Airbnb_NYC_2019)[sapply(Airbnb_NYC_2019, is.character)]
cat("The following are the categorical features in the dataset:", list(cat_cols))

cat_cols <- names(Airbnb_NYC_2019)[sapply(Airbnb_NYC_2019, is.character)]
cat("The following are the categorical features in the dataset:", paste(cat_cols, collapse = ", "))


num_cols <- colnames(Airbnb_NYC_2019)[sapply(Airbnb_NYC_2019, function(x) !is.character(x))]
cat('The following are the non-categorical features in the dataset:\n')
cat(list(num_cols), '\n')


num_cols <- names(Airbnb_NYC_2019)[sapply(Airbnb_NYC_2019, is.numeric)]
cat("The following are the non-categorical features in the dataset:\n")
cat(paste(num_cols, collapse = ", "), "\n")

head(Airbnb_NYC_2019)

tail(Airbnb_NYC_2019, 5)

str(Airbnb_NYC_2019)


cat("The missing values before cleaning the data are:\n")
cat(colSums(is.na(Airbnb_NYC_2019)), "\n")

Airbnb_NYC_2019$name <- ifelse(is.na(Airbnb_NYC_2019$name), "Absent", Airbnb_NYC_2019$name)
Airbnb_NYC_2019$host_name <- ifelse(is.na(Airbnb_NYC_2019$host_name), "Absent", Airbnb_NYC_2019$host_name)


Airbnb_NYC_2019_new <- Airbnb_NYC_2019[, !(names(Airbnb_NYC_2019) %in% c("last_review", "reviews_per_month"))]
head(Airbnb_NYC_2019_new)

cat("The number of missing values after cleaning the data are:\n")
cat(colSums(is.na(Airbnb_NYC_2019_new)), "\n")


host_areas <- aggregate(calculated_host_listings_count ~ host_name + neighbourhood_group, Airbnb_NYC_2019_new, max)
top_hosts <- head(arrange(host_areas, desc(calculated_host_listings_count)), 9)
top_hosts


host_areas <- Airbnb_NYC_2019_new %>%
  group_by(host_name, neighbourhood_group) %>%
  summarise(calculated_host_listings_count = max(calculated_host_listings_count)) %>%
  arrange(desc(calculated_host_listings_count)) %>%
  head(9)

host_areas

#visualizing the hosts with most listings

options(repr.plot.width = 12, repr.plot.height = 6)

top_hosts <- head(host_areas, 9)

host_name <- top_hosts$host_name
host_listing <- top_hosts$calculated_host_listings_count
barplot(host_listing, names.arg = host_name, xlab = "Host Names", ylab = "Number of host listings", main = "Hosts with most listings in NYC", col = "blue", horiz = FALSE)

neighbourhood_counts <- table(Airbnb_NYC_2019_new$neighbourhood_group)


#installed "ggplot2" package

neighbourhood_counts <- as.data.frame(table(Airbnb_NYC_2019_new$neighbourhood_group))

options(repr.plot.width = 12, repr.plot.height = 6)

# Create bar plot
ggplot(neighbourhood_counts, aes(x = Var1, y = Freq)) +
  geom_bar(aes(fill = Var1), stat = "identity") +
  labs(x = "Neighbourhood Group", y = "Number of Listings", title = "Number of Listings in Each Neighbourhood") +
  theme_minimal()


areas_reviews <- aggregate(number_of_reviews ~ neighbourhood_group, Airbnb_NYC_2019_new, max)
areas_reviews <- areas_reviews[order(areas_reviews$number_of_reviews, decreasing = TRUE), ]
areas_reviews

reviews_counts <- table(Airbnb_NYC_2019_new$neighbourhood_group)

reviews_counts <- as.numeric(reviews_counts)

options(repr.plot.width = 13, repr.plot.height = 8)

areas_reviews$labels <- paste0(areas_reviews$neighbourhood_group, "\n", round(areas_reviews$number_of_reviews / sum(areas_reviews$number_of_reviews) * 100, 2), "%")


summary(Airbnb_NYC_2019_new)

aggregate(price ~ 1, data = Airbnb_NYC_2019_new, FUN = function(x) c(mean = mean(x), median = median(x), max = max(x), count = length(x)))

options(repr.plot.width = 10, repr.plot.height = 5)

# Create boxplot
ggplot(Airbnb_NYC_2019_new, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Price Distribution by Neighbourhood Group", x = "Neighbourhood Group", y = "Price") +
  theme_minimal()


q_low <- quantile(Airbnb_NYC_2019_new$price, probs = 0.10)
q_low


q_high <- quantile(Airbnb_NYC_2019_new$price, probs = 0.9)
q_high


Airbnb_NYC_2019_new <- Airbnb_NYC_2019_new[Airbnb_NYC_2019_new$price >= q_low & Airbnb_NYC_2019_new$price <= q_high, ]

#new boxplot

options(repr.plot.width = 10, repr.plot.height = 5)

# boxplot
ggplot(Airbnb_NYC_2019_new, aes(y = price)) +
  geom_boxplot(fill = "white", color = "tan4") +
  labs(title = "Price Distribution", y = "Price") +
  theme_minimal()

# price distribution by neighbourhood groups

options(repr.plot.width = 12, repr.plot.height = 8)

# violin plot
ggplot(Airbnb_NYC_2019_new, aes(x = neighbourhood_group, y = price)) +
  geom_violin(fill = "white", color = "black") +
  labs(title = "Price Distribution by Neighbourhood Groups", x = "Neighbourhood Group", y = "Price") + theme_minimal()

busiest_hosts <- aggregate(number_of_reviews ~ host_name + host_id + room_type + neighbourhood_group, data = Airbnb_NYC_2019_new, FUN = max)

busiest_hosts <- busiest_hosts[order(busiest_hosts$number_of_reviews, decreasing = TRUE), ]
busiest_hosts <- head(busiest_hosts, 10)

print(busiest_hosts)

options(repr.plot.width = 10, repr.plot.height = 5)

# Create bar chart
library(ggplot2)

ggplot(busiest_hosts, aes(x = host_name, y = number_of_reviews)) +
  geom_bar(stat = "identity", fill = "burlywood4") +
  labs(title = "Busiest Hosts", x = "Host Names", y = "Number of Reviews") +  # Corrected "cy" to "y"
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

options(repr.plot.width = 10, repr.plot.height = 5)

# Create histogram
ggplot(Airbnb_NYC_2019_new, aes(x = room_type, fill = room_type)) +
  geom_bar() +
  labs(title = "Distribution of Room Types", x = "Room Type", y = "Number of Listings") + theme_minimal()

df1 <- Airbnb_NYC_2019_new %>%
  group_by(neighbourhood) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

print(df1)

df1 <- as.data.frame(df1)

# Create the bar chart

library(ggplot2)  # Make sure the ggplot2 library is loaded

ggplot(df1, aes(x = count, y = reorder(neighbourhood, count))) +
  geom_bar(stat = "identity", fill = "gold") +
  labs(title = "Top 10 Neighbourhoods with the Most Listings", x = "Total Listings", y = "Neighbourhood") +
  theme_minimal()

top_reviews <- aggregate(number_of_reviews ~ neighbourhood_group + neighbourhood + room_type, data = Airbnb_NYC_2019_new, FUN = max)

top_reviews <- top_reviews[order(top_reviews$number_of_reviews, decreasing = TRUE), ]
top_reviews <- head(top_reviews, 10)

print(top_reviews)

price_bins <- c(0, 50, 100, 150, 200, Inf)
price_labels <- c("Low", "Medium", "Moderate", "High", "Very High")

Airbnb_NYC_2019_new$price_range <- cut(Airbnb_NYC_2019_new$price, breaks = price_bins, labels = price_labels, include.lowest = TRUE)

head(Airbnb_NYC_2019_new)

subset_data <- Airbnb_NYC_2019_new[, c('price_range', 'host_name')]

print(subset_data)

set.seed(123)

train_indices <- createDataPartition(Airbnb_NYC_2019_new$price, p = 0.8, list = FALSE)
train_data <- Airbnb_NYC_2019_new[train_indices, ]
test_data <- Airbnb_NYC_2019_new[-train_indices, ]

cat("Training set dimensions:", dim(train_data), "\n")
cat("Testing set dimensions:", dim(test_data), "\n")

head(train_data)

head(test_data)

last_rows <- tail(Airbnb_NYC_2019_new, 5)

# linear regression model

lm_model <- lm(price ~ ., data = last_rows)

predictions <- predict(lm_model, newdata = last_rows)


print(predictions)

# model summary

summary(lm_model)

predictions <- predict(lm_model, newdata = test_data)

few_rows_test <- head(test_data, 5)  # Change the number 5 to the desired number of rows

predictions <- predict(lm_model, newdata = few_rows_test)

actual_prices <- few_rows_test$price

rmse <- sqrt(mean((actual_prices - predictions)^2))

cat("Root Mean Squared Error (RMSE):", rmse, "\n")

rse <- sqrt(mean((actual_prices - predictions)^2)) / sd(actual_prices)

r_squared <- 1 - sum((actual_prices - predictions)^2) / sum((actual_prices - mean(actual_prices))^2)

cat("Root Squared Error (RSE):", rse, "\n")
cat("R-squared:", r_squared, "\n")

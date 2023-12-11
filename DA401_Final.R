library(tidyverse)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggmap)
library(randomForest)
library(forcats)
library(Metrics)
library(glmnet)


#Importing data
pace_data <- read_csv("pace_data.csv")
pace_data2 <- read_csv("pace_data2.csv")
uscities <- read_csv("uscities.csv")

#Removing columns not needed from US cities dataset
uscities <- uscities %>% select(-county_fips_all, -county_name_all, -cdp, -zips)

#Filtering uscities dataset by population - don't want any super duper tiny towns
uscities <- uscities[uscities$population >= 10000, ]
uscities <- uscities[1:4793, ]

#Merging pace data and uc cities
uscities <- rename(uscities, City = city)
class(pace_data2$City)
class(uscities$City)
pace_data2 <- pace_data2[, 1:4]
pace_data2 <- rename(pace_data2, state_name = State)

state_lookup <- data.frame(
  abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                   "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                   "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                   "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                   "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  full_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", 
                "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)
pace_data2 <- pace_data2 %>%
  left_join(state_lookup, by = c("state_name" = "abbreviation")) %>%
  mutate(state_name = full_name) %>%
  select(-full_name)

class(pace_data2$state_name)
class(uscities$state_name)


#Descriptive Stats for pace_data
summ_pace <- summary(pace_data)

#Descriptive Stats for uscities
summ_cities <- summary(uscities)

#Pace_data correlation matrix
numeric_pace <- pace_data[sapply(pace_data, is.numeric)]
correlation_matrix1 <- cor(numeric_pace)
cor(numeric_pace$`PACE Asset Balance`, numeric_pace$`Term (Years)`)

correlation_matrix1

#uscities correlation matrix
numeric_cities <- uscities[sapply(uscities, is.numeric)]
correlation_matrix <- cor(numeric_cities, use="pairwise.complete.obs")
corrplot(correlation_matrix)


#Ridge Regression
predictors <- as.matrix(pace_data[, c("Coupon", "Term (Years)", "Current Total Debt MTG + PACE", "Mortgage Balance", "Total Project Budget")])
response <- pace_data$`PACE Asset Balance`

# Fit the Ridge Regression model
ridge_model <- glmnet(predictors, response, alpha = 0)

# Perform cross-validation to find the optimal lambda
cv_ridge <- cv.glmnet(predictors, response, alpha = 0)

# Best lambda
best_lambda <- cv_ridge$lambda.min

# Fit the model with the best lambda
final_ridge_model <- glmnet(predictors, response, alpha = 0, lambda = best_lambda)

# Print the summary of the final model (Note: glmnet does not have a typical 'summary' method)
print(final_ridge_model)

# Extract coefficients
ridge_coeffs <- coef(final_ridge_model, s = best_lambda)

# View the coefficients
print(ridge_coeffs)

#Ridge Regerssion stats
stargazer(pace_data, type = "html", out = "summary_stats.html")

# Calculate R-squared
predicted_values <- predict(final_ridge_model, newx = predictors, s = best_lambda)
r_squared <- cor(response, predicted_values)^2

#Distribution of Total Amount Financed in pace_data - dep variable
ggplot(pace_data, aes(x=`PACE Asset Balance`)) + geom_histogram(fill="lightblue", color="black") + ggtitle("Distribution of Total Amount Financed")

#Distribution of population in US cities, omitting all citiest below 10000 pop - most are tiny
ggplot(uscities, aes(x=population)) + geom_histogram(fill="lightblue", color="black") + ggtitle("Distribution of Population")

#Ridge Regression residual plot
# Calculate residuals
residuals_values <- pace_data$`PACE Asset Balance` - predicted_values

# Create a residual plot
plot(predicted_values, residuals_values, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot for Ridge Regression")
abline(h = 0, col = "red") # Adds a horizontal line at 0 for reference


#Visualisation of cities in the uscities dataset
register_google(key = "AIzaSyA34GsIvCy6603uh_3P4-yFObbiweCegHI")

us_map <- get_map(location = "United States", zoom = 4)

ggmap(us_map) + 
  geom_point(data = uscities, aes(x = lng, y = lat), color = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Locations on US Map")


#Anova 
anova_result <- aov(pace_data$`PACE Asset Balance` ~ pace_data$`Property Type`, data=pace_data)
summary(anova_result)

#Anova boxplot viz
ggplot(pace_data, aes(x=`Property Type`, y=`PACE Asset Balance`, fill=`Property Type`)) +
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Boxplot of Values by Property Type",
       x = "Property Type",
       y = "PACE Asset Balance") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Merging data for randomforest
merged_data <- full_join(pace_data2, uscities, by=c("City" = "City", "state_name" = "state_name"))

#Combining State/City names to make it easy to identify cities post analysis
merged_data$city_state <- with(merged_data, paste(City, state_name, sep=" - "))

#Cleaning merged data
merged_data$`Number of Projects`[is.na(merged_data$`Number of Projects`)] <- 0
merged_data$`Total PACE Asset Balance`[is.na(merged_data$`Total PACE Asset Balance`)] <- 0
colnames(merged_data) <- make.names(colnames(merged_data))

#Removing NAs
for (col_name in names(merged_data)) {
  if (is.numeric(merged_data[[col_name]])) {
    merged_data[[col_name]][is.na(merged_data[[col_name]])] <- 0
  } else if (is.factor(merged_data[[col_name]]) || is.character(merged_data[[col_name]])) {
    if(is.factor(merged_data[[col_name]])) {
      levels(merged_data[[col_name]]) <- c(levels(merged_data[[col_name]]), "None")
    }
    merged_data[[col_name]][is.na(merged_data[[col_name]])] <- "None"
  } else if (is.logical(merged_data[[col_name]])) {
    merged_data[[col_name]][is.na(merged_data[[col_name]])] <- FALSE
  }
}

# Count NA in the whole dataset
total_na <- sum(is.na(merged_data))
total_na

#Randomforest Time
set.seed(123) 

merged_data$City <- as.factor(merged_data$City)

train_data <- subset(merged_data, Number.of.Projects > 0)
test_data <- subset(merged_data, Number.of.Projects == 0)

# Drop unused factor levels in 'City'
train_data <- droplevels(train_data)

rf_model <- randomForest(Total.PACE.Asset.Balance ~ . - City, data=train_data)

# Predict on test data
predictions <- predict(rf_model, newdata = test_data)
predictions
test_data$Predicted_PACE_Asset_Balance <- predictions

top_cities <- test_data %>%
  arrange(desc(Predicted_PACE_Asset_Balance)) %>%
  slice(1:10)  # Select the top 10 rows

# View the top 10 cities
print(top_cities)

#Visualisation of location of top 10 cities in the uscities dataset
register_google(key = "AIzaSyA34GsIvCy6603uh_3P4-yFObbiweCegHI")

us_map <- get_map(location = "United States", zoom = 4)

ggmap(us_map) + 
  geom_point(data = top_cities, aes(x = lng, y = lat), color = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Locations on US Map")

#Validation

mae <- mae(test_data$Total.PACE.Asset.Balance, test_data$Predicted_PACE_Asset_Balance)
rmse <- rmse(test_data$Total.PACE.Asset.Balance, test_data$Predicted_PACE_Asset_Balance)
r_squared <- cor(test_data$Total.PACE.Asset.Balance, test_data$Predicted_PACE_Asset_Balance)^2

# Print the metrics
print(paste("MAE:", mae))
print(paste("RMSE:", rmse))
print(paste("R-squared:", r_squared))


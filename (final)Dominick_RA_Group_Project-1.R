################### 1. Data Cleaning & EDA ##############

setwd("D:/_Zzzxy/_JHU/Spring 1/Retail Analytics/Week5")

library(dplyr)
library(ggplot2)
library(readr)
library(VIM)
library(GGally)

df <- read.csv("demo.csv")

# View dataset structure
dim(df)
str(df)
summary(df)
head(df)

# Calculate the percentage of missing values for each column
missing_values <- colSums(is.na(df)) / nrow(df) * 100

# Sort columns in descending order of missing value percentage
missing_summary <- data.frame(Column = names(missing_values), MissingPercentage = missing_values)
missing_summary <- missing_summary %>% arrange(desc(MissingPercentage))

# View the top 20 columns with the most missing values
head(missing_summary, 20)

# Drop all columns after column 57 (group1)
df_dropped <- df[, 1:56]
summary(df_dropped)

# Check for missing values again
colSums(is.na(df_dropped)) / nrow(df_dropped) * 100

# Remove the "gini" column (100% missing) and "mmid" (similar to an ID but not entirely unique)
df_dropped <- df_dropped %>% select(-gini, -mmid)

# Compute the mean values by city (excluding NA values)
zip_mean <- df_dropped %>%
  group_by(city) %>%
  summarise(mean_zip = mean(zip, na.rm = TRUE),
            mean_lat = mean(lat, na.rm = TRUE),
            mean_long = mean(long, na.rm = TRUE),
            mean_weekvol = mean(weekvol, na.rm = TRUE))

# Fill missing values using the city-level means
df_dropped <- df_dropped %>%
  left_join(zip_mean, by = "city") %>%
  mutate(zip = ifelse(is.na(zip), mean_zip, zip),
         lat = ifelse(is.na(lat), mean_lat, lat),
         long = ifelse(is.na(long), mean_long, long),
         weekvol = ifelse(is.na(weekvol), mean_weekvol, weekvol)) %>%
  select(-mean_zip, -mean_lat, -mean_long, -mean_weekvol)

# Check for any remaining NA values
colSums(is.na(df_dropped))

# Remove rows where age9, age60, and ethnic are all missing
df_dropped <- df_dropped %>% 
  filter(!(is.na(age9) & is.na(age60) & is.na(ethnic)))

# Perform KNN imputation for key variables
df_filled <- kNN(df_dropped, variable = c("lat", "long"), k = 3)

# Remove temporary "_imp" columns generated after KNN imputation
df_filled <- df_filled %>% select(-ends_with("_imp"))

# Fill missing city values using the mode of each ZIP code
df_filled <- df_filled %>%
  group_by(zip) %>%
  mutate(city = ifelse(is.na(city), names(which.max(table(city, useNA = "no"))), city)) %>%
  ungroup()

# Fill missing weekly sales volume (weekvol) using city-wise mean
df_filled <- df_filled %>%
  group_by(city) %>%
  mutate(weekvol = ifelse(is.na(weekvol), mean(weekvol, na.rm = TRUE), weekvol)) %>%
  ungroup()

# Compute the mode of the zone variable within each ZIP code (ignoring NA)
zip_zone_mode <- df_filled %>%
  filter(!is.na(zone)) %>%
  group_by(zip) %>%
  summarise(mode_zone = names(which.max(table(zone))))  # Compute mode

# Merge back into the dataset and fill missing zone values
df_filled <- df_filled %>%
  left_join(zip_zone_mode, by = "zip") %>%
  mutate(zone = ifelse(is.na(zone), mode_zone, zone)) %>%
  select(-mode_zone)  # Remove temporary column

# Fill missing zone values using KNN if ZIP code information is insufficient
df_filled <- kNN(df_filled, variable = "zone", k = 3)

# Remove "_imp" columns from KNN imputation
df_filled <- df_filled %>% select(-ends_with("_imp"))

# Create a ZIP Code to City mapping table
zip_city_mapping <- data.frame(
  zip = c(60462, 60139, 60650, 60614, 60050, 60648, 60073, 60160, 60504, 60004, 60445),
  city_fill = c("Orland Park", "Glendale Heights", NA, "Chicago", "McHenry", NA, 
                "Round Lake", "Melrose Park", "Aurora", "Arlington Heights", "Midlothian")
)

# Left join with ZIP code mapping and fill missing city values
df_filled <- df_filled %>%
  left_join(zip_city_mapping, by = "zip") %>%
  mutate(city = coalesce(city, city_fill)) %>%  # Fill city with city_fill values
  select(-city_fill)  # Remove temporary column

# Ensure all missing values are handled
colSums(is.na(df_filled))

summary(df_filled)

# **Retail Market Gap Analysis:**
# Identify high-consumption markets with low store density by analyzing 
# "density" (store density per capita) and "shopindx" (shopping ability index)
ggplot(df_filled, aes(x = density, y = shopindx)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Retail Market Opportunities: Density vs. Shopping Index")

# **Findings:**
# - The regression line indicates that areas with higher density also tend to have higher shopping ability index.
# - This suggests that high-density areas, despite having more stores, generally have stronger purchasing power.
# - **Potential Retail Opportunities:**
#   - **Lower-left corner (low density, low shopindx):** These areas might be retail opportunity zones due to low competition, 
#     but they may also have limited purchasing power.
#   - **Upper-right corner (high density, high shopindx):** Competitive markets, ideal for high-end retail or specialty stores.

# **Assessing Competition:**
# - Should areas with high "shopbird" (highly active shoppers) have more retail competition?
ggplot(df_filled, aes(x = shpbird, y = density)) +
  geom_point(alpha = 0.6, color = "green") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Shopper Bird vs Store Density")

# **Label cities to identify priority retail opportunity locations**
ggplot(df_filled, aes(x = density, y = shopindx, label = city)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  geom_text(vjust = -1, size = 3) +  # Add city labels
  labs(title = "Retail Market Opportunities: Density vs. Shopping Index")

# **High-End Supermarket Site Selection:**
# - Identify regions with low density but high shopindx
df_filled %>%
  filter(density < 0.002 & shopindx > 0.8) %>%
  select(city, density, shopindx) %>%
  arrange(desc(shopindx))
# These areas may be ideal locations for new high-end supermarkets due to high purchasing power but low store density.

# **Impact of Detached Housing (sinhouse) on High-End Supermarket Selection**
ggplot(df_filled, aes(x = sinhouse, y = shopindx)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Single House Proportion vs Shopping Index")

# **Competition Analysis:**
# - Does a high proportion of constrained shoppers (shopcons) impact discount store location strategy?
ggplot(df_filled, aes(x = shpcons, y = weekvol)) +
  geom_point(alpha = 0.6, color = "orange") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Constrained Shoppers vs Weekly Sales Volume")

# **Findings:**
# - Higher shopcons slightly increases weekvol, but the correlation is weak.
# - This suggests that in budget-conscious shopping areas, retail sales volume may be slightly higher.
# - Likely due to constrained shoppers preferring discount stores or bulk purchasing.

# **List cities with high constrained shoppers and high weekly sales volume (ideal discount store locations)**
df_filled %>%
  filter(shpcons > 0.15 & weekvol > 500) %>%
  select(city, shpcons, weekvol) %>%
  arrange(desc(weekvol))

# **Compute Correlation Matrix for Numeric Variables**
num_vars <- df_filled %>% select_if(is.numeric)
ggpairs(num_vars)



################### 2. Cluster Analysis of Consumer Shopping Behavior ##############

data <- read.csv("demo.csv")

# Inspect data structure
str(data)
colnames(data)

# Save the index that removes missing values first
shopping_vars <- data %>% select(shpbird, shpavid, shphurr, shopindx)
shopping_vars_scaled <- scale(na.omit(shopping_vars))  # Only numerical variables are normalized
filtered_rows <- complete.cases(shopping_vars)  # Record which rows have no missing values

# Perform K-means clustering
set.seed(123)
kmeans_result <- kmeans(shopping_vars_scaled, centers = 4, nstart = 25)

# Start by creating an empty cluster column
data$cluster <- NA

# Assign values only to rows with no missing values
data$cluster[filtered_rows] <- kmeans_result$cluster

# Make sure the cluster is of factor type
data$cluster <- as.factor(data$cluster)

# Visualize cluster
library(Rtsne)
tsne_result <- Rtsne(shopping_vars_scaled, perplexity = 30)
tsne_data <- as.data.frame(tsne_result$Y)
tsne_data$cluster <- as.factor(kmeans_result$cluster)

# Compute the convex hull and draw the boundary
convex_hull <- tsne_data %>%
  group_by(cluster) %>%
  slice(chull(V1, V2))  # Gets the convex packet points for each cluster

ggplot(tsne_data, aes(x = V1, y = V2, color = cluster)) +
  geom_point(alpha = 0.7) + 
  geom_polygon(data = convex_hull, aes(x = V1, y = V2, fill = cluster), 
               alpha = 0.2, color = NA) +  
  ggtitle("t-SNE Cluster Visualization with Convex Hull") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  
  scale_fill_brewer(palette = "Set1")  

# summary clusters characters
summary_stats <- data %>%
  group_by(cluster) %>%
  summarise(
    mean_shpbird = mean(shpbird, na.rm = TRUE),
    mean_shpavid = mean(shpavid, na.rm = TRUE),
    mean_shphurr = mean(shphurr, na.rm = TRUE),
    mean_shopindx = mean(shopindx, na.rm = TRUE)
  )

print(summary_stats)

library(dplyr)

cluster_ranges <- data %>%
  filter(!is.na(cluster)) %>% 
  group_by(cluster) %>%
  summarise(
    min_shpbird = min(shpbird, na.rm = TRUE),
    max_shpbird = max(shpbird, na.rm = TRUE),
    
    min_shpavid = min(shpavid, na.rm = TRUE),
    max_shpavid = max(shpavid, na.rm = TRUE),
    
    min_shphurr = min(shphurr, na.rm = TRUE),
    max_shphurr = max(shphurr, na.rm = TRUE),
    
    min_shopindx = min(shopindx, na.rm = TRUE),
    max_shopindx = max(shopindx, na.rm = TRUE)
  )


print(cluster_ranges)



#######################Correlation Analysis#####################################

# Correlation analysis
cor_income <- cor.test(data$shpbird, data$income, use = "complete.obs")
cor_density <- cor.test(data$shpbird, data$density, use = "complete.obs")
cor_poverty <- cor.test(data$shpbird, data$poverty, use = "complete.obs")

# Print correlation results
print(cor_income)
print(cor_density)
print(cor_poverty)

# Visualization of relationships

# income & shpbird
p1 <- ggplot(data, aes(x = income, y = shpbird)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Shopbird vs Income")

p1

ggplot(data, aes(x = income, y = shpbird)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_smooth(method = "loess", color = "blue", size = 1.2, se = TRUE) +
  ggtitle("Shopbird vs Income - LOESS Fit") +
  theme_minimal()

data$income_group <- cut(data$income, 
                         breaks = quantile(data$income, probs = seq(0, 1, 0.33), na.rm = TRUE), 
                         labels = c("Low", "Medium", "High"),
                         include.lowest = TRUE)


ggplot(data %>% filter(!is.na(income_group)), aes(x = income_group, y = shpbird, fill = income_group)) +
  geom_boxplot() +
  ggtitle("Shopbird vs Income Group") +
  theme_minimal()

aov_result <- aov(shpbird ~ income_group, data = data)
summary(aov_result)

# density & shpbird
p2 <- ggplot(data, aes(x = density, y = shpbird)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Shopbird vs Density")

p2

ggplot(data, aes(x = density, y = shpbird)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_smooth(method = "loess", color = "blue", size = 1.2, se = TRUE) +
  ggtitle("Shopbird vs Density - LOESS Fit") +
  theme_minimal()

#out and examples
data$density_group <- cut(data$density, 
                          breaks = c(-Inf, 0.0005, 0.0015, Inf), 
                          labels = c("Low", "Medium", "High"))

ggplot(data %>% filter(!is.na(density_group)), aes(x = density_group, y = shpbird, fill = density_group)) +
  geom_boxplot() +
  ggtitle("Shopbird vs Density Group") +
  theme_minimal()

aov_density <- aov(shpbird ~ density_group, data = data)
summary(aov_density)

#poverty & shpbird
p3 <- ggplot(data, aes(x = poverty, y = shpbird)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Shopbird vs Poverty")

p3

ggplot(data, aes(x = poverty, y = shpbird)) +
  geom_point(alpha = 0.6, color = "black") +
  geom_smooth(method = "loess", color = "blue", size = 1.2, se = TRUE) +
  ggtitle("Shopbird vs Density - LOESS Fit") +
  theme_minimal()



################# 3. Calculation and weight selection ##############
data <- read.csv("demo.csv")
attach(data)

# Calculate Retail Score
data$Retail_Score <- 0.2537 * (0.5 * shpbird + 0.5 * shopindx) + 
  0.2366 * (0.5 * income + 0.5 * hsizeavg) + 
  0.1944 * (0.5 * density + 0.5 * shpcons) + 
  0.1813 * (1 - nocar) + 
  0.1803 * (0.5 * sinhouse + 0.5 * hvalmean)

# Filter valid data (zone between 1 and 16, and Retail_Score is not NA)
data_filtered <- data %>%
  filter(zone %in% 1:16, !is.na(Retail_Score))

# Transform data to long format, including price categories
data_long <- data_filtered %>%
  pivot_longer(cols = c(priclow, pricmed, prichigh), 
               names_to = "pricerange", 
               values_to = "value") %>%
  filter(value == 1) %>%
  select(store, Retail_Score, zone, city, pricerange)  # Retain store number, retail score, zone, city, and price category

# Group by Zone and price category, calculate the average Retail Score
zone_price_scores <- data_long %>%
  group_by(zone, pricerange) %>%
  summarise(avg_retail_score = mean(Retail_Score)) %>%
  arrange(desc(avg_retail_score))

# Print the average Retail Score for each Zone and price category
print(zone_price_scores)

# Bar plot showing the average Retail Score by Zone and price category
ggplot(zone_price_scores, aes(x = interaction(zone, pricerange), y = avg_retail_score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(avg_retail_score, 2)), vjust = -0.3) + 
  labs(title = "Average Retail Score by Zone and Price Category", 
       x = "Zone and Price Category", y = "Average Retail Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# Calculate average Retail Score by price category
price_avg_scores <- data_long %>%
  group_by(pricerange) %>%
  summarise(avg_retail_score = mean(Retail_Score)) %>%
  arrange(desc(avg_retail_score))

# Print the average Retail Score for each price category
print(price_avg_scores)

# Bar plot showing the average Retail Score by price category
ggplot(price_avg_scores, aes(x = reorder(pricerange, avg_retail_score), y = avg_retail_score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(avg_retail_score, 2)), vjust = -0.3) +  
  labs(title = "Average Retail Score by Price Category", 
       x = "Price Category", y = "Average Retail Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Top 10 stores based on Retail Score
top_10_stores <- data_long %>%
  arrange(desc(Retail_Score)) %>%
  head(10)

# Print the top 10 stores' store number, Retail Score, city, and price category
print(top_10_stores)

write.csv(data, "data_with_retail_score_c.csv", row.names = FALSE)

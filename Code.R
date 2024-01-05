# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(randomForest)
require(rms)
library(MASS)
library(klaR)
library(car)
library(ggfortify)
library(scales)
library(cluster)
library(clustMixType)
library(reshape2)
library(knitr)
library(kableExtra)
library(stargazer)

# Load the data
auto_data <- read_csv("/Users/rohankumar/Desktop/McGill/MGSC-661 Data Analytics & AI for business/Final Exam/Automobile_data.csv")

# Histogram of symboling with specific colors
ggplot(auto_data, aes(x=factor(symboling), fill=factor(symboling))) + 
  geom_bar() + 
  scale_fill_manual(values=c("-2"="#7B68EE", "-1"="#6A5ACD", "0"="#4682B4", 
                             "1"="#2E8B57", "2"="#9ACD32", "3"="#ADFF2F")) +
  xlab("Symboling Rating") + 
  ylab("Count") + 
  ggtitle("Distribution of Symboling Ratings") + 
  theme_minimal() + 
  theme(legend.position="none") 

# Boxplot of engine size by symboling with specific colors
ggplot(auto_data, aes(x=factor(symboling), y=`engine-size`, fill=factor(symboling))) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("-2"="#7B68EE", "-1"="#6A5ACD", "0"="#4682B4", 
                             "1"="#2E8B57", "2"="#9ACD32", "3"="#ADFF2F")) +
  xlab("Symboling Rating") + 
  ylab("Engine Size") + 
  ggtitle("Engine Size by Symboling Rating") + 
  theme_minimal() +
  theme(legend.position="none")

# Calculate average symboling score by car make
average_symboling_by_make <- auto_data %>%
  group_by(make) %>%
  dplyr::summarize(average_symboling = mean(symboling, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(average_symboling))

# Create a bar chart with car make on y-axis and average symboling score on the x-axis
ggplot(average_symboling_by_make, aes(x = make, y = average_symboling, fill = make)) +
  geom_col() +
  geom_text(aes(label = round(average_symboling, 2)), position = position_stack(vjust = 0.5), size = 3) + # Adjust position
  coord_flip() +  # Flip the coordinates to put car make on y-axis
  xlab("Car Make") +
  ylab("Average Symboling Score") +
  ggtitle("Bar Chart of Average Symboling Score by Car Make") +
  theme_minimal() +
  theme(legend.position = "none")


# Correlation heatmap
# Filter numeric columns for the correlation matrix
numeric_data <- auto_data[sapply(auto_data, is.numeric)]
cor_matrix <- cor(numeric_data, use="complete.obs")
melted_cor_matrix <- melt(cor_matrix)

ggplot(data = melted_cor_matrix, aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) + 
  ggtitle("Correlation Heatmap")


# Replace hyphens "-" with underscores "_" in column names
colnames(auto_data) <- gsub("-", "_", colnames(auto_data))

# Convert '?' to NA for relevant columns
auto_data <- auto_data %>%
  mutate(across(c(bore, stroke, horsepower), ~na_if(.x, "?")))

# Convert bore, stroke, and horsepower to numeric
auto_data$bore <- as.numeric(auto_data$bore)
auto_data$stroke <- as.numeric(auto_data$stroke)
auto_data$horsepower <- as.numeric(auto_data$horsepower)

# Build linear model for 'bore', 'stroke', and 'horsepower' using 'engine-size'
lm_bore <- lm(bore ~ `engine_size`, data = auto_data, na.action = na.exclude)
lm_stroke <- lm(stroke ~ `engine_size`, data = auto_data, na.action = na.exclude)
lm_horsepower <- lm(horsepower ~ `engine_size`, data = auto_data, na.action = na.exclude)

# Predict and impute missing values
auto_data$bore[is.na(auto_data$bore)] <- predict(lm_bore, newdata = auto_data[is.na(auto_data$bore), ])
auto_data$stroke[is.na(auto_data$stroke)] <- predict(lm_stroke, newdata = auto_data[is.na(auto_data$stroke), ])
auto_data$horsepower[is.na(auto_data$horsepower)] <- predict(lm_horsepower, newdata = auto_data[is.na(auto_data$horsepower), ])

# Plot the distribution of engine-type
ggplot(auto_data, aes(x = `engine_type`)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Engine Types",
       x = "Engine Type",
       y = "Count")

# Convert peak-rpm to numeric (if it's not already)
auto_data$`peak_rpm` <- as.numeric(na_if(auto_data$`peak_rpm`, "?"))

# Manually compute and impute the mean peak-rpm for each engine-type
unique_engine_types <- unique(auto_data$engine_type)

for(engine_type in unique_engine_types) {
  # Calculate mean peak-rpm for this engine type
  mean_peak_rpm <- mean(auto_data$peak_rpm[auto_data$engine_type == engine_type], na.rm = TRUE)
  
  # Impute missing peak-rpm values for this engine type
  auto_data$peak_rpm[is.na(auto_data$peak_rpm) & auto_data$engine_type == engine_type] <- mean_peak_rpm
}

# Convert '?' to NA in num-of-doors
auto_data$`num_of_doors` <- na_if(auto_data$`num_of_doors`, "?")

# Function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(na.omit(v), uniqv)))]
}

# Define the function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Iterate over each body-style and impute missing num-of-doors values
unique_body_styles <- unique(auto_data$body_style)

for(body_style in unique_body_styles) {
  # Calculate mode of num-of-doors for this body style
  mode_num_doors <- get_mode(auto_data$num_of_doors[auto_data$body_style == body_style])
  
  # Impute missing num-of-doors values for this body style
  auto_data$num_of_doors[is.na(auto_data$num_of_doors) & auto_data$body_style == body_style] <- mode_num_doors
}

# Convert 'normalized-losses' to numeric, replacing '?' with NA
auto_data$`normalized_losses` <- as.numeric(na_if(auto_data$`normalized_losses`, "?"))

# Plot histogram of 'normalized-losses'
ggplot(auto_data, aes(x = `normalized_losses`)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Normalized Losses",
       x = "Normalized Losses",
       y = "Count")

# Calculate the mean of 'normalized-losses', excluding NA values
mean_normalized_losses <- mean(auto_data$`normalized_losses`, na.rm = TRUE)

# Replace NA values in 'normalized-losses' with the mean
auto_data$`normalized_losses`[is.na(auto_data$`normalized_losses`)] <- mean_normalized_losses

# Count NA and "?" in each column
na_count <- sapply(auto_data, function(x) sum(is.na(x)))
question_mark_count <- sapply(auto_data, function(x) sum(x == "?"))

# Replace '?' with NA in 'price' and then convert 'price' to numeric
auto_data$price <- ifelse(auto_data$price == "?", NA, auto_data$price)
auto_data$price <- as.numeric(auto_data$price)

# Iterate over each make and impute missing price values
unique_makes <- unique(auto_data$make)

for(make in unique_makes) {
  # Calculate mean price for this make
  mean_price <- mean(auto_data$price[auto_data$make == make], na.rm = TRUE)
  
  # Impute missing price values for this make
  auto_data$price[is.na(auto_data$price) & auto_data$make == make] <- mean_price
}

# Summarize the count of NA values in each column and convert to a more readable format
na_count_readable <- auto_data %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "NA_Count")

# Print all rows of the NA count summary
print(na_count_readable, n = 26)

# Check the structure of the dataset to see data types of each column
str(auto_data)

# Plot histogram of 'normalized-losses'
ggplot(auto_data, aes(x = `engine_size`)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Normalized Losses",
       x = "Engine Size",
       y = "Count")

# Create a named vector to map text to numbers
cylinder_map <- setNames(c(4, 6, 5, 3, 12, 2, 8), c("four", "six", "five", "three", "twelve", "two", "eight"))

# Replace text with corresponding numeric value
auto_data$`num_of_cylinders` <- cylinder_map[auto_data$`num_of_cylinders`]

# Create a named vector to map text to numbers
num_of_doors_map <- setNames(c(4, 2), c("four", "two"))

# Replace text with corresponding numeric value
auto_data$`num_of_doors` <- num_of_doors_map[auto_data$`num_of_doors`]


# Convert categorical variables to dummy variables
auto_data_processed <- auto_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric) %>%
  na.omit()  # Remove any rows with NA values

# Select only numerical columns from the dataset
numerical_data <- auto_data %>% 
  select_if(is.numeric)

# Fit a linear model using all numerical predictors
# Replace 'symboling' with your actual response variable if it's different
lm_model <- lm(symboling ~ ., data = numerical_data)

# Calculate VIF
vif_values <- vif(lm_model)

# Print VIF values
print(vif_values)

#Fit a Random Forest Model for feature importance
rf_model = randomForest(symboling ~ 
                          normalized_losses+make+fuel_type+aspiration+num_of_doors+body_style+
                          drive_wheels+engine_location+wheel_base+width+height+num_of_cylinders+
                          engine_size+fuel_system+bore+stroke+compression_ratio+horsepower+
                          peak_rpm+city_mpg+price, 
                          data = auto_data_processed, importance = TRUE)

# Plot variable importance
importance(rf_model)
varImpPlot(rf_model)

#Making a PCA plot for the dataset
pca=prcomp(auto_data_processed, scale=TRUE)
pca
autoplot(pca, data = auto_data_processed, loadings = TRUE, loadings.label = TRUE )

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
index <- createDataPartition(auto_data$symboling, p = 0.7, list = FALSE)
train_data <- auto_data_processed[index, ]
test_data <- auto_data_processed[-index, ]

# Convert symboling in test_data to factor (if it's not already)
train_data$symboling <- as.factor(train_data$symboling)

# Convert symboling in test_data to factor (if it's not already)
test_data$symboling <- as.factor(test_data$symboling)

# Fit LDA model
lda_model <- lda(symboling ~ normalized_losses + make + num_of_doors + body_style +
                   drive_wheels + wheel_base + width + height + fuel_type + engine_location +
                   engine_size + fuel_system + bore + stroke + num_of_cylinders +
                   compression_ratio + horsepower + peak_rpm + city_mpg + price,
                 data = train_data)

# Make predictions for LDA
predictions_lda <- predict(lda_model, newdata = test_data)
conf_matrix_lda <- confusionMatrix(predictions_lda$class, test_data$symboling)

# Print Confusion Matrices
print(conf_matrix_lda)

dummy_data <- model.matrix(~ . - 1, data = auto_data)

# Convert the model matrix back to a dataframe
dummy_data_df <- as.data.frame(dummy_data)

# Apply Min-Max scaling to the dataset with dummy variables
scaled_data <- as.data.frame(lapply(dummy_data_df, rescale))

# Determine the optimal number of clusters for K-means 
wss <- sapply(1:10, function(k){kmeans(scaled_data, k, nstart = 10)$tot.withinss})

# Enhanced Elbow plot
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares",
     main = "Elbow Method for Determining Optimal Clusters",
     col = "blue", lwd = 2, cex.lab = 1.2, cex.axis = 1.1, cex.main = 1.5)

# Adding a vertical line for the optimal number of clusters
# Let's assume the optimal number of clusters is 3
optimal_clusters <- 7
abline(v = optimal_clusters, col = "red", lwd = 2, lty = 2)

# Adding a text annotation for the optimal number of clusters
text(optimal_clusters, wss[optimal_clusters], labels = paste("Optimal\nclusters:", optimal_clusters), 
     pos = 4, col = "red", cex = 1.2)

# Perform K-means clustering 
set.seed(123) # For reproducibility
k <- 5 
kmeans_result <- kmeans(scaled_data, centers = k)

# Perform Hierarchical clustering
hc_result <- hclust(dist(scaled_data), method = "ward.D2")

# Plot the dendrogram for hierarchical clustering
plot(hc_result, label = FALSE, hang = -1)

# Cut the dendrogram to form clusters
hc_clusters <- cutree(hc_result, k)

#Printing the ceteriods
kmeans_result <- kmeans(scaled_data, centers = 7)

# Get the centroids of the clusters
centroids <- kmeans_result$centers

# Print the centroids
print(centroids)



# Create a data frame for the data description
data_description <- data.frame(
  Column = c('symboling', 'normalized_losses', 'make', 'fuel_type',
             'aspiration', 'num_of_doors', 'body_style', 'drive_wheels',
             'engine_location', 'wheel_base', 'length', 'width',
             'height', 'curb_weight', 'engine_type', 'num_of_cylinders',
             'engine_size', 'fuel_system', 'bore', 'stroke',
             'compression_ratio', 'horsepower', 'peak_rpm', 'city_mpg',
             'highway_mpg', 'price'),
  Description = c('Insurance risk rating of the car: +3 indicates high risk, -3 indicates low risk.',
                  'Average loss payment per insured vehicle year (relative to other cars).',
                  'Manufacturer brand of the car.',
                  'Type of fuel the car uses (e.g., gas or diesel).',
                  'Type of engine aspiration (standard or turbocharged).',
                  'Number of doors on the car.',
                  'Design and shape of the car (e.g., convertible, hatchback).',
                  'Type of drivetrain system (e.g., front-wheel drive, rear-wheel drive).',
                  'Location of the car engine (front or rear).',
                  'Distance between the front and rear wheels.',
                  'Length of the car.',
                  'Width of the car.',
                  'Height of the car from the ground.',
                  'Weight of the car without passengers or cargo.',
                  'Type of engine (e.g., overhead camshaft, rotary).',
                  'Number of cylinders in the car engine.',
                  'Volume inside the engine cylinders (measured in cubic centimeters).',
                  'System used to store and deliver fuel to the engine.',
                  'Diameter of each cylinder in the engine.',
                  'Distance the piston travels within the cylinder.',
                  'Ratio of the volume of the combustion chamber from its largest capacity to its smallest capacity.',
                  'Unit of measurement for engine power.',
                  'The maximum revolutions per minute of the engine.',
                  'Miles per gallon the car can travel on city roads.',
                  'Miles per gallon the car can travel on the highway.',
                  'Listed price of the car.')
)

# Create an HTML table using stargazer
stargazer(data_description, type = "html", summary = FALSE)

cluster_data <- data.frame(
  symboling = c(1, 2, 3, 4, 5, 6, 7),
  normalized_losses = c(0.8312500, 0.4000000, 0.5333333, 0.4346154, 0.4266667, 0.7000000, 0.4871795),
  makealfa.romero = c(0.4232657, 0.4283674, 0.2068063, 0.2693315, 0.1895288, 0.3046168, 0.2612431),
  makeaudi = c(0.09375, 0, 0, 0.1153846, 0, 0, 0),
  makebmw = c(0.03125, 0, 0, 0.1538462, 0, 0, 0),
  makechevrolet = c(0, 0, 0, 0, 0, 0.04545455, 0.02564103),
  makedodge = c(0, 0, 0, 0.01923077, 0, 0.11363636, 0.07692308),
  makehonda = c(0, 0, 1, 0.01923077, 0, 0, 0),
  makeisuzu = c(0.03125, 0, 0, 0, 0, 0, 0.07692308),
  makejaguar = c(0.03125, 0, 0, 0.03846154, 0, 0, 0),
  makemazda = c(0.125, 0, 0, 0.01923077, 0.13333333, 0.13636364, 0.1025641),
  makemercedes.benz = c(0.0625, 0, 0, 0.03846154, 0.26666667, 0, 0),
  makemercury = c(0.03125, 0, 0, 0, 0, 0, 0),
  makemitsubishi = c(0, 0, 0, 0, 0, 0.2045455, 0.1025641),
  makenissan = c(0.09375, 0, 0, 0.05769231, 0.06666667, 0.04545455, 0.23076923),
  makepeugot = c(0, 1, 0, 0, 0, 0, 0),
  makeplymouth = c(0.03125, 0, 0, 0, 0, 0.06818182, 0.07692308),
  makeporsche = c(0.15625, 0, 0, 0, 0, 0, 0),
  makerenault = c(0, 0, 0, 0, 0, 0.02272727, 0),
  makesaab = c(0, 0, 0, 0.03846154, 0, 0.06818182, 0.12820513),
  makesubaru = c(0, 0, 0, 0.01923077, 0, 0.06818182, 0.17948718),
  maketoyota = c(0.3125, 0, 0, 0.07692308, 0.2, 0.18181818, 0.17948718),
  makevolkswagen = c(0, 0, 0, 0.11538462, 0.26666667, 0.04545455, 0),
  makevolvo = c(0, 0, 0, 0.19230769, 0.06666667, 0, 0),
  fuel_typegas = c(1, 0.5454545, 1, 1, 0, 1, 1),
  aspirationturbo = c(0.125, 0.54545455, 0, 0.17307692, 0.53333333, 0.20454545, 0.02564103),
  num_of_doors = c(0, 1, 0.33333333, 0.9038462, 0.8, 0.2045455, 0.8461538),
  body_stylehardtop = c(0.1875, 0, 0, 0, 0.06666667, 0, 0.02564103),
  body_stylehatchback = c(0.59375, 0, 0.58333333, 0, 0.06666667, 0.97727273, 0),
  body_stylesedan = c(0.0625, 0.6363636, 0.3333333, 0.8076923, 0.8, 0, 0.7435897),
  body_stylewagon = c(0, 0.36363636, 0.08333333, 0.19230769, 0.06666667, 0, 0.23076923),
  drive_wheelsfwd = c(0, 0, 1, 0.4615385, 0.6, 0.9545455, 0.8461538),
  drive_wheelsrwd = c(0.96875, 1, 0, 0.48076923, 0.4, 0.02272727, 0.05128205),
  engine_locationrear = c(0.09375, 0, 0, 0, 0, 0, 0),
  wheel_base = c(0.2868076, 0.6880466, 0.2201166, 0.4736488, 0.4600583, 0.2564272, 0.2680721),
  length = c(0.5018657, 0.7468114, 0.2753731, 0.6352468, 0.5629851, 0.3628562, 0.4049369),
  width = c(0.5304688, 0.6742424, 0.3347222, 0.5682692, 0.5672222, 0.3657197, 0.3395299),
  height = c(0.28125, 0.7818182, 0.4472222, 0.6065705, 0.6233333, 0.4007576, 0.5057692),
  curb_weight = c(0.5232981, 0.6722265, 0.2242371, 0.5328818, 0.4923713, 0.2826098, 0.2700165),
  engine_typedohcv = c(0.03125, 0, 0, 0, 0, 0, 0),
  engine_typel = c(0, 1, 0, 0, 0, 0.02272727, 0),
  engine_typeohc = c(0.34375, 0, 1, 0.7115385, 1, 0.8863636, 0.8717949),
  engine_typeohcf = c(0.09375, 0, 0, 0.07692308, 0, 0.06818182, 0.12820513),
  engine_typeohcv = c(0.21875, 0, 0, 0.11538462, 0, 0, 0),
  engine_typerotor = c(0.125, 0, 0, 0, 0, 0, 0),
  num_of_cylinders = c(0.296875, 0.2, 0.2, 0.2807692, 0.24, 0.1977273, 0.2),
  engine_size = c(0.3600236, 0.2823328, 0.141195, 0.3265602, 0.2613836, 0.1748714, 0.1555878),
  fuel_system2bbl = c(0, 0, 0.08333333, 0, 0, 0.63636364, 0.94871795),
  fuel_system4bbl = c(0.09375, 0, 0, 0, 0, 0, 0),
  fuel_systemidi = c(0, 0.4545455, 0, 0, 1, 0, 0),
  fuel_systemmfi = c(0, 0, 0, 0, 0, 0.02272727, 0),
  fuel_systemmpfi = c(0.84375, 0.5454545, 0, 1, 0, 0.2045455, 0),
  fuel_systemspdi = c(0.03125, 0, 0, 0, 0, 0.13636364, 0.05128205),
  fuel_systemspfi = c(0.03125, 0, 0, 0, 0, 0, 0),
  bore = c(0.6617464, 0.7448052, 0.3363095, 0.6549451, 0.5185714, 0.4758117, 0.4831502),
  stroke = c(0.5456867, 0.5190476, 0.6583333, 0.5668498, 0.6685714, 0.5590909, 0.5224664),
  compression_ratio = c(0.1300781, 0.4375, 0.1395833, 0.1081731, 0.95875, 0.1064205, 0.1262821),
  horsepower = c(0.4248698, 0.2159091, 0.1270833, 0.3222126, 0.1372222, 0.1633725, 0.1176282),
  peak_rpm = c(0.4834184, 0.2115028, 0.6530612, 0.4237355, 0.1551020, 0.3871420, 0.3741497),
  city_mpg = c(0.1822917, 0.2626263, 0.4976852, 0.2190171, 0.5129630, 0.4141414, 0.4508547),
  highway_mpg = c(0.2458882, 0.2799043, 0.5285088, 0.2692308, 0.5368421, 0.4665072, 0.5053981),
  price = c(0.34259616, 0.25746216, 0.06628271, 0.31535129, 0.26646807, 0.08829683, 0.06794280)
)

# Create an HTML table using stargazer
stargazer(cluster_data, type = "html", summary = FALSE)

html_table <- kable(cluster_data, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
writeLines(as.character(html_table), "output_table.html")

# Create a data frame with the provided values
data3 <- data.frame(
  Variable_Name = c("normalized_losses", "num_of_doors", "wheel_base", "length", "width", "height", "curb_weight",
                    "num_of_cylinders", "engine_size", "bore", "stroke", "compression_ratio", "horsepower",
                    "peak_rpm", "city_mpg", "highway_mpg", "price"),
  VIF_Score = c(1.482402, 1.897721, 7.672609, 10.162295, 6.168336, 2.676828, 17.313136, 10.547041, 27.910229,
                4.007658, 1.984029, 2.334727, 9.819638, 2.254533, 29.109055, 25.713555, 7.159832)
)


# Create an HTML table using stargazer
stargazer(data3, type = "html", summary = FALSE)

# Create a data frame with the provided values including numbers
data4 <- data.frame(
  Variable_Name = c("normalized_losses", "make", "fuel_type", "aspiration", "num_of_doors", "body_style",
                    "drive_wheels", "engine_location", "wheel_base", "width", "height", "num_of_cylinders",
                    "engine_size", "fuel_system", "bore", "stroke", "compression_ratio", "horsepower",
                    "peak_rpm", "city_mpg", "price"),
  IncMSE = c(21.5406001, 19.7091675, 0.7182711, 0.4111366, 23.5129813, 14.2941306, 7.8402811,
             1.5409820, 22.9677374, 14.9524354, 17.1682291, 3.1674557, 13.4687155, 6.4779796,
             15.7353656, 11.5714425, 7.4199521, 10.7438797, 11.6429150, 13.5527293, 13.9910526),
  IncNodePurity = c(30.40162496, 17.67151857, 0.17710919, 0.52606169, 63.32365192, 27.90097193,
                    3.88144350, 0.06269401, 44.62585360, 12.73350789, 30.99275939, 1.23947603,
                    9.19027823, 2.48778020, 11.33593656, 6.49953974, 5.33168896, 7.19175170,
                    6.80532770, 14.45488013, 12.45756140)
)

# Create an HTML table for the data using stargazer
stargazer(data4, type = "html", summary = FALSE)


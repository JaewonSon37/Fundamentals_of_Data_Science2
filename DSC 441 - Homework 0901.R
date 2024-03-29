# Data Gathering
## Load the dataset
exoplanets <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Fundamentals of Data Science\\DSC 441 - Week 9\\Data File\\NASA_Exoplanets.csv")

## Display the first few rows
head(exoplanets)

## View data types
str(exoplanets)


# Data Exploration
## Summary of the dataset
summary(exoplanets)

## EDA - Box plot
### Load the library
library(ggplot2)

### Distribution of stellar magnitude
boxplot(exoplanets$stellar_magnitude, main = "Brightness of the Planet")

### Distribution of eccentricity of orbits
boxplot(exoplanets$eccentricity, main = "Eccentricity of Orbit")

## EDA - Bar Plot
### Distribution of planet types
ggplot(exoplanets, aes(x = planet_type)) +
  geom_bar()

### Distribution of detection methods
ggplot(exoplanets, aes(x = detection_method)) +
  geom_bar()

## EDA - Correlation Plot
### Load the library
library(ggcorrplot)

### Correlation between selected variables
cor <- cor(exoplanets[, c('stellar_magnitude', 'mass_multiplier',
                          'radius_multiplier', 'orbital_radius', 
                          'orbital_period', 'eccentricity')])
ggcorrplot(cor)


# Data Cleaning
## Remove rows with missing values
exoplanets <- na.omit(exoplanets)
sum(is.na(exoplanets))

## Load the library
library(dplyr)

## Filter rows with conditions
clean_exoplanets <- exoplanets %>%
  filter(mass_wrt == "Earth" & radius_wrt == "Earth")

## Remove columns
clean_exoplanets <- subset(clean_exoplanets, select = -c(name, distance, discovery_year, mass_wrt, radius_wrt))

## Change variables type
clean_exoplanets$planet_type <- as.factor(clean_exoplanets$planet_type)
clean_exoplanets$detection_method <- as.factor(clean_exoplanets$detection_method)
str(clean_exoplanets)

## Remove outliers
### Function to find outliers
find_outliers <- function(data, variable, threshold = 3)
  {
  mean <- mean(data[[variable]])
  std <- sd(data[[variable]])

  upper_limit <- mean + threshold * std
  lower_limit <- mean - threshold * std
  
  outliers <- data[[variable]] > upper_limit | data[[variable]] < lower_limit
  
  return(outliers)
}

### Removing outliers from each variable 
clean_exoplanets <- clean_exoplanets[!find_outliers(clean_exoplanets, "stellar_magnitude"), ]
clean_exoplanets <- clean_exoplanets[!find_outliers(clean_exoplanets, "mass_multiplier"), ]
clean_exoplanets <- clean_exoplanets[!find_outliers(clean_exoplanets, "radius_multiplier"), ]
clean_exoplanets <- clean_exoplanets[!find_outliers(clean_exoplanets, "orbital_radius"), ]
clean_exoplanets <- clean_exoplanets[!find_outliers(clean_exoplanets, "orbital_period"), ]
clean_exoplanets <- clean_exoplanets[!find_outliers(clean_exoplanets, "eccentricity"), ]
summary(clean_exoplanets)


# Data Preprocessing
## Create dummy variables
dummy_exoplanets <- as.data.frame(model.matrix( ~ detection_method - 1, data = clean_exoplanets))
final_exoplanets <- cbind(clean_exoplanets, dummy_exoplanets)
final_exoplanets <- subset(final_exoplanets, select = -c(detection_method))
head(final_exoplanets)


# Clustering
## Set up the dataset
### Removing labels and categorical variable
clustering_exoplantes <- subset(clean_exoplanets, select = -c(planet_type, detection_method))

## Determine the optimal number of clusters
### Load the library
library(factoextra)

### Finding silhoutte scores
fviz_nbclust(clustering_exoplantes, kmeans, method = "silhouette")

## Perform K-means clustering
set.seed(37)
fit <- kmeans(clustering_exoplantes, centers = 3, nstart = 20)
fviz_cluster(fit, data = clustering_exoplantes)

## Perform PCA
pca <- prcomp(clustering_exoplantes, scale. = TRUE)
rotated_data <- as.data.frame(pca$x)

## Add column
rotated_data$Type <- clean_exoplanets$planet_type
rotated_data$Clusters <- as.factor(fit$cluster)

## Visualize the clustered data
ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Clusters)) +
  geom_point(alpha = 0.3)

ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Type)) +
  geom_point(alpha = 0.3)

# Classification
## Prepare the dataset by binning
### Function to bin planet_types
bin <- function(planet_type) {
  if (planet_type == "Neptune-like") {
    return("Neptune-like")
  } else {
    return("Earth-like")
  }
}

### Set up the dataset
classification_exoplanet <- clean_exoplanets
classification_exoplanet$group <- apply(classification_exoplanet, 1, 
                                        function(row) bin(row["planet_type"]))
classification_exoplanet$group <- as.factor(classification_exoplanet$group)
classification_exoplanet <- subset(classification_exoplanet, select = -c(planet_type))

## Partition the data
### Load the library
library(caret)
set.seed(37)
index = createDataPartition(y = classification_exoplanet$group, p = 0.7, list = FALSE)
train_set = classification_exoplanet[index,]
test_set = classification_exoplanet[-index,]

## Decision Tree
### Set up training control for cross-validation
train_control <- trainControl(method = "cv", number = 10)

### Train the decision tree model
decision_tree <- train(group ~ ., data = train_set,
                       method = "rpart", trControl = train_control)
decision_tree

## Support Vector Machine
### Set up training control for cross-validation
train_control <- trainControl(method = "cv", number = 10)

### Train the support vector machine model
svm <- train(group ~ ., data = train_set,
             method = "svmLinear", trControl = train_control)
svm


# Evaluation
## Produce the 2x2 confusion matrix
pred_decision_tree <- predict(decision_tree, test_set)
confusionMatrix(test_set$group, pred_decision_tree)

## Calculate the precision and recall
true_positive <- 428
false_positive <- 0
true_negative <- 37
false_negative <- 5

precision <- true_positive / (true_positive + false_positive)
print(precision)

recall <- true_positive / (true_positive + false_negative)
print(recall)

## Produce the ROC plot
### Load the library
library(pROC)

### Visualize the ROC curve
pred_prob3 <- predict(decision_tree, test_set, type = "prob")
roc_curve3 <- roc((test_set$group), pred_prob3[ , 1])
plot(roc_curve3, print.auc = TRUE)
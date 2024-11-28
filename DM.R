library(dplyr)

bankingDT_Original <- read.csv("Bangking_Dataset_train.csv")
marketingDT_Original <- read.csv("Marketing_Target_train.csv", sep = ";")
bankingDT_Processed <- bankingDT_Original
marketingDT_Processed <- marketingDT_Original

# Preparing the same columns that exist on both datasets
same_columns <- intersect(names(bankingDT_Original), names(marketingDT_Original))

#--------------- PREPARATION BEFORE INTEGRATION -----------------------

# Changing the values on BankingDT so the education matches the one in the other dataset

bankingDT_Processed <- bankingDT_Processed %>%
  mutate(education = recode(education,
                            basic.4y = "primary",
                            basic.6y = "primary",
                            basic.9y = "secondary",
                            high.school = "secondary",
                            professional.course = "tertiary",
                            university.degree = "tertiary",
                            illiterate = "unknown",
                            unknown = "unknown")
        )

# Changing the value -1 of pdays in marketingDT_Processed 
# because marketingDT_Processed value -1 means not previously contacted
# however in bankingDT_Processed previously contacted is assigned as value 999
marketingDT_Processed <- marketingDT_Processed %>%
  mutate(pdays = ifelse(pdays == -1, 999, pdays))

# Changing nonexistent into unknown so the data integration can go smooth
bankingDT_Processed <- bankingDT_Processed %>%
  mutate(poutcome = recode(poutcome,
                            nonexistent="unknown")
  )

# Subsetting the dataset so it only have the same columns
bankingDT_Processed <- bankingDT_Processed[, same_columns] 
marketingDT_Processed <- marketingDT_Processed[, same_columns] 

# Combining the 2 datasets into a single dataset
dataset <- rbind(bankingDT_Processed,marketingDT_Processed)

# Remove features that isn't used
dataset <- subset(dataset, select = -poutcome)
dataset <- subset(dataset, select = -contact)
dataset <- subset(dataset, select = -month)
dataset <- subset(dataset, select = -pdays)
dataset <- subset(dataset, select = -previous)
dataset <- subset(dataset, select = -marital)
dataset <- subset(dataset, select = -education)
dataset <- subset(dataset, select = -default) 

# Check length should be 78161
length(dataset$age)

# Remove duplicated values
dataset <- dataset[!duplicated(dataset), ]

# Check length should be 75571
length(dataset$age)

# Check the remaining column ("age" "job" "housing" "loan" "duration" "campaign" "y")
names(dataset)

# Define the categorical columns where "unknown" values may exist
categorical_columns <- c("job", "housing", "y")

# Check the column that has alot of uknowns
# for (col in categorical_columns) {
#   print(paste("Column:", col))
#   print(table(dataset[[col]]))
# }

# Loop through each categorical column and remove rows with "unknown" values
for (col in categorical_columns) {
  dataset <- dataset[dataset[[col]] != "unknown", ]
}

# Check if "unknown" values have been successfully removed
for (col in categorical_columns) {
  print(unique(dataset[[col]]))
}

# Check length should be 74228
length(dataset$age)

# These packages are for label encoding, install if need
# install.packages("superml")
# install.packages('data.table')
library(data.table)
library(superml)

# --------------ENCODING CATEGORICAL DATA----------------------

# Changing y to 1 or 0 from "yes" and "no"
dataset$y <- ifelse(dataset$y == "yes", 1, 0)

#Target Encoding for the job column
# Calculate the mean of the target variable 'y' for each job category
job_target_means <- dataset %>% group_by(job) %>% summarize(job_mean_target = mean(y, na.rm = TRUE))

# Merge the target means back into the original dataset
dataset <- dataset %>% left_join(job_target_means, by = "job")

# (Optional) Replace the original 'job' column with the encoded values
dataset$job <- dataset$job_mean_target

#Remove the job_mean_target because it is replaced by job
dataset <- subset(dataset, select = -job_mean_target)

# Make sure the target encoding works well
unique(dataset$job)

# Label encoding for "housing" column
encoder = LabelEncoder$new()
encoder$fit(dataset$housing)
dataset$housing <- encoder$fit_transform(dataset$housing)
unique(dataset$housing)

#Label encoding for "loan" column
encoder = LabelEncoder$new()
encoder$fit(dataset$loan)
dataset$loan <- encoder$fit_transform(dataset$loan)
unique(dataset$loan)

names(dataset)

# Check length should be 74228
length(dataset$age)

# Min Max scaler for the numerical value
for (col in c("duration", "campaign", "age")) {
  dataset[[col]] <- (dataset[[col]] - min(dataset[[col]])) / (max(dataset[[col]]) - min(dataset[[col]]))
}

# Removing y as we dont need it anymore
dataset <- subset(dataset, select = -y)

write.csv(dataset, file = "Combine.csv", row.names = FALSE)

# -----------------------------Kebawah belum diganti--------------------------------

# install.packages("Rtsne")
set.seed(4920)
wcss <- vector()
for (i in 1:15) {
  kmeans_temp <- kmeans(dataset, centers = i, nstart = 25)
  wcss[i] <- kmeans_temp$tot.withinss
}

# Plot WCSS to find the elbow point
plot(1:15, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares")

kmeans_result <- kmeans(dataset, centers = 4, nstart=25)

library(Rtsne)
dataset <- dataset[!duplicated(dataset),]
print(length(dataset$age))
tsne_result <-  (dataset, dims = 2, perplexity = 30)
tsne_data <- as.data.frame(tsne_result$Y)
colnames(tsne_data) <- c("Dim1", "Dim2")
tsne_data$Cluster <- as.factor(kmeans_result$cluster)

# install.packages("ggplot2")
library(ggplot2)
ggplot(tsne_data, aes(x = Dim1, y = Dim2, color = Cluster)) +geom_point(size = 3) +labs(title = "K-means Clustering (t-SNE)", x = "Dimension 1", y = "Dimension 2") +theme_minimal()
dataset.with.clusters <- dataset
dataset.with.clusters$clusters <- as.factor(kmeans_result$cluster)
# ggplot(dataset.with.clusters, aes(x = Petal.Length, y = Petal.Width, color = Cluster)) +
#   geom_point(size = 3) +
#   labs(title = "K-means Clustering on Iris Data")

#Install this package for DBSCAN clustering, just uncomment
#install.packages("fpc")
write.csv(dataset, file = "cleaned_dataset.csv",sep = ";", row.names = FALSE)
library(fpc)

# install.packages("dbscan")
# library(dbscan)
# DBSCAN <- dbscan(dataset, eps=0.45, MinPts = 5)

#Install this package for GMM (Gaussian Mixture Model) clustering, just uncomment
# install.packages("mclust")

# library(mclust)

#G = number of clusters
# gmm_model <- Mclust(dataset, G = 3 )

#summary(gmm_model)
#get the cluster assignments
#cluster_assignments <- predict(gmm_model)$classification
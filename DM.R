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
# Separate the dataset into two groups: "yes" (y == 1) and "no" (y == 0)
dataset_yes <- dataset[dataset$y == 1, ]
dataset_no <- dataset[dataset$y == 0, ]


# Removing y as we dont need it anymore
# dataset <- subset(dataset, select = -y)
dataset_yes <- subset(dataset_yes, select = -y)
dataset_no <- subset(dataset_no, select = -y)

# dataset <- dataset[!duplicated(dataset),]
# dataset <- dataset[!duplicated(dataset),]
dataset_yes <- dataset_yes[!duplicated(dataset_yes),]
dataset_no <- dataset_no[!duplicated(dataset_no),]

# Should be 73945
# length(dataset$age)
length(dataset_yes$age)
length(dataset_no$age)

# write.csv(dataset, file = "Combine.csv", row.names = FALSE)
write.csv(dataset_yes, file = "Combine_yes.csv", row.names = FALSE)
write.csv(dataset_no, file = "Combine_no.csv", row.names = FALSE)

# -----------------------------Kebawah belum diganti--------------------------------

# Reading the Combine CSV for better consistency
# dataset = read.csv("Combine.csv")
dataset_yes = read.csv("Combine_yes.csv")
dataset_no = read.csv("Combine_no.csv")

# # Elbow method
# set.seed(4920)
# wcss <- vector()
# for (i in 1:10) {
#   kmeans_temp <- kmeans(dataset, centers = i, nstart = 25)
#   wcss[i] <- kmeans_temp$tot.withinss
# }

# # Plot WCSS to find the elbow point
# plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares")

# Cluster the data using kMeans
# kmeans_result <- kmeans(dataset, centers = 4, nstart=25)

# cluster yes no
#* Clustering for "yes" data
kmeans_yes <- kmeans(dataset_yes, centers = 4, nstart = 25)
dataset_yes$cluster <- kmeans_yes$cluster

#* Clustering for "no" data
kmeans_no <- kmeans(dataset_no, centers = 4, nstart = 25)
dataset_no$cluster <- kmeans_no$cluster

# pca_result <- prcomp(dataset, center = TRUE, scale. = TRUE)
pca_resultyes <- prcomp(dataset_yes, center = TRUE, scale. = TRUE)
pca_resultno <- prcomp(dataset_no, center = TRUE, scale. = TRUE)


#PCA


# Correlation Test
# install.packages("corrplot")
# library(corrplot)
# correlate <- cbind(dataset, pca_result$x)
# cor_matrix <- cor(correlate)
# corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

#install.packages("corrplot")
library(corrplot)
#corr for yes data
correlate_yes <- cbind(dataset_yes, pca_resultyes$x)
cor_matrix_yes <- cor(correlate_yes)
corrplot(cor_matrix_yes, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

#corr for no data
correlate_no <- cbind(dataset_no, pca_resultno$x)
cor_matrix_no <- cor(correlate_no)
corrplot(cor_matrix_no, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# # Add PCA scores and the cluster result to a new DF
# dataset_with_pca <- cbind(cluster = kmeans_result$cluster, pca_result$x)
# dataset_with_pca <- as.data.frame(dataset_with_pca)

# # Remove unused PC
# dataset_with_pca = subset(dataset_with_pca, select = -PC4)
# dataset_with_pca = subset(dataset_with_pca, select = -PC5)
# dataset_with_pca = subset(dataset_with_pca, select = -PC6)

# Combine only PC1, PC2, and PC3 with the cluster results for yes an no data (shortcut)
datasetyes_with_pca <- cbind(cluster = kmeans_yes$cluster, pca_resultyes$x[, 1:3])
datasetyes_with_pca <- as.data.frame(datasetyes_with_pca)
datasetno_with_pca <- cbind(cluster = kmeans_no$cluster, pca_resultno$x[, 1:3])
datasetno_with_pca <- as.data.frame(datasetno_with_pca)


#install.packages("plotly")
# Load the plotly library
library(plotly)

# 3D Scatter plot
#  p <- plot_ly(dataset_with_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(cluster), colors = "Set1") %>%
#   add_markers() %>%
#   layout(scene = list(
#     xaxis = list(title = 'PC1'),
#     yaxis = list(title = 'PC2'),
#     zaxis = list(title = 'PC3')),
#     title = "3D PCA Plot by Cluster"
#   )

# 3D plot for "yes" data using PCA components
p_yes <- plot_ly(datasetyes_with_pca , x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(cluster), colors = "Set1") %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')),
    title = "3D PCA Plot for 'Yes' Data"
  )

# Show the plot
p_yes

# 3D plot for "no" data using PCA components
p_no <- plot_ly(datasetno_with_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(cluster), colors = "Set1") %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')),
    title = "3D PCA Plot for 'No' Data"
  )

p_no

# Save the plot into a html file
# htmlwidgets::saveWidget(p, "3d_pca_plot.html")
htmlwidgets::saveWidget(p_yes, "3d_pca_plot_yes.html")
htmlwidgets::saveWidget(p_no, "3d_pca_plot_no.html")

# # DBSCAN
# install.packages("dbscan")
library(dbscan)
dataset_yes = read.csv("Combine_yes.csv")
dataset_no = read.csv("Combine_no.csv")

dbscan_yes <- dbscan(dataset_yes, eps = 0.5, minPts = 5)
dataset_yes$cluster <- dbscan_yes$cluster
dbscan_no <- dbscan(dataset_no, eps = 0.5, minPts = 5)
dataset_no$cluster <- dbscan_no$cluster

pca_resultyes <- prcomp(dataset_yes, center = TRUE, scale. = TRUE)
pca_resultno <- prcomp(dataset_no, center = TRUE, scale. = TRUE)

library(corrplot)
#corr for yes data
correlate_yes <- cbind(dataset_yes, pca_resultyes$x)
cor_matrix_yes <- cor(correlate_yes)
corrplot(cor_matrix_yes, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

#corr for no data
correlate_no <- cbind(dataset_no, pca_resultno$x)
cor_matrix_no <- cor(correlate_no)
corrplot(cor_matrix_no, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# Combine only PC1, PC2, and PC3 with the cluster results for yes an no data (shortcut)
datasetyes_with_pca <- cbind(cluster = dbscan_yes$cluster, pca_resultyes$x[, 1:3])
datasetyes_with_pca <- as.data.frame(datasetyes_with_pca)
datasetno_with_pca <- cbind(cluster = dbscan_no$cluster, pca_resultno$x[, 1:3])
datasetno_with_pca <- as.data.frame(datasetno_with_pca)

plot_ly(datasetyes_with_pca , x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(cluster), colors = "Set1") %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')),
    title = "3D PCA Plot for 'Yes' Data"
  )

plot_ly(datasetno_with_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(cluster), colors = "Set1") %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')),
    title = "3D PCA Plot for 'No' Data"
  )
# set.seed(4920)
# dbscan_result <- dbscan(dataset, eps = 0.5, minPts = 5)
# dataset <- cbind(dataset, DBSCAN_Cluster = dbscan_result$cluster)
# unique(dataset$DBSCAN_Cluster)
# dataset_with_dbscanpca <- cbind(cluster = dbscan_result$cluster, pca_result$x)
# dataset_with_dbscanpca <- as.data.frame(dataset_with_dbscanpca)

# # Remove unused PC
# dataset_with_dbscanpca = subset(dataset_with_dbscanpca, select = -PC4)
# dataset_with_dbscanpca = subset(dataset_with_dbscanpca, select = -PC5)
# dataset_with_dbscanpca = subset(dataset_with_dbscanpca, select = -PC6)

# plot_ly(dataset_with_dbscanpca, 
#         x = ~PC1, y = ~PC2, z = ~PC3, 
#         color = ~as.factor(cluster), 
#         colors = "Set1") %>%
#   add_markers() %>%
#   layout(scene = list(
#     xaxis = list(title = 'PC1'),
#     yaxis = list(title = 'PC2'),
#     zaxis = list(title = 'PC3')),
#     title = "3D PCA Plot by DBSCAN Cluster"
#   )
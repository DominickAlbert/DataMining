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
dataset_yes <- subset(dataset_yes, select = -y)
dataset_no <- subset(dataset_no, select = -y)

dataset_yes <- dataset_yes[!duplicated(dataset_yes),]
dataset_no <- dataset_no[!duplicated(dataset_no),]

# Should be 8808 and 65420
length(dataset_yes$age)
length(dataset_no$age)

write.csv(dataset_yes, file = "Combine_yes.csv", row.names = FALSE)
write.csv(dataset_no, file = "Combine_no.csv", row.names = FALSE)

# -----------------------------Clustering and PCA--------------------------------

# Reading the Combine CSV for better consistency
dataset_yes <- read.csv("Combine_yes.csv")
dataset_no <- read.csv("Combine_no.csv")

modified_yes <- dataset_yes
modified_no <- dataset_no

# -----------------------------KMEANS--------------------------------

# Clustering for "yes" data
kmeans_yes <- kmeans(dataset_yes, centers = 4, nstart = 25)
modified_yes$cluster <- kmeans_yes$cluster

#* Clustering for "no" data
kmeans_no <- kmeans(dataset_no, centers = 4, nstart = 25)
modified_no$cluster <- kmeans_no$cluster

# -----------------------------Elbow method--------------------------------

# # Elbow method
# set.seed(4920)
# wcss <- vector()
# for (i in 1:10) {
#   kmeans_temp <- kmeans(dataset, centers = i, nstart = 25)
#   wcss[i] <- kmeans_temp$tot.withinss
# }

# # Plot WCSS to find the elbow point
# plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares")

# -----------------------------PCA--------------------------------

pca_resultyes <- prcomp(dataset_yes, center = TRUE, scale. = TRUE)
pca_resultno <- prcomp(dataset_no, center = TRUE, scale. = TRUE)

# -----------------------------Correlation Test--------------------------------

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

# ----------------------------- PCA Part 2 --------------------------------

# Combine only PC1, PC2, and PC3 with the cluster results for yes an no data 
datasetyes_with_pca <- cbind(cluster = kmeans_yes$cluster, pca_resultyes$x[, 1:3])
datasetyes_with_pca <- as.data.frame(datasetyes_with_pca)
datasetno_with_pca <- cbind(cluster = kmeans_no$cluster, pca_resultno$x[, 1:3])
datasetno_with_pca <- as.data.frame(datasetno_with_pca)

# ----------------------------- DBSCAN CLUSTERING ----------------------------

# install.packages("dbscan")

# Load the DBSCAN library
library(dbscan)

# DBSCAN Clustering for "Yes" Data
dbscan_yes <- dbscan(dataset_yes, eps = 0.5, minPts = 5)

modified_yes$dbscan <- dbscan_yes$cluster
datasetyes_with_pca$dbscan <- dbscan_yes$cluster

# DBSCAN Clustering for "No" Data
dbscan_no <- dbscan(dataset_no, eps = 0.5, minPts = 5)

modified_no$dbscan <- dbscan_no$cluster
datasetno_with_pca$dbscan <- dbscan_no$cluster

# ----------------------------- OPTICS CLUSTERING ----------------------------

optics_yes <- optics(dataset_yes, minPts = 5)

# Extract clusters from OPTICS result using the reachability plot (set eps)
optics_clusters <- extractDBSCAN(optics_yes, eps = 0.355)

# Assign the cluster labels to the dataset
datasetyes_with_pca$optic <- optics_clusters$cluster
modified_yes$optic <- optics_clusters$cluster

# Apply OPTICS Clustering for "No" data (using "dbscan" package)
optics_no <- optics(dataset_no, minPts = 5)

# Extract clusters from OPTICS result using the reachability plot (set eps)
optics_clusters_no <- extractDBSCAN(optics_no, eps = 0.355)

# Assign the cluster labels to the "No" data set
datasetno_with_pca$optic <- optics_clusters_no$cluster
modified_no$optic <- optics_clusters_no$cluster

# ----------------------------- Visualization ----------------------------

#install.packages("plotly")
# Load the plotly library
library(plotly)

# 3D plot for "yes" data using PCA components
p_yes <- plot_ly(datasetyes_with_pca , x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(cluster), colors = "Set1") %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')),
    title = "3D Kmeans Plot for 'Yes' Data"
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
    title = "3D Kmeans Plot for 'No' Data"
  )

p_no

# 3D plot for DBSCAN Clustering ("Yes" data)
p_yes_dbscan <- plot_ly(datasetyes_with_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(dbscan), colors = "Set1") %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')),
    title = "3D DBSCAN Plot for 'Yes' Data"
  )

# Show the plot for DBSCAN 'Yes' data
p_yes_dbscan

# 3D plot for DBSCAN Clustering ("No" data)
p_no_dbscan <- plot_ly(datasetno_with_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(dbscan), colors = "Set1") %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')),
    title = "3D DBSCAN Plot for 'No' Data"
  )

# Show the plot for DBSCAN 'No' data
p_no_dbscan

# 3D plot for OPTICS Clustering ("No" data)
p_no_optics <- plot_ly(datasetno_with_pca, 
                       x = ~PC1, 
                       y = ~PC2, 
                       z = ~PC3, 
                       color = ~as.factor(optic), 
                       colors = "Set2",  # Choose a different color set
                       type = "scatter3d", 
                       mode = "markers") %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')
  ),
  title = "3D OPTICS Clustering Plot for 'No' Data")

# Show the plot for OPTICS Clustering 'No' data
p_no_optics

# 3D plot for OPTICS Clustering ("Yes" data)
p_yes_optics <- plot_ly(datasetyes_with_pca, 
                        x = ~PC1, 
                        y = ~PC2, 
                        z = ~PC3, 
                        color = ~as.factor(optic), 
                        colors = "Set1", 
                        type = "scatter3d", 
                        mode = "markers") %>%
  layout(scene = list(
    xaxis = list(title = 'PC1'),
    yaxis = list(title = 'PC2'),
    zaxis = list(title = 'PC3')
  ),
  title = "3D OPTICS Clustering Plot for 'Yes' Data")

# Show the plot for OPTICS Clustering 'Yes' data
p_yes_optics

# ----------------------------- SAMPLING ----------------------------

# Define the sample fraction (e.g., 20% of the data)
sample_fraction <- 0.2

# Perform stratified sampling based on KMeans clusters
sample_no_kmeans <- modified_no %>%
  group_by(cluster) %>%
  sample_frac(sample_fraction)

sample_no_kmeans <- subset(sample_no_kmeans, select = -c(optic, dbscan))

# Perform stratified sampling based on DBSCAN clusters
sample_no_dbscan <- modified_no %>%
  group_by(dbscan) %>%
  sample_frac(sample_fraction)

sample_no_dbscan <- subset(sample_no_dbscan, select = -c(optic, cluster))

# Perform stratified sampling based on OPTICS clusters
sample_no_optics <- modified_no %>%
  group_by(optic) %>%
  sample_frac(sample_fraction)

sample_no_optics <- subset(sample_no_optics, select = -c(cluster, dbscan))

# ----------------------------- EVALUATION ----------------------------

# install.packages("clusterSim")
# install.packages("clValid")

library(clusterSim)
library(clValid)

# Davies-Bouldin Index for sample_no_kmeans
kmeans_db_index <- index.DB(subset(sample_no_kmeans, select = -cluster),sample_no_kmeans$cluster,centrotypes = "centroids")

# Davies-Bouldin Index for sample_no_dbscan
dbscan_db_index <- index.DB(subset(sample_no_dbscan, select = -dbscan),sample_no_dbscan$dbscan,centrotypes = "centroids")

# Davies-Bouldin Index for sample_no_optics
optics_db_index <- index.DB(subset(sample_no_optics, select = -optic),sample_no_optics$optic,centrotypes = "centroids")

# Davies-Bouldin Index for yes_kmeans
yes_kmeans_db_index <- index.DB(dataset_yes,kmeans_yes$cluster)

# Davies-Bouldin Index for yes_optics
yes_optics_db_index <- index.DB(dataset_yes,optics_clusters$cluster)

# Davies-Bouldin Index for yes_dbscan
yes_dbscan_db_index <- index.DB(dataset_yes,dbscan_yes$cluster)

# ----------------------------- DUNN Evaluation ----------------------------

# Dunn Index for sample_no_kmeans
kmeans_dunn_index <- dunn(clusters = sample_no_kmeans$cluster, Data = subset(sample_no_kmeans, select = -cluster))

# Dunn Index for sample_no_dbscan
dbscan_dunn_index <- dunn(clusters = sample_no_dbscan$dbscan, Data = subset(sample_no_dbscan, select = -dbscan))

# Dunn Index for sample_no_optics
optics_dunn_index <- dunn(clusters = sample_no_optics$optic,Data = subset(sample_no_optics, select = -optic))

# Dunn Index for yes_kmeans
yes_kmeans_dunn_index <- dunn(clusters = kmeans_yes$cluster, Data = dataset_yes)

# Dunn Index for yes_optics
yes_optics_dunn_index <- dunn(clusters = optics_clusters$cluster, Data = dataset_yes)

# Dunn Index for yes_dbscan
yes_dbscan_dunn_index <- dunn(clusters = dbscan_yes$cluster, Data = dataset_yes)

# ----------------------------- Printing ----------------------------

# Print the Dunn Indices
kmeans_dunn_index
dbscan_dunn_index
optics_dunn_index
yes_kmeans_dunn_index
yes_optics_dunn_index
yes_dbscan_dunn_index

# Print the Davies-Bouldin Indices
kmeans_db_index$DB
dbscan_db_index$DB
optics_db_index$DB
yes_kmeans_db_index$DB
yes_optics_db_index$DB
yes_dbscan_db_index$DB
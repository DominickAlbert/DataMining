library(dplyr)

bankingDT_Original <- read.csv("Bangking_Dataset_train.csv")
marketingDT_Original <- read.csv("Marketing_Target_train.csv", sep = ";")
bankingDT_Processed <- bankingDT_Original
marketingDT_Processed <- marketingDT_Original

# rm()

# Preparing the same columns that exist on both datasets
same_columns <- intersect(names(bankingDT_Original), names(marketingDT_Original))

# Present information regarding that dataset
# head(bankingDT_Original)
# head(marketingDT_Original)

# unique(bankingDT_Original$job)
# unique(marketingDT_Original$job)

str(bankingDT_Original[same_columns])
str(marketingDT_Original[same_columns])

unique(bankingDT_Original['education'])
unique(marketingDT_Original['education'])

# unique(bankingDT_Original['marital'])
# unique(marketingDT_Original['marital'])

# unique(bankingDT_Original['month'])
# unique(marketingDT_Original['month'])

# unique(bankingDT_Original['poutcome'])
# unique(marketingDT_Original['poutcome'])

# unique(bankingDT_Original['pdays'])
# unique(marketingDT_Original['pdays'])

# unique(bankingDT_Original['previous'])
# unique(marketingDT_Original['previous'])

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

dataset <- rbind(bankingDT_Processed,marketingDT_Processed)

duplicates <- duplicated(dataset)
duplicates <- duplicates[duplicates == TRUE]
View(duplicates)
sum(duplicates)
length(dataset$age)
dataset <- dataset[!duplicated(dataset), ]
length(dataset$age)

# These packages are for label encoding, install if need
# install.packages("superml")
# install.packages('data.table')
library(data.table)
library(superml)

dataset.categorical.columns <- c("job", "marital", "education", "default", "housing")
column.to.drop <- c("poutcome", "contact", "month")

# dataset <- subset(dataset, select = -poutcome)
dataset <- subset(dataset, select = -poutcome)
dataset <- subset(dataset, select = -contact)
dataset <- subset(dataset, select = -month)
dataset <- subset(dataset, select = -y)
dataset <- subset(dataset, select = -campaign)
dataset <- subset(dataset, select = -pdays)
dataset <- subset(dataset, select = -previous)
# for(i in column.to.drop){
#   dataset <- subset(dataset, select = -(c(i)))
# }

#Remove unknowns from categorical data
for(i in dataset.categorical.columns){
  dataset <- subset(dataset, !dataset %>% select(i)=="unknown")
}

print(length(dataset$age))

#OHE for the job column
unique(dataset$job)
encoded.job <- as.data.frame(model.matrix(~job-1, data=dataset))
for (i in colnames(encoded.job)){
  dataset <- cbind(dataset, i = encoded.job %>% select(i))
}
dataset <- subset(dataset, select = -job)
print(length(dataset$job))
colnames(encoded.job)

unique(dataset$marital)
encoded.job <- as.data.frame(model.matrix(~marital-1, data=dataset))
for (i in colnames(encoded.job)){
  dataset <- cbind(dataset, i = encoded.job %>% select(i))
}
dataset <- subset(dataset, select = -marital)
print(length(dataset$job))


#Label encoding for the "education" column
unique(dataset$education)
education.classes <- c("primary", "secondary", "tertiary", "unknown")
encoder = LabelEncoder$new()
encoder$fit(education.classes)
dataset$education <- encoder$fit_transform(dataset$education)
dataset$education <- dataset$education + 1
unique(dataset$education)

#Label encoding for "default" column
encoder = LabelEncoder$new()
encoder$fit(dataset$default)
dataset$default <- encoder$fit_transform(dataset$default)
unique(dataset$default)

#Label encoding for "housing" column
encoder = LabelEncoder$new()
encoder$fit(dataset$housing)
dataset$housing <- encoder$fit_transform(dataset$housing)
unique(dataset$housing)

#Label encoding for "loan" column
encoder = LabelEncoder$new()
encoder$fit(dataset$loan)
dataset$loan <- encoder$fit_transform(dataset$loan)
unique(dataset$loan)

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
tsne_result <- Rtsne(dataset, dims = 2, perplexity = 30)
tsne_data <- as.data.frame(tsne_result$Y)
colnames(tsne_data) <- c("Dim1", "Dim2")
tsne_data$Cluster <- as.factor(kmeans_result$cluster)

#install.packages("ggplot2")
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
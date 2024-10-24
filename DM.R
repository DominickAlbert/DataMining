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

# Define the categorical columns where "unknown" values may exist
categorical_columns <- c("job", "marital", "education", "default", "housing", "contact", "poutcome")

#cek kolom mana yang banyak unknown -> noucome bnyk bgt?
for (col in categorical_columns) {
  print(paste("Column:", col))
  print(table(dataset[[col]]))
}

# # Loop through each categorical column and remove rows with "unknown" values
# for (col in categorical_columns) {
#   dataset <- dataset[dataset[[col]] != "unknown", ]
# }

# # Check if "unknown" values have been successfully removed
# for (col in categorical_columns) {
#   print(unique(dataset[[col]]))
# }

length(dataset$age)



# These packages are for label encoding, install if need
# install.packages("superml")
# install.packages('data.table')
library(data.table)
library(superml)

# dataset.categorical.columns <- c("job", "marital", "education", "default", "housing", "contact", "month", "poutcome")

# length(dataset$age)
# #Remove unknowns from categorical data
# for(i in dataset.categorical.columns){
#   dataset <- subset(dataset, !dataset %>% select(i)=="unknown")
# }

# #Check if all the unknowns are gone
# for(i in dataset.categorical.columns){
#   print(unique(dataset %>% select(i)))
# }

unique(dataset$job)
encoded.job <- as.data.frame(model.matrix(~job-1, data=dataset))
for (i in colnames(encoded.job)){
  dataset <- cbind(dataset, i = encoded.job %>% select(i))
}
print(length(dataset$job))
print(length(dataset$job))
print(length(dataset$jobhousemaid))
colnames(encoded.job)

unique(dataset$marital)


#Label encoding for the "education" column
unique(dataset$education)
education.classes <- c("primary", "secondary", "tertiary", "unknown")
encoder = LabelEncoder$new()
encoder$fit(education.classes)
dataset$education <- encoder$fit_transform(dataset$education)
unique(dataset$education)



#Centers = k
#kmeans(dataset, centers, algorithm=c("Lloyd"))

#Install this package for DBSCAN clustering, just uncomment
#install.packages("fpc")

# library(fpc)

# DBSCAN <- dbscan(dataset, eps=0.45, MinPts = 5)

#Install this package for GMM (Gaussian Mixture Model) clustering, just uncomment
# install.packages("mclust")

# library(mclust)

#G = number of clusters
# gmm_model <- Mclust(dataset, G = 3 )

#summary(gmm_model)
#get the cluster assignments
#cluster_assignments <- predict(gmm_model)$classification
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

library(dplyr)

bankingDT_Original <- read.csv("Bangking_Dataset_train.csv")
marketingDT_Original <- read.csv("Marketing_Target_train.csv", sep = ";")

# rm()

# head(bankingDT_Original)
# head(marketingDT_Original)

# unique(bankingDT['job'])
# unique(marketingDT['job'])

# Preparing the same columns that exist on both datasets
same_columns <- intersect(names(bankingDT_Original), names(marketingDT_Original))

# Present information regarding that dataset
str(bankingDT_Original[same_columns])
str(marketingDT_Original[same_columns])

# unique(bankingDT_Original['education'])
# unique(marketingDT_Original['education'])

# Changing the values on MarketingDT so the education matches the one in the other dataset
marketing_education_mapped <- marketingDT_Original %>%
  mutate(education = recode(education,
                            tertiary = "university.degree",
                            secondary = "high.school",
                            primary = "basic.4y",  # Adjust if necessary
                            unknown = "unknown")
        )

# str(bankingDT_Original[same_columns])
# str(marketing_education_mapped[same_columns])

# unique(bankingDT_Original['marital'])
# unique(marketingDT_Original['marital'])

# unique(bankingDT_Original['month'])
# unique(marketingDT_Original['month'])

# unique(bankingDT_Original['poutcome'])
# unique(marketingDT_Original['poutcome'])

# Changing nonexistent into unkown so the data integration can go smooth
bangkinDT_poutcome_mapped <- bankingDT_Original %>%
  mutate(poutcome = recode(poutcome,
                           nonexistent = "unkown"))

# unique(bangkinDT_poutcome_mapped['poutcome'])
# unique(marketing_education_mapped['poutcome'])

str(bangkinDT_poutcome_mapped[same_columns])
str(marketing_education_mapped[same_columns])

# unique(bankingDT_Original['pdays'])
# unique(marketingDT_Original['pdays'])

# unique(bankingDT_Original['previous'])
# unique(marketingDT_Original['previous'])

# Subsetting the dataset so it only have the same columns
bangkinDT_transform <- bangkinDT_poutcome_mapped[, same_columns] 
marketing_transform <- marketing_education_mapped[, same_columns] 

combined_data <- rbind(bangkinDT_transform,marketing_transform)

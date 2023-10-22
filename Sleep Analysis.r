rm(list = ls())

#--------------------------------------------------------------------------------------------------------

library(caret) #For One-Hot Encoding

#--------------------------------------------------------------------------------------------------------
#Filepath - Change accordingly
path = "/Users/geolangsatnarzary/Study - NTU_NBS/6005 - Statistical/Group Assignment - 3/"
setwd(path)
df = read.csv("Sleep_health_and_lifestyle_dataset.csv")

#Statistical Inferences
cat("Dataset Structure:\n")
str(df)
cat("Dataset Summary:\n")
summary(df)

#Backup of Original df
original_df = df

####################### Preliminary Data Exploration ##########################

#Checking number of NA values in the df
na_count <- colSums(is.na(df))
print("Number of NAs\n")
print(na_count)
#0 NA values

#--------------------------------------------------------------------------------------------------------

#Finding duplicate values in Person.ID
duplicates <- df[duplicated(df$Person.ID) | duplicated(df$Person.ID, fromLast = TRUE), ]
print(duplicates)
#0 NA values

#--------------------------------------------------------------------------------------------------------

#Converting character variables to factor type for easy data manipulation of categorical variables
for (i in 1:ncol(df)) {
  if (is.character(df[, i])) {  # Added a closing parenthesis here
    df[, i] = as.factor(df[, i])
  }
}

#--------------------------------------------------------------------------------------------------------

#Moving Target Variable - Quality of Sleep to the end of the DataFrame
qos_index <- which(names(df) == "Quality.of.Sleep")
col_names <- names(df)
df <- df[, c(col_names[-qos_index], "Quality.of.Sleep")]

#--------------------------------------------------------------------------------------------------------

#Checking normal distribution:
#   1. Histogram for Numeric
#   2. Barplot for Categorical
for (col in colnames(df)) {
  #For storing each chart separately
  dev.new()
  if (is.factor(df[[col]])) {
    freq_table <- table(df[[col]])
    barplot(freq_table, main = paste("Frequency of", col), xlab = col, ylab = "Frequency")
  } else if (is.numeric(df[[col]])) {
    hist(df[[col]], main = paste("Histogram of", col), xlab = col, ylab = "Frequency")
  }
}
#Doesn't seem like there is normal distribution in any variable except for Age - which is almost uniform

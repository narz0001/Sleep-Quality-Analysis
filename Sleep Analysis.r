rm(list = ls())

#--------------------------------------------------------------------------------------------------------

library(caret) #For One-Hot Encoding
library(ggplot2) #For Beautiful Graphs
library(dplyr) #For Data Manipulation (Graph Plotting)
library(stringr) #For String Manipulation (Graph Plotting)

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

#--------------------------------------------------------------------------------------------------------

#Plots to get a better idea of relationship of Quality of Sleep with different parameters
generate_Box_and_BarPlot = function(X) {
  # Create a boxplot using ggplot2
  box_data <- data.frame(Quality.of.Sleep = df$Quality.of.Sleep, Variable = df[[X]])
  boxplot_plot <- ggplot(box_data, aes(x = Variable, y = Quality.of.Sleep, fill = Variable)) +
    geom_boxplot() +
    labs(title = paste("Distribution of Quality of Sleep by", X),
         x = X, y = "Quality of Sleep") +
    scale_fill_brewer(palette = "Set1") +  # Set color palette
    theme_minimal()  # Apply a minimal theme
  
  # Create a barplot using ggplot2
  table_data <- as.data.frame(table(df$Quality.of.Sleep, df[[X]]))
  colnames(table_data) <- c("Quality.of.Sleep", "Variable", "Count")
  barplot_plot <- ggplot(table_data, aes(x = Variable, y = Count, fill = Variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Distribution of Quality of Sleep by", X),
         x = X, y = "Count") +
    scale_fill_brewer(palette = "Set2") +  # Set color palette
    theme_minimal()  # Apply a minimal theme
  
  # Create a new folder if it doesn't exist
  if (!file.exists("Bar and Box Plots")) {
    dir.create("Bar and Box Plots")
  }
  
  # Set the filename for saving the plots
  filename <- str_replace_all(X, " ", "_")
  
  # Save the plots as PNG files
  ggsave(filename = paste("Bar and Box Plots/", filename, ".png", sep = ""), boxplot_plot, width = 6, height = 4)
  ggsave(filename = paste("Bar and Box Plots/", filename, "_barplot.png", sep = ""), barplot_plot, width = 6, height = 4)
}

# Loop through each column (excluding "Quality.of.Sleep")
for (col in colnames(df)[-which(colnames(df) == "Quality.of.Sleep")]) {
  generate_Box_and_BarPlot(col)
}

########################### Data Processing ##################################
#Extracting categorical columns
categorical_columns <- sapply(df, is.factor)
print(categorical_columns)

# One-hot encoding for categorical variables
df_encoded <- predict(dummyVars(~., data = df[categorical_columns,]), newdata = df)

# Correlation matrix
cor_matrix <- cor(df_encoded)
print(cor_matrix)

# Threshold for removing highly correlated columns
threshold <- 0.8

# Find the columns with correlations exceeding the threshold
high_cor_columns <- which(abs(cor_matrix) > threshold & row(cor_matrix) != col(cor_matrix), arr.ind = TRUE)

# Identify and remove the redundant columns
redundant_columns <- unique(high_cor_columns[, "col"])
print(ncol(redundant_columns)) 

#No redundant columns
df_encoded <- df_encoded[, -redundant_columns]

data <- read.csv("hw1_data.csv")

# What are the column names of the dataset?
names(data)

# Extract the first 2 rows of the data frame and print them to the console
data[c(1,2),]

# How many observations (i.e. rows) are in this data frame?
nrow(data)

# Extract the last 2 rows of the data frame and print them to the console
x <- nrow(data)
data[c(x-1,x),]

# What is the value of Ozone in the 47th row?
data[47,][["Ozone"]]

# How many missing values are in the Ozone column of this data frame?
sum(is.na(data[["Ozone"]]))

# What is the mean of the Ozone column in this dataset? 
# Exclude missing values (coded as NA) from this calculation.
x <- data[["Ozone"]]
mean(x[!is.na(x)])

# Extract the subset of rows of the data frame 
# where Ozone values are above 31 and Temp values are above 90. 
# What is the mean of Solar.R in this subset?
data2 <- subset(data , Ozone > 31 & Temp > 90)
mean(data2[["Solar.R"]])

# What is the mean of "Temp" when "Month" is equal to 6?
data3 <- subset(data , Month == 6)
mean(data3[["Temp"]])

# What was the maximum ozone value in the month of May (i.e. Month = 5)?
data4 <- subset(data , !is.na(Ozone) & Month == 5)
max(data4[["Ozone"]])
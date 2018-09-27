# Smart Certificate - R for Data Science
any(grepl("xml", installed.packages() == TRUE))


# Install the library XML to read the Forbes.html file.
install.packages("XML")

# Load the library
library(XML)
library(dplyr)

# Read the file
data <- readHTMLTable("C:/DS Full stack/Graded Assignments/R for Data Science/The World's Most Valuable Brands List - Forbes.html" )

# Check the class
class(data)

# Convert the relevant data into DataFrame
data <- data.frame(data["the_list"])
data <- data[, -1]

# Renaming the variable
data%>%select(Rank = the_list.Rank, Brand = the_list.Brand, Brand_Value = the_list.Brand.Value, One_Yr_Value_Change = the_list.1.Yr.Value.Change, Brand_Revenue = the_list.Brand.Revenue, Company_Advertising = the_list.Company.Advertising, Industry = the_list.Industry) -> data

# View the data
View(data)

# Check the dimension of the data
dim(data)

# Find the number of element inside the object
length(data)

# Check the data type of Brand_Value
class(data["Brand_Value"])

# Unique value in the column Industry
data%>%select(Industry)%>%unique()%>%nrow()

# Number of rows having Industry = Automotive
data%>%filter(Industry == "Automotive")%>%nrow()

# For Column Company_Advertising, count the 
# number of columns having Millions of Dollars
data%>%filter(grepl("M", Company_Advertising))%>%nrow()

# Replace the missing values with NA
data$Company_Advertising[which(data$Company_Advertising == "-")] <- NA
data$Brand_Revenue[which(data$Brand_Revenue == "-")] <- NA

# Number of NA in Col. Company_Advertising
sum(is.na(data$Company_Advertising))

# Drop the rows having missing values
data <- na.omit(data) # Can also be used
summary(data)

# Check the dim.
dim(data)

# Normalize the Column Company_Advertising
df <- as.data.frame(data["Company_Advertising"])
df <- gsub("M", "B", as.numeric(df))
df <- as.numeric(unlist(data["Company_Advertising"]))
mean_df <- round(mean(df), 2)
mean_df

# Clean and find the avg. for Brand Value
df <- as.data.frame(data["Brand_Value"])
df <- as.numeric(unlist(data["Brand_Value"]))
mean_df <- round(mean(df), 2)
mean_df

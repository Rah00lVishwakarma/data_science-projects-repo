# Set the working directory


# Load the library
library(dplyr)
library(ggplot2)

# Read the csv file telecomfinal
data_telecom <- read.csv("C:/telecomfinal.csv", na.strings = c("", " "))

# Get the summary of each variable
# -- Continuous Variable
str(data_telecom$drop_blk_Mean)
str(data_telecom$mou_opkv_Range)
str(data_telecom$custcare_Mean)
str(data_telecom$callwait_Mean)
str(data_telecom$iwylis_vce_Mean)
str(data_telecom$plcd_vce_Mean)
str(data_telecom$avgmou)
str(data_telecom$avgqty)
str(data_telecom$comp_vce_Mean)
str(data_telecom$opk_dat_Mean)
str(data_telecom$recv_sms_Mean)
str(data_telecom$blck_dat_Mean)
str(data_telecom$mou_pead_Mean)
str(data_telecom$drop_dat_Mean)
str(data_telecom$drop_vce_Mean)
str(data_telecom$adjmou)
str(data_telecom$totrev)
str(data_telecom$adjrev)
str(data_telecom$avgrev)
str(data_telecom$comp_dat_Mean)
str(data_telecom$plcd_dat_Mean)

# -- Integer Variables
str(data_telecom$drop_vce_Range)
str(data_telecom$owylis_vce_Range)
str(data_telecom$months)
str(data_telecom$totcalls)
str(data_telecom$callwait_Range)
str(data_telecom$ccrndmou_Range)
str(data_telecom$adjqty)
str(data_telecom$avg3mou)
str(data_telecom$avg3qty)
str(data_telecom$actvsubs)
str(data_telecom$uniqsubs)
str(data_telecom$churn)
str(data_telecom$Customer_ID)

# -- Factor Variables
str(data_telecom$totmrc_Mean)
str(data_telecom$ovrrev_Mean)
str(data_telecom$mou_Mean)
str(data_telecom$rev_Range)
str(data_telecom$mou_Range)
str(data_telecom$change_mou)
str(data_telecom$rev_Mean)
str(data_telecom$ovrmou_Mean)
str(data_telecom$hnd_price)
str(data_telecom$roam_Mean)
str(data_telecom$da_Mean)
str(data_telecom$da_Range)
str(data_telecom$datovr_Mean)
str(data_telecom$datovr_Range)
str(data_telecom$income)
str(data_telecom$eqpdays)
str(data_telecom$avg6mou)
str(data_telecom$avg6qty)
str(data_telecom$age1)
str(data_telecom$age2)
str(data_telecom$models)
str(data_telecom$forgntvl)
str(data_telecom$mtrcycle)
str(data_telecom$numbcars)
str(data_telecom$retdays)
str(data_telecom$truck)
str(data_telecom$crclscod)
str(data_telecom$asl_flag)
str(data_telecom$prizm_social_one)
str(data_telecom$area)
str(data_telecom$refurb_new)
str(data_telecom$hnd_webcap)
str(data_telecom$marital)
str(data_telecom$ethnic)
str(data_telecom$dwlltype)
str(data_telecom$dwllsize)
str(data_telecom$mailordr)
str(data_telecom$occu1)
str(data_telecom$wrkwoman)
str(data_telecom$solflag)
str(data_telecom$proptype)
str(data_telecom$mailresp)
str(data_telecom$cartype)
str(data_telecom$car_buy)
str(data_telecom$children)
str(data_telecom$csa)
str(data_telecom$div_type)


# data_telecom[, sapply(data_telecom, is.numeric)]
# lapply(data_telecom, class) -- data type of each variable


# Step 1 : Understanding Data: Creating a data quality report

# Explore/ Understand the data. Omit the variables with a lot missing values.
# Get the name of the columns
names(data_telecom) # Columns Name
lapply(data_telecom, class) # Data Type of each variable
nrow(data_telecom)  # No of Rows

# Calculate the unique values for each variables
lapply(data_telecom, n_distinct) # can also use length(unique(data_telecom$mou_Mean))

# No. of data available
colSums(data_telecom != "NA")

# Minimum in each columns, but only for numeric and Integer variables
lapply(data_telecom_num, min)
lapply(data_telecom_int, min)

# Maximum in each columns, but only for numeric and Integer variables
lapply(data_telecom_num, max)
lapply(data_telecom_int, max)

# Mean for each columns, but only for numeric and Integer variable
lapply(data_telecom_num, mean)
lapply(data_telecom_int, mean)

# 5th percentile of each columns, but only for numeric and Integer variable
data.frame(lapply(data_telecom_num, quantile, 0.05))
data.frame(lapply(data_telecom_int, quantile, 0.05))

# 10th percentile of each columns, but only for numeric and Integer variable
data.frame(lapply(data_telecom_num, quantile, 0.10))
data.frame(lapply(data_telecom_int, quantile, 0.10))

# 25th percentile of each columns, but only for numeric and Integer variable
data.frame(lapply(data_telecom_num, quantile, 0.25))
data.frame(lapply(data_telecom_int, quantile, 0.25))

# 50th percentile of each columns, but only for numeric and Integer variable
data.frame(lapply(data_telecom_num, quantile, 0.50))
data.frame(lapply(data_telecom_int, quantile, 0.50))

# 75th percentile of each columns, but only for numeric and Integer variable
data.frame(lapply(data_telecom_num, quantile, 0.75))
data.frame(lapply(data_telecom_int, quantile, 0.75))

# 90th percentile of each columns, but only for numeric and Integer variable
data.frame(lapply(data_telecom_num, quantile, 0.90))
data.frame(lapply(data_telecom_int, quantile, 0.90))

# 95th percentile of each columns, but only for numeric and Integer variable
data.frame(lapply(data_telecom_num, quantile, 0.95))
data.frame(lapply(data_telecom_int, quantile, 0.95))

# Variables with a lot of missing values will be omitted from the analysis
colSums(data_telecom == "NA")

# Variables to be omitted : income - 16528, dwlltype - 20824, dwllsize - 24991
# , mailordr - 42246, occu1 - 48490, numbcars - 32479, retdays - 64143, wrkwoman - 57951
# , solflag - 64982, proptype - 47372, mailresp - 41255, cartype - 45003
# , children - 43615, div_type - 53738
data_telecom[c("income", "dwlltype", 
               "dwllsize", "mailordr", 
               "occu1", "numbcars", 
               "retdays", "wrkwoman", 
               "solflag", "proptype", 
               "mailresp", "cartype", 
               "children", "div_type")] <- list(NULL)


# Create separate dataframe based on the type(Numeric, Integer, Factor)
data_telecom_num <- data_telecom[, c("drop_blk_Mean", "mou_opkv_Range", 
                                     "custcare_Mean", "callwait_Mean", 
                                     "iwylis_vce_Mean", 
                                     "plcd_vce_Mean", "avgmou", 
                                     "avgqty", "opk_dat_Mean", 
                                     "recv_sms_Mean", "blck_dat_Mean", 
                                     "mou_pead_Mean", "drop_dat_Mean", 
                                     "drop_vce_Mean", "adjmou", 
                                     "totrev", "adjrev", 
                                     "avgrev", "comp_dat_Mean", 
                                     "plcd_dat_Mean", "comp_vce_Mean")]

data_telecom_int <- data_telecom[c("drop_vce_Range", "owylis_vce_Range", 
                                   "months", "totcalls", 
                                   "callwait_Range", "ccrndmou_Range", 
                                   "adjqty", "avg3mou", 
                                   "avg3qty", "actvsubs", 
                                   "uniqsubs", "churn")]

data_telecom_fact <- data_telecom[c("ovrrev_Mean", "totmrc_Mean", 
                                    "mou_Mean", "rev_Range", 
                                    "mou_Range", "change_mou", 
                                    "rev_Mean", "ovrmou_Mean", 
                                    "hnd_price", "roam_Mean", 
                                    "da_Mean", "da_Range", "datovr_Mean", 
                                    "datovr_Range", "eqpdays",
                                    "avg6mou", "avg6qty", 
                                    "age1", "age2", 
                                    "models", "forgntvl", 
                                    "mtrcycle", "truck",
                                    "crclscod", "asl_flag", 
                                    "prizm_social_one", "area", 
                                    "refurb_new", "hnd_webcap", 
                                    "marital", "ethnic", 
                                    "car_buy", "csa")]


# Write all the dataframes into a csv file
write.csv(data_telecom_fact, file = "C:/Jig17521/data_telecom_fact.csv")
# Write all the dataframes into a csv file
write.csv(data_telecom_int, file = "C:/Jig17521/data_telecom_int.csv")
# Write all the dataframes into a csv file
write.csv(data_telecom_num, file = "C:/Jig17521/data_telecom_num.csv")


# Treatment of missing values
summary(data_telecom_int) # No missing values
summary(data_telecom_num) # No missing values
summary(data_telecom_fact) # Should be treated

# Either you can go with visualization part or simply for table for frequency count
# ovrrev_Mean <- ggplot(data_telecom_fact, aes(x = ovrrev_Mean))
# ovrrev_Mean + geom_bar() + scale_x_discrete(breaks = seq(0, 0.25, 0.01))

# Frequency count
head(sort(table(data_telecom_fact$ovrrev_Mean)/ 66297, decreasing = TRUE))

# impute all the rows of ovrrev_Mean variable with NAs to 0
index <- which(data_telecom_fact$ovrrev_Mean == "NA")
data_telecom_fact[index, "ovrrev_Mean"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$totmrc_Mean)/ 66297, decreasing = TRUE))

# impute all the rows of ovrrev_Mean variable with NAs to 44.99
index <- which(data_telecom_fact$totmrc_Mean == "NA")
data_telecom_fact[index, "totmrc_Mean"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$mou_Mean)/ 66297, decreasing = TRUE))

# impute all the rows of ovrrev_Mean variable with NAs to 0
index <- which(data_telecom_fact$mou_Mean == "NA")
data_telecom_fact[index, "mou_Mean"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$rev_Range)/ 66297, decreasing = TRUE))

# impute all the rows of ovrrev_Mean variable with NAs to 0
index <- which(data_telecom_fact$rev_Range == "NA")
data_telecom_fact[index, "rev_Range"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$mou_Range)/ 66297, decreasing = TRUE))

# impute all the rows of ovrrev_Mean variable with NAs to 0
index <- which(data_telecom_fact$mou_Range == "NA")
data_telecom_fact[index, "mou_Range"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$change_mou)/ 66297, decreasing = TRUE))

# Impute NAs with 0 into col. change_mou
# Since 0 is in the majority
index <- which(data_telecom_fact$change_mou == "NA")
data_telecom_fact[index, "change_mou"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$rev_Mean)/ 66297, decreasing = TRUE))

# Impute NAs with 29.99 into col. change_mou
index <- which(data_telecom_fact$rev_Mean == "NA")
data_telecom_fact[index, "rev_Mean"] <- 29.99

# Frequency count
head(sort(table(data_telecom_fact$ovrmou_Mean)/ 66297, decreasing = TRUE))

# Impute NAs with 0 into col. change_mou
index <- which(data_telecom_fact$ovrmou_Mean == "NA")
data_telecom_fact[index, "ovrmou_Mean"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$hnd_price)/ 66297, decreasing = TRUE))

# Impute NAs with 149.9899902
index <- which(data_telecom_fact$hnd_price == "NA")
data_telecom_fact[index, "hnd_price"] <- 149.9899902

# Frequency count
head(sort(table(data_telecom_fact$roam_Mean)/ 66297, decreasing = TRUE))

# Impute NAs with 0
index <- which(data_telecom_fact$roam_Mean == "NA")
data_telecom_fact[index, "roam_Mean"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$da_Mean)/ 66297, decreasing = TRUE))

# Impute NAs with 0
index <- which(data_telecom_fact$da_Mean == "NA")
data_telecom_fact[index, "da_Mean"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$da_Range)/ 66297, decreasing = TRUE))

# Impute NAs with 0
index <- which(data_telecom_fact$da_Range == "NA")
data_telecom_fact[index, "da_Range"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$datovr_Mean)/ 66297, decreasing = TRUE))

# Impute NAs with 0
index <- which(data_telecom_fact$datovr_Mean == "NA")
data_telecom_fact[index, "datovr_Mean"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$datovr_Range)/ 66297, decreasing = TRUE))

# Impute NAs with 0
index <- which(data_telecom_fact$datovr_Range == "NA")
data_telecom_fact[index, "datovr_Range"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$eqpdays)/ 66297, decreasing = TRUE))

# Impute NAs with 314
index <- which(data_telecom_fact$eqpdays == "NA")
data_telecom_fact[index, "eqpdays"] <- 314

# Frequency count
head(sort(table(data_telecom_fact$avg6mou)/ 66297, decreasing = TRUE))

# Impute all the rows of avg6mou variable with NAs to 0
index <- which(data_telecom_fact$avg6mou == "NA")
data_telecom_fact[index, "avg6mou"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$avg6qty)/ 66297, decreasing = TRUE))

# Impute all the rows of avg6mou variable with NAs to 0
index <- which(data_telecom_fact$avg6qty == "NA")
data_telecom_fact[index, "avg6qty"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$age1)/ 66297, decreasing = TRUE))

# Impute all the rows of age1 variable with NAs to 0
index <- which(data_telecom_fact$age1 == "NA")
data_telecom_fact[index, "age1"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$age2)/ 66297, decreasing = TRUE))

# Impute all the rows of age1 variable with NAs to 0
index <- which(data_telecom_fact$age2 == "NA")
data_telecom_fact[index, "age2"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$models)/ 66297, decreasing = TRUE))

# Impute all the rows of age1 variable with NAs to 0
index <- which(data_telecom_fact$models == "NA")
data_telecom_fact[index, "models"] <- 1

# Frequency count
head(sort(table(data_telecom_fact$forgntvl)/ 66297, decreasing = TRUE))

# Impute all the rows of age1 variable with NAs to 0
index <- which(data_telecom_fact$forgntvl == "NA")
data_telecom_fact[index, "forgntvl"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$mtrcycle)/ 66297, decreasing = TRUE))

# Impute all the rows of age1 variable with NAs to 0
index <- which(data_telecom_fact$mtrcycle == "NA")
data_telecom_fact[index, "mtrcycle"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$truck)/ 66297, decreasing = TRUE))

# Impute all the rows of age1 variable with NAs to 0
index <- which(data_telecom_fact$truck == "NA")
data_telecom_fact[index, "truck"] <- 0

# Frequency count
head(sort(table(data_telecom_fact$prizm_social_one)/ 66297, decreasing = TRUE))

# Impute all the rows of prizm_social_one variable with NAs to S
index <- which(data_telecom_fact$prizm_social_one == "NA")
data_telecom_fact[index, "prizm_social_one"] <- 'S'

# Frequency count
head(sort(table(data_telecom_fact$area)/ 66297, decreasing = TRUE))

# Impute all the rows of prizm_social_one variable with NAs to NEW YORK CITY AREA
index <- which(data_telecom_fact$area == "NA")
data_telecom_fact[index, "area"] <- 'NEW YORK CITY AREA'

# Frequency count
head(sort(table(data_telecom_fact$refurb_new)/ 66297, decreasing = TRUE))

# Impute all the rows of prizm_social_one variable with NAs to NEW YORK CITY AREA
index <- which(data_telecom_fact$refurb_new == "NA")
data_telecom_fact[index, "refurb_new"] <- 'N'

# Frequency count
head(sort(table(data_telecom_fact$hnd_webcap)/ 66297, decreasing = TRUE))

# Impute all the rows of prizm_social_one variable with NAs to WCMB 
index <- which(data_telecom_fact$hnd_webcap == "NA")
data_telecom_fact[index, "hnd_webcap"] <- "WCMB"

# Frequency count
head(sort(table(data_telecom_fact$marital)/ 66297, decreasing = TRUE))

# Impute all the rows of prizm_social_one variable with NAs to WCMB 
index <- which(data_telecom_fact$marital == "NA")
data_telecom_fact[index, "marital"] <- 'U'

# Frequency count
head(sort(table(data_telecom_fact$ethnic)/ 66297, decreasing = TRUE))

# Impute all the rows of prizm_social_one variable with NAs to WCMB 
index <- which(data_telecom_fact$ethnic == "NA")
data_telecom_fact[index, "ethnic"] <- 'N'

# Frequency count
head(sort(table(data_telecom_fact$car_buy)/ 66297, decreasing = TRUE))

# Impute all the rows of prizm_social_one variable with NAs to WCMB 
index <- which(data_telecom_fact$car_buy == "NA")
data_telecom_fact[index, "car_buy"] <- 'UNKNOWN'

# Frequency count
head(sort(table(data_telecom_fact$csa)/ 66297, decreasing = TRUE))

# Impute all the rows of prizm_social_one variable with NAs to WCMB 
index <- which(data_telecom_fact$csa == "NA")
data_telecom_fact[index, "csa"] <- 'NYCBRO917'

# Plot the scatter plot to see any trend in the data
# So that the variable should be selected
par(mfrow = c(3, 7))
list <- names(data_telecom_num)
for(i in 1:length(data_telecom_num)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i])
  # qplot(x = data_telecom_num[, list[i]], y = data_telecom$churn) # Can also be used, but not works inside the loop
  # qqplot(x = data_telecom_num$drop_blk_Mean, y = data_telecom$churn) # Can also be used
  
}

# In above plot all the variables are showing some trend. So, keep them for ana

# Repeat the step for Integer & Factor variable
par(mfrow = c(2, 6))
list <- names(data_telecom_int)
for(i in 1:length(data_telecom_int)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i])
  
}

# You can also find out the pattern via binning

# summary(data_telecom$avgmou)
# summary(data_telecom$churn)
# deciles <- quantile(data_telecom$avgmou, probs = seq(0, 1, 0.1))
# data_telecom$avgmou_group <- cut(data_telecom$avgmou, deciles, include.lowest = T)
# churn = aggregate(data_telecom$churn, by = list(data_telecom$avgmou_group), sum)
# 
# # Count the data points within that range
# count = data_telecom%>%group_by(avgmou_group)%>%summarise(n())
# churn$count = count$`n()`
# churn

# So, by both ways you can look at the churn rate. I prefer to use visualization


# Outliers detection
par(mfrow = c(3, 7))
list <- names(data_telecom_num)
for(i in 1: length(list)){
  
  boxplot(data_telecom_num[i], main = list[i])
}

# Repeat the plot for Integer variable
par(mfrow = c(2, 6))
list <- names(data_telecom_int)
for(i in 1: length(list)){
  
  boxplot(data_telecom_int[, i], main = list[i])
}

# Clearly there are a lot of ouliers but when plotted against 
# churn variable these ouliers are contributing towards the pattern 
# of churn rate. So, they can't be deleted/ imputed

# Now look at factor variables
# Divide the variable into string & Integer variable then separately
# visualize and those are having less contribution towards the churning
# pattern ignore the varibles from the analysis.
par(mfrow = c(3, 7))
par(mar = c(1, 1, 1, 1))
list <- names(data_telecom_fact[1:20])
# Discrete variables
for(i in 1: length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(0, 11622))
}

par(mfrow = c(2, 3))
list <- names(data_telecom_fact[21:33])
list <- list[-c(1, 2, 3, 5, 8, 9, 12)]
# Categorical variables
for(i in 1: length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i])
}


# Clearly the above data shows pattern. Retain them all for analysis
# That means they are leading to churn of the customers

# Now look at deciles for each variable for potential pattern
# If two or more variables showing same churn rate
# Then club them together and perform analysis

par(mfrow = c(3, 7))
list <- names(data_telecom_num)
# 1st Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(0, 6630))
  
}
# 2nd Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(6630, 13260))
  
}
# 3rd Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(13260, 19890))
  
}
# 4th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(19890, 26520))
  
}
# 5th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(26520, 33150))
  
}
# 6th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(33150, 39780))
  
}
# 7th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(39780, 46410))
  
}
# 8th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(46410, 53040))
  
}
# 9th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(53040, 59670))
  
}
# 10th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_num[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(59670, 66300))
  
}

par(mfrow = c(2, 6))
list <- names(data_telecom_int)
# 1st Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(0, 6630))
  
}
# 2nd Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(6630, 13260))
  
}
# 3rd Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(13260, 19890))
  
}
# 4th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(19890, 26520))
  
}
# 5th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(26520, 33150))
  
}
# 6th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(33150, 39780))
  
}
# 7th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(39780, 46410))
  
}
# 8th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(46410, 53040))
  
}
# 9th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(53040, 59670))
  
}
# 10th Iteration
for(i in 1:length(list)){
  
  plot(x = data_telecom_int[, list[i]], y = data_telecom$churn, type = "p", main = list[i], xlim = c(59670, 66300))
  
}

par(mfrow = c(3, 7))
list <- names(data_telecom_fact[1:20])
# 1st Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(0, 6630))
  
}
# 2nd Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(6630, 13260))
  
}
# 3rd Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(13260, 19890))
  
}
# 4th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(19890, 26520))
  
}
# 5th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(26520, 33150))
  
}
# 6th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(33150, 39780))
  
}
# 7th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(39780, 46410))
  
}
# 8th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(46410, 53040))
  
}
# 9th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(53040, 59670))
  
}
# 10th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(59670, 66300))
  
}

par(mfrow = c(2, 3))
list <- names(data_telecom_fact[21:33])
list <- list[-c(1, 2, 3, 5, 8, 9, 12)]
# 1st Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(0, 6630))
  
}
# 2nd Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(6630, 13260))
  
}
# 3rd Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(13260, 19890))
  
}
# 4th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(19890, 26520))
  
}
# 5th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(26520, 33150))
  
}
# 6th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(33150, 39780))
  
}
# 7th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(39780, 46410))
  
}
# 8th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(46410, 53040))
  
}
# 9th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(53040, 59670))
  
}
# 10th Iteration
for(i in 1:length(list)){
  
  plot(x = as.numeric(data_telecom_fact[, list[i]]), y = as.numeric(data_telecom$churn), type = "p", main = list[i], xlim = c(59670, 66300))
  
}

# Club those variable by dividing or by any other means
# , those are showing same trend/ event rate vs churn
# throughout the dataset
# Merge the cols. datovr_mean & datovr_range into one
data_telecom_fact$datovr_mean_range <- as.numeric(data_telecom_fact$datovr_Mean)/ as.numeric(data_telecom_fact$datovr_Range)

# Merge the cols. age1 & age2
data_telecom_fact$age1_age2 <- as.numeric(data_telecom_fact$age1)/ as.numeric(data_telecom_fact$age2)

data_telecom_fact <- data_telecom_fact[-c(13, 14, 18, 19)]

# Merge the cols. actsubs & unisubs
data_telecom_int$subs_act_uni <- as.integer(data_telecom_int$actvsubs)/ as.integer(data_telecom_int$uniqsubs)

data_telecom_int <- data_telecom_int[-(10:11)]

# Create some derived variables to answer some of the questions
# raised in the case study

# For "Network issues are leading to churn"
data_telecom_num$vce_completion_percentage = data_telecom_num$comp_vce_Mean/ data_telecom_num$plcd_vce_Mean

# For "data usage connectivity issues"
data_telecom_num$data_completion_percentage <- data_telecom_num$comp_dat_Mean/ data_telecom_num$plcd_dat_Mean

# For "Cost & Billings"
# data_telecom_num$cost_billings <- as.numeric(data_telecom_num$adjrev)/ as.numeric(data_telecom_int$adjqty)
data_telecom_num$rev_tot_adj <- data_telecom_num$totrev/ data_telecom_num$adjrev

data_telecom_num <- data_telecom_num[-c(6, 19, 20, 21, 16, 17)]
# data_telecom_int <- data_telecom_int[-c(7)]


# Combine all the dataframes
data_telecom <- bind_cols(data_telecom_num, data_telecom_int, data_telecom_fact)


# Impute 0 in place of NaN in data_completion_percentage
index <- which(is.nan(data_telecom$data_completion_percentage))
data_telecom$data_completion_percentage[index] <- 0.0000

# Impute 0 in place of NaN in vce_completion_percentage
index <- which(is.nan(data_telecom$vce_completion_percentage))
data_telecom$vce_completion_percentage[index] <- 0.0000

# Remove all the variables that hasn't any contribution towards churning
# because churning bechaviour has nothing to do 
# with those variables.
data_telecom <- data_telecom[-c(38, 40, 41, 42, 45, 46, 47, 48, 49, 50, 53, 57)]

# Conversion of data type of the variables
# and round of the numeric variables
data_telecom$recv_sms_Mean <- round(as.numeric(data_telecom$recv_sms_Mean))
data_telecom$blck_dat_Mean <- round(as.numeric(data_telecom$blck_dat_Mean))
data_telecom$drop_dat_Mean <- round(as.numeric(data_telecom$drop_dat_Mean))
data_telecom$drop_vce_Mean <- round(as.numeric(data_telecom$drop_vce_Mean))
data_telecom$churn         <- as.integer(data_telecom$churn)
data_telecom$ovrrev_Mean   <- as.numeric(data_telecom$ovrrev_Mean)
data_telecom$mou_Mean      <- as.numeric(data_telecom$mou_Mean)
data_telecom$rev_Range     <- as.numeric(data_telecom$rev_Range)
data_telecom$mou_Range     <- as.integer(data_telecom$mou_Range)
data_telecom$change_mou    <- as.numeric(data_telecom$change_mou)
data_telecom$rev_Mean      <- as.numeric(data_telecom$rev_Mean)
data_telecom$ovrmou_Mean   <- as.numeric(data_telecom$ovrmou_Mean)
data_telecom$roam_Mean     <- round(as.numeric(data_telecom$roam_Mean))
data_telecom$avg6mou       <- as.integer(data_telecom$avg6mou)
data_telecom$avg6qty       <- as.integer(data_telecom$avg6qty)
data_telecom$age1_age2     <- round(as.numeric(data_telecom$age1_age2))
data_telecom$subs_act_uni  <- round(as.numeric(data_telecom$subs_act_uni))
data_telecom$avgqty        <- round(as.numeric(data_telecom$avgqty))
data_telecom$totmrc_Mean   <- as.numeric(data_telecom$totmrc_Mean)

# write the data to a csv file
write.csv(data_telecom, file = "data.csv")

# Divie the dataset into train and test data
sample <- sort(sample(nrow(data_telecom), nrow(data_telecom) * 0.7))

train <- data_telecom[sample, ]
test <- data_telecom[-sample, ]

# Create a logistic regression model between churn vs all
model <- glm(churn ~., data = data_telecom[-46], family = "binomial")
summary(model)

# Do a stepwise regression to get the formula
# having minimum AIC
step(model, direction = 'both')

# After doing stepwise regression with minimum AIC
model <- glm(formula = churn ~ drop_blk_Mean + mou_opkv_Range + custcare_Mean + 
               iwylis_vce_Mean + avgmou + mou_pead_Mean + drop_dat_Mean + 
               drop_vce_Mean + avgrev + vce_completion_percentage + data_completion_percentage + 
               rev_tot_adj + owylis_vce_Range + totcalls + adjqty + avg3mou + 
               subs_act_uni + ovrrev_Mean + totmrc_Mean + mou_Mean + rev_Range + 
               mou_Range + change_mou + rev_Mean + ovrmou_Mean + roam_Mean + 
               avg6mou + prizm_social_one + area + hnd_webcap + marital + 
               ethnic + age1_age2, family = "binomial", data = data_telecom[-46])
summary(model)


# Create dummy variables
data_telecom$prizm_social_oneR_d <- ifelse(data_telecom$prizm_social_one == "R", 1, 0)
data_telecom$prizm_social_oneS_d <- ifelse(data_telecom$prizm_social_one == "S", 1, 0)
data_telecom$prizm_social_oneT_d <- ifelse(data_telecom$prizm_social_one == "T", 1, 0)
data_telecom$prizm_social_oneU_d <- ifelse(data_telecom$prizm_social_one == "U", 1, 0)
data_telecom$areaNEW_YORK_CITY_AREA_d <- ifelse(data_telecom$area == "NEW YORK CITY AREA", 1, 0)
data_telecom$areaNORTHWEST_ROCKY_MOUNTAIN_AREA_d <- ifelse(data_telecom$area == "NORTHWEST/ROCKY_MOUNTAIN_AREA", 1, 0)
data_telecom$areaSOUTH_FLORIDA_AREA_d <- ifelse(data_telecom$area == "SOUTH FLORIDA AREA", 1, 0)
data_telecom$areaTENNESSEE_AREA_d  <- ifelse(data_telecom$area == "TENNESSEE AREA", 1, 0)
data_telecom$hnd_webcapWC_d <- ifelse(data_telecom$hnd_webcap == "WC", 1, 0)
data_telecom$hnd_webcapWCMB_d <- ifelse(data_telecom$hnd_webcap == "WCMB", 1, 0)
data_telecom$ethnicC_d <- ifelse(data_telecom$ethnic == "C", 1, 0)
data_telecom$ethnicF_d <- ifelse(data_telecom$ethnic == "F", 1, 0)
data_telecom$ethnicG_d <- ifelse(data_telecom$ethnic == "G", 1, 0)
data_telecom$ethnicH_d <- ifelse(data_telecom$ethnic == "H", 1, 0)
data_telecom$ethnicI_d <- ifelse(data_telecom$ethnic == "I", 1, 0)
data_telecom$ethnicJ_d <- ifelse(data_telecom$ethnic == "J", 1, 0)
data_telecom$ethnicN_d <- ifelse(data_telecom$ethnic == "N", 1, 0)
data_telecom$ethnicP_d <- ifelse(data_telecom$ethnic == "P", 1, 0)
data_telecom$ethnicS_d <- ifelse(data_telecom$ethnic == "S", 1, 0)
data_telecom$ethnicU_d <- ifelse(data_telecom$ethnic == "U", 1, 0)
data_telecom$ethnicZ_d <- ifelse(data_telecom$ethnic == "Z", 1, 0)

# Remove those cols. for which you have created dummy variables.
data_telecom <- data_telecom[-c(41, 42, 43, 45)]

# Again divide the datasets into train & test datasets
sample <- sort(sample(nrow(data_telecom), nrow(data_telecom) * 0.7))

train <- data_telecom[sample, ]
test <- data_telecom[-sample, ]

# Create the model
model <- glm(formula = churn ~ ., family = "binomial", data = data_telecom[-42])

summary(model)

# Perform stepwise regression
step(model, direction = "both")

model <- glm(formula = churn ~ drop_blk_Mean + mou_opkv_Range + custcare_Mean + 
               callwait_Mean + iwylis_vce_Mean + avgmou + mou_pead_Mean + 
               drop_dat_Mean + drop_vce_Mean + adjmou + avgrev + vce_completion_percentage + 
               data_completion_percentage + rev_tot_adj + owylis_vce_Range + 
               totcalls + adjqty + avg3mou + subs_act_uni + ovrrev_Mean + 
               totmrc_Mean + mou_Mean + rev_Range + mou_Range + change_mou + 
               rev_Mean + ovrmou_Mean + roam_Mean + avg6mou + marital + 
               age1_age2 + prizm_social_oneR_d + prizm_social_oneT_d + areaNEW_YORK_CITY_AREA_d + 
               areaSOUTH_FLORIDA_AREA_d + areaTENNESSEE_AREA_d + hnd_webcapWC_d + 
               hnd_webcapWCMB_d + ethnicC_d + ethnicF_d + ethnicG_d + ethnicH_d + 
               ethnicI_d + ethnicJ_d + ethnicN_d + ethnicP_d + ethnicS_d + 
               ethnicU_d + ethnicZ_d, family = "binomial", data = data_telecom[-42])

summary(model)

# Remove those varables those ain't significant
# from the formula inside the model
model <- glm(formula = churn ~ mou_opkv_Range + custcare_Mean + 
               iwylis_vce_Mean + avgmou + drop_vce_Mean + avgrev + vce_completion_percentage + 
               data_completion_percentage + rev_tot_adj + owylis_vce_Range + 
               totcalls + adjqty + avg3mou + subs_act_uni + ovrrev_Mean + 
               totmrc_Mean + mou_Mean + rev_Range + mou_Range + change_mou + 
               ovrmou_Mean + roam_Mean + age1_age2 + prizm_social_oneR_d + prizm_social_oneT_d + 
               areaNEW_YORK_CITY_AREA_d + areaSOUTH_FLORIDA_AREA_d + areaTENNESSEE_AREA_d + hnd_webcapWC_d + 
               hnd_webcapWCMB_d + ethnicC_d + ethnicF_d + ethnicG_d + ethnicH_d + 
               ethnicI_d + ethnicJ_d + ethnicN_d + ethnicP_d + ethnicS_d + 
               ethnicU_d + ethnicZ_d, family = "binomial", data = data_telecom[-42])

summary(model)

# Predict the churn variables with the help of created
# model of the test data
predict <- predict(model, type = "response", newdata = test)
head(predict)

# For AUC & Concordance values
# Load the pROC library
library(pROC)
roc_curve <- roc(test$churn, predict)
plot(roc_curve, print.auc = TRUE, col = "blue", auc.polygon = TRUE, print.thres = TRUE)

# Getting concordance value
coords(roc = roc_curve, x = "best", best.method = "youden")

# Prepare customer segments based on the probability

# For test data
test$prob <- predict(model, type = "response", newdata = test)
test$LOW <- ifelse(test$prob < 0.244, 1, 0)
test$MEDIUM <- ifelse(test$prob >= 0.244 & test$prob < 0.5, 1, 0)
test$HIGH <- ifelse(test$prob >= 0.5, 1, 0)

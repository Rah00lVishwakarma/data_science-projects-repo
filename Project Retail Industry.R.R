# Merge the file Transactions_File.txt and 
# Products_File.txt and store the result in tr_product

# Set the working directory
setwd("C:/DS Full stack/Graded Assignments/Data Wrangling and EDA with R")

# Merge the two files
library(dplyr)
library(lubridate)

tr_product1 <- read.csv("Transactions_File.txt", sep = "\t", header = TRUE)

tr_product2 <- read.csv("Products_File.txt", sep = "\t", header = TRUE)

tr_product <- bind_rows(tr_product1, tr_product2) # Union can also be used

# Find the Product_Category dominates in terms of total purchase amount in dollars
tr_product%>%group_by(Product_Code, Items_Amount)%>%summarise(Value = sum(Items_Amount, na.rm = TRUE)) -> dominating_category

dominating_category <- na.omit(dominating_category)

max(dominating_category$Value)

dominating_category%>%filter(Product_Code, Value == 1878833)-> index

tr_product2%>%filter(Product_Code == 208) -> index

# Find the Product_Category dominates in terms of total purchase amount in dollars
min(dominating_category$Value)

dominating_category%>%filter(Product_Code, Value == 54) ->index

tr_product2%>%filter(Product_Code == 145) -> index

# Most popular payment method
table(tr_product$Payment_Method)

# How many transactions were carried out at 18:00 hours
tr_product%>%select(Timestamp) ->date_time

date_time$date <- format.Date(date_time$Timestamp, "%y-%m-%d")

date_time$time <- format.Date(date_time$Timestamp, "%H:%M")

date_time%>%filter(time == "18:00")%>%nrow()

# Is mode of transactions are affected by the Hour?
table(date_time$time, tr_product$Payment_Method)

chisq.test(table(date_time$time, tr_product$Payment_Method)) 
# Since p-value is comes out to be in-significant : so, not affected

# Merge the file Customers_File.txt and Transactions_File.txt
tr_cust1 <- read.csv("Customers_File.txt", sep = "\t", header = TRUE)

tr_cust <- bind_rows(tr_cust1, tr_product1)

# Number of rows remains after removing the duplicate rows for Card_Id
index <- which(duplicated(tr_cust$Card_ID))

tr_cust <- tr_cust[-index,]

# Find which age group has maximum contribution
# in terms of amount spent in dollars
tr_cust%>%select(Birth_Date)->date_time

date_time$date <- parse_date_time(date_time$Birth_Date, tz = "UTC", orders = "ymd")

date_time$age <- round(difftime("2017-01-01", date_time$date)/ 365, 2)

# Group customers age
seq(25, 115, by = 15)

date_time <- mutate(date_time, cardID = tr_cust$Card_ID)

# Filtering the age group by cardID
tr_product1%>%group_by(Card_ID, Items_Amount)%>%summarise(Value = max(Items_Amount)) ->df


# load the ggplot to visualize
library(ggplot2)

p1 <- ggplot(tr_product1, aes(x = Card_ID, y = Items_Amount))
g1 <- p1 + geom_point(aes(color = Items_Amount))

p2 <- ggplot(date_time, aes(x = cardID, y = age))
g2 <- p2 + geom_point(aes(color = age)) + scale_y_continuous(breaks = seq(25, 115, by = 15))

# load library grid
library(gridExtra)

grid.arrange(g1, g2)

# Based on the age group which group dominates in spending the amount in dollars
date_time$Gender <- tr_cust1$Gender

p3 <- ggplot(date_time, aes(x = age))
g3 <- p3 + geom_bar(aes(color = Gender)) + scale_x_continuous(breaks = seq(25, 115, by = 15))

grid.arrange(g1, g3)

# Merge the file Customers_File.txt and Campaign_File.txt
cc1 <- read.csv("Campaign_File.txt", sep = "\t", header = TRUE)

cc <- bind_rows(tr_cust1, cc1)

# Proportion of the customers who responded TRUE
cc%>%filter(Campaign_Responce == TRUE)%>%nrow()

table(cc$Campaign_Responce)

round(325/ 5957, 3)

# Find the tenure group of customers where rate is high
cc %>% select(Reg_Date = Registration_Date, Camp_Res = Campaign_Responce) ->cust_res

# Find the tenure
cust_res$tenure <- as.numeric(difftime("2004-12-31", cust_res$Reg_Date)/ 365)



# Visualize the response rate
p4 <- ggplot(cc1, aes(x = Campaign_Responce))
g4 <- p4 + geom_bar()

p5 <- ggplot(cust_res, aes(x = tenure))
g5 <- p5 + geom_histogram() + scale_x_continuous(breaks = seq(3, 7.5, by = 1.5))

grid.arrange(g4, g5)

# Number of customers with tenure 6 and 7.5
cust_res %>% filter(between(tenure, 6, 7.5)) %>% nrow()

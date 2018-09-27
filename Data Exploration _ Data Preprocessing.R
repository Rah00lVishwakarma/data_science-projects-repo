# explore data contribution.csv to come up with insights
# Install and Load the required the library
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)

# Read the contribution.csv file
setwd("C:/DS Full stack/Non Graded Assignments/Data Wrangling and EDA with R")
contribution <- read.csv("contribution.csv")
options(scipen = 999)

# 1. Do analysis at an aggregate level. 
# Finding out how total contributions are distributed by Gender,
# Batch, Marital Status etc
contribution%>%mutate(Contributions = FY04Giving + 
                        FY03Giving + FY02Giving + 
                        FY01Giving + FY00Giving)->contribution

# By Gender
contribution%>%group_by(Gender)%>%summarise(Count=n(),
                                            Percentage_Count = n()/ 1230,
                                            Total_Contribution = sum(Contributions),
                                            Percentage_Contribution = Total_Contribution/ 1205454,
                                            Average = mean(Contributions)
                                            )%>%
                                            ungroup()%>%
                                            arrange(-Total_Contribution)%>%
                                            data.frame()
# Takeaway : Males appear to be the majority contributors while being 
#            as many in number as females

# Bt Class Year
contribution%>%group_by(Class.Year)%>%summarise(Count = n(),
                                                Percentage_Count = n()/ 1230,
                                                Total_Contribution = sum(Contributions),
                                                Percentage_Contribution = Total_Contribution/ 1205454,
                                                Average=mean(Contributions)
                                                )%>%
                                                ungroup()%>%
                                                arrange(-Total_Contribution)%>%
                                                data.frame()
# Takeaway : This result is expected, alumni who have passed a long time 
#            ago are the ones who are contributing the most. Also notice 
#            that they comprise only 10% of the student base

# By Marital Status
contribution%>%group_by(Marital.Status)%>%summarise(Count = n(),
                                                    Percentage_Count = n()/ 1230,
                                                    Total_Contribution = sum(Contributions),
                                                    Percentage_Contribution = Total_Contribution/ 1205454,
                                                    Average = mean(Contributions)
                                                    )%>%
                                                    ungroup()%>%
                                                    arrange(-Total_Contribution)%>%
                                                    data.frame()
# Takeaway : Married people contribute the most and are also the largest group by count

# By Major
contribution%>%group_by(Major)%>%summarise(Count = n(),
                                           Percentage_Count = n()/ 1230,
                                           Total_Contribution = sum(Contributions),
                                           Percentage_Contribution = Total_Contribution/ 1205454,
                                           Average = mean(Contributions)
                                           )%>%
                                           ungroup()%>%
                                           arrange(-Total_Contribution)%>%
                                           data.frame()
# Takeaway : History majors are the highest contributors followed by Mathematics and Econonmics

# By Next Degree
contribution%>%group_by(Next.Degree)%>%summarise(Count = n(),
                                                 Percentage_Count = n()/ 1230,
                                                 Total_Contribution = sum(Contributions),
                                                 Percentage_Contribution = Total_Contribution/ 1205454,
                                                 Average = mean(Contributions)
                                                 )%>%
                                                 ungroup()%>%
                                                 arrange(-Total_Contribution)%>%
                                                 data.frame()
# Takeaway : People who didn't go on to pursue any degree further are the largest contributors

# By Attendance Event
contribution%>%group_by(AttendenceEvent)%>%summarise(Count = n(),
                                                     Percentage_Count = n()/ 1230,
                                                     Total_Contribution = sum(Contributions),
                                                     Percentage_Contribution = Total_Contribution/ 1205454,
                                                     Average = mean(Contributions))%>%
                                                     ungroup()%>%
                                                     arrange(-Total_Contribution)%>%
                                                     data.frame()
# Takeaway : This result is again expected, people who come to fundraising events they contribute the most

# By Fiscal Year
contribution%>%summarize(FY04 = sum(FY04Giving),
                         FY03 = sum(FY03Giving),
                         FY02 = sum(FY02Giving),
                         FY01 = sum(FY01Giving),
                         FY00 = sum(FY00Giving)
                         )%>%
                         melt()->
                         tot_cont

names(tot_cont) <- c("Year","Contribution")
tot_cont$Per_Cont <- round(tot_cont$Contribution/1205454, 2)
tot_cont$Cumu_Contr <- cumsum(tot_cont$Per_Cont)

# Now we will look at how total contributions are changing over the years
# It seems like that total contributions are variable and there is no predictable trend.

# Year wise Analysis
# We will now look at total contribution year wise by:
# 1. Gender
# 2. Batch
# 3. Marital Status
# 4. Attendence at fundrasing events
# 5. Major
# We will focus on the contributors only.

p <- ggplot(tot_cont,aes(x = Year,y = Contribution, fill = Year))
p + geom_bar(stat="identity",alpha=0.7) + 
  geom_text(aes(label = Per_Cont, colour = Per_Cont), vjust = -0.3) + 
  theme_classic() + scale_fill_discrete(c = 50, h = c(1,250), h.start = 50) +
  guides(color=FALSE)

contribution%>%filter(FY04Giving>0)%>%group_by(Gender)%>%summarise(Count = n(),
                                                                   Percentage_Count = n()/ 507,
                                                                   Total_Contribution = sum(FY04Giving),
                                                                   Percentage_Contribution = round(Total_Contribution/ 196061.8,2),
                                                                   Average = mean(FY04Giving)
                                                                   )%>%
                                                                   ungroup()%>%
                                                                   arrange(-Total_Contribution)%>%
                                                                   mutate(Contribution.Year = rep("FY04", 2))%>%
                                                                   data.frame()-> GFY04

contribution%>%filter(FY03Giving>0)%>%group_by(Gender)%>%summarise(Count = n(),
                                                                   Percentage_Count = n()/ 531,
                                                                   Total_Contribution = sum(FY03Giving),
                                                                   Percentage_Contribution = round(Total_Contribution/ 297013.8,2), 
                                                                   Average = mean(FY03Giving)
                                                                   )%>%
                                                                   ungroup()%>%
                                                                   arrange(-Total_Contribution)%>%
                                                                   mutate(Contribution.Year = rep("FY03", 2))%>%
                                                                   data.frame()-> GFY03

contribution%>%filter(FY02Giving>0)%>%group_by(Gender)%>%summarise(Count = n(),
                                                                   Percentage_Count = n()/ 548,
                                                                   Total_Contribution = sum(FY02Giving),
                                                                   Percentage_Contribution = round(Total_Contribution/ 164153.8, 2), 
                                                                   Average = mean(FY02Giving)
                                                                   )%>%
                                                                   ungroup()%>%
                                                                   arrange(-Total_Contribution)%>%
                                                                   mutate(Contribution.Year = rep("FY02", 2))%>%
                                                                   data.frame()-> GFY02

contribution%>%filter(FY01Giving>0)%>%group_by(Gender)%>%summarise(Count = n(),
                                                                   Percentage_Count = n()/ 600,
                                                                   Total_Contribution = sum(FY01Giving),
                                                                   Percentage_Contribution = round(Total_Contribution/ 340130.6, 2),
                                                                   Average = mean(FY01Giving)
                                                                   )%>%
                                                                   ungroup()%>%
                                                                   arrange(-Total_Contribution)%>%
                                                                   mutate(Contribution.Year = rep("FY01", 2))%>%
                                                                   data.frame()-> GFY01

contribution%>%filter(FY00Giving>0)%>%group_by(Gender)%>%summarise(Count = n(),
                                                                   Percentage_Count = n()/ 539,
                                                                   Total_Contribution = sum(FY00Giving),
                                                                   Percentage_Contribution = round(Total_Contribution/ 208093.6, 2 ),
                                                                   Average=mean(FY00Giving)
                                                                   )%>%
                                                                   ungroup()%>%
                                                                   arrange(-Total_Contribution)%>%
                                                                   mutate(Contribution.Year = rep("FY00", 2))%>%
                                                                   data.frame()-> GFY00

Gender<-rbind(GFY00, GFY01, GFY02, GFY03, GFY04)

p <- ggplot(Gender, aes(x = Contribution.Year, y = Total_Contribution, fill = Gender))

Contrib <- p + geom_bar(stat = "identity", position = "stack", alpha = 0.5) + 
  geom_text(aes(label = Percentage_Contribution), position = "stack", vjust = 2) +
  theme_classic() + scale_fill_discrete(c = 50, h = c(1, 100), h.start = 50, direction = -1) +
  ggtitle("Contribution by Gender")

l <- ggplot(Gender,aes(x = Contribution.Year, y = Percentage_Count, group = Gender, colour = Gender))

PerCount <- l + geom_line() + theme_classic() + ggtitle("Percentage Number of Males and Females") +
  scale_color_discrete(c = 50, h = c(1, 100), h.start = 50, direction = -1) +
  xlab("Contribution Year") + ylab("Percent Count")

TotCount <- l + geom_line(aes(y = Count)) + theme_classic() + ggtitle("Total Number of Males and Females") +
  scale_color_discrete(c = 50, h = c(1,100), h.start = 50, direction = -1) +
  xlab("Contribution Year") + ylab("Total Count")

grid.arrange(Contrib, PerCount, TotCount)

contribution%>%filter(FY04Giving>0)%>%group_by(Class.Year)%>%summarise(Count = n(),
                                                                       Percentage_Count = n()/ 507,
                                                                       Total_Contribution = sum(FY04Giving),
                                                                       Percentage_Contribution = round(Total_Contribution/ 196061.8, 2),
                                                                       Average = mean(FY04Giving)
                                                                       )%>%
                                                                       ungroup()%>%
                                                                       arrange(-Total_Contribution)%>%
                                                                       mutate(Contribution.Year = "FY04")%>%
                                                                       data.frame()-> CLY04

contribution%>%filter(FY03Giving>0)%>%group_by(Class.Year)%>%summarise(Count = n(),
                                                                       Percentage_Count = n()/ 531,
                                                                       Total_Contribution = sum(FY03Giving),
                                                                       Percentage_Contribution = round(Total_Contribution/ 297013.8, 2),
                                                                       Average = mean(FY03Giving)
                                                                       )%>%
                                                                       ungroup()%>%
                                                                       arrange(-Total_Contribution)%>%
                                                                       mutate(Contribution.Year="FY03")%>%
                                                                       data.frame()-> CLY03

contribution%>%filter(FY02Giving>0)%>%group_by(Class.Year)%>%summarise(Count = n(),
                                                                       Percentage_Count = n()/ 548,
                                                                       Total_Contribution = sum(FY02Giving),
                                                                       Percentage_Contribution = round(Total_Contribution/ 164153.8, 2),
                                                                       Average = mean(FY02Giving)
                                                                       )%>%
                                                                       ungroup()%>%
                                                                       arrange(-Total_Contribution)%>%
                                                                       mutate(Contribution.Year = "FY02")%>%
                                                                      data.frame()-> CLY02

contribution%>%filter(FY01Giving>0)%>%group_by(Class.Year)%>%summarise(Count = n(),
                                                                       Percentage_Count = n()/ 600,
                                                                       Total_Contribution = sum(FY01Giving),
                                                                       Percentage_Contribution = round(Total_Contribution/ 340130.6, 2),
                                                                       Average = mean(FY01Giving)
                                                                       )%>%
                                                                       ungroup()%>%
                                                                       arrange(-Total_Contribution)%>%
                                                                       mutate(Contribution.Year = "FY01")%>%
                                                                       data.frame()-> CLY01

contribution%>%filter(FY00Giving>0)%>%group_by(Class.Year)%>%summarise(Count = n(),
                                                                       Percentage_Count = n()/ 539,
                                                                       Total_Contribution = sum(FY00Giving),
                                                                       Percentage_Contribution = round(Total_Contribution/ 208093.6, 2 ),
                                                                       Average = mean(FY00Giving)
                                                                       )%>%
                                                                       ungroup()%>%
                                                                       arrange(-Total_Contribution)%>%
                                                                       mutate(Contribution.Year = "FY00")%>%
                                                                       data.frame()-> CLY00


Class <- rbind(CLY00, CLY01, CLY02, CLY03, CLY04)

Class$Class.Year <- as.factor(Class$Class.Year)

p <- ggplot(Class, aes(x = Contribution.Year, y = Total_Contribution, fill = Class.Year))

Contrib <- p + geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  theme_light() + scale_fill_discrete(c = 50, h = c(1, 300), h.start = 50) +
  ggtitle("Contribution By Batch") + ylab("Total Contribution in $") + 
  xlab("Contribution Year")

l <- ggplot(Class, aes(x = Contribution.Year, y = Percentage_Count, group = Class.Year, colour = Class.Year))

PerCount <- l + geom_line() + theme_classic() + ggtitle("Percentage Number of Alumni By Batch") +
  xlab("Contribution Year") + ylab("Percent Count") + scale_color_discrete(c = 50 ,h = c(1, 300), h.start = 50)

TotCount <- l + geom_line(aes(y = Count)) + theme_classic() +
  ggtitle("Total Number of Alumni By Batch") + ylab("Total Count") + 
  xlab("Contribution Year") + scale_color_discrete(c = 50, h = c(1, 300), h.start = 50)

grid.arrange(Contrib, PerCount, TotCount)

##Marital.Status##
contribution%>%filter(FY04Giving>0)%>%group_by(Marital.Status)%>%summarise(Count = n(),
                                                                           Percentage_Count = n()/ 507,
                                                                           Total_Contribution = sum(FY04Giving),
                                                                           Percentage_Contribution = round(Total_Contribution/ 196061.8, 2),
                                                                           Average = mean(FY04Giving)
                                                                           )%>%
                                                                           ungroup()%>%
                                                                           arrange(-Total_Contribution)%>%
                                                                           mutate(Contribution.Year = "FY04")%>%
                                                                           data.frame()-> MSY04

contribution%>%filter(FY03Giving>0)%>%group_by(Marital.Status)%>%summarise(Count = n(),
                                                                           Percentage_Count = n()/ 531,
                                                                           Total_Contribution = sum(FY03Giving),
                                                                           Percentage_Contribution = round(Total_Contribution/ 297013.8, 2),
                                                                           Average = mean(FY03Giving)
                                                                           )%>%
                                                                           ungroup()%>%
                                                                           arrange(-Total_Contribution)%>%
                                                                           mutate(Contribution.Year = "FY03")%>%
                                                                           data.frame()-> MSY03

contribution%>%filter(FY02Giving>0)%>%group_by(Marital.Status)%>%summarise(Count = n(),
                                                                           Percentage_Count = n()/ 548,
                                                                           Total_Contribution = sum(FY02Giving),
                                                                           Percentage_Contribution = round(Total_Contribution/ 164153.8, 2),
                                                                           Average = mean(FY02Giving)
                                                                           )%>%
                                                                           ungroup()%>%
                                                                           arrange(-Total_Contribution)%>%
                                                                           mutate(Contribution.Year = "FY02")%>%
                                                                           data.frame()-> MSY02

contribution%>%filter(FY01Giving>0)%>%group_by(Marital.Status)%>%summarise(Count = n(),
                                                                           Percentage_Count = n()/ 600,
                                                                           Total_Contribution = sum(FY01Giving),
                                                                           Percentage_Contribution = round(Total_Contribution/ 340130.6, 2),
                                                                           Average = mean(FY01Giving)
                                                                           )%>%
                                                                           ungroup()%>%
                                                                           arrange(-Total_Contribution)%>%
                                                                           mutate(Contribution.Year = "FY01")%>%
                                                                           data.frame()-> MSY01

contribution%>%filter(FY00Giving>0)%>%group_by(Marital.Status)%>%summarise(Count = n(),
                                                                           Percentage_Count = n()/ 539,
                                                                           Total_Contribution = sum(FY00Giving),
                                                                           Percentage_Contribution = round(Total_Contribution/ 208093.6, 2),
                                                                           Average = mean(FY00Giving)
                                                                           )%>%
                                                                           ungroup()%>%
                                                                           arrange(-Total_Contribution)%>%
                                                                           mutate(Contribution.Year = "FY00")%>%
                                                                           data.frame()-> MSY00


Marital <- rbind(MSY00, MSY01, MSY02, MSY03, MSY04)

p <- ggplot(Marital, aes(x = Contribution.Year, y = Total_Contribution, fill = Marital.Status))

Contrib <- p + geom_bar(stat = "identity", alpha = 0.6) +
  theme_light() + scale_fill_discrete(c = 50, h = c(1, 250), h.start = 50) +
  ylab("Total Contribution in $") + xlab("Contribution Year") + 
  ggtitle("Contribution by Marital Status")

l <- ggplot(Marital, aes(x = Contribution.Year, y = Percentage_Count, group = Marital.Status, colour = Marital.Status))

PerCount <- l + geom_line() + theme_classic() + 
  ggtitle("Percentage Number of Alumni by Marital Status") +
  xlab("Contribution Year") + ylab("Percent Count") + 
  scale_color_discrete(c = 50, h = c(1, 250), h.start = 50)

TotCount <- l + geom_line(aes(y = Count)) + theme_classic() + 
  ggtitle("Total Number of Alumni by Marital Status") + 
  ylab("Total Count") + xlab("Contribution Year") + 
  scale_color_discrete(c = 50, h = c(1, 250), h.start = 50)

grid.arrange(Contrib, PerCount, TotCount)


##AttendenceEvent##
contribution%>%filter(FY04Giving>0)%>%group_by(AttendenceEvent)%>%summarise(Count = n(),
                                                                            Percentage_Count = n()/ 507,
                                                                            Total_Contribution = sum(FY04Giving),
                                                                            Percentage_Contribution = round(Total_Contribution/ 196061.8, 2),
                                                                            Average = mean(FY04Giving)
                                                                            )%>%
                                                                            ungroup()%>%
                                                                            arrange(-Total_Contribution)%>%
                                                                            mutate(Contribution.Year = "FY04")%>%
                                                                            data.frame()-> ATY04

contribution%>%filter(FY03Giving>0)%>%group_by(AttendenceEvent)%>%summarise(Count = n(),
                                                                            Percentage_Count = n()/ 531,
                                                                            Total_Contribution = sum(FY03Giving),
                                                                            Percentage_Contribution = round(Total_Contribution/ 297013.8, 2),
                                                                            Average = mean(FY03Giving)
                                                                            )%>%
                                                                            ungroup()%>%
                                                                            arrange(-Total_Contribution)%>%
                                                                            mutate(Contribution.Year = "FY03")%>%
                                                                            data.frame()-> ATY03

contribution%>%filter(FY02Giving>0)%>%group_by(AttendenceEvent)%>%summarise(Count = n(),
                                                                            Percentage_Count = n()/ 548,
                                                                            Total_Contribution = sum(FY02Giving),
                                                                            Percentage_Contribution = round(Total_Contribution/ 164153.8, 2),
                                                                            Average = mean(FY02Giving)
                                                                            )%>%
                                                                            ungroup()%>%
                                                                            arrange(-Total_Contribution)%>%
                                                                            mutate(Contribution.Year = "FY02")%>%
                                                                            data.frame()-> ATY02

contribution%>%filter(FY01Giving>0)%>%group_by(AttendenceEvent)%>%summarise(Count = n(),
                                                                            Percentage_Count = n()/ 600,
                                                                            Total_Contribution = sum(FY01Giving),
                                                                            Percentage_Contribution = round(Total_Contribution/ 340130.6, 2),
                                                                            Average = mean(FY01Giving)
                                                                            )%>%
                                                                            ungroup()%>%
                                                                            arrange(-Total_Contribution)%>%
                                                                            mutate(Contribution.Year = "FY01")%>%
                                                                            data.frame()-> ATY01

contribution%>%filter(FY00Giving>0)%>%group_by(AttendenceEvent)%>%summarise(Count = n(),
                                                                            Percentage_Count = n()/ 539,
                                                                            Total_Contribution = sum(FY00Giving),
                                                                            Percentage_Contribution = round(Total_Contribution/ 208093.6, 2),
                                                                            Average = mean(FY00Giving)
                                                                            )%>%
                                                                            ungroup()%>%
                                                                            arrange(-Total_Contribution)%>%
                                                                            mutate(Contribution.Year = "FY00")%>%
                                                                            data.frame()-> ATY00

Attendence <- rbind(ATY00, ATY01, ATY02, ATY03, ATY04)

Attendence$AttendenceEvent <- as.factor(Attendence$AttendenceEvent)

p <- ggplot(Attendence, aes(x = Contribution.Year, y = Total_Contribution, fill = AttendenceEvent))

Contrib <- p + geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  theme_light() + scale_fill_discrete(c = 50, h = c(1, 100), h.start = 50) +
  ylab("Total Contribution in $") + xlab("Contribution Year") +
  ggtitle("Contribution by Attendence Status")

l <- ggplot(Attendence, aes(x = Contribution.Year, y = Percentage_Count, group = AttendenceEvent, colour = AttendenceEvent))

PerCount <- l + geom_line(size = 1.2) + theme_classic() +
  ggtitle("Percentage Number of Alumni by Attendence Status") +
  xlab("Contribution Year") + ylab("Percent Count") + 
  scale_color_discrete(c = 50,h = c(1, 100), h.start = 50)

TotCount <- l + geom_line(aes(y = Count), size = 1.2) +
  theme_classic() + ggtitle("Total Number of Alumni by Attendence Status") +
  ylab("Total Count") + xlab("Contribution Year") +
  scale_color_discrete(c = 50, h = c(1, 100), h.start = 50)

grid.arrange(Contrib, PerCount, TotCount)


##Major##
contribution%>%filter(FY04Giving>0)%>%group_by(Major)%>%summarise(Count = n(),
                                                                  Percentage_Count = n()/ 507,
                                                                  Total_Contribution = sum(FY04Giving),
                                                                  Percentage_Contribution = round(Total_Contribution/ 196061.8, 2),
                                                                  Average = mean(FY04Giving)
                                                                  )%>%
                                                                  ungroup()%>%
                                                                  arrange(-Total_Contribution)%>%
                                                                  mutate(Contribution.Year = "FY04")%>%
                                                                  data.frame()-> MY04

contribution%>%filter(FY03Giving>0)%>%group_by(Major)%>%summarise(Count = n(),
                                                                  Percentage_Count = n()/ 531,
                                                                  Total_Contribution = sum(FY03Giving),
                                                                  Percentage_Contribution = round(Total_Contribution/ 297013.8, 2),
                                                                  Average = mean(FY03Giving)
                                                                  )%>%
                                                                  ungroup()%>%
                                                                  arrange(-Total_Contribution)%>%
                                                                  mutate(Contribution.Year = "FY03")%>%
                                                                  data.frame()-> MY03

contribution%>%filter(FY02Giving>0)%>%group_by(Major)%>%summarise(Count = n(),
                                                                  Percentage_Count = n()/ 548,
                                                                  Total_Contribution = sum(FY02Giving),
                                                                  Percentage_Contribution = round(Total_Contribution/ 164153.8, 2),
                                                                  Average = mean(FY02Giving)
                                                                  )%>%
                                                                  ungroup()%>%
                                                                  arrange(-Total_Contribution)%>%
                                                                  mutate(Contribution.Year = "FY02")%>%
                                                                  data.frame()-> MY02

contribution%>%filter(FY01Giving>0)%>%group_by(Major)%>%summarise(Count = n(),
                                                                  Percentage_Count = n()/ 600,
                                                                  Total_Contribution = sum(FY01Giving),
                                                                  Percentage_Contribution = round(Total_Contribution/ 340130.6, 2),
                                                                  Average = mean(FY01Giving)
                                                                  )%>%
                                                                  ungroup()%>%
                                                                  arrange(-Total_Contribution)%>%
                                                                  mutate(Contribution.Year = "FY01")%>%
                                                                  data.frame()-> MY01

contribution%>%filter(FY00Giving>0)%>%group_by(Major)%>%summarise(Count = n(),
                                                                  Percentage_Count = n()/ 539,
                                                                  Total_Contribution = sum(FY00Giving),
                                                                  Percentage_Contribution = round(Total_Contribution/ 208093.6, 2),
                                                                  Average = mean(FY00Giving)
                                                                  )%>%
                                                                  ungroup()%>%
                                                                  arrange(-Total_Contribution)%>%
                                                                  mutate(Contribution.Year = "FY00")%>%
                                                                  data.frame()-> MY00

Major <- rbind(MY00, MY01, MY02, MY03, MY04)

p <- ggplot(Major%>%filter(Percentage_Contribution>0.08), aes(x = Contribution.Year, y = Total_Contribution, fill = Major))

Contrib <- p + geom_bar(stat = "identity", position = "stack", alpha = 0.6) +
  theme_classic() + ggtitle("Contribution break up by Major: Mninmum contribution of 8%") +
  scale_fill_discrete(h.start = 50, c = 50) + ylab("Total Contribution") + 
  xlab("Contribution Year")

l <- ggplot(Major%>%filter(Percentage_Contribution>0.08), aes(x = Contribution.Year, y = Percentage_Count, group = Major, colour = Major))

PerCount <- l + geom_line() + theme_classic() + ggtitle("Percentage Number of Alumni by Major") + 
  xlab("Contribution Year") + ylab("Percent Count") +
  geom_point(size=5) + scale_color_discrete(h.start = 50, c = 50)

TotCount <- l + geom_line(aes(y = Count)) + theme_classic() +
  ggtitle("Total Number of Alumni by Major") + ylab("Total Count") +
  xlab("Contribution Year") + geom_point(aes(y = Count), size = 5) +
  scale_color_discrete(h.start = 50, c = 50)

grid.arrange(Contrib,PerCount,TotCount)

contribution%>%filter(FY04Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                  Percentage_Count = n()/ 507,
                                                                                  Total_Contribution = sum(FY04Giving),
                                                                                  Percentage_Contribution = round(Total_Contribution/ 196061.8, 2),
                                                                                  Average = mean(FY04Giving)
                                                                                  )%>%
                                                                                  ungroup()%>%
                                                                                  arrange(-Total_Contribution)%>%
                                                                                  mutate(Contribution.Year = "FY04")%>%
                                                                                  data.frame()-> AMY04

contribution%>%filter(FY03Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(), Percentage_Count = n()/ 531,
                                                                                  Total_Contribution = sum(FY03Giving),
                                                                                  Percentage_Contribution = round(Total_Contribution/ 297013.8, 2),
                                                                                  Average = mean(FY03Giving)
                                                                                  )%>%
                                                                                  ungroup()%>%
                                                                                  arrange(-Total_Contribution)%>%
                                                                                  mutate(Contribution.Year = "FY03")%>%
                                                                                  data.frame()-> AMY03

contribution%>%filter(FY02Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                  Percentage_Count = n()/ 548,
                                                                                  Total_Contribution = sum(FY02Giving),
                                                                                  Percentage_Contribution = round(Total_Contribution/ 164153.8, 2),
                                                                                  Average = mean(FY02Giving)
                                                                                  )%>%
                                                                                  ungroup()%>%
                                                                                  arrange(-Total_Contribution)%>%
                                                                                  mutate(Contribution.Year = "FY02")%>%
                                                                                  data.frame()-> AMY02

contribution%>%filter(FY01Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = n()/ 600,
                                                                                   Total_Contribution = sum(FY01Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 340130.6, 2),
                                                                                   Average = mean(FY01Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY01")%>%
                                                                                   data.frame()-> AMY01

contribution%>%filter(FY00Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = n()/ 539,
                                                                                   Total_Contribution = sum(FY00Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 208093.6, 2),
                                                                                   Average = mean(FY00Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY00")%>%
                                                                                   data.frame()-> AMY00

MajorByAttendence <- rbind(AMY00, AMY01, AMY02, AMY03, AMY04)

MajorByAttendence%>%filter(AttendenceEvent==0)%>%group_by(Contribution.Year)%>%summarise(sum(Percentage_Count),
                                                                                         sum(Percentage_Contribution))

MajorByAttendence%>%filter(AttendenceEvent==0)%>%group_by(Contribution.Year,Major)%>%summarise(max(Count),
                                                                                               max(Percentage_Count),
                                                                                               max(Percentage_Contribution)
                                                                                               )%>%
                                                                                               ungroup()%>%
                                                                                               arrange(-`max(Percentage_Count)`)%>%
                                                                                               data.frame()

p <- ggplot(MajorByAttendence%>%filter(AttendenceEvent==0), aes(x = Contribution.Year, y = Count, group = Major, fill = Major))

p + geom_bar(stat = "identity")

p <- ggplot(MajorByAttendence%>%filter(AttendenceEvent==1, Percentage_Count>0.05), aes(x = Contribution.Year, y = Count, group = Major, fill = Major))

Attended <- p + geom_bar(stat = "identity", alpha = 0.6) + 
  theme_classic() + scale_fill_discrete(h.start = 50, c = 50) +
  geom_text(aes(label = Percentage_Contribution), position ="stack", vjust = 2.8) +
  annotate("text", x = "FY02", y = 180, label = "The numbers represent %age contribution", size = 4) +
  ylab("Total number of people attending") + xlab("Contribution Year") +
  ggtitle("Number of Attendees Vs Contribution Year (Only those categories shown where percentage of count > 5%)")

Attended

MajorByAttendence%>%filter(AttendenceEvent==1,Percentage_Count>0.05)

p + ggplot(MajorByAttendence%>%filter(AttendenceEvent==1,Percentage_Count>0.05), aes(x = Contribution.Year, y = Percentage_Count, group = Major, color = Major))

p <- ggplot(MajorByAttendence%>%filter(AttendenceEvent==1,Percentage_Count>0.05), aes(x = Contribution.Year, y = Percentage_Count, group = Major, color = Major))

p + geom_line() + theme_classic() + xlab("Contribution Year") +
  ylab("Percentage of Attendees") + ggtitle("Top 5 attendees by Major") +
  scale_color_discrete(h.start = 50,c = 50)

p + geom_line() + theme_classic() + xlab("Contribution Year") + 
  ylab("Percentage of Attendees") + ggtitle("Top 5 attendees by Major") +
  scale_color_discrete(h.start = 50, c = 50)

p + geom_line() + theme_classic() + xlab("Contribution Year") + 
  ylab("Percentage of Attendees") + ggtitle("Top 5 attendees by Major") + 
  scale_color_discrete(h.start = 50, c = 50) + geom_point(size = 1.2)

p + geom_line() + theme_classic() + xlab("Contribution Year") +
  ylab("Percentage of Attendees") + ggtitle("Top 5 attendees by Major") +
  scale_color_discrete(h.start = 50, c = 50) + geom_point(size = 2)

install.packages("ROCR")

p + geom_line() + theme_classic() + xlab("Contribution Year") +
  ylab("Percentage of Attendees") + ggtitle("Top 5 attendees by Major") + 
  scale_color_discrete(h.start = 50, c = 50) + geom_point(size = 3)

head(MajorByAttendence)

p + geom_line() + theme_classic() + xlab("Contribution Year") +
  ylab("Percentage of Attendees") + ggtitle("Top 5 attendees by Major") +
  scale_color_discrete(h.start=50,c=50) + geom_point(size=3) +
  geom_text(aes(label = Percentage_Count, color = Major))

contribution%>%filter(FY04Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = round(n()/ 507, 2),
                                                                                   Total_Contribution = sum(FY04Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 196061.8, 2),
                                                                                   Average = mean(FY04Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY04")%>%
                                                                                   data.frame()-> AMY04

contribution%>%filter(FY03Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = round(n()/ 531, 2),
                                                                                   Total_Contribution = sum(FY03Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 297013.8, 2),
                                                                                   Average = mean(FY03Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY03")%>%
                                                                                   data.frame()-> AMY03

contribution%>%filter(FY02Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = round(n()/ 548, 2),
                                                                                   Total_Contribution = sum(FY02Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 164153.8, 2),
                                                                                   Average = mean(FY02Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY02")%>%
                                                                                   data.frame()-> AMY02

contribution%>%filter(FY01Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = round(n()/ 600, 2),
                                                                                   Total_Contribution = sum(FY01Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 340130.6, 2),
                                                                                   Average = mean(FY01Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY01")%>%
                                                                                   data.frame()-> AMY01

contribution%>%filter(FY00Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = round(n()/ 539, 2),
                                                                                   Total_Contribution = sum(FY00Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 208093.6, 2),
                                                                                   Average = mean(FY00Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY00")%>%
                                                                                   data.frame()-> AMY00

MajorByAttendence<-rbind(AMY00,AMY01,AMY02,AMY03,AMY04)

p <- ggplot(MajorByAttendence%>%filter(AttendenceEvent==1, Percentage_Count>0.05), aes(x = Contribution.Year, y = Percentage_Count, group = Major, color = Major))

p + geom_line() + theme_classic() + xlab("Contribution Year") +
  ylab("Percentage of Attendees") + ggtitle("Top 5 attendees by Major") +
  scale_color_discrete(h.start = 50, c = 50) + geom_point(size = 3) +
  geom_text(aes(label = Percentage_Count, color = Major))

p + geom_line() + theme_classic() + xlab("Contribution Year") + 
  ylab("Percentage of Attendees") + ggtitle("Top 5 attendees by Major") +
  scale_color_discrete(h.start = 50, c = 50) +
  geom_point(size=3) + 
  geom_text(aes(label=Percentage_Count,color=Major),vjust=2)

contribution%>%filter(FY04Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = round(n()/ 507, 4),
                                                                                   Total_Contribution = sum(FY04Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 196061.8, 2),
                                                                                   Average = mean(FY04Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY04")%>%
                                                                                   data.frame()-> AMY04


contribution%>%filter(FY03Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = round(n()/ 531, 4),
                                                                                   Total_Contribution = sum(FY03Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 297013.8, 2),
                                                                                   Average = mean(FY03Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY03")%>%
                                                                                   data.frame()-> AMY03


contribution%>%filter(FY02Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = round(n()/ 548, 4),
                                                                                   Total_Contribution = sum(FY02Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 164153.8, 2),
                                                                                   Average = mean(FY02Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY02")%>%
                                                                                   data.frame()-> AMY02

contribution%>%filter(FY01Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = round(n()/ 600, 4),
                                                                                   Total_Contribution = sum(FY01Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 340130.6, 2),
                                                                                   Average = mean(FY01Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY01")%>%
                                                                                   data.frame()-> AMY01

contribution%>%filter(FY00Giving>0)%>%group_by(AttendenceEvent, Major)%>%summarise(Count = n(),
                                                                                   Percentage_Count = round(n()/ 539, 4),
                                                                                   Total_Contribution = sum(FY00Giving),
                                                                                   Percentage_Contribution = round(Total_Contribution/ 208093.6, 2),
                                                                                   Average = mean(FY00Giving)
                                                                                   )%>%
                                                                                   ungroup()%>%
                                                                                   arrange(-Total_Contribution)%>%
                                                                                   mutate(Contribution.Year = "FY00")%>%
                                                                                   data.frame()-> AMY00


MajorByAttendence <- rbind(AMY00, AMY01, AMY02, AMY03, AMY04)

p <- ggplot(MajorByAttendence%>%filter(AttendenceEvent==1, Percentage_Count>0.05), aes(x = Contribution.Year, y = Percentage_Count, group = Major, color = Major))

p + geom_line() + theme_classic() + xlab("Contribution Year") + 
  ylab("Percentage of Attendees") + ggtitle("Top 5 attendees by Major") +
  scale_color_discrete(h.start = 50, c = 50) + geom_point(size = 3) +
  geom_text(aes(label = Percentage_Count, color = Major), vjust = 2)

p + geom_line() + theme_classic() + xlab("Contribution Year") + 
  ylab("Percentage of Attendees") + ggtitle("Top 5 attendees by Major") +
  scale_color_discrete(h.start = 50, c = 50) + geom_point(size = 3) +
  geom_text(aes(label = Percentage_Count, color = Major), vjust = 1)


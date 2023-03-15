#Reading data into R and assigning data frame to hr_data
hr_data <- read.csv("~/Data/HR-Employee-Attrition.csv")

#loading packages
library(tidyverse)
library(moments)

# understanding the structure of the data
glimpse(hr_data)

#checking for missing values
sapply(hr_data,function(x) sum(is.na(x)))

#renaming column and changing character datatype to factor
hr_data <- hr_data %>%
  rename( Age = ï..Age) %>%
  mutate(across(where(is.character),as_factor))


# summary information of each variable based on its datatype
summary(hr_data)


#Ensuring data was changed for desired variable
class(hr_data$Attrition)
levels(hr_data$Attrition)


#Visualizing the average age of employees by  categorical variable Attrition
hr_data %>% 
  select(Age ,Attrition) %>% 
  ggplot(aes(Attrition,Age,color= Attrition))+
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=10, size=3.5, color="black") +
  facet_wrap(~ Attrition,scales ="free")+
  labs(title = "Boxplot of Age Disaggregated by Attrition Level",
       x = "Attrition",
       y = "Age" )

#Mean Age by Attrition Level
hr_data %>% 
  select(Age ,Attrition,Gender) %>%
  group_by(Attrition) %>% 
  summarise( Avg_Age_Attrition = mean(Age))

#Step 1 - Using histogram to determine if data is normally distributed
hr_data %>% 
  select(Age ,Attrition) %>%
  ggplot(aes(Age,color=Attrition))+
  geom_histogram(binwidth = 3,alpha=.5,fill="steelblue")+
  facet_wrap(~ Attrition,scales = "free")+
  labs(title = " Historgam of Age Disaggregated by Attrition Level",
       x = "Age",
       y = "Count")


#Step 2 - Using Shapiro Test to determine if data is normally distributed

shapiro.test(hr_data$Age[hr_data$Attrition == "No"])

shapiro.test(hr_data$Age[hr_data$Attrition == "Yes"])


#Step 3 - Using Q-Q Plot to determine if data is normally distributed

hr_data %>% 
  select(Age ,Attrition) %>%
  ggplot(aes(sample=Age,color=Attrition))+
  stat_qq()+
  stat_qq_line(col= "steelblue")+
  facet_wrap(~ Attrition,scales="free")+
  labs(title="QQ Plot of Age Dissaggregated by Attrition Level",
       x = "Normal Qunatiles",
       y = " Sample Qunatiles")



#Perform Mann-Whitney U test

df1 <-  hr_data %>% 
  select(Age ,Attrition)

wilcox.test(Age ~ Attrition,alternative = "less",paired= FALSE,data = df1 )


#Median Age by Attrition Level
df1 %>% 
  select(Age ,Attrition) %>%
  group_by(Attrition) %>% 
  summarise( Median_Age_Attrition = median(Age))

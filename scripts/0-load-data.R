library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)



#load the dataset
Data <- read.csv("data/loan.csv")


#add a new variable
Data <- mutate(Data,
               loan_per_income = loan_amnt / annual_inc)


#check if each person has only 1 loan and conversely. If yes we can get rid of one of them
Data |>
  summarize (number_id=n_distinct(id),number_member_id=n_distinct(member_id))


#select only important variables
Data <- select(Data,id,loan_amnt,funded_amnt,term,int_rate,grade,sub_grade,annual_inc,loan_status,purpose,addr_state,open_acc,total_acc,total_pymnt,emp_length,out_prncp, loan_per_income)


#rename variables
Data <-Data %>%
  rename("us_state"="addr_state",
         "years_of_employment"="emp_length")


#focusing only on fully paid and default
Data <- filter(Data, loan_status == "Default" | loan_status == "Fully Paid" )


#changing default with 0 and fully paid with 1
Data$loan_status <- ifelse(Data$loan_status=="Fully Paid", 1,0)


#ordering                             ???????????????????
Data<- Data[order(Data$annual_inc),]


#counting number of cells without any value for the variable year of employment
Data |>
  select(id,years_of_employment)|>
  group_by(years_of_employment)|>
  summarize( n_distinct(id))      
  

#there are 7443 n/a that we want to get rid of
Data <- filter(Data, Data$years_of_employment != "n/a")


#changing ids
Data["id"]<-c(str_c("id_",c(1:nrow(Data))))


#computing quantile for loan per income
quantile <- quantile(Data$loan_per_income, probs=c(0.2,0.4,0.6,0.8))


#Let's visualize the quantiles
quantile


#we create 5 groups, equal in number, in the variable loan per income
Data$loan_per_income <- cut(Data$loan_per_income, breaks = c(0,0.1032258,0.1583333,0.2150538,0.2938626 ,max(Data$annual_inc)), labels = c(str_c("cat_loan_per_inc_",c(1:5))),include.lowest = TRUE)


#we create a plot with the number of default by category of loan per income
ggplot(data=Data, aes(x = loan_per_income)) +
  geom_bar(data = subset(Data, loan_status == 0), stat = "count") +
  labs(x = "les 5 ratios de loan per income", y = "Default number") +
  ggtitle("the higher the loan per income the most likly default")


#create quantile for total account
quantile1 <- quantile(Data$total_acc, probs=(c(1:9))*0.1)


#lets vizualize the quantiles
quantile1


#we create 5 groups, equal in number, in the variable loan per income
Data$total_acc <- cut(Data$total_acc, breaks = c(0,11,15,18,21,24,27,30,34,41,max(Data$total_acc)), labels = c(str_c("cat_",c(1:10))),include.lowest = TRUE)


#we create a plot with the number of default by category of total account
ggplot(data=Data, aes(x = total_acc)) +
  geom_bar(data = subset(Data, loan_status == 0), stat = "count") +
  labs(x = "10 categories of total account", y = "Default number") +
  ggtitle("c'est contre intuitif et la moyenne est une droite donc je pense quon peut se passer de cette variable")+
  coord_flip()#je pense qu'il y a un effet non linéaire

#total account is not useful so we remove it from the dataset
#Data <- Data %>%
#  select(-total_acc)


#Let's look at the variable term and check the number of possible terms
Data|>
  group_by(term)|>
  summarize(number_of_idss <- n_distinct(id))


#since there exists only two terms we can change 36 - 0 et 60 - 1
Data$term <- ifelse(Data$term==" 60 months", 1,0)


#lets find out if the variable purpose is important
Data_summary <- Data %>%
  group_by(purpose) %>%
  summarise(percentage_Default = sum(loan_status == 0) / n())


#let's plot it
ggplot(Data_summary, aes(x = purpose, y = percentage_Default)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(percentage_Default)), vjust = 0, hjust=-0.2, size = 3.2) +
  labs(x = "Purpose", y = "Percentage of Default") +
  ggtitle("Some categories of purpose are less exposed to Default")+
  coord_flip()


#create quantile for int_rate
quantile2 <- quantile(Data$total_acc, probs=(c(1:9))*0.1)

















Data|>
    group_by(purpose)|>
    summarize(number_of_idss <- n_distinct(id))


Data|>
  group_by(loan_status)|>
  summarize(number_of_idss <- n_distinct(id))

200351 / (200351+1148)

mean(Data$loan_status)



#je vais m'occuper de years of employment

Data$loan_status <- ifelse(Data$loan_status=="Fully Paid", 1,0)

#je vais moccuper de purpose

ggplot(data=Data, aes(x = purpose)) +
  geom_bar(data = subset(Data, loan_status == 0), stat = "count")

ggplot(data=Data, aes(x = purpose)) +
  geom_bar(data = subset(Data, loan_status == 1), stat = "count")





ggplot(data=Data, aes(x = years_of_employment)) +
  geom_bar(data = subset(Data, loan_status == 0), stat = "count")

Data |>
  group_by(years_of_employment)|>
  summarize (numberidd = n_distinct(id))



#creation of quatile for out_prncp
quantile2 <- quantile(Data$out_prncp, probs=0.9)


#Let's see the quantile
quantile2

Data |>
  summarize (number_id=n_distinct(out_prncp))


ggplot(data=Data, aes(x = out_prncp)) +
  geom_bar( stat = "count") 




library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(data.table)
library(ggpubr)

# Load Data --------------------------------------------------------------------

#We load the dataset 
data <- read.csv("data/loan.csv")



#Let's rename two variables
data <- data %>%
  rename("us_state"="addr_state",
         "years_employment"="emp_length")


#We want the interest rates values to be percentages
data$int_rate = data$int_rate / 100


#We want to add a new variable
data <- mutate(data,
               loan_per_income = loan_amnt / annual_inc)




#focusing only on fully paid and default
data <- filter(data, loan_status == "Default" | loan_status == "Fully Paid" )




#changing default with 1 and fully paid with 0
data$loan_status <- ifelse(data$loan_status=="Fully Paid", 0,1)



#Let's look at the variable term and check the number of possible terms
data|>
  group_by(term)|>
  summarize(number_of_idss <- n_distinct(id))



#since there exists only two terms we change 36 with 0 and 60 with 1
data$term <- ifelse(data$term==" 60 months", 1,0)



#check if each person's id has only 1 loan's id and conversely. If this is the 
#case then we will keep only one of those two variable in the dataset
data |>
  summarize (number_id=n_distinct(id),number_member_id=n_distinct(member_id))



#select only important variables
data <- select(data,id,loan_amnt,funded_amnt,term,int_rate,grade,sub_grade,
               annual_inc,loan_status,purpose,us_state,open_acc,total_acc,
               total_pymnt,years_employment,out_prncp, loan_per_income)


#observing the number of cells without any value for the variable 
#year of employment. This column is the only one with missing values.
data |>
  select(id,years_employment)|>
  group_by(years_employment)|>
  summarize( n_distinct(id)) 


#there are 7443 n/a that we want to get rid of
data <- filter(data, data$years_employment != "n/a")



#changing ids
data["id"]<-c(str_c("id_",c(1:nrow(data))))



#CREATION OF 2 REGIONS in USA.
rep <-c("TX","OK","AR","LA","MS","AL","FL","TN","SC","KY","NC","WV","MT","ID","WY","UT","AK","ND","SD","IA","NE","KS","MO","IN","OH")
dem <-c("VA","GA","DE","MD","HI","AZ","NM","CO","NV","CA","OR","WA","MN","ME","NH","MA","VT","RI","CT","NY","PA","NJ","DC","IL","MI","WI")

data$us_state[data$us_state %in% rep] <- "REP"
data$us_state[data$us_state %in% dem] <- "DEM"



# creation of a subset of only numeric variables
num_col <- names(data)[sapply(data, is.numeric)]



# loop to get rid of 0.1% of highest values
for (col in num_col) {
  if (is.numeric(data[[col]])) {
    threshold <- quantile(data[[col]], probs = 0.999)
    data <- data[data[[col]] <= threshold, ]
  }
}

# Analysis ---------------------------------------------------------------------

#Lets create a function that shows a graph for the number of occurrences for each
#group within the variable and a second graph that shows the probability to
#default for each group of the variable.
plot_function <- function(variable,
                          x_title, 
                          title,
                          variabley, 
                          color,
                          xtitle,
                          maintitle,
                          turn)
  {
  plot1 <- ggplot(data = data,
                   aes(x = variable , 
                       y = loan_status)) +
    stat_summary(fun = "mean", 
                 geom = "bar", 
                 position = "dodge") +
    labs(x = x_title,
         y = expression("default probability")) +  
    ggtitle(title) +  
    geom_text(stat = "summary",
              aes(label = scales::percent(after_stat(y))), 
              position = position_dodge(width = 0.9), 
              hjust = -0.1,
              vjust = 0) +
    coord_flip() +
    geom_hline(aes(yintercept = mean(loan_status, na.rm = TRUE)), 
               color = "red", linetype = "dashed", linewidth = 1) + 
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) 
  
  #plot2 <- ggplot(data = data ,
  #         aes(x = !!variable, 
  #           y = variabley)) +
  #  geom_bar(stat = "identity", fill = color, alpha = 0.7) +  
  #  geom_text(aes(label = variabley), vjust = -0.18, size = 3) +  
  #  labs(x = xtitle, y= "Number of occurrences") +
  #  ggtitle(maintitle) +
  # theme_minimal() +
  #  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1),
  #        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
  #        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))+
  #  theme(axis.title.y = element_text(angle = 0, vjust=0.5))
  
  #if (turn == "yes") {
  #  plot2 = plot2 + coord_flip()
  #}
  
  #grid.arrange(plot1, plot2, ncol = 1)
  
  print(plot1)
}

#We create a function that creates 10 groups within a specified variable.
#The number of observations within each group should be more or less the same.
#Though it might be not the same due to several occurrences of the same value.

create_group_function <- function(variable, new_var_name) {
  data <- data %>%
    mutate(!!sym(new_var_name) := as.integer(cut(!!variable, 
                                                 quantile(!!variable, 
                                                 probs = seq(0, 1, by = 0.1)), 
                                                 include.lowest = TRUE)))
  return(data)
}

#This creates a function that counts the occurrences of each group within 
#the variable
count_var <- function( new_var_name, name_col) {
  
    data %>%
    group_by(!!new_var_name) %>%
    mutate(!!name_col := n()) %>%
    ungroup()
}



# 1. graphs for loan per income     

#let's create 10 groups for the variable loan per income
data <- create_group_function(data$loan_per_income, "loan_inc_group")

#Let's create a variable that counts the number of occurrences for each group
data<-count_var(data$loan_inc_group, "count_loan_inc")

#Lets plot those group with respect to their default probability
plot_loan_per_inc <- plot_function(data$loan_inc_group,
      "Loan per Income\nCategories ",
      "The higher the ratio the higher the default probability.",
      data$count_loan_inc,
      "darkred",
      "Purpose",
      "The number of observations is about the same for each group",
      "no")
      
print(plot_loan_per_inc)

#2. graph for interest rates

#let's create 10 groups for the variable loan per income
data <- create_group_function(data$int_rate, "int_rate_group")
#Let's create a variable that counts the number of occurrences for each group
data<-count_var(data$int_rate_group, "count_int_rate")
#Lets plot those group with respect to their default probability
plot_interest <- plot_function(data$int_rate_group,
          "Interest rate\nCategories ",
          "The higher the interest rate the higher the default probability.",
          data$count_int_rate,
          "darkred",
          "Interest rate\nCategories",
          "The number of observations is about the same for each group",
          "no")


print(plot_interest)

#3. graph for total account
#let's create 10 groups for the variable total account
data <- create_group_function(data$total_acc, "total_acc_group")
#Let's create a variable that counts the number of occurrences for each group
data<-count_var(data$total_acc_group, "count_total_acc")
#Lets plot those group with respect to their default probability
plot_total_acc <- plot_function(data$total_acc_group,
        "Total accounts\nCategories ",
        "There might be a negative correlation.",
         data$count_total_acc,
        "darkred",
        "Total accounts\nCategories",
        "The number of observations is\nabout the same for each group",
                "no")

print(total_acc)

#4. graph for grade
#Let's create a variable that counts the number of occurrences for each group
data <- count_var(data$sub_grade, "count_grade")
#Lets plot those group with respect to their default probability and their 
#occurrences
plot_grade <- plot_function(data$grade,
        "Grades ",
        "The better the grade\nthe lower the default probability.",
         data$count_grade,
        "darkred",
        "Grades",
        "E, F and G grades have very few observations",
        "no")
print(plot_grade)

#5. graph for Purpose
#Let's create a variable that counts the number of occurrences for each group
data <- count_var(data$purpose, "count_purpose")
#Lets plot those group with respect to their default probability
plot_purpose <- plot_function(data$purpose,
              "Purpose ",
              "3 purposes have a zero probability to default.",
              data$count_purpose,
              "darkred",
              "Purpose",
              str_wrap("Educational, wedding as well as 
              renewable energy loans have few observations.",width=40),
              "yes")

print(plot_purpose)



# Political Analysis -----------------------------------------------------------

#In this part, we want to understand the differences between Republicans and
#Democrats. 

# We create a dataset that selects only 2 purposes "credit_card" and
#"debt_consolidation" because those purposes are the most represented ones.
subset_data <- data[data$purpose %in% c("credit_card", "debt_consolidation"),]

#Here, we will look at the difference of default probability.
#Let's create a variable that counts the number of occurrences for each region
data <- count_var(data$us_state, "count_state")

plot_def_region <- plot_function(data$us_state,
            "Regions ",
            str_wrap("Republicans are more likely to default.",width=40),
            data$count_state,
            "darkred",
            "Regions",
            str_wrap("both regions have enough observations",width=40),
            "no")

print(plot_def_region)

#As we have seen that default probability is very different for those two
#regions. With the following graphs, we will try to explain where this comes 
#from. Unfortunately we will see that it is not due to a single reason but a lot
#of different factors.

#Can the composition of grade for each region explain the previous difference ?
plot_def_grade <- ggplot(data = data, aes(x = us_state, fill = grade)) +
  geom_bar(position = "fill", stat = "count") +
  labs(x = "US State", y = "Percentage", fill = "Grade") +
  ggtitle("Both composition are similar.") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Set2")  

print(plot_def_grade)

#Do Republicans have more loans with risky purposes ?
#Let's plot the risk of each purpose and the composition of purpose 
#for each region

#This graph will show the probability of default for each purpose
plot_def_purpose <- ggplot(data = data,
       aes(x = purpose , 
           y = loan_status,
           fill = purpose)) +  
  stat_summary(fun = "mean", 
               geom = "bar", 
               position = "dodge") +
  labs(x = "Purpose",
       y = "Default Probability") +  
  ggtitle("Moving, Medical and Vacations purposes are the most dangerous ones")+  
  geom_text(stat = "summary",
            aes(label = scales::percent(after_stat(y))), 
            position = position_dodge(width = 0.9), 
            hjust = 1,
            vjust = 0.35) +
  coord_flip() + 
  scale_fill_manual(values = c(
    "vacation"="darkred",
    "moving"="darkred",
    "medical"="darkred")) +  
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE)

#Let's create a palette of 14 colours 
my_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                "#ff9896", "#aec7e8", "#ffbb78", "#98df8a")

#this graph displays the cmoposition of purpose for each region.
#It uses the palette we have created.
plot_composition_purp <- ggplot(data = data, aes(x = us_state, fill = purpose)) +
  geom_bar(position = "fill", stat = "count") +
  labs(x = "US State", y = "Composition of purposes", fill = "Grade") +
  ggtitle("The composition of purposes is extremely similar") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = my_palette)

#We show both graph at the same time
plot_risk_compo_purp <- grid.arrange(plot_composition_purp ,
                                     plot_def_purpose, 
                                     ncol = 1)
print(plot_risk_compo_purp)

#Now let's see if both loan per income ratios are similar
#We create a variable that compute the mean of loan per income for each region
mean_dem <- mean(data$loan_per_income[data$us_state == "DEM"], na.rm = TRUE)
mean_rep <- mean(data$loan_per_income[data$us_state == "REP"], na.rm = TRUE)

#The following graph shows the density of loan per income for each region and 
#their respective mean.
plot_density_loan_income <- ggplot(data = data,
       aes(x = loan_per_income, color = us_state)) +
  geom_density(alpha = 0.5, linewidth = 1.5) +
  geom_vline(xintercept = c(mean_dem, mean_rep),
             color = c("blue", "darkred"), 
             linetype = "dashed", size = 1) +
  annotate("text", x = mean_dem - 0.03, y = 0.2, 
           label = round(mean_dem, 3), 
           color = "blue", size = 4, vjust = 0) +
  annotate("text", x = mean_rep + 0.03, y = 0.2,
           label = round(mean_rep, 3), 
           color = "darkred", size = 4, vjust = 0) +
  labs(title = "       Democrats tend to have lower loan per income ratios,
       but the difference is very small.",
       x = "Loan per income",y="Density") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkred"))

print(plot_density_loan_income)


# The difference of default probability between Republicans and Democrats could 
#potentially be explained by a higher loan per income ratio for the purposes 
#that have the most observations.
#We create a graph that shows the loan per income for the two most obeserved
#purposes
plot_loan_income_purpose <- ggplot(data = subset_data,
                                   aes(x = us_state,
                                       y = loan_per_income, 
                                       fill=us_state)) +
  stat_summary(fun = "mean", 
    geom = "bar", 
    position = "dodge")+
  geom_text(stat = "summary",
            aes(label = scales::percent(after_stat(y))), 
            position = position_dodge(width = 0.9), 
            hjust = 0.5,
            vjust = -0.3)+
  labs(x = " ", y = "Mean Loan per Income") +
  ggtitle("Republicans have highest loan per income ratios for each purpose.") +
  theme_minimal()+
  scale_fill_manual(values = c("DEM" = "blue", "REP" = "darkred")) 
#Then, we use facet_wrap to represent the previous graph for each region.
plot_facet_loan_inc_purp <- plot_loan_income_purpose + facet_wrap(~purpose)

print(plot_facet_loan_inc_purp)

 


#Let's see the difference of mean of interest rate for the regions
plot_def_interest <- ggplot(data = subset_data, aes(x = us_state, y = int_rate, fill=us_state)) +
  stat_summary(
    fun = "mean", 
    geom = "bar", 
    position = "dodge") +
  geom_text(stat = "summary",
            aes(label = scales::percent(after_stat(y))), 
            position = position_dodge(width = 0.9), 
            hjust = 0.5,
            vjust = -0.3)+
  labs(x = " ", y = "Mean Interest Rate") +
  ggtitle("Republicans have highest loan per income ratios for each purpose.") +
  theme_minimal()+
  scale_fill_manual(values = c("DEM" = "blue", "REP" = "darkred"))

print(plot_def_interest)

#Now we want to see the difference of total accounts between the regions
plot_def_total_acc <- ggplot(data = data, aes(x = us_state, y = total_acc, fill=us_state)) +
  stat_summary(
    fun = "mean", 
    geom = "bar", 
    position = "dodge") +
  geom_text(stat = "summary",
            aes(label = after_stat(y)), 
            position = position_dodge(width = 0.9), 
            hjust = 0.5,
            vjust = -0.3)+
  labs(x = " ", y = "Mean total accounts") +
  ggtitle("Republicans have more accounts on average.") +
  theme_minimal()+
  scale_fill_manual(values = c("DEM" = "blue", "REP" = "darkred"))

print(plot_def_total_acc)


       
# Get a feel for the data and understand the key metrics 

library(usmap)

# Function test ----------------------------------------------------------------

#Lets create a function that shows a graph for the number of occurrences for each
#group within the variable and a second graph that shows the probability to
#default for each group of the variable.
plot_function <- function(data, 
                          variable,
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
  
  plot2 <- ggplot(data = data ,
                  aes(x = !!variable, 
                      y = variabley)) +
    geom_bar(stat = "identity", fill = color, alpha = 0.7) +  
    geom_text(aes(label = variabley), vjust = -0.18, size = 3) +  
    labs(x = xtitle, y= "Number of occurrences") +
    ggtitle(maintitle) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1),
          axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1),
          plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))+
    theme(axis.title.y = element_text(angle = 0, vjust=0.5))
  
  if (turn == "yes") {
    plot2 = plot2 + coord_flip()
  }
  
  grid.arrange(plot1, plot2, ncol = 1)
  
}

#We create a function that creates 10 groups within a specified variable.
#The number of observations within each group should be more or less the same.
#Though it might be not the same due to several occurrences of the same value.

create_group_function <- function(data, variable, new_var_name) {
  data_grouped <- data %>%
    mutate(!!sym(new_var_name) := as.integer(cut(!!variable, 
                                                 quantile(!!variable, 
                                                          probs = seq(0, 1, by = 0.1)), 
                                                 include.lowest = TRUE)))
  return(data_grouped)
}

#This creates a function that counts the occurrences of each group within 
#the variable
count_var <- function(data, new_var_name, name_col) {
  
  data %>%
    group_by(!!new_var_name) %>%
    mutate(!!name_col := n()) %>%
    ungroup()
}


# Plot functions ---------------------------------------------------------------

# Categorical variables functions ..............................................

# Plot the default probability for each categorical variable 
plot_cat_relative_default <- function(data, factor_name) {
  summary <- data |>
    group_by(across({{ factor_name }}))|>
    summarize(default_probability = mean(loan_status=="Default"), 
              n = n()) |>
    ungroup()
  
  plt <- ggplot(summary,  aes_string(x = "default_probability", y = 
                                       factor_name))+
    geom_point()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(plt)
}

# Plot the categorical variables with the default and the paid debt 
plot_cat_default <- function(data, factor_name) {
  plt <- ggplot(data, aes_string(x = factor_name, fill = "loan_status"))+
    geom_bar()
  print(plt)
}

# Continuous variables functions ...............................................

# Plot the continuous variable histogram of what is paid and what is defaulted 
plot_cont_default_n_paid <- function(data, column) {
  plt <- ggplot(data, aes_string(x = column, fill = "loan_status"))+
    geom_histogram()
  print(plt)
}

# Plot the defaults for continuous value in absolute terms  
plot_cont_default <- function(data, column) {
  
  default_data <- filter(data, loan_status == "Default")
  plt <- ggplot(default_data, aes_string(x = column))+
    geom_histogram()
  print(plt)
}

# Plot the percentage of default for each continuous data 
plot_cont_default_percentage <- function(data, column) {

  # break down the data in 30 bins, find the percentage of default for each bin
  percentage <- data |>
    group_by(cut(!!sym(column), breaks = 30)) |>
    summarize(default_percentage = mean(loan_status == "Default")) |>
    ungroup()
  
  # rename the column for plotting 
  colnames(percentage) <- c("bins", "default_percentage")
  
  # create a vector for the x-axis 
  min_x <- min(data[[column]])
  max_x <- max(data[[column]])
  x_axis <- seq(from = min_x, to = max_x, length.out = nrow(percentage))
  
  # add the x axis to the percentage 
  percentage$x_axis <- x_axis 
  
  # plot the percentage 
  plt <- ggplot(percentage, aes(x = x_axis, y = default_percentage)) +
    geom_bar(stat = "identity") + 
    labs(x = clean_columns_names[column], 
         y = "Percentage of default")
  print(plt)
}

# Plotting ---------------------------------------------------------------------

# Categorical Data plotting ....................................................

cat_data_names <- c("term", "grade", "sub_grade", "purpose", 
                    "years_employment", "us_state")

# General view 
for (cat_data in cat_data_names){
  plot_cat_default(data_loan, cat_data)
  Sys.sleep(2)
}

# Probability of default 
for (cat_data in cat_data_names){
  plot_cat_relative_default(data_loan, cat_data)
  Sys.sleep(2)
}

# Continuous Data plotting .....................................................

# General view 
for (cont_data in cont_data_names){
  plot_cont_default_n_paid(data_loan, cont_data)
  Sys.sleep(2)
}

# Number of defaults 
for (cont_data in cont_data_names){
  plot_cont_default(data_loan, cont_data)
  Sys.sleep(2)
}

# Default percentage 
for (cont_data in cont_data_names) {
  plot_cont_default_percentage(data_loan, cont_data)
  Sys.sleep(2)
}

# Valentin Analysis ------------------------------------------------------------

# 1. graphs for loan per income     

#let's create 10 groups for the variable loan per income
data_val <- create_group_function(data_loan, data_loan$loan_per_income, "loan_inc_group")

#Let's create a variable that counts the number of occurrences for each group
data_val <- count_var(data_val, data_val$loan_inc_group, "count_loan_inc")

#Lets plot those group with respect to their default probability
plot_loan_per_inc <- plot_function(data_val, data_val$loan_inc_group,
                                   "Loan per Income\nCategories ",
                                   "The higher the ratio the higher the default probability.",
                                   data_val$count_loan_inc,
                                   "darkred",
                                   "Purpose",
                                   "The number of observations is about the same for each group",
                                   "no")

print(plot_loan_per_inc)

#2. graph for interest rates

#let's create 10 groups for the variable loan per income
data_val <- create_group_function(data_val, data_val$int_rate, "int_rate_group")
#Let's create a variable that counts the number of occurrences for each group
data_val <-count_var(data_val, data_val$int_rate_group, "count_int_rate")
#Lets plot those group with respect to their default probability
plot_interest <- plot_function(data_val,
                               data_val$int_rate_group,
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
data_val <- create_group_function(data_val, data_val$total_acc, "total_acc_group")
#Let's create a variable that counts the number of occurrences for each group
data_val <- count_var(data_val, data_val$total_acc_group, "count_total_acc")
#Lets plot those group with respect to their default probability
plot_total_acc <- plot_function(data_val, data_val$total_acc_group,
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
data_val <- count_var(data_val$sub_grade, "count_grade")
#Lets plot those group with respect to their default probability and their 
#occurrences
plot_grade <- plot_function(data_val, 
                            data_val$grade,
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
data_val <- count_var(data_val, data_val$purpose, "count_purpose")
#Lets plot those group with respect to their default probability
plot_purpose <- plot_function(data_val, data_val$purpose,
                              "Purpose ",
                              "3 purposes have a zero probability to default.",
                              data$count_purpose,
                              "darkred",
                              "Purpose",
                              str_wrap("Educational, wedding as well as 
              renewable energy loans have few observations.",width=40),
                              "yes")

print(plot_purpose)


# State analysis ---------------------------------------------------------------

# Get a feel of the data by states 

state_data <- data_loan |>
  group_by(across("us_state"))|>
  summarize(default_probability = mean(loan_status=="Default"), 
            n = n(), 
            mean_income = mean(annual_inc)) |>
  ungroup()

colnames(state_data)[colnames(state_data) == "us_state"] <- "state"


plot_usmap(data = state_data, values= "mean_income")+
  scale_fill_continuous() + 
  labs(title = "Default Probability by US States") +
  theme(legend.position = "right")



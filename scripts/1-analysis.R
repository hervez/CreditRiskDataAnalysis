library(rlang)
library(dplyr)
library(ggplot2)
# Quick load -------------------------------------------------------------------

write.csv(Data, file = "data/quick_data.csv", row.names = FALSE)

Data <- read.csv("data/quick_data.csv")

summary(Data)

# Factorize the dataframe

factor_names <- c("loan_status", "term", "grade", "sub_grade", "purpose", 
                  "years_of_employment", "us_state")
for (factor_name in factor_names){
  Data[, factor_name] <- factor(Data[, factor_name])
}

# Functions --------------------------------------------------------------------

# Simple function to apply regression to each variabel 
simple_logistic_regression <- function(data, predictor) {
  model <- glm(loan_status ~ data[, predictor], data = data, 
               family = binomial(logit))
  
  # Summarize the model
  summary_info <- summary(model)
  
  y.pred = predict(model, type="response")
  plot(data[, predictor], y.pred, col=(round(y.pred)+1))

  # Extract coefficients and significance
  coef <- coef(model)
  p_value <- summary_info$coefficients[2, 4]  # P-value for predictor
  
  # Return the results
  return(list(
    Predictor = predictor,
    Coefficient = coef[2],
    P_Value = p_value
  ))
}

# Plot the default probability for each categorical variable 
plot_cat_relative_default <- function(data, factor_name) {
  summary <- data |>
    group_by(across({{ factor_name }}))|>
    summarize(default_probability = mean(loan_status=="Default"), 
              n = n()) |>
    ungroup()
    
    plt <- ggplot(summary,  aes_string(x = "default_probability", y = 
                                         factor_name))+
    geom_point()
    print(plt)
}

# Plot the categorical variables with the default and the paid debt 
plot_cat_default <- function(data, factor_name) {
  plt <- ggplot(data, aes_string(x = factor_name, fill = "loan_status"))+
    geom_bar()
  print(plt)
}

# Plot the continuous variable histogram of what is paid and what is defaulted 
plot_cont_default_n_paid <- function(data, column) {
  plt <- ggplot(data, aes_string(x = column, fill = "loan_status"))+
    geom_histogram()
  print(plt)
}

# Plot the defaults for continuous value in absolute terms  
plot_cont_default <- function(data, column) {
  
  default_data <- filter(data, loan_status == "Default")
  plt <- ggplot(default_data, aes_string(x = column, fill = "loan_status"))+
    geom_histogram()
  print(plt)
}

# Plot the percentage of default for each continuous data 
plot_cont_default_percentage <- function(data, column) {
  column_sym <- sym(column)
  percentage <- data |>
    group_by(cut(!!column_sym, breaks = 20)) |>
    summarize(default_percentage = mean(loan_status == "Default")) |>
    ungroup()
  colnames(percentage) <- c("bins", "default_percentage")

  print(percentage)
  plt <- ggplot(percentage, aes(x = bins, y = default_percentage)) +
    geom_bar(stat = "identity") + 
    labs(x = clean_columns_names[column], 
         y = "Percentage of default")
  print(plt)
}

plot_cont_default_percentage(Data, "loan_amnt")

# Graphical analysis parameters ------------------------------------------------
clean_columns_names <- c(
  "loan_amnt" = "Loan amount", 
  "int_rate" = "Interest rate", 
  "grade" = "Grade", 
  "sub_grade" = "Sub-grade", 
  "annual_inc" = "Annual Income", 
  "loan_status" = "Loan status", 
  "purpose" = "Purpose", 
  "us_state" = "U.S. state", 
  "open_acc" = "Open account", 
  "total_acc" = "Total account", 
  "total_pymnt" = "Total payment", 
  "years_of_employment" = "Years of employment", 
  "out_prncp" = "Out principal", 
  "loan_per_income" = "Loan per income"
)

# Advanced Data cleaning -------------------------------------------------------

cont_data_names <- c("loan_amnt", "int_rate", "annual_inc", "total_pymnt", 
                     "loan_per_income", "open_acc", "total_acc", "out_prncp")

# check for redundant data with correlation
correlation_matrix <- cor(Data[cont_data_names])

# Create a heatmap to visualize the resulting correlation matrix. 
ggplot(data = data.frame(x = rep(colnames(correlation_matrix), each = 
                                   ncol(correlation_matrix)),
                         y = rep(colnames(correlation_matrix), times = 
                                   ncol(correlation_matrix)),
                         corr = as.vector(correlation_matrix))) +
  geom_tile(aes(x = x, y = y, fill = corr)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check for extreme value with box plot 
plot_cont_boxplot <- function(data, column) {
  plt <- ggplot(data)+
    geom_boxplot(column)
  print(plt)
}

# Data visualization -----------------------------------------------------------

cat_data_names <- c("term", "grade", "sub_grade", "purpose", 
                    "years_of_employment", "us_state")
# Plot categorical data
for (cat_data in cat_data_names){
 plot_cat_relative_default(Data, cat_data)
 Sys.sleep(2)
}

#Plot categorical data general view 
for (cat_data in cat_data_names){
  plot_cat_default(Data, cat_data)
  Sys.sleep(2)
}

# Plot Continuous data 
cont_data_names <- c("loan_amnt", "int_rate", "annual_inc", "total_pymnt", 
                     "loan_per_income", "open_acc", "total_acc", "out_prncp")

for (cont_data in cont_data_names){
  plot_cont_default_n_paid(Data, cont_data)
  Sys.sleep(2)
}

for (cont_data in cont_data_names){
  plot_cont_default(Data, cont_data)
  Sys.sleep(2)
}

for (cont_data in cont_data_names) {
  plot_cont_default_percentage(Data, cont_data)
  Sys.sleep(2)
}

for (cont_data in cont_data_names) {
  plot_cont_boxplot(Data, cont_data)
  Sys.sleep(2)
}

#Plot categorical data general view 
for (cat_data in cat_data_names){
  plot_cat_default(data_defaulted, cat_data)
  Sys.sleep(2)
}

data_defaulted <- data_defaulted |> 
  arrange(desc(loan_amnt))

print(head(data_defaulted, 20))

# GLM --------------------------------------------------------------------------

results_list <- data.frame()
predictor_variables <- c("loan_amnt", "int_rate", "annual_inc")  
for (predictor in predictor_variables) {
  result <- simple_logistic_regression(Data, predictor)
  print(result)
}

pertinent_data <- Data |> 
  select("loan")

model <- glm(loan_status ~ loan_amnt * int_rate, data = Data, family = binomial
             (logit))

summary(model)

# Extreme value Analysis -------------------------------------------------------

data_defaulted <- Data|> 
  filter(loan_status== "Default")

summary(data_defaulted)

for (cat_data in cat_data_names){
  plot_cat_relative_default(data_defaulted, cat_data)
  Sys.sleep(2)
}

ggplot(Data, aes(x=loan_amnt, y=loan_amnt))+
  geom_boxplot()


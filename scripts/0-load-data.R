library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(viridis)

# Graphical analysis parameters ------------------------------------------------
clean_columns_names <- c(
  "term" = "Term", 
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
  "years_employment" = "Years of employment", 
  "out_prncp" = "Out principal", 
  "loan_per_income" = "Loan per income", 
  "funded_amnt" = "Funded amount"
)

ordered_factor = c(
  "term" = FALSE,
  "grade" = TRUE,
  "sub_grade" = TRUE,
  "purpose" = FALSE, 
  "years_employment" = TRUE, 
  "us_state" = FALSE, 
  "politics" = FALSE
)

# data_loan Loading ------------------------------------------------------------

data_loan <- read.csv("data/loan.csv")

# get a first impression 
summary(data_loan)

# data_loan filtering-----------------------------------------------------------

# focusing only on fully paid and default
data_loan <- filter(data_loan,
                    loan_status == "Default" | loan_status == "Fully Paid" )

# check if each person has only 1 loan and conversely. If yes we can get rid of 
# one of them
data_loan |>
  summarize (number_id=n_distinct(id),number_member_id=n_distinct(member_id))

# select only important variables
data_loan <- select(data_loan,id,loan_amnt,funded_amnt,term,int_rate,grade,sub_grade,
                    annual_inc,loan_status,purpose,addr_state,open_acc,total_acc,
                    total_pymnt,emp_length)


# counting number of cells without any value for the variable year of employment
data_loan |>
  select(id,emp_length)|>
  group_by(emp_length)|>
  summarize( n_distinct(id))|> 
  ungroup()

# there are 7443 n/a that we want to get rid of
data_loan <- filter(data_loan, data_loan$emp_length != "n/a")

# check remaining data 
summary(data_loan)

# Adding new entries -----------------------------------------------------------

# add a new variable
data_loan <- mutate(data_loan,
                    loan_per_income = loan_amnt / annual_inc)

# add a column on wether the state is republican or democrat 
rep <-c("TX","OK","AR","LA","MS","AL","FL","TN","SC","KY","NC","WV","MT","ID",
        "WY","UT","AK","ND","SD","IA","NE","KS","MO","IN","OH")
dem <-c("VA","GA","DE","MD","HI","AZ","NM","CO","NV","CA","OR","WA","MN","ME",
        "NH","MA","VT","RI","CT","NY","PA","NJ","DC","IL","MI","WI")

data_loan$politics[data_loan$addr_state %in% rep] <- "REP"
data_loan$politics[data_loan$addr_state %in% dem] <- "DEM"

# Cosmetic changes -------------------------------------------------------------

# rename variables
data_loan <-data_loan %>%
  rename("us_state"="addr_state",
         "years_employment"="emp_length")

# changing ids
data_loan["id"]<-c(str_c("id_",c(1:nrow(data_loan))))

#We want the interest rates values to be percentages
data_loan$int_rate = data_loan$int_rate / 100

# check remaining data 
summary(data_loan)

# Factorisation ---------------------------------------------------------------

# factorize the dataframe
# get a list of categorical data 
factor_names <- c("loan_status", "term", "grade", "sub_grade", "purpose", 
                  "years_employment", "us_state", "politics")

# check that the factor are coherent 
for (factor_name in factor_names){
  unique_values <- unique(data_loan[, factor_name])
  print(unique_values)
  print(length(unique_values))
}

# factorize them 
for (factor_name in factor_names){
  data_loan[, factor_name] <- factor(data_loan[, factor_name])
}

# Extreme values clean-up ------------------------------------------------------

# Define the vector of continuous values 
cont_data_names <- c("loan_amnt", "int_rate", "annual_inc", "total_pymnt", 
                     "loan_per_income", "open_acc", "total_acc")

# Get a representation of the extremes values with some boxplots 
for (cont_data_name in cont_data_names){
  plt <- ggplot(data_loan, aes_string(x = 1, y = cont_data_name )) + 
    geom_boxplot(outlier.colour = "red",  outlier.shape = 1) +
    theme_minimal() + 
    labs(x = clean_columns_names[cont_data_name], 
         y = "")
  
  print(plt)
}

# filter the 99% percentile 
for (cont_data_name in cont_data_names){
  threshold <- quantile(data_loan[[cont_data_name]], probs = 0.99)
  print(cont_data_name)
  print(threshold)
  extreme_data <- filter(data_loan, !!sym(cont_data_name) >= threshold)
  data_loan <- filter(data_loan, !!sym(cont_data_name) < threshold)
}

# Check the new boxplots 
for (cont_data_name in cont_data_names){
  plt <- ggplot(data_loan, aes_string(x = 1, y = cont_data_name )) + 
    geom_boxplot(outlier.colour = "red",  outlier.shape = 1) +
    theme_minimal()
    labs(x = clean_columns_names[cont_data_name], 
         y = "")

}

# Create the data frame for the dashboard --------------------------------------

data_states <- data_loan |> 
  group_by(across(us_state))|> 
  summarize(default_probability = mean(loan_status=="Default"), 
          mean_income = mean(annual_inc), 
          mean_int_rate = mean(int_rate), 
          mean_loan = mean(loan_amnt), 
          mean_loan_per_inc = mean(loan_per_income), 
          n_data = n()) |> 
  ungroup()

data_purpose <- data_loan |> 
  group_by(across(purpose))|> 
  summarize(default_probability = mean(loan_status=="Default"), 
            mean_income = mean(annual_inc), 
            mean_int_rate = mean(int_rate), 
            mean_loan = mean(loan_amnt), 
            mean_loan_per_inc = mean(loan_per_income),
            n_default = sum(loan_status == "Default"), 
            n_data = n()) |> 
  ungroup()

data_grade <- data_loan |> 
  group_by(across(sub_grade))|> 
  summarize(default_probability = mean(loan_status=="Default"), 
            mean_income = mean(annual_inc), 
            mean_int_rate = mean(int_rate), 
            mean_loan = mean(loan_amnt), 
            mean_loan_per_inc = mean(loan_per_income),
            n_default = sum(loan_status == "Default"), 
            n_data = n()) |> 
    ungroup()

data_grade <- mutate(data_grade, grade = substr(sub_grade, 1, 1))

# Save the data ----------------------------------------------------------------

write.csv(data_loan, file = "data/data_loan.csv", row.names = FALSE)
write.csv(data_states, file = "data/data_states.csv", row.names = FALSE)
write.csv(data_purpose, file = "data/data_purpose.csv", row.names = FALSE)
write.csv(data_grade, file = "data/data_grade.csv", row.names = FALSE)

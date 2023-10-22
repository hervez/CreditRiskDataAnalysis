# Get a feel for the data and understand the key metrics 

library(usmap)
library(viridis)

# Plot functions ---------------------------------------------------------------

# Utils ........................................................................

# Find the angle to write the text for the graph 
find_angle <- function(data, factor_name){
  n_level <- nlevels(data[, factor_name])
  if (n_level < 8){
    angle = 0
  } else if ( n_level < 15){
    angle = 45
  } else {
    angle = 90 
  }
  
  return(angle)
}

# Categorical variables functions ..............................................

# Plot the default probability for each categorical variable 
plot_cat_relative_default <- function(data, factor_name) {

  # Get a summary dataframe 
  summary <- data |>
    group_by(across({{ factor_name }}))|>
    summarize(default_probability = mean(loan_status=="Default"), 
              n = n()) |>
    ungroup()
  
  if (!ordered_factor[factor_name]){
    summary <- summary[order(summary$default_probability),]
    factors <- as.character(summary[[factor_name]])
    summary[[factor_name]] <- factor(factors, levels=factors) 
  }

  plt <- ggplot(summary,  aes_string(x = "default_probability", y = 
                                         factor_name, fill = "default_probability"))+
    geom_bar(stat = "identity")+ 
    labs(x = "Percentage of default", 
         y = clean_columns_names[factor_name], 
         fill = "Probability of default") + 
    scale_fill_viridis(direction = -1) + 
    scale_y_discrete(labels = function(x) gsub("_", " ", x)) + 
    theme_minimal()  

  print(plt)
}

# Plot the categorical variables with the default and the paid debt 
plot_cat_default <- function(data, factor_name) {
  angle <- find_angle(data, factor_name)
  
  plt <- ggplot(data, aes_string(x = factor_name, fill = "loan_status"))+
    geom_bar() + 
    labs(x = clean_columns_names[factor_name], 
         y = "Count", 
         fill = "Loan status") + 
    theme(legend.position = "bottom") +
    scale_fill_viridis(discrete=TRUE, direction = -1) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = angle, hjust = 1)) + 
    scale_x_discrete(labels = function(x) gsub("_", " ", x))
    
  print(plt)
}

# Continuous variables functions ...............................................

# Plot the continuous variable histogram of what is paid and what is defaulted 
plot_cont_default_n_paid <- function(data, cont_name) {
  plt <- ggplot(data, aes_string(x = cont_name, fill = "loan_status"))+
    geom_histogram() + 
    labs(x = clean_columns_names[cont_name], 
         y = "Count", 
         fill = "Count") + 
    scale_fill_viridis(discrete = TRUE, direction = -1) + 
    theme_minimal()
  
  print(plt)
}

# Plot the defaults for continuous value in absolute terms  
plot_cont_default <- function(data, cont_name) {
  
  default_data <- filter(data, loan_status == "Default")
  plt <- ggplot(default_data, aes(x = !!sym(cont_name), fill=..count..))+
    geom_histogram() + 
    labs(x = clean_columns_names[cont_name], 
         y = "Count", 
         fill = "Count") + 
    scale_fill_viridis(direction = -1) +
    theme_minimal()
  
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
  plt <- ggplot(percentage, aes(x = x_axis, y = default_percentage, fill= default_percentage)) +
    geom_bar(stat = "identity") + 
    labs(x = clean_columns_names[column], 
         y = "Percentage of default", 
         fill = "Default percentage") + 
    scale_fill_viridis(direction = -1) +
    theme_minimal()
  
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
  scale_fill_viridis( direction = -1) + 
  labs(title = "Default Probability by US States") +
  theme(legend.position = "right")



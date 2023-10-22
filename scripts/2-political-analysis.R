#In this part, we want to understand the differences between Republicans and
#Democrats. 

library(tidyr)

# Functions --------------------------------------------------------------------

# Categorical Data .............................................................

# Plot the absolute distribution for republicans and democrats across all the 
# categorical data 
plot_cat_pol_default <- function(data, factor_name) {
  # find the angle for the x-axis scale
  angle <- find_angle(data, factor_name)
  
  # create the graph 
  plt <- ggplot(data, aes_string(x = factor_name, fill = "politics"))+
    geom_bar(position = "dodge")+ 
    labs(x = clean_columns_names[factor_name], 
         y = "Count") + 
    theme(legend.position = "bottom") +
    scale_fill_manual(values = c("REP" = "red", "DEM" = "blue")) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = angle, hjust = 1)) + 
    scale_x_discrete(labels = function(x) gsub("_", " ", x))
  
  print(plt)
}

# Plot the probability of default for republican or democrats for each 
# categorical data
plot_cat_pol_relative_default <- function(data, factor_name) {
  summary <- data |>
    group_by(across({{ factor_name }}))|>
    summarize(DEM = mean(loan_status=="Default" & politics=="REP"), 
              REP = mean(loan_status=="Default" & politics=="DEM")) |>
    ungroup()
  
  summary <- pivot_longer(summary, c("REP", "DEM"),
                          names_to = "politics", values_to = "default_probability")
  print(summary)
  
  # find the angle for the x-axis scale
  angle <- find_angle(data, factor_name)
  
  plt <- ggplot(summary, aes(x = !!sym(factor_name), y =default_probability, fill=politics))+ 
    geom_bar(stat = "identity", position = "dodge") + 
    labs(x = clean_columns_names[factor_name], 
         y = "Count") + 
    theme(legend.position = "bottom") +
    scale_fill_manual(values = c("REP" = "red", "DEM" = "blue")) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = angle, hjust = 1)) + 
    scale_x_discrete(labels = function(x) gsub("_", " ", x))
  
  print(plt)
}


# Plot The density of the continuous data distribution for both republicans and
# democrats 
plot_density_pol <- function(data, factor_name){
  
  # Find the mean for the republicans and the democrats
  mean_dem <- mean(data[, factor_name][data$politics == "DEM"])
  mean_rep <- mean(data[, factor_name][data$politics == "REP"])
  
  # Set the x position for the mean label 
  x_range <- max(data[,factor_name]) - min(data[, factor_name])
  if (mean_dem < mean_rep){
    dem_x_pos <- -(0.1 * x_range)
    rep_x_pos <- (0.1 * x_range)
  } else {
    dem_x_pos <- (0.1 * x_range)
    rep_x_pos <- -(0.1 * x_range)
  }
  
  # Create the plot 
  plt <- ggplot(data = data, aes(x = !!sym(factor_name),y=after_stat(scaled), color = politics)) +
    geom_density(alpha = 0.5, linewidth = 1.5)+
    geom_vline(xintercept = c(mean_dem, mean_rep),
               color = c("blue", "red"), 
               linetype = "dashed", size = 1) +
    annotate("text", x = mean_dem + dem_x_pos, y = 0.1, 
             label = round(mean_dem, 2), 
             color = "blue", size = 4, vjust = 0) +
    annotate("text", x = mean_rep + rep_x_pos, y = 0.1,
              label = round(mean_rep, 2), 
              color = "red", size = 4, vjust = 0) +
    labs(x = clean_columns_names[factor_name],y="Density") +
    theme_minimal() + 
    scale_color_manual(values = c("blue", "red"))

  print(plt)
}

# Continuous Data ..............................................................

# Plot the continuous variable histogram of what is paid and what is defaulted 
plot_cont_pol <- function(data, column) {
  plt <- ggplot(data, aes_string(x = column, fill = "politics"))+
    geom_histogram(position = "dodge") + 
    theme_minimal() + 
    scale_fill_manual(values = c("REP" = "red", "DEM" = "blue")) + 
    labs(x = clean_columns_names[column], 
         y = "Count")
    
  print(plt)
}

# Plotting ---------------------------------------------------------------------

# Categorical Data .............................................................

plot_cat_relative_default(data_loan, "politics")

pol_cat_data_names <- c("term", "grade", "sub_grade", "purpose", "years_employment")

for (cat_data in pol_cat_data_names){
  plot_cat_pol_default(data_loan, cat_data)
} 
  
for (cat_data in pol_cat_data_names){
  plot_cat_pol_relative_default(data_loan, cat_data)
}

# Continuous Data ..............................................................

for (cont_data in cont_data_names){
  plot_cont_pol(data_loan, cont_data)
}

for (cont_data in cont_data_names){
  plot_density_pol(data_loan, cont_data)
}

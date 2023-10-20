library(glmnet)
library(flexdashboard)

# Checking the correlation -- --------------------------------------------------

cont_data_names <- c("loan_amnt", "int_rate", "annual_inc", "total_pymnt", 
                     "loan_per_income", "open_acc", "total_acc", "out_prncp")

# check for redundant data with correlation
correlation_matrix <- cor(data_loan[cont_data_names])

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

# Modelling --------------------------------------------------------------------

# Prepare the data   
x_data <- data_loan[, -which(names(data_loan) == "loan_status")]
y_data <- as.numeric(data_loan$loan_status)

# Compute the model using GLMnet 
model <- glmnet(x=x_data, y=y_data, alpha = 1)

# Model evaluation -------------------------------------------------------------

# Create plots of the model 
plot(model, xvar = "lambda")
plot(model)




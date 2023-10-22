library(glmnet)

# Checking the correlation -- --------------------------------------------------

cont_data_names <- c("loan_amnt", "int_rate", "annual_inc", "total_pymnt", 
                     "loan_per_income", "open_acc", "total_acc", "funded_amnt")

# check for redundant data with correlation
correlation_matrix <- cor(data_loan[cont_data_names])

# Create a heatmap to visualize the resulting correlation matrix. 
ggplot(data = data.frame(x = rep(colnames(correlation_matrix), each = 
                                   ncol(correlation_matrix)),
                         y = rep(colnames(correlation_matrix), times = 
                                   ncol(correlation_matrix)),
                         corr = as.vector(correlation_matrix)), 
       aes(x = x, y = y, fill = corr)) +
  geom_tile() +
  labs(fill = "Correlation") +
  scale_fill_viridis(direction = 1, limits = c(-1, 1)) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  scale_x_discrete(labels = function(x) clean_columns_names[x]) + 
  scale_y_discrete(labels = function(x) clean_columns_names[x]) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

1# Modelling --------------------------------------------------------------------

# Prepare the data   
x_data <- data_loan %>%
  select(- loan_status, - id)
y_data <- as.numeric(data_loan$loan_status)

# Compute the model using GLMnet 
model <- glmnet(x=x_data, y=y_data, alpha = 1)

# Model representation ---------------------------------------------------------

print(model)
plot(model)

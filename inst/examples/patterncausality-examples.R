# Load required package
library(patterncausality)

# Load sample data
data(climate_indices)
X <- climate_indices$AO
Y <- climate_indices$AAO

# Example 1: Basic Pattern Causality Analysis
# Using pcLightweight for quick causality assessment
result <- pcLightweight(X, Y, 
                       E = 3,        # Embedding dimension 
                       tau = 2,      # Time delay
                       metric = "euclidean", 
                       h = 1,        # Prediction horizon
                       weighted = TRUE)
print(result)

# Example 2: Full Details Analysis
# Get detailed causality analysis
details <- pcFullDetails(X, Y, 
                        E = 2, 
                        tau = 1, 
                        metric = "euclidean", 
                        h = 3, 
                        weighted = TRUE)
# Access predicted and real causality status
predict_status <- details$causality_predicted
real_status <- details$causality_real

# Example 3: Parameter Optimization
# Search for optimal parameters
optimal_params <- optimalParametersSearch(Emax = 3, tauMax = 3,
                                        metric = "euclidean",
                                        h = 0,dataset = climate_indices[,-1])
print(optimal_params)

# Example 4: Cross Validation
# Perform cross-validation analysis
numberset <- c(100,150,200,250,300,350,400,450,500)
cv_results <- pcCrossValidation(X,Y,3,2,"euclidean",1,FALSE,numberset,FALSE)
print(cv_results$results)

# Example 5: Causality Matrix
# Generate causality matrix for multiple variables
causality_matrix <- pcMatrix(dataset = climate_indices[,-1], 
                            E = 3, 
                            tau = 1, 
                            metric = "euclidean", 
                            h = 2, 
                            weighted = FALSE)
print(causality_matrix)

effects <- pcEffect(causality_matrix)

# Print the effects summary
print(effects)
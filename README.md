# Predicting Secondary School Student Performance: Replication and Improvement
This repository contains the code and analysis for our project, which replicates and extends the 2008 study by Cortez & Silva, "Using Data Mining to Predict Secondary School Student Performance." The study employed machine learning models to predict the final grades (G3) of secondary school students, utilizing demographic, social, and academic data.

Our project builds on their work in two phases:

1. Replication – Faithfully reproducing the results of the original study across regression, binary classification, and five-level classification tasks.

2. Extension – Implementing systematic feature selection methods to reduce noise, build simpler models, and improve predictive performance in real-world scenarios.

The full analysis and findings are detailed in the accompanying Final Report

-------------------------------------------------------------------------------------------------------

# Key Findings and Improvements
The extension work, which focused on the most challenging real-world scenario (Setup C, where no prior grades are available), yielded significant improvements:

1. Identified Core Predictors: Using Random Forest-based feature ranking, an optimal subset of 9 core predictors for the Mathematics dataset and 23 for the Portuguese dataset was identified.
   
2. Improved Model Performance: Models trained on these refined feature sets consistently performed better, including a 10% lower RMSE in regression for the Random Forest and 5% increase in accuracy for the SVM classifier on the 5-level Portuguese classification task.
   
3. Enhanced Simplicity & Interpretability: For Mathematics, the final model achieved comparable results with 70% fewer features (9 vs. 30+), making it simpler and easier to interpret.

4. Additional Performance Metric: We analysed changes in Precision as an additional performance metric in the Binary Classification Case.
   
6. Solving the Precision Problem: The feature selection process significantly improved Random Forest Precision on the Binary Classification task, producing a more reliable predictor.

-------------------------------------------------------------------------------------------------------------------------------------------

# Feature Selection Process

To identify the optimal set of "core predictors," a Filter Method was implemented based on the feature importance metric from the randomForest package. The importance of each feature was calculated using the Percentage Increase in Mean Squared Error (%IncMSE). This method works by training a Random Forest Model and then, for each feature, randomly permuting its values in the out-of-bag (OOB) samples and measuring how much the model's prediction error (MSE) increases. A larger increase in error implies a more important feature.

```r
# Train a Random Forest model on the Setup C data
rf_model <- randomForest(G3 ~ ., data = data_C, importance = TRUE)

# Extract importance scores (%IncMSE) and sort them
raw_importance <- importance(rf_model, type = 1)
sorted_importance <- sort(raw_importance[,1], decreasing = TRUE)

# Get the final ranked list of feature names
ranked_features <- names(sorted_importance)

```

## Assumptions

We used the same features obtained in the Random Forest Regression Case for each subject for SVM, as well as in the binary and 5-level classification cases. In doing so, we made two core assumptions-

1. Model-Independence: We assumed that, due to the robustness of Random Forest (RF) as a model, the same features selected in RF would yield improved results in the SVM model.

2. Case-Independence: We assumed that the same features selected in the regression case would also yield improved results in the binary and 5-level classification cases.

These assumptions allowed us to obtain sufficiently improved results while substantially reducing computational costs. 

## Number of Top Features vs. RF Performance:

The graphs below plot the cross-validated RMSE against the number of top-ranked features used to train the model. This visualization allowed us to identify the "sweet spot" where the model's performance maximized (RMSE minimized), indicating the optimal number of features to retain.

![Top Features vs. RF Performance Graph](top_features_graph.png)









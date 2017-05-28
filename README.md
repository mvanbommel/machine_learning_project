# machine_learning_project
This repository contains the report and code for a project to estimate a model to predict the output of an unknown dataset. The report can be found in **Report.pdf**

## Code By Report Section
### The Task
- **Data2016.csv** - training set data
- **Data2016test.csv** - test set data

### Preliminary Exploration
- **preliminary_exploration.R** - checking for missing data and examining variable types, distributions, and correlations

### Initial Test of Regression Methods
- **initial_test_tuning_RF_BART_NN.R** - tuning the Random Forest, BART, and Neural Network models
- **initial_test_all_methods.R** - computing root MSPE and relative root MSPE values over 10 fold cross validation for all regression methods examined

### Variable Selection
- **initial_test_variable_importance.R** - using the regression methods to produce variable importance results
- **group_1_variable_test_results.R** - testing the model performance of all regression methods using every combination of variables in Group 1
- **group_2_variable_test_results.R**, **group_3_variable_test_results.R**, **group_4_variable_test_results.R** - testing the model performance of all regression methods using the variables in Group 1 and all possible combinations of variables in Groups 2, 3, or 4

### Final Test of Regression methods
- **final_test_tuning_RF_BART_NN.R** - tuning the Random Forest, BART, and Neural Network models using only the selected variables
- **final_test_all_methods.R** - computing root MSPE and relative root MSPE values over 10 fold cross validation for all regression methods examined using only the selected variables

### Predict Test Output
- **final_prediction.R** - producing the output predictions of the test set using the selected Random Forest regression model
- **predictions.csv** - the predicted output values for the test set

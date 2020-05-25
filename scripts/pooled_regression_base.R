# fit and forecast from a normal model
fit_normal_model = function(fitting_data) {
  # create the formula
  no_of_predictors = ncol(fitting_data) - 1
  formula = paste(names(fitting_data)[2:length(names(fitting_data))], collapse='+')
  formula = paste("y ~ ", formula, sep="")
  
  formula = as.formula(formula)
  
  # fit the model
  model = glm(formula = formula, data=fitting_data)

}
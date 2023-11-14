#'Creating a linear model of data
#'
#'This function allows one to create a linear model of a data frame.
#'
#'@param data Where the data is selected from (data frame)
#'@param x_var The name of the column in quotes of numeric data that serves as the x-variable (string)
#'@param y_var The name of the column in quotes of numeric data that serves as the y-variable (string)
#'@return The summary of the linear model information
#'
#'@export


linear_model <- function(data, x_var, y_var){
  if (!is.numeric(data[[x_var]]) || !is.numeric(data[[y_var]])){
    print("X and Y axes need to be numeric columns.")
  } else {
    lm <- data %>% 
      lm(formula = as.formula(paste({{ x_var }} , "~", {{ y_var }})))
    summary(lm)
  }}

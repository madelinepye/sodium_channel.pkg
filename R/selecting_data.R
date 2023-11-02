#'Selecting specific data columns for use
#'
#'This function allows one to clean their data by selecting specific columns out of their data frame to work with and visualize.
#'Returns the selected columns as a new data frame
#'
#'@param data Where data is selected from (data frame)
#'@param columns Name(s) of desired column(s) from initial data (string)
#'@return The selected columns (data frame)
#'
#'@export 

selecting_data <- function(data, columns){
  if (!is.character(columns)){
    print("Column argument must be a variable that represents string values or a single string value.")
  } else {
    clean <- data %>% 
    select(all_of({{columns}}))
  return(clean)}
}

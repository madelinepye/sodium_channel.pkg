#'Calculating the median
#'
#'This function allows data to be described in terms of median for a data set that is based on averages taken from individuals.
#'Returns median value
#'
#'@param data Where the data is selected from (data frame)
#'@param column The name of the column containing the numerical data (string)
#'@return The median of the data from the column selected
#'
#'@export

median_calc <- function(data, column){
  if (!is.numeric(data[[column]])){
    print("Column data must be numeric.")
    } else {    
      number <- data %>% 
      summarize(median = median(data[[column]]))
      return(number)}
}

#'Summarizes data
#'
#'This function allows one to summarize a continuous variable based on a categorical grouping variable. By doing this, one can compare the continuous data trends between the different categories of the data frame.
#'Returns a data frame of the summary, including a mean, minimum, and maximum based on the grouping variable
#'
#'@param data Where the data is selected from (data frame)
#'@param continuous_var The name of the numeric column being summarized (string)
#'@return A data frame with the summary of the data based on the grouping variable
#'
#'@export

summarize_data <- function(data, continuous_var){
 if (!is.numeric(data[[continuous_var]])){
    print("Continuous variable needs to be a numeric column.")
  } else {
  summary_data <- data %>% 
    summarize(mean_data = mean(data[[continuous_var]]),
              min_data = min(data[[continuous_var]]),
              max_data = max(data[[continuous_var]]))
  return(summary_data)}
}

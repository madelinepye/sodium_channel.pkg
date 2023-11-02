#'Creating a linear model of data
#'
#'This function allows one to create a linear model of a data frame with color variation of the scatterplot points based on a categorical variable in the data and specifically purple and pink coloring for the linear model.
#'
#'@param data Where the data is selected from (data frame)
#'@param x_var The name of the column in quotes of numeric data that serves as the x-axis (string)
#'@param y_var The name of the column in quotes of numeric data that serves as the y-axis (string)
#'@return The visual graph with the linear model
#'
#'@export


lm_plot <- function(data, x_var, y_var){
     if (!is.numeric(data[[x_var]])){
       print("X variable must be a numeric column.")
       } else  if (!is.numeric(data[[y_var]])){
         print("Y variable must be a numeric column.")
      } else {
       lm_plot <- ggplot(data, mapping = aes(data[[x_var]], data[[y_var]])) + geom_point()
       lm_plot <- lm_plot + geom_smooth(method = "lm", color = "purple", linewidth = 0.5, fill = "pink")
       return(lm_plot)}
}

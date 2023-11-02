#'Creates a scatterplot
#'
#'This function allows one to create a scatterplot of a data frame with the color of the points being determined based off of a categorical variable in the data frame.
#'Returns the visual scatterplot
#'
#'@param data Where the data is selected from (data frame)
#'@param x_data The column of the data that serves as the x-axis
#'@param y_data The column of the data that serves as the y-axis
#'@param color The column of data that will determine point colors
#'@param title The title of the scatterplot that will be above the data (string)
#'@param x_title The title of the x-axis (string)
#'@param y_title The title of the y-axis (string)
#'@return The visual scatterplot from input data
#'
#'@export

splot_sodium <- function(data, x_data, y_data, color, title, x_title, y_title){
  if (is.character(title) == FALSE){
    print("Title must be a string value.")
  } else if (is.character(x_title) == FALSE){
    print("X axis label must be a string value.")
  } else if (is.character(y_title) == FALSE){
    print("Y axis label must be a string value.")
  } else {scatterplot <- ggplot(data, aes(x = {{x_data}}, y = {{y_data}}, color = {{color}})) + geom_point()
  scatterplot1 <- scatterplot + labs(title = title, x = x_title, y = y_title)
  return(scatterplot1)}
}

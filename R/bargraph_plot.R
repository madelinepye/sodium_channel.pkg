#'Creating a bar graph
#'
#'This function allows one to create a bar graph with colored bars along with editable title and axes titles.
#'Returns the visual graph
#'
#'@param data Where the data is selected from (data frame)
#'@param x_data The column of the data that serves as the x-axis
#'@param y_data The column of the data that serves as the y-axis
#'@param color The column of data that will determine bar colors
#'@param title The title of the bar graph that will be above the data (string)
#'@param x_title The title of the x-axis (string)
#'@param y_title The title of the y-axis (string)
#'@return The visual bar graph
#'
#'@export

bargraph_plot <- function(data, x_data, y_data, color, title, x_title, y_title){
  if (is.character(title) == FALSE){
    print("Title must be a string value.")
  } else if (is.character(x_title) == FALSE){
    print("X axis label must be a string value.")
  } else if (is.character(y_title) == FALSE){
    print("Y axis label must be a string value.")
  } else {
    bar_plot <- ggplot(data, mapping = aes (x = {{x_data}}, y = {{y_data}}, fill = {{color}})) + geom_col()
    bar_plot1 <- bar_plot + labs(title = title, x = x_title, y = y_title)
    return(bar_plot1)}
}

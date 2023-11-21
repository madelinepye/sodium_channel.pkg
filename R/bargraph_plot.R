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
#'@return The bar graph with colored bars
#'
#'@export

bargraph_plot <- function(data, x_data, y_data, color, title, x_title, y_title){
  if (!is.character(title)){
    print("Title must be a string value.")
  } else if (!is.character(x_title)){
    print("X-axis label must be a string value.")
  } else if (!is.character(y_title)){
    print("Y-axis label must be a string value.")
  } else if (!is.character(data[[x_data]])){
    print("X-axis data must be catergorical data.")
  } else if (!is.numeric(data[[y_data]])){
    print("Y-axis must be numeric data.")
  } else if (!is.character(data[[color]])){
    print("The data that determines the color must be categorical.")
  } else {
    bar_plot <- ggplot(data, mapping = aes (x = !!sym(x_data), y = !!sym(y_data), fill = !!sym(color))) + geom_col()
    bar_plot1 <- bar_plot + labs(title = title, x = x_title, y = y_title)
    return(bar_plot1)}
}

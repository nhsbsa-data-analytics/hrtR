#' @title Easy helper for group chart in highcharter
#'
#' @name group_chart_hc
#'
#' @description
#' Create group chart in highcharter
#'
#' @param data the data object to pass to the chart
#' @param x the column in the data to show on the x axis
#' @param y the column in the data to show on the y axis
#' @param type the highcharter chart type to use (defaults to line)
#' @param group the column in the data which includes the groups to use in the chart
#' @param xLab the label to use on the x axis (defaults to NULL)
#' @param yLab the label to use on the y axis (defaults to NULL)
#' @param title the chart title (defaults to NULL)
#' @param dlOn TRUE/FALSE whether to include data labels on the chart (defaults to TRUE)
#' @param currency TRUE/FALSE whether chart y values are currency (GBP) (defaults to TRUE)
#' @param marker TRUE/FALSE enable markers in chart (defaults to TRUE)
#'
#' @import highcharter
#'
#' @export
#'
#' @examples
#' group_chart_hc()

group_chart_hc <- function(
  data,
  x, 
  y,
  type = "line",
  group,
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  dlOn = TRUE,
  currency = FALSE,
  marker = TRUE
) {
  # this function creates a group bar chart with NHSBSA data vis standards
  # applied. includes datalabel formatter to include "£" if needed.
  
  `%>%` <- magrittr::`%>%`
  
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  
  group <- rlang::enexpr(group)
  
  # set font to arial
  font <- "Arial"
  
  # get number of groups. max number of groups is 9 for unique colors
  num_groups <- length(unique(data[[group]]))
  
  if(num_groups <= 5){
    # set colors
    colors <- c("#03045e","#0077b6","#00b4d8","#90e0ef","#caf0f8")
    
  } else {
    # set colors
    colors <- c("#03045E","#023E8A","#0077B6","#0096C7","#00B4D8","#48CAE4","#90E0EF","#ADE8F4","#CAF0F8")
    
  }
  
  
  # check currency argument to set symbol
  if(currency == TRUE) {
    
    dlFormatter <- highcharter::JS(
      paste0(
        "function () {
        
        var ynum = this.point.",y," ;
      
        if(ynum >= 1000000000) {
      
        result = ynum.toLocaleString('en-GB', {maximumSignificantDigits: 4, minimumSignificantDigits: 4, style: 'currency', currency: 'GBP'});
      
          } else {
      
        result = ynum.toLocaleString('en-GB', {maximumSignificantDigits: 3, minimumSignificantDigits: 3, style: 'currency', currency: 'GBP'});
      
          }
      
      return result
    
        }"
      )
    )
    
  } else {
    
    dlFormatter <- highcharter::JS(
      paste0(
        "function () {

      var ynum = this.point.",y, " ;
      
      if(ynum >= 1000000000) {
      
      result = ynum.toLocaleString('en-GB', {maximumSignificantDigits: 4, minimumSignificantDigits: 4});
      
      } else {
      
       result = ynum.toLocaleString('en-GB', {maximumSignificantDigits: 3, minimumSignificantDigits: 3});
      
      }
      
      return result
      
    }"
      )
    )
    
  }
  # ifelse(is.na(str_extract(!!y, "(?<=\\().*(?=,)")),!!y,str_extract(!!y, "(?<=\\().*(?=,)")),
  
  # check chart type to set grid lines
  gridlineColor <- ifelse(type == "line", "#e6e6e6", "transparent")
  
  # check chart type to turn on y axis labels
  yLabels <- ifelse(type == "line", TRUE, FALSE)
  
  # highchart creation
  chart <- highcharter::highchart() %>% 
    highcharter::hc_chart(style = list(fontFamily = font)) %>% 
    highcharter::hc_colors(colors) %>% 
    # add only series
    highcharter::hc_add_series(data = data,
                               type = type,
                               marker = list(enabled = marker),
                               highcharter::hcaes(x = !!x,
                                                  y = !!y,
                                                  group = !!group),
                               groupPadding = 0.1,
                               pointPadding = 0.05,
                               dataLabels = list(enabled = dlOn,
                                                 formatter = dlFormatter,
                                                 style = list(textOutline = "none"))) %>% 
    highcharter::hc_xAxis(type = "category",
                          title = list(text = xLab)) %>% 
    # turn off y axis and grid lines
    highcharter::hc_yAxis(title = list(text = yLab),
                          labels = list(enabled = yLabels),
                          gridLineColor = gridlineColor,
                          min = 0) %>% 
    highcharter::hc_title(text = title,
                          style = list(fontSize = "16px",
                                       fontWeight = "bold")) %>% 
    highcharter::hc_legend(enabled = TRUE) %>% 
    highcharter::hc_tooltip(enabled = FALSE) %>% 
    highcharter::hc_credits(enabled = TRUE)
  
  # explicit return
  return(chart)
  
}

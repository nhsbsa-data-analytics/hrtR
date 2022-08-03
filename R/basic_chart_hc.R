
#' @title Easy helper for basic chart in highcharter
#'
#' @name basic_chart_hc
#'
#' @description
#' Create basic chart in highcharter
#'
#' @param data the data object to pass to the chart
#' @param x the column in the data to show on the x axis
#' @param y the column in the data to show on the y axis
#' @param type the highcharter chart type to use (defaults to line)
#' @param xLab the label to use on the x axis (defaults to NULL)
#' @param yLab the label to use on the y axis (defaults to NULL)
#' @param title the chart title (defaults to NULL)
#' @param seriesName the Series name (defaults to Series 1)
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
#'  function to build a basic chart with one series using highcharter
#'  
#'  data labels are formatted by custom JS functions that allow for currency
#'
basic_chart_hc <- function(
  data,
  x, 
  y,
  type = "line",
  xLab = NULL,
  yLab = NULL,
  title = NULL,
  seriesName = "Series 1",
  color = "#005EB8",
  dlOn = TRUE,
  currency = FALSE,
  alt_text = NULL
) {
  
  `%>%` <- magrittr::`%>%`
  
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  
  font <- "Arial"
  
  if(currency == TRUE) {
    
    dlFormatter <- highcharter::JS(
      paste0(
        "function () {
        
        var ynum = this.point.", y," ;
      
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

      var ynum = this.point.", y," ;
      
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
  
  # check chart type to set grid lines
  gridlineColor <- ifelse(type == "line", "#e6e6e6", "transparent")
  
  # check chart type to turn on y axis labels
  yLabels <- ifelse(type == "line", TRUE, FALSE)
  
  chart <- highcharter::highchart() %>% 
    highcharter::hc_chart(style = list(fontFamily = font)) %>% 
    # add only series
    highcharter::hc_add_series(data = data,
                               name = seriesName,
                               color = color,
                               type = type,
                               highcharter::hcaes(x = !!x,
                                                  y = !!y),
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
    highcharter::hc_legend(enabled = FALSE) %>% 
    highcharter::hc_tooltip(enabled = FALSE) %>% 
    highcharter::hc_credits(enabled = TRUE) %>% 
    highcharter::hc_caption(text = alt_text)
  
  return(chart)
  
}

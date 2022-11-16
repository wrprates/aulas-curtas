library(highcharter)
library(dplyr)

#' profile_chart
profile_chart <- function(
  df,
  rowheight = 225,
  ncol = 2,
  colors = "#4a57a6",
  color_num = "#4192b5",
  fontsize = "1.2em",
  js_tooltip = "function(){
    var pcnt = (this.y / this.series.data.map(p => p.y).reduce((a, b) => a + b, 0)) * 100;
    return  '<b>'+ this.series.name + '</b> - <i>' + this.x + '</i><br>' + 'Qtde.: ' + this.y + '<br>' +
            'Perc.: <b>' + Highcharts.numberFormat(pcnt) + '%</b>';
    }",
  js_tooltip_num = NULL,
  theme = highcharter::hc_theme_bloom()
) {
  purrr::map(names(df), function(x) {
    if (is.numeric(df[[x]]) | is.list(df[[x]])) {
      df_chart <-
        df[[x]] %>%
        highcharter::hchart(showInLegend = FALSE) %>%
        highcharter::hc_add_theme(theme) %>%
        highcharter::hc_title(text = x, style = list(fontSize = fontsize)) %>%
        highcharter::hc_colors(color_num) %>%
        highcharter::hc_chart(zoomType = "xy") %>%
        highcharter::hc_yAxis(title = list(text = ""))
      if (!is.null(js_tooltip_num)) {
        df_chart <- df_chart %>%
          highcharter::hc_tooltip(formatter = JS(js_tooltip_num),
                                  useHTML = FALSE)
      }
      df_chart
    } else {
      df_chart <- df %>%
        dplyr::select(paste0(x)) %>%
        dplyr::group_by(get(paste0(x))) %>%
        dplyr::summarise(n = n())  %>%
        dplyr::arrange(desc(n))

      colnames(df_chart) = c("level", "n")

      highcharter::highchart() %>%
        highcharter::hc_chart(type = "column") %>%
        highcharter::hc_xAxis(categories = df_chart$level) %>%
        highcharter::hc_add_series(
          name = x,
          data = df_chart$n,
          showInLegend = FALSE
        ) %>%
        highcharter::hc_add_theme(theme) %>%
        highcharter::hc_title(text = x, style = list(fontSize = fontsize)) %>%
        highcharter::hc_tooltip(formatter = JS(js_tooltip), useHTML = FALSE) %>%
        highcharter::hc_colors(colors) %>%
        highcharter::hc_chart(zoomType = "xy") %>%
        highcharter::hc_yAxis(title = list(text = ""))
    }
  }) %>%
    highcharter::hw_grid(rowheight = rowheight, ncol = ncol) %>% htmltools::browsable()
}

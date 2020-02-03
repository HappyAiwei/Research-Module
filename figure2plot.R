rm(list=ls())
source("RM_functions.R")

mb2 <- read.csv(file = "MedianBias2.csv", header = TRUE, sep = ",")

f2_25 <- plot_ly(mb2, x = ~a, y = ~jive25, name = 'JIVE', type = 'scatter', mode = 'lines',
                 line = list(color = 'rgb(255, 215, 0)', width = 2), showlegend = FALSE) %>%
  add_trace(y = ~tsls25, name = '2SLS', line = list(color = 'rgb(22, 96, 167)', width = 2), showlegend = FALSE) %>%
  add_trace(y = ~liml25, name = 'LIML', line = list(color = 'rgb(0, 139, 69)', width = 2), showlegend = FALSE) %>%
  add_trace(y = ~ols25, name = 'OLS', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dot'), showlegend = FALSE) %>%
  
  layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "R^2", gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list (title = "Median Bias", gridcolor = 'rgb(255,255,255)',
                       showgrid = TRUE,
                       showline = FALSE,
                       showticklabels = TRUE,
                       tickcolor = 'rgb(127,127,127)',
                       ticks = 'outside',
                       zeroline = FALSE))


f2_50 <- plot_ly(mb2, x = ~a, y = ~jive50, name = 'JIVE', type = 'scatter', mode = 'lines',
                 line = list(color = 'rgb(255, 215, 0)', width = 2)) %>%
  add_trace(y = ~tsls50, name = '2SLS', line = list(color = 'rgb(22, 96, 167)', width = 2)) %>%
  add_trace(y = ~liml50, name = 'LIML', line = list(color = 'rgb(0, 139, 69)', width = 2)) %>%
  add_trace(y = ~ols50, name = 'OLS', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dot')) %>%
  
  layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "R^2", gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list (title = "Median Bias", gridcolor = 'rgb(255,255,255)',
                       showgrid = TRUE,
                       showline = FALSE,
                       showticklabels = TRUE,
                       tickcolor = 'rgb(127,127,127)',
                       ticks = 'outside',
                       zeroline = FALSE))
r1 <- subplot(f2_25, f2_50, nrows = 1, shareY = TRUE, titleX=TRUE)%>% 
  layout(annotations = list(
    list(x = 0.2 , y = 1.05, text = "n=25", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.8 , y = 1.05, text = "n=50", showarrow = F, xref='paper', yref='paper')))


f2_100 <- plot_ly(mb2, x = ~a, y = ~jive100, name = 'JIVE', type = 'scatter', mode = 'lines',
                 line = list(color = 'rgb(255, 215, 0)', width = 2), showlegend = FALSE) %>%
  add_trace(y = ~tsls100, name = '2SLS', line = list(color = 'rgb(22, 96, 167)', width = 2), showlegend = FALSE) %>%
  add_trace(y = ~liml100, name = 'LIML', line = list(color = 'rgb(0, 139, 69)', width = 2), showlegend = FALSE) %>%
  add_trace(y = ~ols100, name = 'OLS', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dot'), showlegend = FALSE) %>%
  
  layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "R^2", gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list (title = "Median Bias", gridcolor = 'rgb(255,255,255)',
                       showgrid = TRUE,
                       showline = FALSE,
                       showticklabels = TRUE,
                       tickcolor = 'rgb(127,127,127)',
                       ticks = 'outside',
                       zeroline = FALSE))


f2_200 <- plot_ly(mb2, x = ~a, y = ~jive200, name = 'JIVE', type = 'scatter', mode = 'lines',
                 line = list(color = 'rgb(255, 215, 0)', width = 2)) %>%
  add_trace(y = ~tsls200, name = '2SLS', line = list(color = 'rgb(22, 96, 167)', width = 2)) %>%
  add_trace(y = ~liml200, name = 'LIML', line = list(color = 'rgb(0, 139, 69)', width = 2)) %>%
  add_trace(y = ~ols200, name = 'OLS', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dot')) %>%
  
  layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "R^2", gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list (title = "Median Bias", gridcolor = 'rgb(255,255,255)',
                       showgrid = TRUE,
                       showline = FALSE,
                       showticklabels = TRUE,
                       tickcolor = 'rgb(127,127,127)',
                       ticks = 'outside',
                       zeroline = FALSE))
r2 <- subplot(f2_100, f2_200, nrows = 1, shareY = TRUE, titleX=TRUE)%>% 
  layout(annotations = list(
    list(x = 0.2 , y = 1.05, text = "n=100", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.8 , y = 1.05, text = "n=200", showarrow = F, xref='paper', yref='paper')))


f2_400 <- plot_ly(mb2, x = ~a, y = ~jive400, name = 'JIVE', type = 'scatter', mode = 'lines',
                  line = list(color = 'rgb(255, 215, 0)', width = 2), showlegend = FALSE) %>%
  add_trace(y = ~tsls400, name = '2SLS', line = list(color = 'rgb(22, 96, 167)', width = 2), showlegend = FALSE) %>%
  add_trace(y = ~liml400, name = 'LIML', line = list(color = 'rgb(0, 139, 69)', width = 2), showlegend = FALSE) %>%
  add_trace(y = ~ols400, name = 'OLS', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dot'), showlegend = FALSE) %>%
  
  layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "R^2", gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list (title = "Median Bias", gridcolor = 'rgb(255,255,255)',
                       showgrid = TRUE,
                       showline = FALSE,
                       showticklabels = TRUE,
                       tickcolor = 'rgb(127,127,127)',
                       ticks = 'outside',
                       zeroline = FALSE))


f2_800 <- plot_ly(mb2, x = ~a, y = ~jive800, name = 'JIVE', type = 'scatter', mode = 'lines',
                  line = list(color = 'rgb(255, 215, 0)', width = 2)) %>%
  add_trace(y = ~tsls800, name = '2SLS', line = list(color = 'rgb(22, 96, 167)', width = 2)) %>%
  add_trace(y = ~liml800, name = 'LIML', line = list(color = 'rgb(0, 139, 69)', width = 2)) %>%
  add_trace(y = ~ols800, name = 'OLS', line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dot')) %>%
  
  layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
         xaxis = list(title = "R^2", gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list (title = "Median Bias", gridcolor = 'rgb(255,255,255)',
                       showgrid = TRUE,
                       showline = FALSE,
                       showticklabels = TRUE,
                       tickcolor = 'rgb(127,127,127)',
                       ticks = 'outside',
                       zeroline = FALSE))
r3 <- subplot(f2_400, f2_800, nrows = 1, shareY = TRUE, titleX=TRUE)%>% 
  layout(annotations = list(
    list(x = 0.2 , y = 1.05, text = "n=400", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.8 , y = 1.05, text = "n=800", showarrow = F, xref='paper', yref='paper')))

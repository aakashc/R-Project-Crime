library(rCharts)
library(shiny)
library(RCurl)
library(RJSONIO)

load('dfs.rda')
load('dfc.rda')

shinyServer(function(input, output) {
  
  output$chart1 <- renderChart({
    chart1=dPlot(x="state", y=input$crimevar,data=dfs, type="bar",width=700,height=550,bounds = list(x=50, y=10, width=600, height=350)) 
    chart1$xAxis(type="addCategoryAxis", orderRule=input$crimevar)
    chart1$yAxis(type="addMeasureAxis", outputFormat="0f")
    chart1$addParams(dom = 'chart1')
    return(chart1)
  })
  
  
  output$chart2 <- renderChart({
    cityplot=dPlot(x="cities", y=input$crimevar,data=dfc, type="bar",width=700,height=550,bounds = list(x=50, y=10, width=600, height=350)) 
    cityplot$xAxis(type="addCategoryAxis", orderRule=input$crimevar)
    cityplot$yAxis(type="addMeasureAxis",outputFormat="0f")
    cityplot$addParams(dom = 'chart2')
    return(cityplot)
  })
  
})

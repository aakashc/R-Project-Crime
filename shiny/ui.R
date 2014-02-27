library(rCharts)
library(shiny)
library(RCurl)
library(RJSONIO)

load('dfs.rda')


shinyUI(pageWithSidebar(
  h4("Crime Against Women In India in 2012 - By State and By City"),
  sidebarPanel(
    selectInput("crimevar", "Crime Variable:",
                choices = names(dfs[,c(-1)]))),
  mainPanel(
    tabsetPanel(
      tabPanel("States",showOutput("chart1","dimple")),
      tabPanel("Cities", showOutput("chart2","dimple"))
    )
  )))
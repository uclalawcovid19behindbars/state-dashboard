library(shiny)
library(plotly)
library(behindbarstools)

METRICS <- c(
    "Cumulative Cases", 
    "Cumulative Deaths", 
    "Tests Administered", 
    "Individuals Tested", 
    "Active Cases", 
    "Individuals Vaccinated (1+ dose)", 
    "Individuals Vaccinated (Fully)"
)

POPULATIONS <- c(
    "Incarcerated People", 
    "Staff"
)

STATES <- datasets::state.name %>% 
    append("Federal") %>% 
    append("ICE")

shinyUI(fluidPage(
    
    tags$style(type = "text/css", ".selectize-input {font-size: 14px;} .selectize-dropdown {font-size: 14px}"),

    titlePanel(
        h1("State & Federal Systemwide Dashboard", align = "center"), 
        windowTitle = "COVID Behind Bars Dashboard"
    ), 
    fluidRow(
        column(12, p("This is a work-in-progress internal dashboard used by the", 
        a("UCLA Law COVID-19 Behind Bars", href = "https://uclacovidbehindbars.org/"), "Data Team.",  
                     align = "center"))
    ), 
    fluidRow(
        column(12, p("It compares our scraped data to systemwide totals reported by 
                     The Marshall Project for state and federal prison systems.",  
                     align = "center"))
    ), 
    fluidRow(
        column(4, selectizeInput("state", "State", 
                              choices = STATES, 
                              width = "100%")),
        column(4, selectizeInput("metric", "Metric", 
                              choices = METRICS, 
                              width = "100%")), 
        column(4, selectizeInput("population", "Population", 
                              choices = POPULATIONS, 
                              width = "100%"))
    ), 
    fluidRow(
        column(12, plotlyOutput("plot", height = "500px"))
    ), 
    fluidRow(
        column(12, align = "center", 
               tags$footer(
                   a("UCLA COVID-19 Behind Bars", 
                     href = "https://uclacovidbehindbars.org/"), " | ", 
                   a("Dashboard Source Code", 
                     href = "https://github.com/uclalawcovid19behindbars/ts-dashboard"), " | ", 
                   a("Data Repository", 
                     href = "https://github.com/uclalawcovid19behindbars/data"))))
))


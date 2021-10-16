library(remotes)
library(shiny)
library(tidyverse)
library(behindbarstools)
library(plotly)
library(scales)

shinyServer(function(input, output) {
    
    withProgress(message = "Loading Data...", style = "old", {
        remote_state_loc <- paste0(
            "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/", 
            "master/historical-data/historical_state_counts.csv")
        state_ctypes <- parseCols(remote_state_loc)
        
        scrape <- remote_state_loc %>% 
            readr::read_csv(col_types = paste0(state_ctypes, collapse = "")) %>%
            mutate(Staff.Tadmin = NA)
    })
    
    output$plot <- renderPlotly(
        {getPlot(scrape, input$state, input$metric, input$population)})
})

parseCols <- function(path){
    jnk <- read.csv(path, nrows=1, check.names=FALSE)
    ctypes <- rep("c", ncol(jnk))
    names(ctypes) <- names(jnk)
    ctypes[stringr::str_starts(names(ctypes), "Residents|Staff")] <- "d"
    ctypes[names(ctypes) == "Population.Feb20"] <- "d"
    ctypes[names(ctypes) == "Date"] <- "D"
    
    return(ctypes)
}

getPlot <- function(df, state, metric, population){
    
    variable <- getMetric(metric, population)
    
    filtered_df <- df %>% 
        filter(State == state) %>% 
        filter(!is.na(!!sym(variable)))
    
    plt <- ggplot()  
    
    if (nrow(filtered_df) == 0){
        plt <- plt 
    }
    else {
        plt <- plt + 
            geom_line(data = filtered_df, 
                      mapping = aes(x = Date, y = !!sym(variable), 
                                    text = sprintf("UCLA Behind Bars<br>Date: %s<br>%s: %s", 
                                                   Date, 
                                                   metric, 
                                                   comma(!!sym(variable), accuracy = 1))), 
                      size = 1.0, color = "#D7790F", group = 1)
    } 
    
    plt <- plt + 
        labs(title =  str_c(metric, " Among ", population, "\n", state)) + 
        scale_x_date(date_labels = "%b %Y", limits = c(as.Date("2020-03-01"), Sys.Date())) +
        scale_y_continuous(labels = comma_format(accuracy = 1)) +
        customTheme
    
    font <- list(
        family = "Helvetica",
        size = 15,
        color = "black"
    )
    
    label <- list(
        bgcolor = "#EFEEEC",
        bordercolor = "transparent",
        font = font
    )
    
    print(ggplotly(plt, tooltip = "text") %>% 
          style(hoverlabel = label) %>%
          layout(font = font)
    )
}    

getMetric <- function(metric, population){
    lookup_residents <- c(
        "Cumulative Cases" = "Residents.Confirmed", 
        "Cumulative Deaths" = "Residents.Deaths", 
        "Tests Administered" = "Residents.Tadmin", 
        "Active Cases" = "Residents.Active", 
        "Individuals Vaccinated (1+ dose)" = "Residents.Initiated",
        "Individuals Vaccinated (Fully)" = "Residents.Completed"
    )
    
    lookup_staff <- c(
        "Cumulative Cases" = "Staff.Confirmed", 
        "Cumulative Deaths" = "Staff.Deaths", 
        "Tests Administered" = "Staff.Tadmin", 
        "Active Cases" = "Staff.Active", 
        "Individuals Vaccinated (1+ dose)" = "Staff.Initiated", 
        "Individuals Vaccinated (Fully)" = "Staff.Completed" 
    )
        
    if (population == "Incarcerated People"){
        lookup_residents[metric] %>% 
            unname()
    
    } else if (population == "Staff"){
        lookup_staff[metric] %>% 
            unname()
    }
}

customTheme <- 
    theme_classic(base_size = 12) +     
    theme(axis.title.y = element_blank(), 
          axis.title.x = element_blank(), 
          plot.title = element_text(hjust = 0.5), 
          panel.grid.major.y = element_line(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()
    )

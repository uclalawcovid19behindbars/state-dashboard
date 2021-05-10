library(remotes)
library(shiny)
library(tidyverse)
library(behindbarstools)
library(plotly)
library(scales)

shinyServer(function(input, output) {
    
    withProgress(message = "Loading Data...", style = "old", {
        
        # Read UCLA data 
        remote_loc <- "http://104.131.72.50:3838/scraper_data/summary_data/scraped_time_series.csv"
        jnk <- read.csv(remote_loc, nrows=1, check.names=FALSE)
        ctypes <- rep("c", ncol(jnk))
        names(ctypes) <- names(jnk)
        ctypes[stringr::str_starts(names(ctypes), "Residents|Staff")] <- "d"
        ctypes[names(ctypes) == "Population.Feb20"] <- "d"
        ctypes[names(ctypes) == "Date"] <- "D"
        
        ucla <- remote_loc %>% 
            readr::read_csv(col_types = paste0(ctypes, collapse = "")) %>%
            filter(!Age %in% c("Juvenile")) %>% 
            mutate(State = ifelse(Jurisdiction == "federal", "Federal", State)) %>%
            mutate(State = ifelse(Jurisdiction == "immigration", "ICE", State)) %>%
            filter(Jurisdiction %in% c("state", "federal", "immigration")) %>% 
            group_by(Date, State) %>% 
            summarise_if(is.numeric, sum_na_rm)
        
        # Read MP data 
        mp_data_wide <- read_mpap_data(all_dates = TRUE)
        mp <- mp_data_wide %>%
            filter(!is.na(Date)) %>%
            mutate(Date = lubridate::floor_date(Date, "week")) %>%
            tidyr::pivot_longer(
                -(State:Date), names_to = "Measure", values_to = "MP") %>%
            group_by(State, Date, Measure) %>%
            summarize(MP = max_na_rm(MP), .groups = "drop") %>% 
            select(State, Date, Measure, MP) %>% 
            pivot_wider(names_from = "Measure", values_from = "MP") 
    })
    
    output$plot <- renderPlotly(
        {getPlot(mp, ucla, input$state, input$metric, input$population)})
})

getPlot <- function(mp_df, ucla_df, state, metric, population){
    
    variable <- getMetric(metric, population)
    
    filtered_mp <- mp_df %>% 
        filter(State == state) %>% 
        filter(!is.na(!!sym(variable)))
    
    filtered_ucla <- ucla_df %>% 
        filter(State == state) %>% 
        filter(!is.na(!!sym(variable)))
    
    plt <- ggplot()
    
    if (nrow(filtered_mp) == 0 & nrow(filtered_ucla) == 0){
        plt <- plt +
            scale_x_date(date_labels = "%b %Y", limits = c(as.Date("2020-03-01"), Sys.Date())) 
    }
    if (nrow(filtered_mp) > 0){
        plt <- plt + 
            geom_bar(data = filtered_mp, 
                     mapping = aes(x = Date, y = !!sym(variable), 
                                   text = sprintf("The Marshall Project<br>Date: %s<br>%s: %s", 
                                                  Date, 
                                                  metric, 
                                                  comma(!!sym(variable), accuracy = 1))), 
                     stat = "identity", alpha = 0.3, fill = "#84816F")
    }
    if (nrow(filtered_ucla) > 0){
        plt <- plt + 
            geom_line(data = filtered_ucla, 
                      mapping = aes(x = Date, y = !!sym(variable), 
                                    text = sprintf("UCLA Behind Bars<br>Date: %s<br>%s: %s", 
                                                   Date, 
                                                   metric, 
                                                   comma(!!sym(variable), accuracy = 1))), 
                      size = 1.0, color = "#D7790F", group = 1)
    } 
    
    plt <- plt + 
        labs(title =  str_c(metric, " Among ", population, "\n", state)) + 
        scale_x_date(date_labels = "%b %Y") +
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
        "Individuals Tested" = "Residents.Tested", 
        "Individuals Recovered" = "Residents.Recovered"
    )
    
    lookup_staff <- c(
        "Cumulative Cases" = "Staff.Confirmed", 
        "Cumulative Deaths" = "Staff.Deaths", 
        "Tests Administered" = "Staff.Tadmin", 
        "Individuals Tested" = "Staff.Tested", 
        "Individuals Recovered" = "Staff.Recovered"
    )
        
    if (population == "Incarcerated People"){
        lookup_residents[metric] %>% 
            unname()
    
    } else if (population == "Staff"){
        lookup_staff[metric] %>% 
            unname()
    }
}

max_na_rm <- function(x){
    if(all(is.na(x))){
        return(NA)
    }
    max(x, na.rm = TRUE)
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

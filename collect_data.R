library(quantmod)
library(stringr)
library(readr)
library(dplyr)
library(sparkline)
library(shiny)
library(tidyr)
library(ggplot2)
library(zoo)

covid_quant <- read_csv("covid_quant.csv")

#convert the quantmod download into something standardised
process_quant_data <- function(quant_data) {
  
  processed_data <- quant_data %>% 
    as.data.frame() %>%
    select(4) %>%
    tibble::rownames_to_column() %>%
    rename(date = 1, close = 2) %>%
    tidyr::fill(close) %>%
    mutate(date= lubridate::parse_date_time(date, orders = "%Y-%m-%d")) %>%
    filter(date >= "2020-01-01")
  
  return(processed_data)
  
}

#standardised ggplot graph for the processed data
standard_graph <- function(processed_data) {
  
  processed_data %>%
    ggplot(aes(x=date, y= close, group = 1)) +
    theme_classic()  +
    geom_line(size = 1, color = "black") +
    #scale_x_discrete(name = "Date") +
    scale_y_continuous(name="Value") +
    theme(
      panel.grid.major.y = element_line(colour = "gray"),
      text = element_text(size=20)#,
      #axis.text.x = element_text(angle=0,hjust = 1, vjust = -2) 
      #axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
      #axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank()
    )
}

for(code in covid_quant$yahoo_code) {
  
  print(code)
  getSymbols(code)
  code2 <- str_remove(code, "[/^]")
  
  spark_data <- get(code2) %>%
    process_quant_data()
  
  covid_quant$spark[covid_quant$yahoo_code == code] <- spk_chr(spark_data$close)
  
  quote <- getQuote(code)
  
  change_year_start <- ((quote$Last / slice(spark_data,1)$close) - 1 ) * 100
  change_week <- ((quote$Last / slice(spark_data,n()-5)$close))
                        
  covid_quant$current[covid_quant$yahoo_code == code] <- round(quote$Last,2)
  covid_quant$time[covid_quant$yahoo_code == code] <- 
    as.character(
      quote$`Trade Time`
    )
  
  
  #covid_quant$change_today[covid_quant$yahoo_code == code] <- round(quote$Change,2)
  covid_quant$todays_per_change[covid_quant$yahoo_code == code] <- paste0(round(quote$`% Change`,2), "%")
  covid_quant$week_per_change[covid_quant$yahoo_code == code] <- paste0(round(change_week, 2), "%")
  covid_quant$change_since_year_start[covid_quant$yahoo_code == code] <- paste0(round(change_year_start,2), "%")
  
}



filename <- paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", 
                   Sys.Date()-0,
                   ".xlsx")
filename2 <- paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", 
                   Sys.Date()-1,
                   ".xlsx")
filename3 <- paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-", 
                   Sys.Date()-2,
                   ".xlsx")


#download.file("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-16.xls", "my_covid.xls")

t <- try(
download.file(filename, "ecdc_covid.xlsx",
              #method = "wininet",
              mode = "wb"))

if(class(t) == "try-error") {
t2 <- try( download.file(filename2, "ecdc_covid.xlsx",
                      #method = "wininet",
                      mode = "wb"))
} else if (class(t2) == "try-error") {
  t2 <- try( download.file(filename3, "ecdc_covid.xlsx",
                           #method = "wininet",
                           mode = "wb"))
}

try(
download.file("https://www.arcgis.com/sharing/rest/content/items/e5fd11150d274bebaaf8fe2a7a2bda11/data",
              "covid.xlsx",
              mode = "wb")
)

covid_cases2 <-readxl::read_xlsx("my_covid.xlsx")
covid_cases3 <-readxl::read_xlsx("covid.xlsx")

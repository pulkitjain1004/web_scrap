# Created by Pulkit Jain
# Creation Date: 2/18/2018
# Purpose: Coding Assesment for Big Data Federation. Analysis of Gold/Silver Prices.



getData <- function(url_gold, url_silver){
  
  # Web scrapping for gold and silver prices
  # Save the data in directory raw_data
  
  # XPath is   //*[(@id = "curr_table")]
  # Used Google Chrome plugin SelectorGadget

  raw_silver <- url_silver %>%
  read_html() %>%
  html_nodes(xpath= '//*[(@id = "curr_table")]') %>%
  html_table()

  raw_gold <- url_gold %>%
    read_html() %>%
    html_nodes(xpath= '//*[(@id = "curr_table")]') %>%
    html_table()

  if(!dir.exists("raw_data")){
    dir.create("raw_data")
  }

  write.csv(raw_gold, file ="raw_data/raw_gold.csv")
  write.csv(raw_silver, file ="raw_data/raw_silver.csv")
  
  message("Raw Data for Gold and Silver Prices have been saved in directory raw_data")
}


readData <- function(commodity){
  
  # Read data from locally stored file
  
  if( tolower(commodity) == "gold" ){
    read_data <- read.csv("raw_data/raw_gold.csv", header = T, stringsAsFactors = F)
  } else {
    read_data <- read.csv("raw_data/raw_silver.csv", header = T, stringsAsFactors = F)
  }
  
  # str(read_data)
  # Change structure of Date from string to Date Format
  
  read_data$Date <- as.Date(read_data$Date,format='%b %d, %Y')
  read_data$Price <- gsub(",", "", read_data$Price)
  read_data$Price <- as.numeric(read_data$Price)
  # str(read_data)
  
  return(read_data)
}


getCommodityPrice <- function(stDate, endDate, commodity){
  
  # commodity = "Silver"
  # stDate = '2018-01-25'
  # endDate = '2018-02-08'
  
  # Filter Data
  
  result <- readData(commodity) %>% select(Date, Price) %>%
    filter(between(Date,as.Date(stDate), as.Date(endDate))) %>%
    summarize(Mean = mean(Price), Variance = var(Price))
  
  result = c(Commodity = commodity, result)
  # print(result)
  
  return(result)
}
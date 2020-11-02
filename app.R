

# __________________________________________________________________________________________________
# __________________________________________________________________________________________________


# Calling required libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(quantmod)
library(tidyquant)
library(plotly)
library(shinythemes)


# __________________________________________________________________________________________________
# __________________________________________________________________________________________________


# Function to fetch ticker price data
getdata <- function(ticker,start,end){
  start <- as.Date(start)
  end <- as.Date(end)
  
  dt <- getSymbols(
    ticker,
    from = start,
    to = end,
    auto.assign = F
  )
  return(dt)
}

# Function to process fetched data to fit AH traaing strategy
sellOpen_buyClose <- function(pricedt){
  dt <- pricedt %>%
    as.data.frame() %>% 
    rownames_to_column("date") %>% 
    mutate(date = as.Date(date))
  
  names(dt) <- names(dt) %>% str_extract(pattern= "\\w+$") %>% tolower()
  
  dt <- dt %>% 
    select(date,open,close) %>% 
    mutate(
      prevclose = lag(close,n=1L),
      pct = case_when(
          !is.na(prevclose) ~ (open)/prevclose*1,
          is.na(prevclose) ~ 1
        ),
      `(Afterhours) Buy at Close, Sell Next Open` = cumprod(pct)
    ) %>% 
    select(date,`(Afterhours) Buy at Close, Sell Next Open`)
  
  return(dt)
}

# Function to process fetched data to fit intraday trading strategy
sellClose_buyOpen <- function(pricedt){
  dt <- pricedt %>%
    as.data.frame() %>% 
    rownames_to_column("date") %>% 
    mutate(date = as.Date(date))
  
  names(dt) <- names(dt) %>% str_extract(pattern= "\\w+$") %>% tolower()
  
  dt <- dt %>% 
    select(date,open,close) %>% 
    mutate(
      pct = close/open,
      `(Intraday) Buy at Open, Sell at Close` = cumprod(pct)
    ) %>% 
    select(date,`(Intraday) Buy at Open, Sell at Close`)
  
  return(dt)
}

# Function to process fetched data to fit buy-and-hold strategy
hodl <- function(pricedt){
  dt <- pricedt %>%
    as.data.frame() %>% 
    rownames_to_column("date") %>% 
    mutate(date = as.Date(date))
  
  names(dt) <- names(dt) %>% str_extract(pattern= "\\w+$") %>% tolower()
  
  dt <- dt %>% 
    select(date,open,close) %>% 
    mutate(
      prevclose = lag(close,n=1L),
      pct = case_when(
          !is.na(prevclose) ~ (close)/prevclose*1,
          is.na(prevclose) ~ 1
        ),
      `(Overall) Passive Investment` = cumprod(pct)
    ) %>% 
    select(date,`(Overall) Passive Investment`)
  
  return(dt)
}

# Joining processed data from previous functions and drawing ggplotly output
returns_plot <- function(ticker,pricedt){
  gg <- sellOpen_buyClose(pricedt) %>% 
  left_join(sellClose_buyOpen(pricedt)) %>% 
  left_join(hodl(pricedt)) %>% 
  pivot_longer(
    cols = -date,
    names_to = "Strategy",
    values_to = "returns"
  ) %>% 
  ggplot(
    aes(
      x = date,
      y = returns*100-100,
      col = Strategy,
      group=1,
      text = paste(
          "Date: ",date, 
          "\n% Gains:", returns*100-100, 
          "\nTime Strategy:",Strategy
        )
    )
  )+
  geom_line()+
  theme_minimal()+
  labs(
    title = paste("Percentage Change in Value for $", ticker, " for Intraday, AH, and Overall",sep =""),
    x = "Date",
    y = "% Change",
    col = " "
  )+
  scale_color_viridis_d()
  
  ggplotly(
    gg,
    tooltip = "text"
  ) %>% 
  layout(
    font = list(color = 'white'),
    plot_bgcolor='rgba(254,247,234,0)',
    paper_bgcolor='rgba(254,247,234,0)',
    xaxis = list(
      rangeslider = list(type = "date")
    )
  )
}
 
# Main function
plot_strats <- function(ticker,start,end){
  dt <- getdata(ticker,start,end) 
  returns_plot(ticker,dt)
}


# __________________________________________________________________________________________________
# __________________________________________________________________________________________________


# (INPUT) Defining UI for application 
ui <- fluidPage(
  
    theme = shinytheme("simplex"),

    # Application title
    titlePanel("Quick Backtesting for Intraday, Afterhours, and Persistent Investment"),

    # Sidebar with inputs 
    sidebarLayout(
        sidebarPanel(
            textInput("ticker",
                      label = h3("Type in Ticker (All CAPS):"),
                      value = "SPY"),
            dateInput("start",
                      label = h3("Start Date (for 100% baseline): "),
                      value = "2000-01-01"),
            dateInput("end",
                      label = h3("End Date"),
                      value = "2020-11-02"),
            submitButton(text = "Generate Plot")
        ),

        # Show plot
        mainPanel(
           plotlyOutput("outplot")
        )
    )
)


# __________________________________________________________________________________________________
# __________________________________________________________________________________________________


# (OUTPUT) Calling defined functions to render output plot
server <- function(input, output) {

    output$outplot <- renderPlotly({
        plot_strats(input$ticker,
                    input$start,
                    input$end)
    })
}


# __________________________________________________________________________________________________
# __________________________________________________________________________________________________


# Running the application 
shinyApp(ui = ui, server = server)
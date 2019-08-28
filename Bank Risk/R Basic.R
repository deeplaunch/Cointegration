setwd("U:\\My Documents\Training\Quantitative Banking II\Quantitative_Methods_In_Banking")

# Reverse/Sort vector
x <- c(1, 2, 3) #"c" for "concatentate"
rev(x)
sort(x, index= TRUE)

# Data pipeline
library(magtittr)

# Datafram: write functions on entire rows/columns (excel is on cells)
library(quantmod)

nasdaq_names <- stockSymbols(exchange = "NASDAQ")
amex_names <- stockSymbols(exchange = "AMEX")
nyse_names <- stockSymbols(exchange = "NYSE")

company_names <- rbind(nasdaq_names,amex_names, nyse_names)

# Create an HTML table widget using the DataTables library
library(DT)
datatable(company_names, filter = "top") # then "export"

# Webscrapping
library(rvest)
library(magtittr)
ulr = "https://finance.yahoo.com/quote/AAPL/profile?p=AAPL"
doc = read_html(url)
tbl = doc%>%html_nodes("table")%>%html_table()
tbl

# Run a script written elsewhere
source()

# Aggregate Data
data <- read.csv("DST")

# Calculate on conditions- "which" function returns the index
idx = which(!is.na(co_names$IPOyears))
df = co_names[idx,]

# Interactive plotting (add things step by step)
library(rbokeh)
p = figure(width=500, height=300) %>% 
    ly_points((IPOyear, numIPO), data=res, hover=c(IPOyear, numIPO)) %>% 
    ly_lines()

# Resources- CRAN: R archive
# Print

## Decision Tree Algorithm 
if (!require("readxl")) install.packages("readxl")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("knitr")) install.packages("knitr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("arules")) install.packages("arules")

library(readxl)
library(lubridate)
library(dplyr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(arules)
## put the current path where the dataset file is
Online_Retail <- read_excel("D:/data_mining/group project/Online Retail ex.xls")
Online_Retail$InvoiceDate <- as.POSIXct(Online_Retail$InvoiceDate, format = "%Y-%m-%d %H:%M:%S %p")
Online_Retail <- Online_Retail %>%
  mutate(
    Date = date(InvoiceDate),
    Time = format(InvoiceDate, "%H:%M:%S"),
    AMPM = format(InvoiceDate, "%p"),
    Description = as.character(Description)
  )%>%
  filter(!is.na(CustomerID))
View(Online_Retail)
glimpse(Online_Retail)

#What time do people often purchase online

# Extract the hour from the invoice_date
Online_Retail$purchase_hour <- format(Online_Retail$InvoiceDate, "%H")

# Aggregate the data to count the number of purchases per hour
hourly_purchases <- Online_Retail %>%
  group_by(purchase_hour) %>%
  summarise(purchases = n())

# Plot the frequency of purchases by hour
ggplot(hourly_purchases, aes(x = purchase_hour, y = purchases)) +
  geom_bar(stat = "identity", fill = "indianred") +
  xlab("Hour of Day") +
  ylab("Number of Purchases") +
  ggtitle("Frequency of Online Purchases by Hour of Day")

peak_times <- Online_Retail %>%
  group_by(purchase_hour) %>%
  summarise(NumberOfInvoices = n_distinct(InvoiceNo)) %>%
  arrange(purchase_hour)

ggplot(peak_times, aes(x=purchase_hour, y=NumberOfInvoices)) +
  geom_line(group=1, color="blue") +
  geom_point(color="red") +
  labs(title="Shopping Activity by Hour", x="Hour of Day", y="Number of Invoices")

#Top 10 Best Sellers
top_sellers <- Online_Retail %>%
  group_by(Description) %>%
  summarise(TotalSales = sum(Quantity, na.rm = TRUE)) %>%
  arrange(desc(TotalSales)) %>%
  top_n(10, TotalSales)
ggplot(top_sellers, aes(x = reorder(Description, TotalSales), y = TotalSales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # Flips the axes for horizontal bars
  labs(title = "Top 10 Best Sellers", x = "Total Sales", y = "") +
  theme_minimal()

#How many items each customer buy
Online_Retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(Items = mean(Quantity)) %>%
  ggplot(aes(x=Items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

# Graph to show product popularity through description
# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
# Load data set
## put the current path where the dataset file is
Online_Retail_ex <- read_excel("D:/data_mining/group project/Online Retail ex.xlsx")
View(Online_Retail_ex)
# Count the number of unique descriptions
unique_descriptions <- n_distinct(Online_Retail_ex$Description)
# Calculate description counts
description_counts <- Online_Retail_ex  %>%count(Description) %>% arrange(desc(n)) %>% slice(1:30)
# Plotting
ggplot(description_counts, aes(x = Description, y = n)) + geom_bar(stat = "identity", fill = "#6a0dad") + coord_flip() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(x = "Product Description", y = "Counts", title = "Product description popularity") + theme_minimal()

# Sales of Product in different countries
# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
Online_Retail_ex <- read_excel("D:/data_mining/group project/Online Retail ex.xlsx")
View(Online_Retail_ex)
# Calculate country counts
country_counts <- Online_Retail_ex %>%  count(Country) %>% arrange(desc(n)) %>%slice(1:30)
# Create a bar plot
ggplot(country_counts, aes(x = Country, y = n, fill = Country)) +  geom_bar(stat = "identity") +  scale_fill_brewer(palette = "Purples_r", direction = -1) + coord_flip() +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +   labs(x = "Country", y = "Count", title = "Top 30 Countries") + scale_y_log10() +    theme_minimal()


# Show the data with respect to country which returns most of the products
# Load data set
Online_Retail_ex <- read_excel("D:/data_mining/group project/Online Retail ex.xlsx")
View(Online_Retail_ex)
print(Online_Retail_ex)
correct_data <- na.omit(Online_Retail_ex)
print(correct_data)
#return_data containing only the rows where the Quantity column is less than zero
return_data <- correct_data[correct_data$Quantity < 0, ]
print(return_data)
# Count occurrences of each country
country_data <- return_data %>%count(Country) %>%arrange(desc(n)) %>%slice(1:30)
# Create the bar plot
ggplot(country_data, aes(x = Country, y = n, fill = n)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_gradient(low = "#f2e6ff", high = "#6a0dad", guide = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Country", y = "Count", title = "Top 30 countries by count") +
  scale_y_log10() +
  coord_flip()

## Correlation Matrix
# Load data set
Online_Retail_ex <- read_excel("D:/data_mining/group project/Online Retail ex.xlsx")
View(Online_Retail_ex)
print(Online_Retail_ex)
correct_data <- na.omit(Online_Retail_ex)
print(correct_data)
library(lubridate)
## Let's move to daily sales
correct_data$InvoiceDate <- as_datetime(correct_data$InvoiceDate)
# Convert 'InvoiceDate' column to date-time format
correct_data$InvoiceDate <- as_datetime(correct_data$InvoiceDate)
# Calculate revenue
correct_data$Revenue <- correct_data$Quantity * correct_data$UnitPrice
# Extract year
correct_data$Year <- year(correct_data$InvoiceDate)
# Extract quarter
correct_data$Quarter <- quarter(correct_data$InvoiceDate)
# Extract month
correct_data$Month <- month(correct_data$InvoiceDate)
# Extract week
correct_data$Week <- isoweek(correct_data$InvoiceDate)
# Extract weekday (0 for Monday, 1 for Tuesday, ..., 6 for Sunday)
 correct_data$Weekday <- wday(correct_data$InvoiceDate)
# Extract day
correct_data$Day <- day(correct_data$InvoiceDate)
# Assuming correct_data is your dataframe
correct_data_sorted <- correct_data %>% 
  +     arrange(desc(Revenue)) %>%  # Sort dataframe by 'Revenue' column in descending order
  +     slice(1:5)                  # Select the top 5 rows
# Print the first few rows
print(correct_data_sorted)
## Here we can see the top items that make the most revenue.


# Sort dataframe by 'Revenue' column in descending order and count occurrences of each revenue value
top_revenue_data <- correct_data %>%arrange(desc(Revenue)) %>%count(Revenue) %>%  slice(1:30)
# Create the bar plot
ggplot(top_revenue_data, aes(x = factor(Revenue), y = n)) +
      geom_bar(stat = "identity", fill = "#6a0dad") +
    labs(x = "Revenue", y = "Count", title = "Top 30 Revenue Data") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


## From this we can see the sweet spot is -8.5 where we can get the maximum sales

summary(correct_data[c("Quantity", "Revenue")])
## Let's check which are popular days to shop.


# Sort the data by 'Revenue' in descending order and count occurrences of each day
day_data <- correct_data %>%arrange(desc(Revenue)) %>%  count(Day)
# Create the bar plot
ggplot(day_data, aes(x = factor(Day), y = n)) +
  geom_bar(stat = "identity", fill = "#6a0dad") +
  labs(x = "Days of week", y = "Frequency", title = "Frequency of Days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "dotted"))



## look like day 6 is very popular
##Let's check which month is very popular

# Sort the data by 'Revenue' in descending order and count occurrences of each month
month_data <- correct_data %>%
  arrange(desc(Revenue)) %>%
  count(Month)

## Looks like November is the most popular month among shoppers.

# Filter the data for November and sort it by 'Revenue' in descending order
november_data <- correct_data %>%
    filter(Month == 11) %>%
      arrange(desc(Revenue))
# Select the first few rows and reset the index
 november_data <- november_data %>% slice(1:5) 
 # Print the resulting dataframe
  print(november_data)
  # Print the resulting dataframe
   print(november_data)

   # Create the bar plot
ggplot(november_days_data, aes(x = factor(Day), y = n)) +
     geom_bar(stat = "identity", fill = "#6a0dad") +
     labs(x = "Days of November", y = "Frequency", title = "Revenues of days during November") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
     theme(panel.grid.major = element_line(colour = "gray", linetype = "dotted"))  
 
## look like 3rd nov is the most revenue day in month nov    
# Count occurrences of each StockCode in the november_data dataframe
stockcode_counts <- november_data %>% 
  count(StockCode)
# Count occurrences of each StockCode in the november_data dataframe
stockcode_counts <- november_data %>% 
  count(StockCode)

# Select the top 30 occurrences
november_stock_data <- head(stockcode_counts, 30)           
# Create the bar plot
ggplot(november_stock_data, aes(x = factor(StockCode), y = n)) +
  geom_bar(stat = "identity", fill = "#6a0dad") +
  labs(x = "Stock codes sales for November", y = "Frequency", title = "Top 30 Stock Code Sales for November") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "dotted"))

## Looks like product code 23355 is very popular during month of November.
# Count occurrences of each StockCode in the correct_data dataframe
stockcode_counts <- correct_data %>% 
  count(StockCode)

# Select the top 30 occurrences
stock_data <- head(stockcode_counts, 30)
# Create the bar plot
ggplot(stock_data, aes(x = factor(StockCode), y = n)) +
  geom_bar(stat = "identity", fill = "#6a0dad") +
  labs(x = "Stock codes sales during the year", y = "Frequency", title = "Top 30 Stock Code Sales during the Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "dotted"))

## Looks like stock code 15056 is popular during the year opposed to code 23355 during the month of November.
##Now let's move to Preprocessing of data and start removing outliers
# Calculate the 1st and 99th percentiles for 'Revenue'
low_revenue <- quantile(correct_data$Revenue, 0.01)
high_revenue <- quantile(correct_data$Revenue, 0.99)

# Calculate the 1st and 99th percentiles for 'Quantity'
low_quantity <- quantile(correct_data$Quantity, 0.01)
high_quantity <- quantile(correct_data$Quantity, 0.99)
# Filter data based on revenue and quantity
correct_data <- correct_data %>%
  filter(Revenue >= low_revenue & Revenue <= high_revenue,
         Quantity >= low_quantity & Quantity <= high_quantity)

# Remove infinite values
correct_data <- correct_data[!is.infinite(correct_data$Revenue),]

# Summarize the data
summary(correct_data)


# Create the histogram plot
ggplot(correct_data, aes(x = Quantity)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(x = "Daily product sales", y = "Frequency", title = "Histogram of Daily Product Sales") +
  theme_minimal()

## We can see the lower quantities are popular during purchase

correct_data$DescriptionLength <- nchar(correct_data$Description)
# Sort the 'DescriptionLength' values in descending order and count their occurrences
description_len_data <- correct_data %>%
  arrange(desc(DescriptionLength)) %>%
  count(DescriptionLength)

# Select the top 30 values
description_len_data <- head(description_len_data, 30)

install.packages("corrplot")
library(corrplot)


# Select relevant columns for correlation analysis
correlation_data <- correct_data[c('UnitPrice', 'Revenue', 'CustomerID', 'Day', 'DescriptionLength', 'Year', 'Month')]

# Calculate correlation matrix
correlation_matrix <- cor(correlation_data)

# Plot correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

## This we can see by correlation matrix how month is related to revenue of the product and its unit price.


#EURIRS TIME SERIES

#1 - IMPORT THE DATA
library(tidyverse)
library(rvest)
options (digits = 2)
rm(list=ls())

# Reading EURIRS data from website (updated daily)
url <- "https://www.euribor.it/eurirs-oggi/"
page <- read_html(url) #Creates an html document from URL
series <- html_table(page, fill = TRUE) #Parses tables into data frames, page contains only one table
series <- data.frame(series) # not even sure if this command is needed

# taming the date (greatest issue in all my scripts)
series$EURIRS <- format(paste(series$X.EURIRS, "/2022", sep=""), format = "%d/%m/%Y")

#removing erroneous date column
series <- select (series,-X.EURIRS)

#Tidy data by making a table where every column is a variable, every row is an observation, every cell is a single value
series2 <- series %>%
  pivot_longer(c("X10.ANNI","X15.ANNI","X20.ANNI","X25.ANNI","X30.ANNI" ), names_to = "Rate_Name", values_to = "Rate_Value") %>%
  arrange (as.Date(EURIRS, "%d/%m/%Y"), Rate_Name)

#1.1 OPTIONAL - DO IT WHEN THE NEW YEAR ARRIVES: SAVE 2021 DATA INTO AN CSV FILE
#write.csv(x = series2, file = "data_past_years/2021.csv")

#1.2 OPTIONAL - DO IT IF YOU HAVE SAVED PAST YEAR DATA INTO CSV AND WANT TO PLOT IT ALONG CURRENT YEAR
past_years <- read.csv(file = "data_past_years/2021.csv")
past_years <- select(past_years,-X) #removing colum that appeared????

#1.3 OPTIONAL - APPEND THE TWO DATAFRAMES TOGETHER, SO YOU CONCATENATE THE YEARS ONE AFTER ANOTHER
series2 <- rbind(past_years,series2)

#1.4 ADDING PMT COLUMN WITH RELATED INSTALLMENT FOR A CERTAIN DEBT CONTRACT AT GIVEN PARAMETERS
# any bank will apply a pricing over the base rata
bank_spread <- 0.73
# how much money will the lender ask?
C <- 100800

# adds payment in the data frame, fomula calculates the pym (posticipated)
# the interest i is the sum of base and bank spread (ask your bank) divided by 100 to have a rate in percentage and by 12 to have a monthly rate, so 1200
# the number of periods n is calculated from the rate name which contains the number of years (substr...) * 12 to have the number of months in the period 300 in 25 years for instance
series2 <- series2 %>%
  mutate (pmt = as.numeric((1 + 1/(((1+(bank_spread+Rate_Value)/1200))^(as.numeric(substr(Rate_Name,2,3))*12) - 1))*
         ((bank_spread+Rate_Value)/1200)*C))


#2 - USE THE DATA
# select one particular rate - for later use
my_rate <- "X25.ANNI" #my morgage will be with this base rate... that's why is MY rate....

# Calculating Mean and SD to plot h-lines if needed
# mean value in the year
avg <- series2 %>%
  filter(Rate_Name == my_rate) %>%
  pull(Rate_Value) %>%
  mean()

# std dev value in the year
sd <- series2 %>%
  filter(Rate_Name == my_rate) %>%
  pull(Rate_Value) %>%
  sd()

# last value, for the chart title
last_rate <- series2 %>%
  filter(Rate_Name == my_rate & as.Date(EURIRS, "%d/%m/%Y") == max(as.Date(EURIRS, "%d/%m/%Y"))) %>%
  pull(Rate_Value)

#plot into GGPLOT2
series2 %>%
  filter (Rate_Name == my_rate) %>%   # comment this line to plot all rates, or use this line to focus in a single rate only
  ggplot (aes(x= as.Date(EURIRS, "%d/%m/%Y"), y=Rate_Value, color=Rate_Name)) +
  geom_hline(yintercept = avg, linetype = "dashed",  size = 0.5, color = "blue") + # these 3 lines make sense only if you focus on a single rate
  geom_hline(yintercept = avg-sd, linetype = "dashed", size = 0.3, color = "red") + # these 3 lines make sense only if you focus on a single rate
  geom_hline(yintercept = avg+sd, linetype = "dashed", size = 0.3, color = "red") + # these 3 lines make sense only if you focus on a single rate
  geom_point(size = 1) +
  geom_line ()+
  xlab("")+
  ylab("Rates") +
  ggtitle (paste("EURIRS Rates trend - Rate Name:", my_rate,"- Last Value:", last_rate)) +
  theme_bw() +
  theme (legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1))

### next developments:
#1 add in the chart the value of the pmt every each---- 15 days?
#x= seq (min(as.Date(series2$EURIRS, "%d/%m/%Y")), max((as.Date(series2$EURIRS, "%d/%m/%Y"))), by = 15)
# some code      
#max (as.Date(series2$EURIRS, "%d/%m/%Y"))
#min (as.Date(series2$EURIRS, "%d/%m/%Y"))
#c <- seq (min(as.Date(series2$EURIRS, "%d/%m/%Y")), max((as.Date(series2$EURIRS, "%d/%m/%Y"))), by = 15)
#geom_label(aes(x= as.Date(EURIRS, "%d/%m/%Y"), y= Rate_Value, label = pmt )) +


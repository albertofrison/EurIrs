#EURIRS TIME SERIES
#1 IMPORT THE DATA
library(tidyverse)
library(rvest)

# Reading EURIRS data from a website
url <- "https://www.euribor.it/eurirs-oggi/"
page <- read_html(url) #Creates an html document from URL
series <- html_table(page, fill = TRUE) #Parses tables into data frames, page contains only one table
series <- data.frame(series)

series$EURIRS <- format(paste(series$EURIRS, "/2021", sep=""), format = "%d/%m/%Y") # formatting the date (greatest issue in the whole script)

#Tidy data by making a table where every column is a variable, every row is an observation, every cell is a single value (I hope)
series2 <- series %>%
  pivot_longer(c("X10.ANNI","X15.ANNI","X20.ANNI","X25.ANNI","X30.ANNI" ), names_to = "Rate_Name", values_to = "Rate_Value") %>%
  arrange (as.Date(EURIRS, "%d/%m/%Y"), Rate_Name)

#2 USE THE DATA!!!
# Calculating Mean and SD to plot h-lines if needed
my_rate <- "X25.ANNI"

avg <- series2 %>%
  filter(Rate_Name == my_rate) %>%
  pull(Rate_Value) %>%
  mean()

sd <- series2 %>%
  filter(Rate_Name == my_rate) %>%
  pull(Rate_Value) %>%
  sd()

series2 %>%
# filter (Rate_Name == my_rate) %>% # comment this line to plot all rates, or use this line to focus in a single rate only
  ggplot (aes(x= as.Date(EURIRS, "%d/%m/%Y"), y=Rate_Value, color=Rate_Name)) +
  #geom_hline(yintercept = avg, linetype = "dotted",  size = 0.5, color = "blue") + # these 3 lines make sense only if you focus on a single rate
  #geom_hline(yintercept = avg-sd, linetype = "dotted", size = 0.3, color = "red") + # these 3 lines make sense only if you focus on a single rate
  #geom_hline(yintercept = avg+sd, linetype = "dotted", size = 0.3, color = "red") + # these 3 lines make sense only if you focus on a single rate
  geom_point(size = 1) +
  geom_line ()+
  xlab("")+
  ylab("Rates") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle ("EURIRS Rates trend") +
  theme (legend.position = "bottom")
  # scale_fill_discrete(name = "RATE") 
  #scale_x_date(limit=c(as.Date("2021-10-01"),as.Date("2021-12-31")))
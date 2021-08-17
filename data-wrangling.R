######################################################################################################
#Data wrangling
#################################################################################################
lapply(c("tidyverse", "lubridate", "reshape2", "stringr", "plotly", "roll", "data.table", "clifro"), library, character.only = TRUE)# Make sure date is in right format and change column name 

str(data)
colnames(df)[1] <- "Date"
df$Date<-as.POSIXct(df$Date,format="%Y-%m-%d %H:%M")

#confirm date in POSIXct or whatever date format you want to work with
str(data)

#Select columns based on "contains"
new_df<-df %>% 
  select("Date","Year", "Month", "WaterYear", contains("Avg"))

#Drop by column position
new_df = select(df, -c(33:35))

#Drop by row position
new_df<-df[-c(1:7),]

#Filter to specific date range
df_watyr19<-df %>% 
  filter(Date >= as_datetime("2018-10-01 00:00:00"), Date <= as_datetime("2019-10-01 00:00:00"))

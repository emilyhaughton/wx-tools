######################################################################################################
#Data wrangling
#################################################################################################
lapply(c("tidyverse", "lubridate", "reshape", "stringr", "plotly", "roll", "data.table", "clifro"), library, character.only = TRUE)# Make sure date is in right format and change column name 

str(data)
colnames(df)[1] <- "Date"
new_df$Date<-as.POSIXct(df$Date,format="%Y-%m-%d %H:%M")

#confirm date in POSIXct or whatever date format you want to work with
str(data)

#Select columns based on "contains"
new_df<-df_wtryr %>% 
  select("Date","Year", "Month", "watyr", contains("SC"))

#Drop by column position
new_df = select(df, -c(33:35))

#Drop by row position
new_df<-df[-c(1:7),]

#Change wide to long format
depth_long<-depth_data %>%
    pivot_longer(DepthKoeyePT_Avg:DepthKoeyePT2_Avg,names_to="variable", values_to="value")


#Filter to specific date range
df_watyr19<-df %>% 
  filter(Date >= as_datetime("2018-10-01 00:00:00"), Date <= as_datetime("2019-10-01 00:00:00"))

#Aggregate time interval
df_daily<-df_long %>%
  group_by(Date= cut(Date, breaks="24 hours"))

df_daily <- aggregate(df["variable"], 
                  list(hour=cut(as.POSIXct(df$Date)-1, "24 hours")),
                  mean, na.rm=TRUE)

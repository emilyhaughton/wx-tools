#install.packages('googlesheets')
#install.packages('lubridate')
#install.packages('tidyverse')
#install.packages('here')
#install.packages('readr')
#install.packages('ggplot2')
#install.packages('plotly')
#install.packages('reshape')
#install.packages('stats')
#install.packages('msir')

library(here)
library(ggplot2)
library(tidyverse)
library(plotly)
library(reshape)
library(stats)
library(googlesheets)
library(lubridate)
library(fANCOVA)
library(msir)
library(KScorrect)

# Explanation of code
## Action required

# Import stage-discharge data. 
## Adapt watershed and version number
HQ <- gs_title('Metadata Rating curve 844')
HQ <- gs_read(HQ, ws = "Rating curve v4")

# Add columns with absolute Q uncertainty. 
# Filter measurements that are indicated as 'Y' under 'Final_rating_curve'
HQ <- HQ %>% 
  mutate(Q_abs_unc = Q_rel_unc/100 * Q_meas) %>% 
  filter(Final_rating_curve == 'Y')
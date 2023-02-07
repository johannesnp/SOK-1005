#Assignment 1


rm(list=ls())
library(tidyverse)
library(zoo)
library(lubridate)


#Calculate the 12-month(right-aligned) moving average

#Scraping data from URL
df_lower_trop <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
df_mid_trop <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")
df_trop <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
df_lower_strat <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

#Deleting unwanted data
df_lower_trop <- df_lower_trop[2:530,-4:-29]
df_mid_trop <- df_mid_trop[2:530,-4:-29]
df_trop <- df_trop[2:530,-4:-29]
df_lower_strat <- df_lower_strat[2:530,-4:-29]

# Converting year and month to date variable
df_lower_trop <- df_lower_trop %>% 
  mutate(Date = ymd(paste(df_lower_trop$Year, df_lower_trop$Mo, 1, sep="-"))) %>% 
  select(Date, Globe) 

df_mid_trop <- df_mid_trop %>% 
  mutate(Date = ymd(paste(df_mid_trop$Year, df_mid_trop$Mo, 1, sep="-"))) %>% 
  select(Date, Globe) 

df_trop <- df_trop %>% 
  mutate(Date = ymd(paste(df_trop$Year, df_trop$Mo, 1, sep="-"))) %>% 
  select(Date, Globe) 

df_lower_strat <- df_lower_strat %>% 
  mutate(Date = ymd(paste(df_lower_strat$Year, df_lower_strat$Mo, 1, sep="-"))) %>% 
  select(Date, Globe) 

# Converting data from characters to numbers

df_lower_trop <- df_lower_trop %>% mutate_at(vars(Globe), ~as.numeric(.))
df_mid_trop <- df_mid_trop %>% mutate_at(vars(Globe), ~as.numeric(.))
df_trop <- df_trop %>% mutate_at(vars(Globe), ~as.numeric(.))
df_lower_strat <- df_lower_strat %>% mutate_at(vars(Globe), ~as.numeric(.))

# Calculating rolling average

df_lower_trop <- df_lower_trop %>% mutate(lower_trop_rollmean = rollmean(Globe, 13, fill=NA,align="right"))
df_mid_trop <- df_mid_trop %>% mutate(mid_trop_rollmean = rollmean(Globe,13, fill=NA,align="right"))
df_trop <- df_trop %>% mutate(trop_rollmean = rollmean(Globe,13, fill=NA,align="right"))
df_lower_strat <- df_lower_strat %>% mutate(lower_strat_rollmean = rollmean(Globe,13, fill=NA,align="right"))

# Combining dataframes to one
df_all <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
df_all <- df_all[2:530,-3:-29]
df_all <- df_all %>% 
  mutate(Date = ymd(paste(df_all$Year, df_all$Mo, 1, sep="-"))) %>% 
  select(Date) 

df_all <- df_all %>% mutate(Year = year(Date),
                              Month = month(Date),
                              Month2 = month(Date, label = TRUE, abbr = FALSE))

df_all$lower_trop_rollmean <- df_lower_trop$lower_trop_rollmean
df_all$mid_trop_rollmean <- df_mid_trop$mid_trop_rollmean
df_all$trop_rollmean <- df_trop$trop_rollmean
df_all$lower_strat_rollmean <- df_lower_strat$lower_strat_rollmean

# Plotting graphs
df_all %>%
  group_by(Year) %>% 
  ggplot() +
  geom_line(aes(x=Date, y=lower_trop_rollmean, col="Lower Troposphere"))+
  geom_line(aes(x=Date, y=mid_trop_rollmean, col = "Mid Troposphere"))+
  geom_line(aes(x=Date, y=trop_rollmean, col="Troposphere"))+
  geom_line(aes(x=Date, y=lower_strat_rollmean, col="Lower Stratosphere"))+
  geom_line(aes(x=Date, y=rowMeans(df_all[5:8],na.rm=T), color="Average"), size=1.5)+
  labs(title = "Rolling mean and average of atmospheric temperatures",
       x = "Years",
       y = "Temperatures",
       colour = "Atmospheric layers")
  
  
  

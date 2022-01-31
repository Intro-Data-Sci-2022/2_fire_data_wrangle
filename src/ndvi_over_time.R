library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)


#Reading in the data. 
ndvi <- read_csv('data/hayman_ndvi.csv') %>%
  rename(burned=2,unburned=3) %>%
  filter(!is.na(burned),
         !is.na(unburned))

# Converting from wide to long data
ndvi_long <- gather(ndvi,
                    key='site',
                    value='NDVI',
                    -DateTime)

# Plotting all the data
ggplot(ndvi_long,aes(x=DateTime,y=NDVI,color=site)) 
  geom_point(shape=1) +
  geom_line() +
  theme_few() + 
  scale_color_few() +
  theme(legend.position=c(0.3,0.3))

# Summarizing the data by year
ndvi_annual <- ndvi_long %>%
  mutate(year=year(DateTime)) %>%
  mutate(month=month(DateTime)) %>%
  filter(month %in% c(5,6,7,8,9)) %>%
  group_by(site,year) %>%
  summarize(mean_NDVI=mean(NDVI))

#Here making an annual plot
ggplot(ndvi_annual,aes(x=year,y=mean_NDVI,color=site)) +
  geom_point(shape=1) + 
  geom_line() +
  theme_few() + 
  scale_color_few() + 
  theme(legend.position=c(0.3,0.3))


# Plotting seasonal variation by summarizing over month instead of year
ndvi_month <- ndvi_long %>%
  mutate(year=year(DateTime)) %>%
  mutate(month=month(DateTime)) %>%
  group_by(site,month) %>%
  summarize(mean_NDVI=mean(NDVI))

# Same plot as above but with month on x-axis
ggplot(ndvi_month,aes(x=month,y=mean_NDVI,color=site)) +
  geom_point(shape=1) + 
  geom_line() +
  theme_few() + 
  scale_color_few() + 
  theme(legend.position=c(0.6,0.2))


## Adding another groupd called treatment (pre or post-burn)
ndvi_month_pre_post <- ndvi_long %>% 
  mutate(year = year(DateTime),
         month = month(DateTime),
         treatment = cut(year,breaks=c(0,2003,2020),
                         labels=c('pre-burn','post-burn'))) %>%
  group_by(month,site,treatment) %>%
  summarize(mean_ndvi = mean(NDVI))

# Plot that splits out data by burned and unburned (facet_wrap(~site))            
ggplot(ndvi_month_pre_post,aes(x=month,y=mean_ndvi,color=treatment)) +
  geom_point(shape=1) + 
  geom_line() +
  theme_few() + 
  scale_color_few() + 
  theme(legend.position=c(0.6,0.2)) + 
  facet_wrap(~site)


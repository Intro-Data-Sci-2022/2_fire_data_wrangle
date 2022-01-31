library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

#Reading in files
files <- list.files('data',full.names=T)


#files

#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')


ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

#Use view to look at a data like you would in excel
#View(ndmi)


## join all datasets together and rename columns
full_wide <- inner_join(ndmi %>% dplyr::select(-data),
                        ndvi %>% dplyr::select(-data),
                        by='DateTime') %>%
  inner_join(ndsi %>% dplyr::select(-data),by='DateTime') %>%
  rename(burned_ndmi = 2,unburned_ndmi=3,
         burned_ndvi=4, unburned_ndvi=5,
         burned_ndsi=6, unburned_ndsi=7) %>%
  filter_if(is.numeric,all_vars(!is.na(.))) %>%
  mutate(month=month(DateTime),
         year = year(DateTime))

## Plot of burned plots of moisture (ndmi) vs greenness (ndvi)
full_wide %>%
  filter(!month %in% c(11,12,1,2,3,4,5)) %>%
ggplot(.,aes(x=burned_ndmi,y=burned_ndvi,color=month)) + 
  geom_point()


# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))

# Plot all three different types
ggplot(full_long,aes(x=DateTime,y=value,color=site)) + 
  geom_line() + 
  facet_wrap(~data)

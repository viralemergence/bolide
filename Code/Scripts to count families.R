
library(tidyverse)
data <- read_csv('~/Github/bolide/Data.csv')
data <- data %>% filter(!is.na(data$Virus))
library(stringr)

groups = c('Bagaza virus' = 'flavi',
           'Baiyangdian virus' = 'flavi',
           'Barmah Forest virus' = 'alpha',
           'Bovine ephemeral fever virus' = 'rhabdo',
           'Cache valley virus' = 'bunya',
           'Chandipura virus' = 'rhabdo',
           'Chikungunya virus' = 'alpha',
           'Chittoor virus' = 'bunya',
           'Deer tick virus' = 'flavi',
           'Dengue virus' = 'flavi',
           'Eastern equine encephalitis virus' = 'alpha',
           'Everglades virus' = 'alpha',
           'Getah virus' = 'alpha',
           'Ife virus' = 'zzzzzzzzzzorbi',
           'Ingwavuma virus' = 'bunya',
           'Japanese encephalitis virus' = 'flavi',
           'Karelian fever virus' = 'alpha',
           'La Crosse virus' = 'bunya',
           'Mayaro virus' = 'alpha',
           'Murray Valley encephalitis virus' = 'flavi',
           'Rabensburg virus' = 'flavi',
           'Rift Valley fever virus' = 'bunya',
           'Rocio virus' = 'flavi',
           'Ross River virus' = 'alpha',
           'Saint Louis encephalitis virus' = 'flavi',
           'Schmallenberg virus' = 'bunya',
           'Shuni virus' = 'bunya',
           'Sindbis virus' = 'alpha',
           'Umbre virus' = 'bunya',
           'Usutu virus' = 'flavi',
           'Venezuelan equine encephalitis virus' = 'alpha',
           'West Nile virus' = 'flavi',
           'Western equine encephalitis virus' = 'alpha',
           'Yellow fever virus' = 'flavi',
           'Zika virus' = 'flavi')

data %>% 
  mutate(Virus = recode(Virus, !!!groups)) %>% 
  select(`Mosquito species`, Virus, `Title (from Imported table)`) %>%
  distinct() %>%
  na.omit() %>% 
  rename(Mosquito = 'Mosquito species') %>% 
  select(Virus, `Title (from Imported table)`) %>%
  distinct() %>% 
  group_by(Virus) %>% 
  count()

table(groups)
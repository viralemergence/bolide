
library(tidyverse)
data <- read_csv('~/Github/bolide/Data.csv')
data <- data %>% filter(!is.na(data$Virus))
library(stringr)

acronyms = c('Baiyangdian virus' = 'BYDV',
             'Barmah Forest virus' = 'BFV',
             'Bovine ephemeral fever virus' = 'BEFV',
             'Cache valley virus' = 'CVV',
             'Chikungunya virus' = 'CHIKV',
             'Deer tick virus' = 'DTV',
             'Dengue virus' = 'DENV',
             'Eastern equine encephalitis virus' = 'EEEV',
             'Everglades virus' = 'EVEV',
             'Getah virus' = 'GETV',
             'Ife virus' = 'IFEV',
             'Japanese encephalitis virus' = 'JEV',
             'Karelian fever virus' = 'KFV',
             'La Crosse virus' = 'LACV',
             'Mayaro virus' = 'MAYV',
             'Murray Valley encephalitis virus' = 'MVEV',
             'Rabensburg virus' = 'RABV',
             'Rift Valley fever virus' = 'RVFV',
             'Rocio virus' = 'ROCV',
             'Ross River virus' = 'RRV',
             'Saint Louis encephalitis virus' = 'SLEV',
             'Schmallenberg virus' = 'SBV',
             'Shuni virus' = 'SHUV',
             'Sindbis virus' = 'SINV',
             'Usutu virus' = 'USUV',
             'Venezuelan equine encephalitis virus' = 'VEEV',
             'West Nile virus' = 'WNV',
             'Western equine encephalitis virus' = 'WEEV',
             'Yellow fever virus' = 'YFV',
             'Zika virus' = 'ZIKV')
data %>% 
  mutate(Virus = recode(Virus, !!!acronyms)) %>% 
  select(`Mosquito species`, Virus, `DOI (from Imported table)`) %>%
  distinct() %>%
  na.omit() %>% 
  rename(Mosquito = 'Mosquito species') %>% 
  group_by(Mosquito, Virus) %>% 
  count() %>% # View()
  rename('Studies' = 'n') %>% 
  ggplot(aes(factor(Virus), Mosquito, fill = Studies)) + geom_raster() + 
  theme_bw() + 
  scale_x_discrete(position = "top") + 
  scale_y_discrete(limits=rev) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 7.5),
        axis.text.y = element_text(size = 6),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)) + 
  xlab("") + ylab("") 


data %>% 
  mutate(Virus = recode(Virus, !!!acronyms)) %>% 
  rename(Mosquito = 'Mosquito species') %>% 
  select(Mosquito, Virus) %>%
  unique() %>% 
  count(Virus) %>%
  arrange(-n)


### GENUS FIGURE 

data %>%
  mutate(Virus = recode(Virus, !!!acronyms)) %>%
  select(`Mosquito species`, Virus, `DOI (from Imported table)`) %>%
  mutate(MosquitoGenus = word(`Mosquito species`,1)) %>%
  distinct() %>%
  na.omit() %>%
  rename(Mosquito = 'MosquitoGenus') %>%
  group_by(Mosquito, Virus) %>%
  count() %>%
  rename('Studies' = 'n') %>% 
  ggplot(aes(factor(Virus), Mosquito, fill = Studies)) + geom_raster() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 7.5),
        axis.text.y = element_text(size = 7),
        legend.text=element_text(size=9)) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=rev) +
  xlab("") + ylab("")

##### MAPS

library(classInt)
library(rgdal)
library(RColorBrewer)
library(rworldmap)

congo <- c("Republic of Congo" = "Congo (Brazzaville)")
data %>% 
  as_tibble() %>% 
  rename(Country = `Mosquito origin (country)`) %>%
  mutate(Country = recode(Country, !!!congo)) %>%
  select(`DOI (from Imported table)`, Country) %>%
  unique() %>% 
  count(Country) -> demo

#demo$n <- factor(demo$n, levels=1:285)

# Attach that all to the map
sPDF <- joinCountryData2Map(demo,
                            joinCode = "NAME",
                            nameJoinColumn = "Country")

x <- ggthemr('flat', set_theme = FALSE)

cols <- brewer.pal(11, "Spectral")
cols <- rev(colorRampPalette(cols)(107))

spplot(sPDF, 'n', col.regions = cols)

###### Figure 5

data %>% 
  as_tibble() %>% 
  rename(Country = `Mosquito origin (country)`) %>%
  mutate(Country = recode(Country, !!!congo)) %>%
  select(Virus, Country) %>%
  unique() %>% 
  count(Country) -> demo

#demo$n <- factor(demo$n, levels=1:285)

# Attach that all to the map
sPDF <- joinCountryData2Map(demo,
                            joinCode = "NAME",
                            nameJoinColumn = "Country")

x <- ggthemr('flat', set_theme = FALSE)

cols <- brewer.pal(11, "Spectral")
cols <- rev(colorRampPalette(cols)(107))

spplot(sPDF, 'n', col.regions = cols)


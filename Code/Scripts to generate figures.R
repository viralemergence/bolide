
library(tidyverse); library(magrittr)
data <- read_csv('~/Github/bolide/Data/Data.csv')
data <- data %>% filter(!is.na(data$Virus))
library(stringr)

source("~/Github/virion/Code/001_TaxizeFunctions.R")
mos <- unique(data$`Mosquito species`)
mtable <- hdict(mos)
mvec <- mtable[!(mtable$HostOriginal==mtable$Host),]$Host
names(mvec) <- mtable[!(mtable$HostOriginal==mtable$Host),]$HostOriginal

data %<>% mutate(`Mosquito species` = recode(`Mosquito species`, !!!mvec)) 
data %<>% filter(!str_detect(`Mosquito species`, "Culicoides"))


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

acronyms = c('Bagaza virus' = 'BAGV',
             'Baiyangdian virus' = 'BYDV',
             'Barmah Forest virus' = 'BFV',
             'Bovine ephemeral fever virus' = 'BEFV',
             'Cache valley virus' = 'CVV',
             'Chandipura virus' = 'CHPV',
             'Chikungunya virus' = 'CHIKV',
             'Chittoor virus' = 'CHITV',
             'Deer tick virus' = 'DTV',
             'Dengue virus' = 'DENV',
             'Eastern equine encephalitis virus' = 'EEEV',
             'Everglades virus' = 'EVEV',
             'Getah virus' = 'GETV',
             'Ife virus' = 'IFEV',
             'Ingwavuma virus' = 'INGV',
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
             'Umbre virus' = 'UMBV',
             'Usutu virus' = 'USUV',
             'Venezuelan equine encephalitis virus' = 'VEEV',
             'West Nile virus' = 'WNV',
             'Western equine encephalitis virus' = 'WEEV',
             'Yellow fever virus' = 'YFV',
             'Zika virus' = 'ZIKV')

levs <- acronyms[order(groups)]
names(levs) <- NULL

# data %>%
#   mutate(Virus = recode(Virus, !!!acronyms)) %>%
#   select(`Mosquito species`, Virus, `Title (from Imported table)`) %>%
#   distinct() %>%
#   na.omit() %>%
#   rename(Mosquito = 'Mosquito species') %>%
#   group_by(Mosquito, Virus) %>%
#   count() %>%
#   arrange(-n)
# 
# data %>%
#   mutate(Virus = recode(Virus, !!!acronyms)) %>%
#   select(`Mosquito species`, Virus) %>%
#   distinct() %>%
#   na.omit() %>%
#   group_by(Virus) %>%
#   count() %>%
#   arrange(-n)

data %>% 
  mutate(Virus = recode(Virus, !!!acronyms)) %>% 
  select(`Mosquito species`, Virus, `Title (from Imported table)`) %>%
  distinct() %>%
  na.omit() %>% 
  rename(Mosquito = 'Mosquito species') %>% 
  group_by(Mosquito, Virus) %>% 
  count() %>% # View()
  ungroup() %>%
  complete(Mosquito, Virus, fill = list(Studies = NA)) %>%
  mutate(Virus = fct_relevel(Virus, levs)) %>% 
  rename('Studies' = 'n') %>% 
  ggplot(aes(Virus, Mosquito, fill = Studies)) + geom_raster() + 
  theme_bw() + 
  scale_x_discrete(position = "top") + 
  scale_y_discrete(limits=rev) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 7.5),
        axis.text.y = element_text(size = 6),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)) + 
  xlab("") + ylab("") + scale_fill_viridis_b(option = "turbo", na.value = NA, breaks = c(1,2,10,20,30,40))

    #scale_fill_gradient(
    #low = 'skyblue1',
    #high = 'red',
    #na.value = "grey97")



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
  select(`Mosquito species`, Virus, `Title (from Imported table)`) %>%
  mutate(MosquitoGenus = word(`Mosquito species`,1)) %>%
  distinct() %>%
  na.omit() %>%
  rename(Mosquito = 'MosquitoGenus') %>%
  group_by(Mosquito, Virus) %>%
  count() %>%
  mutate(Virus = factor(Virus)) %>% 
  mutate(Virus = fct_relevel(Virus, levs)) %>% 
  rename('Studies' = 'n') %>% 
  ggplot(aes(factor(Virus), Mosquito, fill = Studies)) + geom_raster() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 7.5),
        axis.text.y = element_text(size = 7),
        legend.text=element_text(size=9)) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=rev) +
  xlab("") + ylab("") + scale_fill_viridis_c(option = "turbo", na.value = NA)# , breaks = c(1,2,10,20,30,40,50,60,70,80))

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
  select(`Title (from Imported table)`, Country) %>%
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

###### Figure 3

years <- read_csv("./Data/StudyYear.csv")

data %>% 
  rename(Title = `Title (from Imported table)`) %>%
  left_join(years %>% select(Title, Year)) %>% 
  select(Virus, Title, Year) %>% distinct() %>% 
  select(Virus, Year) -> byyear

byyear %>% 
  group_by(Virus, Year) %>%
  summarize(Studies = n()) %>%
  group_by(Virus) %>% summarize(Studies = sum(Studies)) %>%
  top_n(10) %>% pull(Virus) -> top

chikv <- data.frame(x1 = 2010.995, y1 = 9.5, x2 = 2011.005, y2 = 10.5)
byyear %>%
  filter(Virus %in% top) %>% 
  ggplot(aes(x = Year, y = Virus, color = Virus), col = MetBrewer::met.brewer('Signac', n = 10)[1], alpha = 0.5, size = 3) + 
  
  geom_segment(aes(x = 2005, y = 9.5, xend = 2005, yend = 10.5), color = MetBrewer::met.brewer('Signac', n = 10)[1], size = 1.5) + 
  geom_segment(aes(x = 2011, y = 9.5, xend = 2011, yend = 10.5), color = MetBrewer::met.brewer('Signac', n = 10)[1], size = 1.5) + 
  geom_segment(aes(x = 2013, y = 9.5, xend = 2013, yend = 10.5), color = MetBrewer::met.brewer('Signac', n = 10)[1], size = 1.5) + 
  geom_segment(aes(x = 2017, y = 9.5, xend = 2017, yend = 10.5), color = MetBrewer::met.brewer('Signac', n = 10)[1], size = 1.5) + 
  geom_segment(aes(x = 2019, y = 9.5, xend = 2019, yend = 10.5), color = MetBrewer::met.brewer('Signac', n = 10)[1], size = 1.5) + 
  
  geom_segment(aes(x = 1996, y = 8.5, xend = 1996, yend = 9.5), color = MetBrewer::met.brewer('Signac', n = 10)[2], size = 1.5) +
  geom_segment(aes(x = 2000, y = 8.5, xend = 2000, yend = 9.5), color = MetBrewer::met.brewer('Signac', n = 10)[2], size = 1.5) +
  geom_segment(aes(x = 2002, y = 8.5, xend = 2002, yend = 9.5), color = MetBrewer::met.brewer('Signac', n = 10)[2], size = 1.5) +
  geom_segment(aes(x = 2009, y = 8.5, xend = 2009, yend = 9.5), color = MetBrewer::met.brewer('Signac', n = 10)[2], size = 1.5) +
  geom_segment(aes(x = 2013, y = 8.5, xend = 2013, yend = 9.5), color = MetBrewer::met.brewer('Signac', n = 10)[2], size = 1.5) +
  geom_segment(aes(x = 2019, y = 8.5, xend = 2019, yend = 9.5), color = MetBrewer::met.brewer('Signac', n = 10)[2], size = 1.5) +
  
  geom_segment(aes(x = 1995, y = 7.5, xend = 1995, yend = 8.5), color = MetBrewer::met.brewer('Signac', n = 10)[3], size = 1.5) +
  geom_segment(aes(x = 1996, y = 7.5, xend = 1996, yend = 8.5), color = MetBrewer::met.brewer('Signac', n = 10)[3], size = 1.5) +
  geom_segment(aes(x = 1999, y = 7.5, xend = 1999, yend = 8.5), color = MetBrewer::met.brewer('Signac', n = 10)[3], size = 1.5) +
  geom_segment(aes(x = 2005, y = 7.5, xend = 2005, yend = 8.5), color = MetBrewer::met.brewer('Signac', n = 10)[3], size = 1.5) +
  geom_segment(aes(x = 2009, y = 7.5, xend = 2009, yend = 8.5), color = MetBrewer::met.brewer('Signac', n = 10)[3], size = 1.5, linetype = '11') +
  
  geom_segment(aes(x = 2006, y = 6.5, xend = 2006, yend = 7.5), color = MetBrewer::met.brewer('Signac', n = 10)[4], size = 1.5) +
  
  geom_segment(aes(x = 2006, y = 6.5, xend = 2006, yend = 7.5), color = MetBrewer::met.brewer('Signac', n = 10)[4], size = 1.5) +
  
  geom_segment(aes(x = 1990, y = 5.5, xend = 1990, yend = 6.5), color = MetBrewer::met.brewer('Signac', n = 10)[5], size = 1.5) +
  geom_segment(aes(x = 2005, y = 5.5, xend = 2005, yend = 6.5), color = MetBrewer::met.brewer('Signac', n = 10)[5], size = 1.5) +
  
  geom_segment(aes(x = 1995, y = 4.5, xend = 1995, yend = 5.5), color = MetBrewer::met.brewer('Signac', n = 10)[6], size = 1.5) +
  geom_segment(aes(x = 1998, y = 4.5, xend = 1998, yend = 5.5), color = MetBrewer::met.brewer('Signac', n = 10)[6], size = 1.5) +
  geom_segment(aes(x = 2011, y = 4.5, xend = 2011, yend = 5.5), color = MetBrewer::met.brewer('Signac', n = 10)[6], size = 1.5) +
  
  geom_segment(aes(x = 1999, y = 3.5, xend = 1999, yend = 4.5), color = MetBrewer::met.brewer('Signac', n = 10)[7], size = 1.5) +
  geom_segment(aes(x = 2011, y = 3.5, xend = 2011, yend = 4.5), color = MetBrewer::met.brewer('Signac', n = 10)[7], size = 1.5) +
  
  geom_segment(aes(x = 1994, y = 2.5, xend = 1994, yend = 3.5), color = MetBrewer::met.brewer('Signac', n = 10)[8], size = 1.5, linetype = '11') +
  geom_segment(aes(x = 2009, y = 2.5, xend = 2009, yend = 3.5), color = MetBrewer::met.brewer('Signac', n = 10)[8], size = 1.5) +
  
  geom_segment(aes(x = 1999, y = 1.5, xend = 1999, yend = 2.5), color = MetBrewer::met.brewer('Signac', n = 10)[9], size = 1.5) +
  geom_segment(aes(x = 2000, y = 1.5, xend = 2000, yend = 2.5), color = MetBrewer::met.brewer('Signac', n = 10)[9], size = 1.5) +
  geom_segment(aes(x = 2001, y = 1.5, xend = 2001, yend = 2.5), color = MetBrewer::met.brewer('Signac', n = 10)[9], size = 1.5) +
  geom_segment(aes(x = 2012, y = 1.5, xend = 2012, yend = 2.5), color = MetBrewer::met.brewer('Signac', n = 10)[9], size = 1.5) +
  geom_segment(aes(x = 2015, y = 1.5, xend = 2015, yend = 2.5), color = MetBrewer::met.brewer('Signac', n = 10)[9], size = 1.5) +
  geom_segment(aes(x = 2016, y = 1.5, xend = 2016, yend = 2.5), color = MetBrewer::met.brewer('Signac', n = 10)[9], size = 1.5) +
  geom_segment(aes(x = 2018, y = 1.5, xend = 2018, yend = 2.5), color = MetBrewer::met.brewer('Signac', n = 10)[9], size = 1.5) +
  
  geom_segment(aes(x = 2007, y = 0.5, xend = 2007, yend = 1.5), color = MetBrewer::met.brewer('Signac', n = 10)[10], size = 1.5) +
  geom_segment(aes(x = 2013, y = 0.5, xend = 2013, yend = 1.5), color = MetBrewer::met.brewer('Signac', n = 10)[10], size = 1.5) +
  geom_segment(aes(x = 2015, y = 0.5, xend = 2015, yend = 1.5), color = MetBrewer::met.brewer('Signac', n = 10)[10], size = 1.5) +
  
  #geom_vline(xintercept = 2011, linetype = 'dotted', size = 1.5, col = MetBrewer::met.brewer('Signac', n = 10)[1], alpha = 0.5) +
  #geom_vline(xintercept = 2013, linetype = 'dotted', size = 1.5, col = MetBrewer::met.brewer('Signac', n = 10)[1], alpha = 0.5) +
  #geom_vline(xintercept = 2001, linetype = 'dotted', size = 1.5, col = MetBrewer::met.brewer('Signac', n = 10)[2], alpha = 0.5) +
  #geom_vline(xintercept = 1995, linetype = 'dotted', size = 1.5, col = MetBrewer::met.brewer('Signac', n = 10)[3], alpha = 0.5) +
  #geom_vline(xintercept = 2015, linetype = 'dotted', size = 1.5, col = MetBrewer::met.brewer('Signac', n = 10)[10], alpha = 0.5) + 
  geom_point(size = 8.5, alpha = 0.25, stroke = 0.175, legend = FALSE) + 
  xlab("") + ylab("") + theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line( size=.3, color="grey75"), 
    text = element_text(size = 16),
    legend.position = 'n') + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))  +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  scale_y_discrete(limits=rev) + 
  scale_color_manual(values = MetBrewer::met.brewer('Signac', n = 10)[1:10]) 
  

  

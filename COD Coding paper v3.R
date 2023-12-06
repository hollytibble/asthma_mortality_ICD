### pre-amble
library(tidyverse)
library(lubridate)
rm(list=ls())

### Death records
mortality <- read_csv("1516-0489_NRS_Deaths_Stillbirths.csv") %>%
  filter(NRSDeaths==1) %>%
  mutate(DEATH_DATE=as.Date(paste(substr(DEATH_DATE,1,4),
                            substr(DEATH_DATE,5,6),
                            substr(DEATH_DATE,7,8),
                            sep="/"),format="%Y/%m/%d")) %>%
  select(-NRSDeaths,-NRSStillbirths) 

nrow(mortality)
min(mortality$DEATH_DATE)
max(mortality$DEATH_DATE)

mortality <- mortality %>%
  pivot_longer(cols=PRIMARY_CAUSE_OF_DEATH:SECONDARY_CAUSE_OF_DEATH_9,
               names_to="position") %>%
  mutate(position = ifelse(position=="PRIMARY_CAUSE_OF_DEATH",1,
                           as.numeric(substr(position,26,26))+2)) %>%
  filter(!is.na(value)) 
names(mortality)[1]<-"ID" 

### codes per death record
summary(mortality %>% count(ID) %>% select(n))

### keep only first occurrence of code if multiple for the same death
mortality<-mortality %>% 
  mutate(year=year(DEATH_DATE)) %>%
  group_by(ID,year,DEATH_DATE,value) %>%
  slice(1) %>%
  ungroup %>%
  arrange(ID,year,DEATH_DATE,position) 

### codes per death record, after removing duplicates
summary(mortality %>% count(ID) %>% select(n))

### Figure 1: number of codes per death, per year
f1<-mortality %>%
  count(ID,year,name="num_codes") %>% 
  mutate(num_codes = ifelse(num_codes>=5,"5+",as.character(num_codes))) %>%
  add_count(year,name="total") %>%
  count(year,total,num_codes, name="frequency") %>%
  mutate(percent = frequency*100/total)
ggplot(f1) +
  geom_bar(aes(x=year, y=percent, fill=num_codes),
           stat="identity")  +
  ylab("Percent") + xlab("Year") +
  labs(fill="Number \nof Codes") +
  theme_classic() +
  scale_fill_brewer(palette="YlOrRd")

### Distinguish between asthma codes
mortality<-mortality %>%  
  mutate(asthma = substr(value,1,3)=="J45",
         asthma_attack = substr(value,1,3)=="J46",
         flag = (asthma | asthma_attack),
         chapter = substr(value,1,1)) %>%
  group_by(ID) %>%
  mutate(flag2 = sum(flag)) 

### percent of deaths that were asthma related, by year
f2<-mortality %>%
  distinct(ID,year,flag2) %>%
  ungroup %>%
  add_count(year,name="total") %>%
  count(year,total,flag2, name="count") %>%
  filter(flag2==1) %>%
  select(-flag2) %>%
  mutate(percent = count*100/total)
ggplot(f2) +
  geom_smooth(aes(x=year, y=percent))  +
  ylab("Asthma-Related Mortality Percent") + xlab("Year") +
  theme_classic()

### keep only deaths with asthma in some position
mortality<-mortality %>% 
  filter(flag2>0) %>%
  select(-flag2) %>%
  # make a flag (primary_asthma) for deaths with asthma as primary cause 
  group_by(ID) %>%
  mutate(temp = (position==1 & flag==TRUE),
         primary_asthma = sum(temp)>0) %>%
  select(-temp) %>%
  ungroup 

### prevalence of asthma (J45) vs asthma attack (J46), or both, in any position
mortality %>% ungroup %>% filter(flag==1) %>%
  group_by(ID,DEATH_DATE) %>%
  summarise(asthma=sum(asthma)>0,
            asthma_attack =sum(asthma_attack)>0) %>%
  ungroup %>%
  count(asthma,asthma_attack)

### How many deaths had asthma as the primary cause
nrow(mortality %>% ungroup %>% filter(primary_asthma) %>% distinct(ID))

### Were these J45 or J46?
mortality %>% ungroup %>% filter(primary_asthma & position==1) %>% distinct(ID,asthma) %>% count(asthma)

#### Did this change over time?
f3<-mortality %>% ungroup %>% filter(primary_asthma & position==1) %>%
  add_count(year,name="year_total") %>%
  distinct(ID,year,year_total,asthma) %>%
  count(year,year_total,asthma) %>%
  filter(asthma) %>%
  mutate(perc_primary_J45 = n*100/year_total) %>%
  select(-asthma)
# ggplot(f3) +
#   geom_smooth(aes(x=year, y=perc_primary_J45))
  
### Infection recorded, by primary cause of death
mortality %>%
  mutate(INF = ifelse(substr(value,1,1)=="A" |
                        substr(value,1,1)=="B" |
                        (substr(value,1,2)=="G0" & as.numeric(substr(value,3,3))<=5) |
                        substr(value,1,2)=="H1" |
                        (substr(value,1,1)=="J" & as.numeric(substr(value,2,2))<=3) |
                        (substr(value,1,2)=="L0" & as.numeric(substr(value,3,3))<=8) |
                        (substr(value,1,2)=="M0" & as.numeric(substr(value,3,3))<=2), 1,0),
         P_A = (position==1 & asthma),
         P_AA = (position==1 & asthma_attack==1)) %>%
  group_by(ID) %>%
  summarise(INF = sum(INF)>0,
            P_A =sum(P_A),
            P_AA=sum(P_AA)) %>%
  ungroup %>% 
  select(-ID) %>%
  count(P_A,P_AA,INF)

### most common secondary causes in any position
mortality %>% filter(primary_asthma & position>1) %>% 
  ungroup %>% count(value, sort=T) %>% head(5)

###  primary cause CHAPTER when asthma secondary
mortality %>% filter(!primary_asthma & position==1) %>% ungroup %>% 
  add_count(name = "total") %>%
  count(chapter,total) %>% mutate(perc = n*100/total) %>%
  select(-total)

### top 5 most common primary causes when asthma secondary
mortality %>% filter(!primary_asthma & position==1) %>% 
  ungroup %>% 
  mutate(value=substr(value,1,3)) %>% 
  count(value) %>%
  arrange(-n) %>%
  select(value,n) %>%
  head(5)




  
  


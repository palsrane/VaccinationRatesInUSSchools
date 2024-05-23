library(tidyverse)
library(rvest)
library(dplyr)
library(ggplot2)
library(pivottabler)
library(ggpattern)
library(patchwork)
library(stringr)
library(gridExtra)

`%notin%` <- Negate(`%in%`)

setwd("C:/Users/palla/Desktop/GoogleDataAnalyticsCapstoneProject/VaccinationRatesInUSSchools")

#Cleaning data----

#This is modified from the #TidyTuesday website 

# `measles.csv`

# |variable |class     |description |
# |:--------|:---------|:-----------|
# |index    |double    | Index ID |
# |state    |character | School's state |
# |year     |character | School academic year|
# |name     |character | School name|
# |type     |character | Whether a school is public, private, charter |
# |city     |character | City |
# |county   |character | County |
# |district |character | School district |
# |enroll   |double    | Enrollment |
# |mmr      |double    | School's Measles, Mumps, and Rubella (MMR) vaccination rate |
# |overall  |double    | School's overall vaccination rate|
# |xrel     |double    | Percentage of students exempted from vaccination for religious reasons |
# |xmed     |double    | Percentage of students exempted from vaccination for medical reasons |
# |xper     |double    | Percentage of students exempted from vaccination for personal reasons |
# |lat      |double    | Lattitude  |
# |lng      |double    | Longitude  |

url_wsj <- "https://raw.githubusercontent.com/WSJ/measles-data/master/all-measles-rates.csv"

wsj <- read_csv(url_wsj)

list_of_urls <- "https://github.com/WSJ/measles-data/tree/master/individual-states"

raw_states <- list_of_urls %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]] %>% 
  select(1) %>%  #changed select(Name) to select(1) becase there were three columns with headers 'Name'
  mutate(Name = str_remove(Name, "\\.csv")) %>% 
  filter(str_length(Name) > 3, str_length(Name) < 20) %>% 
  pull(Name)

raw_states=raw_states[2:32] # had to add this line of code because the first element on the list was "parent directory.."  and the last, 33rd element was "View all files"

all_states <- glue::glue("https://raw.githubusercontent.com/WSJ/measles-data/master/individual-states/{raw_states}.csv") %>% 
  map(read_csv)

#As it turns out not every state had all of state, city, county, district information. Hence in the original code was limiting the identifier column to just state.
# clean_states <- all_states %>% 
#   map(~select(., state, name, lat, lng)) %>%   #Only having state and school name was leading to cross matching in states where multiple schools with same name were present
#   map(~mutate_at(., vars(lat, lng), as.numeric)) %>% 
#   bind_rows() %>% 
#   filter(!is.na(lat))

#Hence added as many parameters that could have been added out of "state", "name", "district", "county", "city" for each state
clean_states <- all_states %>% 
  map(~select(., tidyselect::any_of(c("state", "name", "district", "county", "city", "lat","lng")))) %>% 
  map(~mutate_at(., vars(lat, lng), as.numeric)) %>% 
  bind_rows() %>% 
  filter(!is.na(lat))

wsj1 <- wsj %>% 
  left_join(clean_states, by = c("name", "state","district", "county", "city"))

#creating a new identifier to remove duplications
wsj1=mutate(wsj1,new_id=paste0(state,year,name,type,city,county,district,enroll,mmr,overall,xrel,xmed,xper)) 
wsj1=wsj1[!duplicated(wsj1$new_id),]

write_csv(select(wsj1,-"new_id"),"measles_nonduplicated.csv")

# Looking at the data on Tableau revealed another problem with the data. 86 of the records, 
# 3 from Florida and 83 from Vermont had incorrect longitudes, and this problem persisted 
# in the source files where the latitudes and longitudes were taken from. 
clean_states[clean_states$lng>0,]
# Upon checking with google maps, it was clear that in some cases the longitudes were marked
# as +ve where should have been -ve. 
tmp=wsj1[wsj1$lng>0,]
tmp[!is.na(tmp$name),]
count(tmp[!is.na(tmp$name),])
rm(tmp)
# However, in most coases, it looked like the lattitudes were incorrectly entered as longitudes.
# Since it was not possible to correct each and every case manually, it was decided that 
# for these 86 cases, the incorrect longitudes would be replaced with generated longitudes 
# usually assigned to the respective state by Tableau.
# Tableau assigns -72.7678 to Vermont and -81.55 to Florida.
wsj1$lng[(wsj1$state=="Vermont" & wsj1$lng>0)]= -72.7678
#One record from Florida that had the correct value but marked +ve
wsj1$lng[(wsj1$state=="Florida" & wsj1$lng>80)]= -1* wsj1$lng[(wsj1$state=="Florida" & wsj1$lng>0)] 
wsj1$lng[(wsj1$state=="Florida" & wsj1$lng>0)]= -81.55

write_csv(select(wsj1,-"new_id"),"measles_nonduplicated_ModifiedIncorrectLng.csv")
#----

# Data review ----
vacc_rec=read_csv("measles_nonduplicated_ModifiedIncorrectLng.csv")

### Checking for any NAs in mmr and overall column
length(vacc_rec$mmr[is.na(vacc_rec$mmr)])
length(vacc_rec$overall[is.na(vacc_rec$overall)])

### The lack of vaccination report is marked as -1 in the dataset.
### Unfortunately almost 44% or 20,176 of the schools in this report did not report overall vaccination rates,and 
### 39% or 18,087of the schools did not report any mmr vaccination rates
### Percentage of schools that did not report overall vaccination rates
paste(as.character(round(count(vacc_rec[vacc_rec$overall==-1,])/count(vacc_rec)*100,2)),"%")
length(vacc_rec$overall[vacc_rec$overall==-1])

### Percentage of schools that did not report mmr vaccination rates
paste(as.character(round(count(vacc_rec[vacc_rec$mmr==-1,])/count(vacc_rec)*100,2)),"%")
length(vacc_rec$mmr[vacc_rec$mmr==-1])

ggplot(data = vacc_rec) + 
  geom_point(mapping = aes(x = overall, y = mmr, color=state, alpha=0.5)) +
  labs(x=('Overall vaccination rate (%)'),y='MMR vaccination rate (%)') + 
  scale_color_manual(values = c('black','forestgreen', 'red2', 'orange', 'cornflowerblue', 
                                 'magenta', 'darkolivegreen4', 'indianred1', 'tan4', 'darkblue', 
                                 'mediumorchid1','firebrick4',  'yellowgreen', 'lightsalmon', "#661100",
                                 "tan1",'darkgray', 'wheat4', '#DDAD4B', 'chartreuse', 
                                 'seagreen1', 'moccasin', 'mediumvioletred', 'seagreen','cadetblue1',
                                 "#999933","tan2" ,   "tomato3" , "#7CE3D8","gainsboro",
                                 "darkolivegreen","coral1","darkorange4"))


### 29,323 schools reported their mmr vaccination rates
length(vacc_rec$mmr[vacc_rec$mmr!=-1])


PerMMRVacVsNumSchool<- ggplot(data=vacc_rec[vacc_rec$mmr!=-1,], aes(y=mmr)) +
  geom_histogram(binwidth=1, fill="darkorange4",color = "black", alpha=0.5)+
  labs(title='Number of Schools vs % of students with MMR vaccination', 
       x='Number of schools',y='MMR Vaccination (%)')


### Over 94% of the schools that reported mmr vaccinations had 80% or more of the kids vaccinated and 
### less than 0.9% of the schools had vaccinataion rate under 50%.
#Percentage of schools with >=80% mmr vacciation of those that reported
paste(as.character(round(count(vacc_rec[vacc_rec$mmr>=80,])/count(vacc_rec[vacc_rec$mmr!=-1,])*100,2)),"%")
#Percentage of schools with <50% overall vacciation of those that reported
paste(as.character(round(count(vacc_rec[vacc_rec$mmr<50 & vacc_rec$mmr!=-1,])/count(vacc_rec[vacc_rec$mmr!=-1,])*100,2)),"%")

TypeSchMMR <- ggplot(data = vacc_rec[vacc_rec$mmr!=-1 & !is.na(vacc_rec$type),]) +
  geom_bar(mapping = aes(x = type, fill=type)) +
  labs(title='MMR Vaccination rates by type of school',x=('Type of school'),
       y='Number of schools reporting MMR vaccinations') +
  scale_fill_manual(values=c('BOCES' =  'coral4', 'Charter' = 'goldenrod',
                             'Kindergarten' = 'darkolivegreen4','Private' = 'darkslategray', 'Nonpublic' = 'darkslategray3',
                             'Public' = 'coral'))+ 
  theme(axis.text.x = element_text(angle = 90))

TypeSchMMRBOx <- ggplot(data = vacc_rec[vacc_rec$mmr !=-1 & !is.na(vacc_rec$type),], aes(x=type, y=mmr)) + 
  geom_boxplot(aes(fill=type))+
  labs(title='MMR Vaccination rates by type of school',x=('Type of school'),
       y='MMR vaccination rate (%)') +
  scale_fill_manual(values=c('BOCES' =  'coral4', 'Charter' = 'goldenrod',
                             'Kindergarten' = 'darkolivegreen4','Private' = 'darkslategray', 'Nonpublic' = 'darkslategray3',
                             'Public' = 'coral'))+ 
  theme(axis.text.x = element_text(angle = 90))

tmp = mutate(vacc_rec,mmr_stat= ifelse(mmr!=-1,1,-1))
tmp$mmr_stat=factor(tmp$mmr_stat)
tmp = tmp[!is.na(tmp$type),]

TypeSchFracMMR <- ggplot(data = tmp) +
  geom_bar(mapping = aes(x = type, fill= type ,alpha = mmr_stat), position="fill") +
  labs(title='MMR Vaccination rates by type of school',x=('Type of school'),
       y='Fraction of Schools reporting MMR vaccinations') +
  scale_fill_manual(values=c('BOCES' =  'coral4', 'Charter' = 'goldenrod',
                             'Kindergarten' = 'darkolivegreen4','Private' = 'darkslategray', 'Nonpublic' = 'darkslategray3',
                             'Public' = 'coral'))+ 
  scale_alpha_manual(values = c("-1"=0.5,"1"=1))+
  theme(axis.text.x = element_text(angle = 90))




### 26,234 schools reported overall vaccination rates
length(vacc_rec$overall[vacc_rec$overall!=-1])

PerOverallVacVsNumSchool <- ggplot(data=vacc_rec[vacc_rec$overall!=-1,],aes(y=overall))+
  geom_histogram(binwidth=1,color="black", fill="khaki",alpha=0.5) +
  labs(title='Number of Schools vs % of students with overall vaccination', x='Number of schools',y='Overall Vaccination (%)')

### Almost 92% of the schools that reported overall vaccinations had 80% or more of the kids vaccinated and 
### less than 1.3% of the schools had vaccinataion rate under 50%.

#Percentage of schools with >=80% overall vacciation of those that reported
paste(as.character(round(count(vacc_rec[vacc_rec$overall>=80,])/count(vacc_rec[vacc_rec$overall!=-1,])*100,2)),"%")
#Percentage of schools with <50% overall vacciation of those that reported
paste(as.character(round(count(vacc_rec[vacc_rec$overall<50 & vacc_rec$overall!=-1,])/count(vacc_rec[vacc_rec$overall!=-1,])*100,2)),"%")

TypeSchOverallBox <- ggplot(data = vacc_rec[vacc_rec$overall !=-1 & !is.na(vacc_rec$type),], aes(x=type, y=overall)) + 
  geom_boxplot(aes(fill=type))+
  labs(title='Overall Vaccination rates by type of school',x=('Type of school'),
       y='Overall vaccination rate (%)') +
  scale_fill_manual(values=c('BOCES' =  'coral4', 'Charter' = 'goldenrod',
                             'Kindergarten' = 'darkolivegreen4','Private' = 'darkslategray', 
                             'Nonpublic' = 'darkslategray3', 'Public' = 'coral'))+ 
  theme(axis.text.x = element_text(angle = 90))

TypeSchOverall <- ggplot() +
  geom_bar(data = vacc_rec[vacc_rec$overall!=-1 & !is.na(vacc_rec$type),],mapping = aes(x = type, fill=type)) +
  #geom_bar(data = vacc_rec[!is.na(vacc_rec$type),],mapping = aes(x = type, fill=type )) +
  labs(title='Overall Vaccination rates by type of school',x=('Type of school'),
       y='Number of schools reporting Overall vaccinations') +
  scale_fill_manual(values=c('BOCES' =  'coral4', 'Charter' = 'goldenrod',
                             'Kindergarten' = 'darkolivegreen4','Private' = 'darkslategray', 
                             'Nonpublic' = 'darkslategray3', 'Public' = 'coral'))+ 
  theme(axis.text.x = element_text(angle = 90))

tmp = mutate(vacc_rec,overall_stat= ifelse(overall!=-1,1,-1))
tmp$overall_stat=factor(tmp$overall_stat)
tmp = tmp[!is.na(tmp$type),]

TypeSchFracOverall <- ggplot(data = tmp) +
  geom_bar(mapping = aes(x = type, fill= type ,alpha = overall_stat), position="fill") +
  labs(title='Overall Vaccination rates by type of school',x=('Type of school'),
       y='Fraction of Schools reporting Overall vaccinations') +
  scale_fill_manual(values=c('BOCES' =  'coral4', 'Charter' = 'goldenrod',
                             'Kindergarten' = 'darkolivegreen4','Private' = 'darkslategray', 'Nonpublic' = 'darkslategray3',
                             'Public' = 'coral'))+ 
  scale_alpha_manual(values = c("-1"=0.5,"1"=1))+
  theme(axis.text.x = element_text(angle = 90))
rm(tmp)

ggplot() +
  geom_histogram(data=vacc_rec[vacc_rec$mmr!=-1,], aes(y=mmr),binwidth=1, fill="darkorange4", alpha=1, color='black') +
  geom_histogram(data=vacc_rec[vacc_rec$overall!=-1,],aes(y=overall),binwidth=1,fill="khaki", alpha=0.75, color='black') +
  labs(title="Overall & MMR vaccination rates",subtitle="among the schools that reported these numbers", 
       y="MMR and Overall Vaccination Rates")


### 12,252 schools reported both vaccination rates.
length(vacc_rec$index[vacc_rec$overall!=-1 & vacc_rec$mmr!=-1])
ggplot(data = vacc_rec[vacc_rec$overall!=-1 & vacc_rec$mmr!=-1,]) +
  geom_point(mapping = aes(x = overall, y = mmr, color=state))+ 
  scale_color_manual(values = c('black','forestgreen', 'red2', 'orange', 'cornflowerblue', 
                                'magenta', 'darkolivegreen4', 'indianred1', 'tan4', 'darkblue', 
                                'mediumorchid1','firebrick4',  'yellowgreen', 'lightsalmon', "#661100",
                                "tan1",'darkgray', 'wheat4', '#DDAD4B', 'chartreuse', 
                                'seagreen1', 'moccasin', 'mediumvioletred', 'seagreen','cadetblue1',
                                "#999933","tan2" ,   "tomato3" , "#7CE3D8","gainsboro",
                                "darkolivegreen","coral1","darkorange4"))
#----

#Data transformation ----
# Creating a temporary table to run the summarize function on
tmpdata=vacc_rec[vacc_rec$overall!=-1 | vacc_rec$mmr!=-1,]
tmpdata$overall[tmpdata$overall==-1]<-NA
tmpdata$mmr[tmpdata$mmr==-1]<-NA
tmpdata=mutate(tmpdata,
               mmr_count=ifelse(is.na(mmr),0,1),
               overall_count=ifelse(is.na(overall),0,1)) #created separate columns in order to count number of schools with a given stat

#Creating the pivot table
Summary_stats<- tmpdata %>% group_by(state) %>% 
  summarize(Total_schools = n(), 
            schools_with_mmr = sum(mmr_count), 
            avg_mmr=ifelse(sum(mmr_count)>0, mean(mmr, na.rm = TRUE), NA), 
            min_mmr=ifelse(sum(mmr_count)>0, min(mmr, na.rm = TRUE), NA),
            max_mmr=ifelse(sum(mmr_count)>0, max(mmr, na.rm = TRUE), NA),
            median_mmr=median(mmr, na.rm = TRUE),
            schools_with_overall=sum(overall_count),
            avg_overall=ifelse(sum(overall_count)>0, mean(overall, na.rm = TRUE), NA),
            min_overall=ifelse(sum(overall_count)>0, min(overall, na.rm = TRUE), NA),
            max_overall=ifelse(sum(overall_count)>0, max(overall, na.rm = TRUE), NA),
            median_overall=median(overall, na.rm = TRUE))

#states with reports of mmr vaccinations
Summary_stats$state[Summary_stats$schools_with_mmr!=0]

#states with reports of overall vaccinations
Summary_stats$state[Summary_stats$schools_with_overall!=0]

write.csv(Summary_stats,"summary_stats_by_state.csv")


School_cnt_bar_data = as.data.frame(rbind(rename(mutate(Summary_stats[,c("state","schools_with_mmr")],Vaccination = "mmr"),"Num_of_schools"="schools_with_mmr"),
                                          rename(mutate(Summary_stats[,c("state","schools_with_overall")],Vaccination = "overall"),"Num_of_schools"="schools_with_overall")))

ggplot(data=School_cnt_bar_data) +
  geom_bar(aes(y=state, x=Num_of_schools, fill=Vaccination), color='black', stat = "identity", position = "dodge", alpha=0.8) +
  labs(title="Schools with Overall & MMR vaccination by state", y="States") + 
  theme(axis.text.x = element_text(angle = 90), legend.position='top') +
  scale_fill_manual(values=c("mmr"="darkorange4","overall" = "khaki"))

thm <- theme(axis.text = element_text(size = 40), axis.title = element_text(size = 40),
             strip.text.x = element_text(size =40), plot.title = element_text(size = 45),
             legend.position = "none",
             plot.margin = margin(2,2,2,2, "cm") )

jpeg(paste0("VaccinationSummary.jpg"),width=3000,height = 4800)
print(grid.arrange(PerMMRVacVsNumSchool + thm , PerOverallVacVsNumSchool + thm ,
                   TypeSchMMRBOx + thm , TypeSchOverallBox + thm,
                   TypeSchMMR  +thm, TypeSchOverall + thm,
                   TypeSchFracMMR + thm, TypeSchFracOverall + thm, ncol=2 )) 
dev.off()

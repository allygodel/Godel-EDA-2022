
library(tidyverse)
library(readxl)

KidsCount1_URL <- "https://datacenter.kidscount.org/rawdata.axd?ind=9714&loc=1"
download.file(KidsCount1_URL, "data/KidsCountInfluenza.xlsx", mode = "wb")

InfluenzeSchool <- 
  read_excel("data/KidsCountInfluenza.xlsx") %>% 
  print()

KidsCount2_URL <- "https://datacenter.kidscount.org/rawdata.axd?ind=10883&loc=1"
download.file(KidsCount2_URL, "data/KidsCountSARS.xlsx", mode = "wb")

SARSSchool <- 
  read_excel("data/KidsCountSARS.xlsx") %>% 
  separate(col = "TimeFrame", into = c("start_date", "end_date"), sep = "-") %>% 
  mutate(
    start_date = paste0(start_date, ", ", str_extract(end_date, "\\d{4}")),
    start_date = as.Date(start_date, format = "%b %e, %Y"),
    end_date = as.Date(end_date, format = "%b %e, %Y"),
    Data = as.numeric(Data)
  ) %>% 
  print() ##??

CDCSARS <- "https://covidtracking.com/data/download/national-history.csv"
download.file(CDCSARS, "data/CDCSARSTracker.csv")

SARSTracker <- 
  read_csv("data/CDCSARSTracker.csv") %>% 
  print()


#Will have to enter FluView_Stack for Influenza 2018-2019 manually through "Import Data"
view(FluView_StackedColumnChart_Data) %>% 
  print()

##read_excel("data/FluView_StackedColumnChart_Data.xlsx") %>% 
tranmute(
  Year = YEAR,
  Week = WEEK,
  Total = `TOTAL SPECIMENS`,
  end_date = paste(year, week),
  date2 = as.Date(end_date, format = "%Y %V")
) ##??

## Graphs for Influenza School TimeSeries
print(FluView_StackedColumnChart_Data)

FluView_StackedColumnChart_Data %>% 
  ggplot(aes(x = WEEK, y = `TOTAL SPECIMENS`)) +
  geom_jitter()  +
  geom_line() +
  labs(
    title = "Students Missing 11 or More Days; Illness and Injury",
    subtitle = "2018-2019; Nation Wide",
    x = "Oct. 1, 2018 ~ Sep. 23, 2019",
    y = "Total Specimens"
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  )
ggsave("covid_online.png")


print(InfluenzeSchool)

## Graphs for Influenza School
InfluenzeSchool %>% 
  filter(
    LocationType == "Nation") %>% 
  ggplot(aes(x = TimeFrame, y = Data, color = `Age group`)) +
  geom_jitter()  +
  geom_line() +
  labs(
    title = "Students Missing 11 or More Days; Illness and Injury",
    subtitle = "2016-2019; Nation Wide",
    x = "Date",
    y = "Age Group"
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  )
ggsave("covid_online.png") ##Connect with line?


##Graphs for SARSTracker TimeSeries
print(SARSTracker)

SARSTracker %>% 
  ggplot(aes(x = date, y = positive)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Positive SARS-CoV-2 Tests",
    subtitle = "April 2020 to March 2021",
    x = "Date",
    y = "Number of Cases"
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  )
ggsave("covid_online.png") 
##How to make Number's of Cases not scientific 

##Graphs for SARSSchool 
print(SARSSchool)


SARSSchool %>% 
  distinct(COVIDImpactEduc)

##Online
SARSSchool %>% 
  filter(
    LocationType == "Nation",
    COVIDImpactEduc == "Classes moved to distance learning: using online resources"
  ) %>% 
  ggplot(aes(x = end_date, y = Data)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Classes Moved to Distance Learning: Using Online Resources",
    subtitle = "April 2020 to March 2021",
    x = "Date",
    y = "Percent of Classes"
  ) +
  theme_bw(base_size = 16
  )
ggsave("covid_online.png")

##Paper
SARSSchool %>% 
  filter(
    LocationType == "Nation",
    COVIDImpactEduc == "Classes moved to distance learning: using paper materials sent home"
  ) %>% 
  ggplot(aes(x = end_date, y = Data)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Classes Moved to Distance Learning; Paper Materials Sent Home",
    subtitle = "April 2020 to March 2021",
    x = "Date",
    y = "Percent of Classes"
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical")
ggsave("covid_online.png")

##Cancelled
SARSSchool %>% 
  filter(
    LocationType == "Nation",
    COVIDImpactEduc == "Classes were cancelled"
  ) %>% 
  ggplot(aes(x = end_date, y = Data)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Classes Cancelled",
    subtitle = "April 2020 to March 2021",
    x = "Date",
    y = "Percent of Classes"
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  )
ggsave("covid_online.png")

##Other Change
SARSSchool %>% 
  filter(
    LocationType == "Nation",
    COVIDImpactEduc == "Classes change in some other way"
  ) %>% 
  ggplot(aes(x = end_date, y = Data)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Classes Changed in Some Other Way",
    subtitle = "April 2020 to March 2021",
    x = "Date",
    y = "Percent of Classes"
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  )
ggsave("covid_online.png")

##No Change, No Close
SARSSchool %>% 
  filter(
    LocationType == "Nation",
    COVIDImpactEduc == "No change to classes because schools did not close"
  ) %>% 
  ggplot(aes(x = end_date, y = Data)) +
  geom_point() +
  geom_line() +
  labs(
    title = "No Change to Classes; Schools Did Not Close",
    subtitle = "April 2020 to March 2021",
    x = "Date",
    y = "Percent of Classes"
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  )
ggsave("covid_online.png")
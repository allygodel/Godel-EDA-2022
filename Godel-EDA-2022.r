
library(tidyverse)
library(readxl)

KidsCount1_URL <- "https://datacenter.kidscount.org/rawdata.axd?ind=9714&loc=1"
download.file(KidsCount1_URL, "data/KidsCountInfluenza.xlsx", mode = "wb")

InfluenzeSchool <- 
  read_excel("data/KidsCountInfluenza.xlsx") %>% 
  print()

KidsCount2_URL <- "https://datacenter.kidscount.org/rawdata.axd?ind=10883&loc=1"
download.file(KidsCount2_URL, "data/KidsCountSARS.xlsx", mode = "wb")

CDCSARS <- "https://covidtracking.com/data/download/national-history.csv"
download.file(CDCSARS, "data/CDCSARSTracker.csv")

SARSTracker <- 
  read_csv("data/CDCSARSTracker.csv") %>% 
  print()

FluView_StackedColumnChart_Data <-
  read_excel("data/FluView_StackedColumnChart_Data.xlsx") %>% 
  print()

# Influenza Graph ------------------------------------------------------

# FROM CHRIS
FluView_StackedColumnChart_Data <- 
  read_excel("data/FluView_StackedColumnChart_Data.xlsx") %>% 
  transmute(
    year = YEAR,
    week = WEEK,
    total = `TOTAL SPECIMENS`,
    date = MMWRweek::MMWRweek2Date(year, week)
  ) %>% 
  print()

FluView_StackedColumnChart_Data %>% 
  ggplot(aes(x = date, y = total)) +
  geom_point()  +
  geom_line(color = "red") +
  labs(
    title = "Total Specimens Viewed (Influenza B, A, and H1N1 Observed)",
    subtitle = "2018-2019; Nation Wide",
    x = "Oct. 1, 2018 ~ Sep. 23, 2019",
    y = "Total Specimens"
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  )
ggsave("graphs/Influenza_online.png")

print(InfluenzeSchool)


# Graphs for Influenza School ---------------------------------------------

library(viridis)

InfluenzeSchool %>% 
  distinct(TimeFrame)

InfluenzeSchool %>% 
  filter(
    LocationType == "Nation",
    TimeFrame == "2018-2019") %>% 
  ggplot(aes(x = `Age group`, y = Data, fill = `Age group`)) +
  geom_col() +
  labs(
    title = "Percent of Students Missing 11 or More Days; Illness and Injury",
    subtitle = "2018-2019; Nation Wide",
    x = "Age Group",
    y = "Percent of Students Missing 11 or More Days"
  ) +
  scale_fill_viridis_d(option="viridis") +
  theme_bw(base_size = 16)
ggsave("graphs/InfluenzaSchool_online.png")


# Graphs for SARSTracker TimeSeries ---------------------------------------

print(SARSTracker)

SARSTracker %>% 
  ggplot(aes(x = date, y = positive)) +
  geom_point() +
  geom_line(color = "red") +
  labs(
    title = "Positive SARS-CoV-2 Tests",
    subtitle = "Janurary 2020 to Janurary 2021",
    x = "Date",
    y = "Number of Cases",
    prettyNum(03e+07, big.mark = ",", scientific = FALSE)
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  )
ggsave("graphs/covid_online.png") 


# Graphs for SARSSchool ---------------------------------------------------
SARSSchool %>% 
  distinct(COVIDImpactEduc)

##Online
SARSSchool %>% 
  filter(
    LocationType == "Nation",
    COVIDImpactEduc %in% c(
      "Classes moved to distance learning: using online resources",
      "Classes moved to distance learning: using paper materials sent home",
      "Classes change in some other way",
      "Classes were cancelled",
      "Classes change in some other way",
      "No change to classes because schools did not close")
    ) %>% 
  ggplot(aes(x = end_date, y = Data)) +
  geom_point() +
  geom_line(aes(color = COVIDImpactEduc)) +
  facet_wrap(~COVIDImpactEduc, ncol= 1) +
  labs(
    title = "Distance Learning; Using Online Resources",
    subtitle = "June 2020 to April 2021",
    x = "Month",
    y = "Percent of Classes"
  ) +
  guides(color = "none")

ggsave("graphs/covidschool_online.png")
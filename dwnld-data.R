KidsCount1_URL <- "https://datacenter.kidscount.org/rawdata.axd?ind=9714&loc=1"
download.file(KidsCount1_URL, "data/KidsCountInfluenza.xlsx", mode = "wb")

KidsCount2_URL <- "https://datacenter.kidscount.org/rawdata.axd?ind=10883&loc=1"
download.file(KidsCount2_URL, "data/KidsCountSARS.xlsx", mode = "wb")

SARSTracker<- "https://covidtracking.com/data/download/national-history.csv"
download.file(CDCSARS, "data/CDCSARSTracker.csv")

FluView_StackedColumnChart_Data <-
  read_excel("data/FluView_StackedColumnChart_Data.xlsx")


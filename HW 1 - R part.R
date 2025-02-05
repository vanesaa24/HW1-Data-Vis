library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#Part 1.
# 1
Crime <- read.csv("crime_data.csv")
(head(Crime, 5))

#2,3
(Missing <- colSums(is.na(Crime)))
Cleanable <- names(Missing[Missing > 0.5 * nrow(Crime)])

Crime2.0 <- Crime %>%
  select(-all_of(Cleanable)) %>%
  mutate(DATE.OCC = as.Date(DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p")) %>%
  mutate(Year = format(DATE.OCC, "%Y")) %>%
  mutate(Month = format(DATE.OCC, "%m")) %>%
  mutate(Day = format(DATE.OCC, "%d")) %>%
  mutate(TIME.OCC = as.character(TIME.OCC))

TimeOfCrime <- c()
for (i in 1:length(Crime2.0$TIME.OCC)) {
  if (nchar(Crime2.0$TIME.OCC[i]) < 4) {
    while (nchar(Crime2.0$TIME.OCC[i]) < 4) {
      Crime2.0$TIME.OCC[i] = paste0("0", Crime2.0$TIME.OCC[i])
    }
    result = Crime2.0$TIME.OCC[i]
  } else {result = Crime2.0$TIME.OCC[i]}
  TimeOfCrime <- c(TimeOfCrime, result)
}

Hour <- substr(TimeOfCrime, 1, 2)
Minute <- substr(TimeOfCrime, 3, 4)
time <- paste0(Hour, sep = ":", Minute)
TimeOfCrime <- format(time, format = "%H:%M")

Crime2.0$TimeOfCrime <- TimeOfCrime

#4,5
Crime2.1 <- Crime2.0 %>%
  filter(Year == 2023) %>%
  filter(Crm.Cd.Desc == "BURGLARY") %>%
  group_by(AREA.NAME) %>%
  summarise(NumOfCrimes = n(),
            AvgVicAge = round(mean(Vict.Age),0)) %>%
  arrange(desc(NumOfCrimes))

#Part 3.
#1
Crime2.2 <- Crime2.0 %>%
  group_by(Month) %>%
  summarise(NumOfCrimes = n())
#2
Crime2.3 <- Crime2.0 %>%
  filter(Weapon.Desc != "")
(nrow(Crime2.3))
#3
Crime2.4 <- Crime2.0 %>%
  group_by(Premis.Desc) %>%
  summarise(NumOfCrimes = n())

#Part 4.
Crime2.0$Severity.Score = c("1")
for (i in 1:nrow(Crime2.0)) {
  if (Crime2.0$Crm.Cd.Desc[i] == "BURGLARY") {
    if (Crime2.0$Weapon.Desc[i] != "") {
      Crime2.0$Severity.Score[i] = "8"
    } else {Crime2.0$Severity.Score[i] = "3"}
  } else if (Crime2.0$Weapon.Desc[i] != "") {
    Crime2.0$Severity.Score[i] = "5"
  } else {Crime2.0$Severity.Score[i] = "1"}
}

Crime2.5 <- Crime2.0 %>%
  mutate(Severity.Score = as.numeric(Severity.Score)) %>%
  group_by(AREA.NAME) %>%
  summarise(Total.Severity.Score = sum(Severity.Score))


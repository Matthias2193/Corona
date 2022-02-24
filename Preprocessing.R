#Import libraries
library(utils)
library(tidyverse)
#Import the data sets
#data <- read.csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_ei/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
cases <- read.csv("Data/Cases.csv")
vaccination <- read.csv("Data/Vaccination.csv")
hospitalisation <- read.csv("Data/Hospitalisation.csv")
measures <- read.csv("Data/Measures.csv")
testing <- read.csv("Data/Testing.csv")
variants <- read.csv("Data/Variants.csv")
rki_cases <- read.csv("Data/RKI_COVID19.csv")

#Preprocessing
##Cases
cases <- cases[cases$countriesAndTerritories == "Germany",]
cases$day <- sprintf("%02d",cases$day)
cases$month <- sprintf("%02d",cases$month)
cases$dateRep <- as.Date(cases$dateRep, format = "%d/%m/%Y")
cases$calenderWeek <- strftime(cases$dateRep, format = "%V")
cases$year <- as.character(cases$year)
cases$countriesAndTerritories <- NULL
cases$geoId <- NULL
cases$countryterritoryCode <- NULL
cases$continentExp <- NULL


##Cases RKI
rki_cases[, 1:3] <- NULL
rki_cases[, 7:ncol(rki_cases)] <- NULL
rki_cases[,c("Geschlecht", "ObjectId")] <- NULL

rki_cases <- rki_cases %>% 
  group_by(Altersgruppe, Meldedatum) %>%
  summarise_at(vars(c("AnzahlFall", "AnzahlTodesfall")), 
               funs(sum))

for(a in unique(rki_cases$Altersgruppe)){
  rki_cases[, paste(a, "Cases", sep = "_")] <- apply(rki_cases, 1,  function(x){return(ifelse(x["Altersgruppe"] == a, as.numeric(x["AnzahlFall"]), 0))})
  rki_cases[, paste(a, "Deaths", sep = "_")] <- apply(rki_cases, 1,  function(x){return(ifelse(x["Altersgruppe"] == a, as.numeric(x["AnzahlTodesfall"]), 0))})
  
}

rki_cases <- rki_cases %>% 
  group_by(Meldedatum) %>%
  summarise_at(vars(colnames(rki_cases)[3:ncol(rki_cases)]), 
               funs(sum))
rki_cases$day <- strftime(rki_cases$Meldedatum, format = "%d")
rki_cases$month <- strftime(rki_cases$Meldedatum, format = "%m")
rki_cases$year <- strftime(rki_cases$Meldedatum, format = "%Y")
rki_cases$calenderWeek <- strftime(rki_cases$Meldedatum, format = "%V")
rki_cases <- rename(rki_cases, cases = AnzahlFall)
rki_cases <- rename(rki_cases, deaths = AnzahlTodesfall)
rki_cases$date <- as.Date(rki_cases$Meldedatum)

##Vaccination
vaccination <- vaccination[vaccination$TargetGroup == "ALL",]
vaccination <- vaccination[vaccination$ReportingCountry == "DE",]
vaccination$year <- lapply(vaccination$YearWeekISO, function(x){
  return(unlist(strsplit(x, "-"))[1])
})
vaccination$calenderWeek <- unlist(lapply(vaccination$YearWeekISO, function(x){
  return(as.numeric(unlist(strsplit(x, "W"))[2]))
}))
vaccination$year <- as.character.numeric_version(vaccination$year)
vaccination$calenderWeek <- sprintf("%02d",vaccination$calenderWeek )
for(v in unique(vaccination$Vaccine)){
  vaccination[, paste(v, "FirstDose", sep = "_")] <- apply(vaccination, 1,  function(x){return(ifelse(x["Vaccine"] == v, as.numeric(x["FirstDose"]), 0))})
  vaccination[, paste(v, "SecondDose", sep = "_")] <- apply(vaccination, 1,  function(x){return(ifelse(x["Vaccine"] == v, as.numeric(x["SecondDose"]), 0))})
  vaccination[, paste(v, "DoseAdditional1", sep = "_")] <- apply(vaccination, 1,  function(x){return(ifelse(x["Vaccine"] == v, as.numeric(x["DoseAdditional1"]), 0))})
  vaccination[, paste(v, "Received", sep = "_")] <- apply(vaccination, 1,  function(x){return(ifelse(x["Vaccine"] == v, as.numeric(x["NumberDosesReceived"]), 0))})
}
vaccination$ReportingCountry <- NULL
vaccination$Denominator <- NULL
vaccination$YearWeekISO <- NULL

vaccination <- vaccination %>% 
  group_by(calenderWeek, year) %>%
  summarise_at(vars(colnames(vaccination)[14:length(colnames(vaccination))]), 
               funs(sum))

for(n in colnames(vaccination)[3:ncol(vaccination)]){
  vaccination[paste("cummulative", n, sep = "_")] <- 0
  for(r in 1:nrow(vaccination)){
    vaccination[r, paste("cummulative", n, sep = "_")] <- sum(
      filter(vaccination, year < vaccination[r, ]$year | (calenderWeek <=  vaccination[r,]$calenderWeek & year == vaccination[r, ]$year))[n]
    )
  }
}

##Testing
testing <- testing[testing$country_code == "DE",]
testing$year <- lapply(testing$year_week, function(x){
  return(unlist(strsplit(x, "-"))[1])
})
testing$calenderWeek <- unlist(lapply(testing$year_week, function(x){
  return(as.numeric(unlist(strsplit(x, "W"))[2]))
}))
testing[, 1:7] <- NULL
testing$testing_data_source <- NULL
testing$population <- NULL
testing$year <- as.character.numeric_version(testing$year)
testing$calenderWeek <- sprintf("%02d",testing$calenderWeek )

##Hospitalisation
hospitalisation <- rename(hospitalisation, number_hosp = value)
hospitalisation <- hospitalisation[hospitalisation[,1] == "Germany",]
hospitalisation[,c("year_week", "source", "url")] <- NULL
hospitalisation$date <- as.Date(hospitalisation$date)
hospitalisation$day <- strftime(hospitalisation$date, format = "%d")
hospitalisation$month <- strftime(hospitalisation$date, format = "%m")
hospitalisation$year <- strftime(hospitalisation$date, format = "%Y")
hospitalisation$calenderWeek <- strftime(hospitalisation$date, format = "%V")
hospitalisation[,1] <- NULL
hospitalisation$da <- NULL
hosp_daily <- hospitalisation[hospitalisation$indicator == "Daily ICU occupancy",]
hosp_daily$indicator <- NULL
hosp_daily$date <- NULL
hosp_weekly <- hospitalisation[hospitalisation$indicator == "Weekly new hospital admissions per 100k",]

##Measures
measures <- filter(measures, Country == "Germany")
measures$date_end <- as.Date(measures$date_end)
measures$date_end[is.na(measures$date_end)] <- Sys.Date()
new_measures <- data.frame(date = seq(as.Date("2020-01-01"), Sys.Date(), by = "days"))
for (m in unique(measures$Response_measure)) {
  new_measures[,m] <- FALSE
  for (r in 1:nrow(new_measures)) {
    if(sum(measures$Response_measure == m) > 0){
      for(s in 1:sum(measures$Response_measure == m)){
        if(new_measures$date[r] >= filter(measures, Response_measure == m)[s,]$date_start &&
           new_measures$date[r] <= filter(measures, Response_measure == m)[s,]$date_end){
          new_measures[,m][r] <- TRUE
        }
      }
    }
  }
}
new_measures$day <- strftime(new_measures$date, format = "%d")
new_measures$month <- strftime(new_measures$date, format = "%m")
new_measures$year <- strftime(new_measures$date, format = "%Y")


##Variants
variants <- filter(variants, country_code == "DE")
variants$year <- lapply(variants$year_week, function(x){
  return(unlist(strsplit(x, "-"))[1])
})
variants$calenderWeek <- unlist(lapply(variants$year_week, function(x){
  return(as.numeric(unlist(strsplit(x, "-"))[2]))
}))
for(w in variants$calenderWeek){
  variants[variants$calenderWeek == w, ]$number_sequenced <- max(variants[variants$calenderWeek == w, ]$number_sequenced)
}
variants$percent_variant <- variants$number_detections_variant/variants$number_sequenced
variants$percent_cases_sequenced <- variants$number_sequenced/variants$new_cases
variants[,1:5] <- NULL
variants$valid_denominator <- NULL
variants$number_sequenced_known_variant <- NULL
for(v in unique(variants$variant)){
  variants[, paste(v, "number_detections_variant", sep = "_")] <- apply(variants, 1,  function(x){return(ifelse(x$variant == v, x$number_detections_variant, 0))})
  variants[, paste(v, "percent_variant", sep = "_")] <- apply(variants, 1,  function(x){return(ifelse(x$variant == v, x$percent_variant, 0))})
}
variants <- variants %>% 
  group_by(calenderWeek, year, number_sequenced, percent_cases_sequenced) %>%
  summarise_at(vars(colnames(variants)[8:length(colnames(variants))]), 
               funs(sum))
variants$year <- as.character.numeric_version(variants$year)
variants$calenderWeek <- sprintf("%02d",variants$calenderWeek )

new_data <- merge(rki_cases, vaccination, by = c("calenderWeek", "year"), all.x = T)
new_data <- merge(new_data, testing, by = c("calenderWeek", "year"), all.x = T)
new_data <- merge(new_data, hosp_daily, by = c("day", "month", "year", "calenderWeek"), all.x = T)
new_data <- merge(new_data, new_measures, by = c("day", "month", "year"), all.x = T)
new_data <- merge(new_data, variants, by = c("calenderWeek", "year"), all.x = T)
new_data$Meldedatum <- as.Date(new_data$Meldedatum)
new_data <- rename(new_data, date = Meldedatum)
new_data[,c("date.x", "date.y")] <- NULL

new_data$weekday <- weekdays(new_data$date)

data <- na.omit(new_data)
data <- arrange(data, date)
data_min <- new_data[,c("date", "cases", "deaths")]
data_min <- arrange(data_min, date)



write.csv(data, file = "Data/data.csv")
write.csv(data_min, file = "Data/data_min.csv")


#Clean data
data <- read.csv("Data/Data.csv")
data[,9:22] <- NULL

#Combine data of all vaccines
data$FirstDose <- apply(data[,grepl("FirstDose",colnames(data))][,1:4]  , 1, sum)
data$SecondDose <- apply(data[,grepl("SecondDose",colnames(data))][,1:4]  , 1, sum)
data$DoseAdditional <- apply(data[,grepl("DoseAdditional",colnames(data))][,1:4]  , 1, sum)

doses <- c("FirstDose", "SecondDose", "DoseAdditional1")
vacs <- c("COM", "MOD", "AZ", "JANSS")
for(d in doses){
  temp_names <- c()
  for(v in vacs){
    temp_names <- c(temp_names, paste("cummulative_", v, "_FirstDose", sep=""))
  }
  data[,paste("cummulative", d, sep="_")] <- apply(data[, temp_names], 1, sum)
}

#Remove numbers of detections of variants to instead focus on percentage
data[,9:42] <- NULL
data[,grepl("number_detections",colnames(data))] <- NULL

#Remove all variants with max percentage < 1
remove_cols <- c()
for(n in grep("percent_variant",colnames(data))){
  if(summary(data[,n])["Max."] < 1){
    remove_cols <- c(remove_cols, n)
  }
}
data[,remove_cols] <- NULL

#Remove all measures which are constant through the observed time frame
for(n in colnames(data)){
  if(length(summary(data[,n])) == 2){
    data[,n] <- NULL
  }
}




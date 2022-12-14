# ..................................
# This script organize the data and produce some summaries

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")
library("magrittr")
library("tidyverse")
library("raster")
library("sf")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

load("processing/trial-data.rda")

# keep only the projects with data on it
keep <- lapply(cmdata, function(x){
  length(x[["data"]]) > 1
})

keep <- unlist(keep)

sum(keep) / length(keep)

# add the logic vector to the data with project info
projects$keep <- keep

cmdata <- cmdata[keep]

# put the data into data.frame format
dat <- lapply(cmdata, function(x){
  x <- as.data.frame(x, 
                     tidynames = TRUE,
                     pivot.wider = TRUE)
  
  names(x) <- make_clean_names(names(x))
  
  x
  
})

# get unique col names across the data.frames
uniquenames <- lapply(dat, function(x){
  names(x)
})

uniquenames <- unique(unlist(uniquenames))


# for the summaries here, I only use data from package distribution and participant registration
uniquenames <- uniquenames[grep("id$|package_|registration_|_longitude|_latitude", uniquenames)]

uniquenames

# fix some codes that diverged over time
dat <- lapply(dat, function(x){
  names(x)[names(x) == "registration_gender1"] <- "registration_gender"
  names(x)[names(x) == "registration_biogender"] <- "registration_gender"
  names(x)[names(x) == "registration_bioage"] <- "registration_age"
  names(x)[names(x) == "registration_kebele"] <- "registration_village"
  names(x)[names(x) == "registration_location"] <- "registration_village"
  
  k1 <- names(x) %in% uniquenames
  k2 <- uniquenames %in% names(x)
  k <- unique(c(names(x)[k1], uniquenames[k2]))
  
  x <- x[, k]
  x
})

# put the data together 
dat <- rowbind(dat)

# Coordinates
# get the coordinates as an independent data.frame
keep <- grepl("_longitude|_latitude", names(dat))

lonlat <- dat[, keep]

dat <- dat[, !keep]

lon <- grep("_longitude", names(lonlat))
lon <- lonlat[, lon]

lon <- as.vector(apply(lon, 1, function(x){
  # I'll take the reverse as this increases the likelihood of 
  # getting the coordinates from the trial, not the point of 
  # delivery
  names(x)[rev(which(!is.na(x)))[1]]
}))

lon[is.na(lon)] <- "registration_pointofdelivery_longitude"

lat <- gsub("_longitude", "_latitude", lon)

table(lon)
table(lat)

# keep only the selected columns, one per plot
lonlat <- data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)], 
                     latitude = lonlat[cbind(1:nrow(lonlat), lat)])


lonlat[1:2] <- lapply(lonlat[1:2], as.numeric)

plot(lonlat)

# latitude higher than 23 is wrong
lonlat$longitude[lonlat$latitude > 23] <- NA
lonlat$latitude[lonlat$latitude > 23] <- NA

# also lonlat with 0, 0
lonlat$longitude[lonlat$longitude == 0 & lonlat$latitude == 0] <- NA
lonlat$latitude[lonlat$longitude == 0 & lonlat$latitude == 0] <- NA

plot(lonlat)


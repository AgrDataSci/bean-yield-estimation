# ..................................
# This script organize the data and produce some summaries

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")


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


# fix some codes that diverged over time
dat <- lapply(dat, function(x){
  names(x)[names(x) == "registration_gender1"] <- "registration_gender"
  names(x)[names(x) == "registration_biogender"] <- "registration_gender"
  names(x)[names(x) == "registration_bioage"] <- "registration_age"
  names(x)[names(x) == "registration_kebele"] <- "registration_village"
  names(x)[names(x) == "registration_location"] <- "registration_village"

  x
})

# put the data together 
dat <- rowbind(dat)

# create an id combining package id and project code 
dat$block_id <- paste(dat$package_project_name, dat$id, sep = "-")

dat <- dat[, union("block_id", names(dat))]



# Coordinates
# get the coordinates as an independent data.frame
keep <- grepl("_longitude|_latitude", names(dat))

lonlat <- dat[, keep]


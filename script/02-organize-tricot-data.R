# ..................................
# This script organize the data and produce some summaries

# load packages
library("ClimMobTools")
library("readxl")
library("tidyverse")
library("gosset")
library("janitor")

list.files("data", full.names = TRUE)

# read tricot data
load("processing/trial-data.rda")

# read yield and grain data collected on-farm 
# check names of sheets
filename = "data/Yield determination survey.xlsx"
excel_sheets(filename)
# data file
dat = read_excel(filename, sheet = 2, na = "NA")
# descriptors and labels
descrip = read_excel(filename, sheet = 3, na = "NA")

# make clean names
names(dat) = make_clean_names(names(dat))

names(dat)

# put the tricot data into data.frame format
cmdata = lapply(cmdata, function(x){
  x = as.data.frame(x, 
                     tidynames = TRUE,
                     pivot.wider = TRUE)
  
  names(x) = make_clean_names(names(x))
  x
})

# get unique col names across the tricot data
uniquenames = lapply(cmdata, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

# fix some codes that diverged over time on tricot data
cmdata = lapply(cmdata, function(x){
  names(x)[names(x) == "registration_gender1"] = "registration_gender"
  names(x)[names(x) == "registration_biogender"] = "registration_gender"
  names(x)[names(x) == "registration_bioage"] = "registration_age"
  names(x)[names(x) == "registration_kebele"] = "registration_village"
  names(x)[names(x) == "registration_location"] = "registration_village"
  
  x
})

# put the tricot data together 
cmdata = rowbind(cmdata)

cmdata

# create an id combining package id and project code 
cmdata$block_id = paste(cmdata$package_project_name, cmdata$id, sep = "-")

cmdata = cmdata[, union("block_id", names(cmdata))]

# write tricot data as csv
# first remove farmer name
keep = !grepl("participant_name", names(cmdata))

cmdata = cmdata[keep]

write.csv(cmdata, "data/bean-kilindi-karatu-tricot-data.csv", 
          row.names = F)



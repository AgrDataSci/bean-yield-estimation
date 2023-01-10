# ..................................
# This script organize the data and produce some summaries

# load packages
library("ClimMobTools")
library("readxl")
library("gosset")
library("janitor")

list.files("data", full.names = TRUE)

load("processing/trial-data.rda")

# check names of sheets
filename = "data/Yield determination survey.xlsx"

excel_sheets(filename)

# data file
dat = read_excel(filename, sheet = 2, na = "NA")

# descriptors and labels
descrip = read_excel(filename, sheet = 3, na = "NA")

# make clean names
names(dat) = make_clean_names(names(dat))







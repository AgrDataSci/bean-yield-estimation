# ..................................
# This script organizes the seed and yield data 
# collected on farm and at the lab
# load packages
library("readxl")
library("tidyverse")
library("gosset")
library("ClimMobTools")
library("PlackettLuce")
library("janitor")
library("car")

source("script/00-functions.R")

list.files("data", full.names = TRUE)

# read the tricot data
cmdata = read.csv("data/bean-kilindi-karatu-tricot-data.csv")

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

# create the same id for the yield data
dat = dat[!is.na(dat$package_id), ]

dat$block_id = paste(dat$climmob_code, dat$package_id, sep = "-")

dat = dat[, union("block_id", names(dat))]

dat

#  check if the data contains the same id
all(dat$block_id %in% cmdata$block_id)

# check the varieties assessed
pack_index = paste0("package_item_", letters[1:3])

sort(table(unlist(cmdata[pack_index])))

# ..............................
# unlist the seed data 
# some were collected three times and others 10 times 
# first deal with the ones collected three times 
names(dat)
pattern = c("mc", "sw", "t", "w", "l")
pattern_name = c("moisture_content", "hundred_seed_weight", 
                 "thickness", "width", "length")

r = c(3, 3, 10, 10, 10)

# we need to make an id for all the entries 
list_dat = list()

# run over pattern
for (i in seq_along(pattern)) {
  
  dat_i = data.frame()
  
  # run over plots a b c
  for (j in 1:3) {
    
    x = paste0(paste0(pattern[i], letters[j]), 1:r[i])
    
    x = dat[, x]
    
    print(names(x))
    
    x = as.matrix(x)
    
    if (r[i] > 3) {
      
     x = t(apply(x, 1, function(y){
        v1 = min(y)
        v2 = max(y)
        v3 = median(y)
        c(v1, v2, v3)
      }))
     
    }
    
    x = data.frame(block_id = rep(dat$block_id, times = 3),
                   plot = letters[j],
                   rep = rep(1:3, each = nrow(dat)),
                   value = as.vector(x))
    
    dat_i = rbind(dat_i, x)
    
  }
  
  names(dat_i)[names(dat_i) == "value"] = pattern_name[i]
  
  dat_i$id = paste(dat_i$block_id, dat_i$block, dat_i$rep, sep = "-")
  
  list_dat[[i]] = dat_i
  
}

lapply(list_dat, head)

lapply(list_dat, nrow)

# ..............................
# ..............................
# yield data ####
paste(names(dat), collapse = "', '")
sel = c('block_id',
        'farmer_volume_var_a','farmer_volume_var_b','farmer_volume_var_c',
        'tech_volume_var_a','tech_volume_var_b','tech_volume_var_c',
        'grain_yield_var_a','grain_yield_var_b','grain_yield_var_c',
        'n_plant_var_a', 'n_plant_var_b', 'n_plant_var_c', 
        'percent_survival_var_a', 'percent_survival_var_b', 'percent_survival_var_c')


yield = dat[, sel]

yield

# merge with tricot data to get block information (varieties names)
yield = merge(cmdata[, c("block_id", pack_index)], yield, by = "block_id")

# remove _var_ and _item_ strings in column names to help in transposing
# the data frame
names(yield) = gsub("_var_", "_item_", names(yield))

# transpose the table to long format
names(yield)

yield = transpose(data = yield, 
                  id = "block_id", 
                  blocks = pack_index,
                  variables = c("farmer_volume", "tech_volume", "grain_yield",
                                "n_plant", "percent_survival"), 
                  variables_block = paste0("_item_", letters[1:3]))

yield$id = paste0(yield$id, "-1")

#yield = yield[,-c(2:3)]

# now put the seed metric data together
# joining by id
lapply(list_dat, head)

list_dat = lapply(list_dat, function(x){
  x$id = paste(x$block_id, x$plot, x$rep, sep = "-")
  x
})

result = data.frame(id = list_dat[[5]][, "id"])

length(unique(result$id))

# merge data
for(i in seq_along(list_dat)){
  result = merge(result, list_dat[[i]][,c(4:5)], by = "id", all.x = TRUE)
}

result = merge(result, yield, by = "id", all.x = TRUE)

# separate block_id
block_id = strsplit(result$id, "-")
result$block_id = unlist(lapply(block_id, function(x){
  x = paste(x[1], x[2], sep = "-")
  x
}))

# organize the columns
result = result[, union(c("id", "block_id", "plot", "tech"), names(result))]

paste(names(result), collapse = "', '")

fill_NAs = c('plot', 'tech', 'farmer_volume', 'tech_volume', 'grain_yield', 'n_plant', 'percent_survival')

for(i in seq_along(fill_NAs)) {
  for(j in 2:nrow(result)) {
    result[j, fill_NAs[i]] = ifelse(is.na(result[j, fill_NAs[i]]), 
                                    result[j-1, fill_NAs[i]],
                                    result[j, fill_NAs[i]])
  }
}

write.csv(result,
          "data/grain-yield-seed-metric-data.csv", 
          row.names = FALSE)


# ............................
# now work with the ranked yield collected 
# on the sampled farms
rank_yield = dat[c("block_id", "vy_pr", "vy_nr", "hy_pr", "hy_nr")]

rank_yield = as.data.frame(rank_yield)

rank_yield[rank_yield == 1] = "A"
rank_yield[rank_yield == 2] = "B"
rank_yield[rank_yield == 3] = "C"

keep = cmdata$block_id %in% result$block_id

pack = cmdata[keep, 1:5]

head(pack)

rank_yield = merge(pack, rank_yield, by = "block_id")

traits = getTraitList(rank_yield, c("pr", "nr"), c("Yield", "GrainYield"))

R = lapply(traits, function(x){
  rank_tricot(rank_yield,
              pack_index, 
              x$string)
})

rank_data = data.frame()

for (i in seq_along(traits)) {
  
  r = unclass(R[[i]])
  
  for (j in seq_along(rank_yield$block_id)) {
    
    id = rank_yield$block_id[j]
    
    plots = as.vector(unlist(rank_yield[rank_yield$block_id == id, pack_index]))
    
    x = r[j, plots]
    
    d = data.frame(block_id = id, 
                   plot = letters[1:3],
                   tech = plots,
                   trait = as.vector(traits[[i]]$trait_label),
                   rank = x)
    
    rank_data = rbind(rank_data, d)
    
  }
  
}


mod = lapply(R, function(x){
  PlackettLuce(x[-16, ])
})


compare(mod[[1]], mod[[2]])

write.csv(rank_data, "data/on-farm-ranking.csv", row.names = FALSE)


# now work with the yield data from ClimMob 
R = rank_tricot(cmdata,
                pack_index,
                c("post_harvest_yield_pos","post_harvest_yield_neg"),
                validate.rankings = TRUE)
sum(!is.na(R))

# get the gps data
index = grep("block_id|vegetative_geotrial_lat|vegetative_geotrial_lon|pointofdelivery_lat|pointofdelivery_lon", names(cmdata))

lonlat = cmdata[keep, index]

lonlat$vegetative_geotrial_longitude = ifelse(is.na(lonlat$vegetative_geotrial_longitude),
                                              lonlat$registration_pointofdelivery_longitude, 
                                              lonlat$vegetative_geotrial_longitude)

lonlat$vegetative_geotrial_latitude = ifelse(is.na(lonlat$vegetative_geotrial_latitude),
                                              lonlat$registration_pointofdelivery_latitude, 
                                              lonlat$vegetative_geotrial_latitude)


lonlat = lonlat[,c("block_id","vegetative_geotrial_longitude","vegetative_geotrial_latitude")]

write.csv(lonlat, "data/sampled-plots-coordinates.csv", row.names = FALSE)






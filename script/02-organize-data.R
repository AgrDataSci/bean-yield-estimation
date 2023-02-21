# ..................................
# This script organize the data and produce some summaries

# load packages
library("ClimMobTools")
library("readxl")
library("tidyverse")
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

names(dat)

# put the data into data.frame format
cmdata = lapply(cmdata, function(x){
  x = as.data.frame(x, 
                     tidynames = TRUE,
                     pivot.wider = TRUE)
  
  names(x) = make_clean_names(names(x))
  
  x
  
})

# get unique col names across the data.frames
uniquenames = lapply(cmdata, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))


# fix some codes that diverged over time
cmdata = lapply(cmdata, function(x){
  names(x)[names(x) == "registration_gender1"] = "registration_gender"
  names(x)[names(x) == "registration_biogender"] = "registration_gender"
  names(x)[names(x) == "registration_bioage"] = "registration_age"
  names(x)[names(x) == "registration_kebele"] = "registration_village"
  names(x)[names(x) == "registration_location"] = "registration_village"
  
  x
})

# put the data together 
cmdata = rowbind(cmdata)

# create an id combining package id and project code 
cmdata$block_id = paste(cmdata$package_project_name, cmdata$id, sep = "-")

cmdata = cmdata[, union("block_id", names(cmdata))]

# create the same id for the yield data
dat = dat[!is.na(dat$package_id), ]

dat$block_id = paste(dat$climmob_code, dat$package_id, sep = "-")

dat = dat[, union("block_id", names(dat))]

# merge the data to get variable names
keep = cmdata$block_id %in% dat$block_id

cmdata = cmdata[keep, ]

# check the varieties assessed
pack_index = paste0("package_item_", letters[1:3])

sort(table(unlist(cmdata[pack_index])))

# focus only in the yield data 
paste(names(dat), collapse = "','")
sel = c('block_id', 'farmer_volume_var_a','farmer_volume_var_b','farmer_volume_var_c',
        'tech_volume_var_a','tech_volume_var_b','tech_volume_var_c',
        'grain_yield_var_a','grain_yield_var_b','grain_yield_var_c')


yield = dat[, sel]

# merge
yield = merge(cmdata[, c("block_id", pack_index)], yield, by = "block_id")

names(yield) = gsub("_var_", "_item_", names(yield))


# transpose the table to long format
names(yield)

yield2 = data.frame()

variables = c("farmer_volume", "tech_volume", "grain_yield")

for (i in seq_along(variables)) {
  
  y_i = data.frame(cbind(block_id = rep(yield$block_id, 3), 
                         plot =  rep(letters[1:3], each = nrow(yield)),
                         variety = as.vector(unlist(yield[pack_index]))))
  
  y_i$id = paste(y_i$block_id, y_i$plot, sep = "-")
  
  
  val_i = data.frame(block_id = rep(yield$block_id, 3),
                     plot =  rep(letters[1:3], each = nrow(yield)),
                     variable = variables[i],
                     value = as.numeric(unlist(yield[paste0(variables[i], 
                                                            "_item_", letters[1:3])])))
  
  val_i$id = paste(val_i$block_id, val_i$plot, sep = "-")
  
  val_i = val_i[, -c(1, 2)]
  
  y_i = merge(y_i, val_i, by = "id")
  
  yield2 <- rbind(yield2, y_i)
  
}


yield2 %>% 
  filter(variable != "grain_yield") %>% 
  ggplot(aes(y = value,
             color = variety)) +
  facet_grid(~ variable) +
  geom_boxplot()


cor

xx <- na.omit(data.frame(farmer = yield2[yield2$variable == "farmer_volume", "value"],
                         technician = yield2[yield2$variable == "tech_volume", "value"]))

plot(xx[,1], xx[,2], 
     xlab = "Yield (farmer)",
     ylab = "Yield (technician)")
lines(1, 1)

coef(lm(farmer ~ technician, data = xx))

ggplot(xx, aes(y = technician,
               x = farmer)) +
  geom_jitter() +
  geom_abline(slope = 0.86)










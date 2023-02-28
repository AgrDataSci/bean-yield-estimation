# ..................................
# This script 

# load packages
library("readxl")
library("tidyverse")
library("gosset")
library("janitor")

source("../script/00-functions.R")

list.files("../data", full.names = TRUE)

# read the tricot data
cmdata = read.csv("../data/bean-kilindi-karatu-tricot-data.csv")

# read yield and grain data collected on-farm 
# check names of sheets
filename = "../data/Yield determination survey.xlsx"
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

# check the varieties assessed
pack_index = paste0("package_item_", letters[1:3])

sort(table(unlist(cmdata[pack_index])))

# focus only in the yield data 
paste(names(dat), collapse = "','")

# ..............................
# average the seed geometry data 
# some were collected three times and others 10 times 
# first deal with the ones collected three times 
pattern_3 = c("mc", "sw")
pattern_name_3 = c("moisture_content", "hundred_seed_weight")

averages_3 = data.frame(block_id = dat$block_id)

# run over pattern
for (i in seq_along(pattern)) {
  
  dat_i = data.frame(row.names = 1:nrow(dat))
  # run over plots a b c
  for (j in 1:3) {
    
    x = paste0(pattern_3[i], letters[j], c(1:3))
    
    x = rowMeans(dat[x])
    
    dat_i = cbind(dat_i, x = x)
    
  }
  
  names(dat_i) = paste0(pattern_name_3[i], paste0("_var_", letters[1:3]))
  
  averages_3 = cbind(averages_3, dat_i)
  
}


#add seed geometry, length, width  and thickness
#thickness variety a
pattern_10 = c("t","w","l")
pattern_name_10 = c("tickness_block","width_block", "length_block")


averages_10 = data.frame(block_id = dat$block_id)



# run over pattern
for (i in seq_along(pattern)) {
  
  dat_i = data.frame(row.names = 1:nrow(dat))
  
# run over plots a b c
  for (j in 1:3) {
    
    x = paste0(pattern_10[i], letters[j], c(1:10))
    
    x = rowMeans(dat[x])
    
    dat_i = cbind(dat_i, x = x)
    
  }
  
  names(dat_i) = paste0(pattern_name_10[i], paste0(letters[1:3]))
  
  averages_10 = cbind(averages_10, dat_i)
  
}

# add to the main data 
dat = cbind(dat, averages_3)

# add to the main data 
dat = cbind(dat, averages_10)


# ..............................
sel = c('block_id',
        'farmer_volume_var_a','farmer_volume_var_b','farmer_volume_var_c',
        'tech_volume_var_a','tech_volume_var_b','tech_volume_var_c',
        'grain_yield_var_a','grain_yield_var_b','grain_yield_var_c')


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
                  variables = c("farmer_volume", "tech_volume", "grain_yield"), 
                  variables_block = paste0("_item_", letters[1:3]))


# Scatter Plot
plot(yield$farmer_volume,
     yield$tech_volume,
     main="Tech volume Vs Farmer volume Scatter plot",
     xlab="Farmer volume", 
     ylab="Tech volume",
     pch=20)
# regression line (y~x)
abline(lm(farmer_volume ~ tech_volume, data = yield),
       col="red") 


# Paired Samples Wilcoxon Test
rank_test = wilcox.test(yield$farmer_volume,
                        yield$tech_volume,
                        paired = TRUE)
rank_test

# Printing the results
print(rank_test)

# use Bland and Altman method to compare the measures in yield
# by farmers and technicians

compare_dat = data.frame(farmer = yield$farmer_volume,
                         technician = yield$tech_volume)

compare_dat[compare_dat == 0] = NA

compare_dat = na.omit(compare_dat)

compare_plot = 
  compare(log(compare_dat$farmer),
          log(compare_dat$technician),
          labels = "") +
  geom_jitter() +
  labs(x = "Average Volumetric Yield", 
       y = "Difference (Farmer - Technician)")

ggsave("output/comparison-farmer-vs-tech-volum-yield.png",
       plot = compare_plot,
       height = 13,
       width = 13,
       dpi = 500,
       units = "cm")


# compare grain yield against farmers and tech volume
grain_yield = yield

grain_yield[grain_yield == 0] = NA

grain_yield = na.omit(grain_yield)

# get the conversion rate dividing the grain yield by the volume
grain_yield$conv_rate_tech = grain_yield$grain_yield / grain_yield$tech_volume

grain_yield$conv_rate_farmer = grain_yield$grain_yield / grain_yield$farmer_volume




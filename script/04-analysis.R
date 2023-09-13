# ..................................
# Data analysis with grain yield from farms
library("tidyverse")
library("ggplot2")
library("gosset")
library("PlackettLuce")
library("car")

# read the data
list.files("data", full.names = TRUE)

dat = read.csv("data/grain-yield-seed-metric-data.csv")

market = read.csv("data/market-classes-2022.csv")

dat = merge(dat, market[c("tech", "market_class")], by = "tech", all.x = TRUE)

sort(unique(dat$tech))

unique(dat$tech) %in% unique(market$tech)

# the first thing is to compare the volume collected by farmers 
# and technicians, we use the Bland and Altman method 
boxplot(dat$farmer_volume)
boxplot(dat$tech_volume)

out = boxplot.stats(dat$farmer_volume)$out

out = dat$farmer_volume %in% out

dat[out, ]


cor(dat$farmer_volume, dat$tech_volume)

# ..................................
# ..................................
# Compare tech vs farmer ####
# Scatter Plot
plot(y = dat$farmer_volume,
     x = dat$tech_volume,
     main="Tech volume Vs Farmer volume",
     xlab="Farmer volume",
     ylab="Tech volume",
     pch=20)
# regression line (y~x)
abline(lm(farmer_volume ~ tech_volume, data = dat),
       col="red")


# use Bland and Altman method to compare the measures in yield
# by farmers and technicians
compare_dat = data.frame(farmer = dat$farmer_volume,
                         technician = dat$tech_volume)

# as the approach used log() we need to remove the 0s 
# othewise we get inf values
compare_dat[compare_dat == 0] = NA

compare_dat = na.omit(compare_dat)

compare_plot =
  compare(log(compare_dat$farmer),
        log(compare_dat$technician),
        labels = "") +
  geom_jitter() +
  labs(x = "Average Volumetric Yield",
       y = "Difference (Farmer - Technician)")

compare_plot

 ggsave("output/comparison-farmer-vs-tech-volum-yield.png",
       plot = compare_plot,
       height = 13,
       width = 13,
       dpi = 500,
       units = "cm")

# ..................................
# ..................................
# Effect of seed moisture on seed weight ####
plot(dat$moisture_content, dat$hundred_seed_weight)

# remove outliers
boxplot(dat$moisture_content)

out = boxplot.stats(dat$moisture_content)$out

out = dat$moisture_content %in% out

plot(dat[!out, c("moisture_content", "grain_yield")])

mod_dat = dat[!out, c("moisture_content", "tech", "grain_yield")]

mod_dat = na.omit(mod_dat)

head(mod_dat)

sort(table(mod_dat$tech))

# effect of variety on moisture content 
mod = lm(moisture_content ~ tech, data = mod_dat)

summary(mod)

anova(mod)

mod_dat$preds = predict(mod, newdata = mod_dat)

ggplot(mod_dat, aes(y = tech, x = moisture_content)) +
  geom_boxplot() +
  labs(y = "Variety",
       x = "Moisture content (%)",
       title = paste0("R^2 = ",
                      round(pseudoR2(mod)$McFadden, 3))) +
  theme_classic() 

ggsave("output/moisture-content.pdf",
       plot = last_plot(),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 500)

capture.output(summary(mod),
               file = "output/mod-moisture-content-by-variety.txt")


# effect of variety and moisture content on the seed weight
mod2 = lm(grain_yield ~ ., data = mod_dat)

summary(mod2)


# ..................................
# ..................................
# Bean dimensions ####
# Plot the data as boxplots to find any possible outlier
bean = dat[,c("width", "thickness",
              "length", "hundred_seed_weight",
              "tech", "block_id", "plot", "market_class")]

boxplot(bean$thickness)

boxplot(bean$width)

boxplot(bean$length)

# we see that we have outliers in width and thickness
boxplot(bean$width)
boxplot(bean$thickness)
boxplot(bean$length)
boxplot(bean$hundred_seed_weight)

# ......................................
# Part 2 - remove outliers 
runover = c("width", "thickness",
            "length", "hundred_seed_weight")

for (i in seq_along(runover)) {
  # use function boxplot.stats to identify possible 
  # outliers
  out = boxplot.stats(bean[, runover[i]])$out
  
  rmv = !bean[, runover[i]] %in% out
  
  bean = bean[rmv, ]
  
}

# check the plots again
boxplot(bean$width)
boxplot(bean$thickness)
boxplot(bean$length)
boxplot(bean$hundred_seed_weight)

bean = 
  bean %>% 
  group_by(block_id, plot, tech) %>% 
  summarise(width = mean(width),
            thickness = mean(thickness),
            length = mean(length),
            hundred_seed_weight = mean(hundred_seed_weight)) %>% 
  ungroup()

bean$location = ifelse(grepl("kilin", bean$block_id),
                       "Kilindi",
                       "Karatu")

# ......................................
# Part 3 - Fit a simple linear model
head(bean)

plot(bean$length, 
     bean$hundred_seed_weight)

# fit a linear model using only length
mod = lm(hundred_seed_weight ~ length,
         data = bean)

summary(mod)

plot(bean$length, 
     bean$hundred_seed_weight, 
     xlab = "Length (mm)",
     ylab = "Hundred seed weight (g)")
abline(mod, col = "red")

# ......................................
# Part 4 - fit a multiple linear model 
# a second model with length, width and thickness combined
# plot(bean$width, 
#      bean$hundred_seed_weight)
# 
# plot(bean$thickness, 
#      bean$hundred_seed_weight)
# 
# mod1 = lm(hundred_seed_weight ~  length + width + thickness + tech + location + tech:location,
#           data = bean)
# 
# summary(mod1)
# 
# mod2 = lm(hundred_seed_weight ~ length + width + thickness + tech,
#           data = bean)
# 
# summary(mod2)
# 
# avPlots(mod2)

# # ......................................
# # ......................................
# # Part 5 - Use model equation to predict new hundred seed weight ##
# # extract equation
# coefs = coefficients(mod2)
# 
# coefs
# 
# eq = paste0("HSW = ", 
#             round(coefs[1], 1),
#             " + ",
#             round(coefs[2], 1),
#             "(Length) + ",
#             round(coefs[3], 1),
#             ("(Width) + "),
#             round(coefs[4], 1), 
#             "(Thickness)")
# 
# 
# eq

# ......................................
# ......................................
# Estimate seed weight ##

plot(y = dat$grain_yield, x = dat$farmer_volume)

mod = lm(grain_yield ~ farmer_volume + market_class, data = dat)

summary(mod)

abline(mod, col = "red")

coefs = coefficients(mod)

eq = paste0("Grain weight = ", 
            round(coefs[1], 3),
            " + ",
            round(coefs[2], 3),
            " * (FarmerVolume) + ",
            round(coefs[3], 3),
            " * (ClassRed) + ",
            round(coefs[4], 3),
            " * (ClassRedMottled) + ",
            round(coefs[5], 3),
            " * (ClassSugar)")

pdf(file = "output/lm-yield-farmer-volume.pdf",
    width = 13,
    height = 8)
plot(y = dat$grain_yield, 
     x = dat$farmer_volume,
     main = eq,
     ylab = "Grain weight (g)",
     xlab = "Farmer volume estimation")
abline(mod, col = "red")
dev.off()

summary(mod)

capture.output(summary(mod), 
               file = "output/model-bean-weigth-farmer.txt")



# 
# 
# 
# # seed moisture affecting seed weight
# boxplot(yield$moisture_content)
# 
# moist = yield[yield$moisture_content < 20, ]
# 
# boxplot(moist$moisture_content)
# 
# mean(moist$moisture_content, na.rm = T)
# 
# 
# # compare grain yield against farmers and tech volume
# grain_yield = yield
# 
# grain_yield[grain_yield == 0] = NA
# 
# grain_yield = na.omit(grain_yield)
# 
# # get the conversion rate dividing the grain yield by the volume
# grain_yield$conv_rate_tech = grain_yield$grain_yield / grain_yield$tech_volume
# 
# grain_yield$conv_rate_farmer = grain_yield$grain_yield / grain_yield$farmer_volume
# 
# 
# 
# 
# 
# 
# 
# # .................................
# # .................................
# # fit a linear model
# # Plot the data as boxplots to find any possible outlier
# # remove entries with zeros 
# yield = yield[yield$hundred_seed_weight != 0, ]
# 
# boxplot(yield$hundred_seed_weight)
# 
# boxplot(yield$thickness)
# 
# boxplot(yield$width)
# 
# boxplot(yield$length)
# 
# 
# # ......................................
# # ......................................
# # Fit a simple linear model ####
# # Now we can start fitting the model 
# # plot all the data in yield
# head(yield)
# 
# # fit a linear model using only length
# mod = lm(hundred_seed_weight ~ length,
#          data = yield)
# 
# summary(mod)
# 
# plot(yield$length, 
#      yield$hundred_seed_weight, 
#      xlab = "Length (mm)",
#      ylab = "Hundred seed weight (g)")
# abline(mod, col = "red")
# 
# # ......................................
# # ......................................
# # fit a multiple linear model ####
# # a second model with length, width and thickness combined
# mod2 = lm(hundred_seed_weight ~ length + thickness,
#           data = yield)
# 
# summary(mod2)
# 
# avPlots(mod2)
# 
# mod3 = lm(hundred_seed_weight ~ length + width + thickness,
#           data = yield)
# 
# summary(mod3)
# 
# avPlots(mod3)
# 
# AIC(mod2)
# AIC(mod3)
# 
# # ......................................
# # ......................................
# # Part 5 - Use model equation to predict new hundred seed weight ####
# # extract equation
# coefs = coefficients(mod2)
# 
# coefs
# 
# eq = paste0("HSW = ", 
#             round(coefs[1], 1),
#             " + ",
#             round(coefs[2], 1),
#             "(Length) + ",
#             round(coefs[3], 1),
#             ("(Width) + "),
#             round(coefs[4], 1), 
#             "(Thickness)")
# 
# # let's use this equation to predict the hundred seed weight 
# # of a new lot of seeds 
# var1 = 1.37 
# var2 = 0.716
# var3 = 0.467
# 
# eq
# 
# -59.4 + (33.6*var1) + (34.7*var2) + (55.6*var3)
# 
# # ......................................
# # ......................................
# # Check model goodness-of-fit ####
# # goodness-of-fit of our models
# # using Akaike Information Criteria
# AIC(mod)
# 
# AIC(mod2)
# 
# # compare actual values against predicted values 
# # first steps in ML
# actualval = yield[1:20, ]
# 
# predict(mod2, newdata = actualval)
# 
# # data frame with actual vs predicted
# pdat = data.frame(predicted = predict(mod2, newdata = actualval), 
#                   actual = actualval[, "hundred_seed_weight"])
# 
# # plot using ggplot2
# ggplot(pdat, aes(x = predicted, y = actual)) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, col = "red") +
#   labs(x = "Predicted values",
#        y = "Actual values",
#        title = "Predicted vs Actual values") +
#   theme_classic()
# 
# 
# 
# 
# 
# mod4 = lm(grain_yield ~ tech_volume, data = yield)
# 
# plot(yield$tech_volume, yield$grain_yield)
# abline(mod4, col = "red")
# summary(mod4)
# 

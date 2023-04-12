# # Scatter Plot
# plot(yield$farmer_volume,
#      yield$tech_volume,
#      main="Tech volume Vs Farmer volume Scatter plot",
#      xlab="Farmer volume", 
#      ylab="Tech volume",
#      pch=20)
# # regression line (y~x)
# abline(lm(farmer_volume ~ tech_volume, data = yield),
#        col="red") 
# 
# 
# # Paired Samples Wilcoxon Test
# rank_test = wilcox.test(yield$farmer_volume,
#                         yield$tech_volume,
#                         paired = TRUE)
# rank_test
# 
# # Printing the results
# print(rank_test)
# 
# # use Bland and Altman method to compare the measures in yield
# # by farmers and technicians
# 
# compare_dat = data.frame(farmer = yield$farmer_volume,
#                          technician = yield$tech_volume)
# 
# compare_dat[compare_dat == 0] = NA
# 
# compare_dat = na.omit(compare_dat)
# 
# compare_plot = 
#   compare(log(compare_dat$farmer),
#         log(compare_dat$technician),
#         labels = "") +
#   geom_jitter() +
#   labs(x = "Average Volumetric Yield", 
#        y = "Difference (Farmer - Technician)")
# 
# ggsave("output/comparison-farmer-vs-tech-volum-yield.png",
#        plot = compare_plot,
#        height = 13,
#        width = 13,
#        dpi = 500,
#        units = "cm")
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

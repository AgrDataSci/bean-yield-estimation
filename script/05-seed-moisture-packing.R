# Preparations ---------------------------------------------------
# Load necessary libraries
library(lme4)
library(emmeans)
library(ggplot2)


# Load data
dd <- read.csv("data/grain-yield-seed-metric-data.csv")
dd$dry_yield <- dd$grain_yield * (100-dd$moisture_content)/100


# Fit a mixed-effects model
# Variety is treated as a random effect, block is treated as a fixed effect
model1 <- lmer(dry_yield ~ (1|tech) + block_id + percent_survival, data = dd)
summary(model1)
blups_dry_yield <- unlist(ranef(model1))


model2 <- lmer(grain_yield ~ (1|tech) + block_id + percent_survival, data = dd)
summary(model2)
blups_grain_yield <- unlist(ranef(model2))


plot(blups_dry_yield, blups_grain_yield, pch = "+")
abline(lm(blups_dry_yield ~ blups_grain_yield), col = "red")
cor(blups_dry_yield, blups_grain_yield)
cor(blups_dry_yield, blups_grain_yield, method = "spearman")
# The correlation is high


# Is there a variety effect in moisture rate?
model3 <- lmer(moisture_content ~ tech + (1|block_id), data = dd)
summary(model3)


# Extract estimated marginal means
emmeans_model <- emmeans(model3, ~ tech)


# Pairwise comparisons
pairwise_results <- pairs(emmeans_model)
print(pairwise_results)
# No strong evidence for a biasing effect


# Now use the moisture values to see if they explain the discrepancy between BLUPs
moisture_content_values <- as.data.frame(emmeans_model)$emmean


dd2 <- data.frame(moisture_content_values, blups_grain_yield, blups_dry_yield)
# Create scatter plot
p = ggplot(dd2, aes(x = blups_grain_yield, y = blups_dry_yield, size = moisture_content_values)) +
  geom_point() +
  labs(
    title = "",
    x = "Grain yield with variable moisture",
    y = "Dry yield",
    size = "Moisture content (%)"
  ) +
  theme_minimal(base_size = 12) 

p

ggsave("output/moisture-vs-dry-yield.pdf",
       plot = p,
       width = 15,
       height = 13,
       units = "cm")

# No trend seems to be present


# How well does volume + moisture estimation predict dry yields?
dd$corrected_volume <- (dd$tech_volume * (100 - dd$moisture_content))/100


model4 <- lm(dry_yield ~ corrected_volume, data = dd)
summary(model4)
model5 <- lm(dry_yield ~ tech_volume, data = dd)
summary(model5)
# improvement is very slight, but should try with the BLUPs


# Is the density influenced by grain dimensions?
dd$density <- dd$dry_yield / dd$tech_volume
lm_3 <- lm(density ~ hundred_seed_weight, data=dd)
summary(lm_3)


dd$seed_volume <- dd$thickness * dd$width * dd$length
dd$seed_length <- sqrt(dd$thickness^2 + dd$width^2 + dd$length^2)

plot(dd$density, dd$seed_volume)

lm_3 <- lm(density ~ seed_length, data=dd)
lm_4 <- lm(density ~ seed_volume, data=dd)
summary(lm_3)
summary(lm_4)
# The packing density is not really affected by seed size, good!
# But what is the theoretical expectation here?
# What also could influence is the size distribution, not just averages

# .........................................
# .........................................
# Compare yield measures in cassava plots 
# collected by farmers and technicians in Nigeria
# First run Mar 2024
library("tidyverse")
library("ggplot2")
library("patchwork")
library("gosset")
library("PlackettLuce")
library("car")

# cassava data
dat = read.csv("data/tricot-data-nextgen-cassava.csv")

x = dat[,c("state", paste0("variety", letters[1:3]), paste0("rootweight_plot_", letters[1:3]))]

x = na.omit(x)

x$enumerator = ifelse(x$state == "Imo", "technician", "farmer")

table(x$enumerator)

# bean data
dat2 = read.csv("data/grain-yield-seed-metric-data.csv")

bean = data.frame(variety = rep(dat2$tech, 2), 
                  enumerator = rep(c("farmer", "technician"), each = nrow(dat2)),
                  yield = c(dat2$farmer_volume, dat2$tech_volume))

bean$logyield = log10(bean$yield)


# ...............................
# cassava
yield = data.frame(state = rep(x$state, each = 3),
                   enumerator = rep(x$enumerator, each = 3),
                   variety = unlist(x[,paste0("variety", letters[1:3])]),
                   yield = unlist(x[,paste0("rootweight_plot_", letters[1:3])]))

yield = yield[yield$yield != 0, ]

yield$logyield = log10(yield$yield)

cass_plot = 
  ggplot(yield, aes(y = variety, x = logyield, fill = enumerator)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "",
       title = "B")

bean_plot = 
  ggplot(bean, aes(y = variety, x = logyield, fill = enumerator)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "",
       y = "",
       title = "A")


bean_plot / cass_plot + plot_layout(heights = c(1, 2))


ggsave("output/boxplot-farmer-vs-technician-bean-cassava.pdf",
       height = 10,
       width = 7)



mod = lm(logyield ~ enumerator + variety, data = yield)

summary(mod)





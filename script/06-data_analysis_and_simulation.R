# .........................................
# .........................................
# First round Apr 2024
# Updated Mar 2025

library(ggplot2)
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(caret)
library(ggpubr)
library(blandr)
library(stringr)
library(stringi)
library(performance)

setwd('bean-yield-estimation')

# read data

data0 = read.csv("data/grain-yield-seed-metric-data.csv")

data00 <- data0 %>% select(-c(id,moisture_content:length))

data00 <- data00[!duplicated(data00),]

data0_long <- data00 %>% select(c('block_id','farmer_volume', 'tech_volume', 'grain_yield')) %>% 
  pivot_longer(-block_id,names_to = 'variable',values_to = 'value')


# -------------------- Exploratory data analysis----------------------------

data0_long$variable <- factor(data0_long$variable,levels = c( "grain_yield","farmer_volume", "tech_volume"  ))

g0 <- data0_long %>% ggplot(aes(x=variable,y=value)) +geom_boxplot() +xlab('') +ylab('Yield t/ha') + theme_bw()

ggsave('Results/boxplot0.png',g0,height = 3,width =5.5 )

data_sum <- data0_long %>% group_by(variable) %>% summarise(
  Min=min(value),Max=max(value),
  Mean=mean(value), 
  Median=median(value),
  Sd=sd(value)) %>% as.data.frame(data_sum)

row.names(data_sum) <- data_sum$variable

data_sum <- data_sum[,-1]

round(t(data_sum),4)

# -------Correlation analysis between estimated and measured yield------------

datDif <- data00 %>% select(c('block_id','farmer_volume', 'tech_volume', 'grain_yield')) %>% 
  mutate(dif_farm_tech = farmer_volume-tech_volume,dif_farm_yield = grain_yield-farmer_volume,
         dif_techn_yield = grain_yield-tech_volume) %>% 
  select(block_id,dif_farm_tech,dif_farm_yield,dif_techn_yield) %>% 
  pivot_longer(-block_id,names_to = 'variable',values_to = 'value')


dif <- data.frame(name=unique(datDif$variable),newname=c('tech_volume-farmer_volume','grain_yield-farmer_volume',
                                                         'grain_yield-tech_volume'))

datDif <- datDif %>% left_join(dif,by=c('variable'='name'))


datDif2 <- data00 %>% select(c('block_id','farmer_volume', 'tech_volume','grain_yield' )) %>%
  pivot_longer(-c(block_id,grain_yield),names_to = 'Measurement',values_to = 'value')


datDif2$Measurement2 <- stri_replace_all_fixed(datDif2$Measurement, 
                                               c("farmer_volume", "tech_volume"), 
                                               c("Farmer volume", "Technician volume"), 
                                               vectorize_all = FALSE)

g2 <- ggplot(datDif2,aes(y=value,x= grain_yield)) + geom_point(aes(color=Measurement2))+
  #geom_abline(intercept = 0,slope=1)+
  geom_smooth(method ='lm',aes(color=Measurement2))+ theme_bw()+
  theme(legend.position = c(0.8, 0.2))+
  xlab('Researcher-weighted yield (kg)')+ylab('Estimated yield (number of tins)')+
  annotate("text", x = 0, y = 4.4, 
           label = expression(r == 0.954 ~ "***" ), 
           color = "#F8766D", size = 5, hjust = 0) +  # Default ggplot2 red
  annotate("text", x = 0, y = 4.1, 
           label = expression(r == 0.991 ~ "***"), 
           color = "#00BFC4", size = 5, hjust = 0) + labs(color='Measurement') 



g3 <- ggplot(data00,aes(x=farmer_volume,y=tech_volume)) + geom_point() +theme_bw()+
  geom_abline(intercept = 0,slope = 1) + ylab("Tech volume (number of tins)") + 
  xlab("Farmer volume (number of tins)")+ 
  annotate("text", x = 0, y = 4.4, 
           label = expression(r == 0.961 ~ "***"  ), 
           color = "black", size = 5, hjust = 0) 

grr <- ggarrange(g2,g3,labels = c("A","B"))


# ---------------------------blandr  test--------------------------------------

data0020 <- data0 %>% select(-c(id,moisture_content:length))

data0020 <- data0020[!duplicated(data0020),]

statistics.results <- blandr.statistics( data0020$farmer_volume , data0020$tech_volume )

test <- ggplot(aes(y=dif,x=two_measures),data=data002)+geom_point()+
  ylab('Difference (Farmer volume - Technician volume)')+xlab('Mean volums')+
  geom_hline(aes(yintercept =mean(dif,na.rm=T)),color='blue',lwd=1.5)+
  geom_hline(aes(yintercept =-0.7156608),color='black')+
  geom_hline(aes(yintercept =0.4380745 ),color='black')+
  theme_bw()

ggsave('Results/bland.png',test)

# --------------------Test for Median comparisons ------------------------------

# normal tests

shapiro.test(data00$grain_yield-data00$tech_volume)
shapiro.test(data00$grain_yield-data00$farmer_volume)
shapiro.test(data00$farmer_volume-data00$tech_volume)

# t.test

wilcox.test(data00$grain_yield,data00$farmer_volume,paired = T)
wilcox.test(data00$grain_yield,data00$tech_volume,paired = T)
wilcox.test(data00$tech_volume,data00$farmer_volume,paired = T)


# --------------Models to analize intrinsic and extrinsic factors--------------


dd <- read.csv('data/aggregated_dataset.csv')

##please note grain_yield mean! units are needed by the way
summary(dd)

# Interaction with extension officers, Participation in armer field schools, Experience

#Ext-I	Interaction with extension officers
# Interaction with farmer field schools
# Experience in measuring volumes

dd$Gender <- as.factor(dd$Gender)
dd$Ext.I  <- as.factor(dd$Ext.I)
dd$Exp.MV <- as.factor(dd$Exp.MV)
dd$tech   <- as.factor( dd$tech)


dd$block_id <- as.factor(dd$block_id)
dd$farmer_volume.sc<-scale(dd$farmer_volume,scale=F)

dim(dd)

dd <- dd[dd$farmer_volume!=0,] 

dim(dd)

summary(dd)

#remove outliers

dd <- dd[complete.cases(dd),]

dim(dd)

# Full model with intrisic and and extrinsic factors

sort(table(dd$tech))

dim(dd)

#dd<- dd[!(dd$tech %in% c('Gloria','KAB 36','COD MLB 0033',"NUA 48")),]

dim(dd)

summary(dd)

# Variables excluded Ext.I and Exp.MV

# Check for random effects

dd %>% ggplot(aes(x=thickness,y=grain_yield))+geom_point()+geom_smooth(method = 'lm',se=F)+facet_wrap(~tech)

dd %>% ggplot(aes(x=moisture_content,y=grain_yield))+geom_point()+geom_smooth(method = 'lm',se=F)+facet_wrap(~tech)

dd %>% ggplot(aes(x=width,y=grain_yield))+geom_point()+geom_smooth(method = 'lm',se=F)+facet_wrap(~tech)

dd %>% ggplot(aes(x=length,y=grain_yield))+geom_point()+geom_smooth(method = 'lm',se=F)+facet_wrap(~tech)

dd %>% ggplot(aes(x=farmer_volume,y=grain_yield))+geom_point()+geom_smooth(method = 'lm',se=F)+facet_wrap(~tech)


dd$moisture_content.sc <- scale(dd$moisture_content,scale = F)

dd$thickness.sc <- scale(dd$thickness,scale = F)

dd$width.sc <- scale(dd$width,scale = F)

dd$length.sc <- scale(dd$length,scale = F)

dd$Age.sc <- scale(dd$Age,scale = F)

dd$Ac.T1.sc <- scale(dd$Ac.T1,scale = F)

dd$Ac.T2.sc <- scale(dd$Ac.T2,scale = F)


full_model <- lmer(grain_yield~ tech+farmer_volume.sc + moisture_content.sc + thickness.sc +  width.sc  + length.sc +
                     width.sc:tech +
                     moisture_content.sc:tech  + 
                     length.sc:tech +
                     thickness.sc:tech+
                     Age.sc +  Gender+Ext.I +
                     Ac.T1.sc+Ac.T2.sc+
                     Exp.MV+
                     (1|block_id),data=dd)

# select the best predictors

reduc_mod <- get_model(reduced_model_final)

anova(reduc_mod)

# good of fitness


fem0 <- lmer(grain_yield~ farmer_volume+
               (1|block_id),data=dd)

r2(fem0)

fem1 <- lmer(grain_yield~ farmer_volume+
               tech+(1|block_id),data=dd)


r2(fem1)

fem2 <- reduc_mod

r2(fem2)

# --------------Evaluating the effects of makert classs --------------

variety_info <- read.csv('data/market-classes-2022.csv')

data01 <- left_join(data00,variety_info[c('tech','market_class')],by='tech')

data01 <- data01[-c(25,89,49),]

plot(data01$grain_yield,data01$farmer_volume)
plot(data00$grain_yield,data00$farmer_volume)

mod1 <- lmer(grain_yield ~ farmer_volume+(1|block_id),data = data01, REML = FALSE)

mod01 <- lmer(grain_yield~farmer_volume+market_class+(1|block_id),data = data01, REML = FALSE)

anova(mod1,mod01)


#----------------Simulation study to evaluate the sample size---------------


set.seed(1234)

data=data00

blocks <- rnorm(1000,0,0.04918)

blk <- data.frame(block = paste0('block',rep(1:1000)),blocks)

farm_volum <- sample(unique(data01$farmer_volume),1000*3,replace = T)

paste0('block',rep(1:1000,each=3))

resid_error <- rnorm(1000*3,0,0.09354)

err <- data.frame(id = paste0('block',rep(1:1000,each=3)),resid_error,farm_volum=farm_volum)

effects <- err %>% left_join(blk,by = c('id'='block'))

effects <- effects %>% mutate(grain_yield=0.04305 + 0.88401*farm_volum +  blocks + resid_error)

summary(lmer(grain_yield~farm_volum+(1|blocks),data=effects))

set.seed(1234)

pars <- expand.grid(iter=1:100,n=c(5,10,15,20,25,30,40,50,60,70,80,90,100))

uniq_blocks <- unique(effects$id)


simulation2 <-
  lapply(1:nrow(pars),function(w){
    
    pr <- pars[w,]
    
    nObs  <-pr$n
    
    smp <- sample(1:length(uniq_blocks),100)
    
    val <- uniq_blocks[smp]
    
    training   <- effects[!(effects$id %in% val),]
    validation <- effects[effects$id %in% val,]
    
    train_block <- unique(training$id)
    
    sampled_block <- sample(train_block,nObs)
    
    
    lmer1       <- lmer(grain_yield~farm_volum+(1|id),data=training[training$id %in% sampled_block,])
    
    pred1 <- predict(lmer1,validation,re.form = ~0)
    
    res1 <- postResample(pred1,validation$grain_yield)
    
    tb <- res1
    
    data.frame(iter=pr$iter,nObs=nObs,RMSE = tb[1], Rsquared  = tb[2],    
               MAE = tb[3] )
    
  }
  )

full_sim <- do.call(rbind,simulation2)

full_sim1 <- full_sim[c('nObs','RMSE','Rsquared')]

full_sim1 <- full_sim1 %>% pivot_longer(!iter,names_to = 'Metric',values_to = 'Values')

gra2 <- ggplot(full_sim1,aes(x=factor(nObs),y=Values))+
  geom_boxplot()+facet_grid(Metric~.,scales = 'free')+xlab('Number of farmers sampled')+theme_bw()


ggsave('boxplot_metric2.png',gra2,height = 5.5,width = 4.5)





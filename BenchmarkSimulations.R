setwd("~/GitHub/Forecasting-Tournament") #igor's working directory
rm(list = ls())

hdata = read.csv('historical_data.csv')

naive0 = function(varName){MASE = vector(mode = 'numeric', length = 10000)

  TrueValues = hdata[[varName]][hdata$Month > 0] # placeholder
  NaiveErrors = abs(diff(hdata[[varName]][hdata$Month < 0]))
  Preds = rep(hdata[[varName]][hdata$Month == -1], 12)
  Error = abs(Preds - TrueValues)
  MASE = mean(Error, na.rm = TRUE)/mean(NaiveErrors, na.rm = TRUE)

print(MASE)
return(MASE)
}

MASE_posAffect0 = naive0('posaffect')
MASE_negAffect0 = naive0('negaffect')
MASE_eafric0 = naive0('eafric')
MASE_easian0 = naive0('easian')
MASE_egend0 = naive0('egend')
MASE_iafric0 = naive0('iafric')
MASE_iasian0 = naive0('iasian')
MASE_igend0 = naive0('igend')
MASE_lifesat0 = naive0('lifesat')
MASE_ideoldem0 = naive0('ideoldem') # note a missing value
MASE_ideolrep0 = naive0('ideolrep') # note a missing value
MASE_polar0 = naive0('polar')

# Model 1: Predict by randomly sampling from previous values at each time point
naive1 = function(varName){MASE = vector(mode = 'numeric', length = 10000)
for(sim in 1:10000){
  TrueValues = hdata[[varName]][hdata$Month > 0] # placeholder
  NaiveErrors = abs(diff(hdata[[varName]][hdata$Month < 0]))
  Preds = sample(hdata[[varName]][hdata$Month < 0], 12, replace = TRUE)
  Error = abs(Preds - TrueValues)
  MASE[sim] = mean(Error, na.rm = TRUE)/mean(NaiveErrors, na.rm = TRUE)
}
print(mean(MASE, na.rm =TRUE))
par(mfrow = c(2,1))
par(mar = c(3,1,1,0)+.1)
plot(hdata$Month, hdata[[varName]], type = 'l')
abline(v = 0, lty = 2)
hist(MASE)
return(MASE)
}

MASE_posAffect1 = naive1('posaffect')
MASE_negAffect1 = naive1('negaffect')
MASE_eafric1 = naive1('eafric')
MASE_easian1 = naive1('easian')
MASE_egend1 = naive1('egend')
MASE_iafric1 = naive1('iafric')
MASE_iasian1 = naive1('iasian')
MASE_igend1 = naive1('igend')
MASE_lifesat1 = naive1('lifesat')
MASE_ideoldem1 = naive1('ideoldem') # note a missing value
MASE_ideolrep1 = naive1('ideolrep') # note a missing value
MASE_polar1 = naive1('polar')

MASE_set1 = data.frame(MASE_posAffect1,MASE_negAffect1, MASE_eafric1,
                       MASE_easian1,MASE_egend1,MASE_iafric1,MASE_iasian1,MASE_igend1,MASE_lifesat1,
                       MASE_ideoldem1,MASE_ideolrep1,MASE_polar1)

# Model 2: naive auto-regressive prediction 
# find the mean and standard deviation of month-to-month change
# extrapolate a random walk from last observed data point based on mean change with standard dev.
naive2 = function(varName){MASE = vector(mode = 'numeric', length = 10000)
  TrueValues = hdata[[varName]][hdata$Month > 0] # placeholder
  NaiveErrors = abs(diff(hdata[[varName]][hdata$Month < 0]))
  meanChange = mean(diff(hdata[[varName]][hdata$Month < 0]), na.rm = TRUE)
  sdChange = sd(diff(hdata[[varName]][hdata$Month < 0]), na.rm = TRUE)
  MASE = vector(mode = 'numeric', length = 10000)
  for(sim in 1:10000){
    Preds = hdata[[varName]][hdata$Month == -1]
    for(i in 2:13){
      Preds[i] = Preds[i-1] + rnorm(1, mean = meanChange, sd = sdChange)
    }
    Error = abs(Preds[2:13] - TrueValues)
    MASE[sim] = mean(Error, na.rm = TRUE)/mean(NaiveErrors, na.rm = TRUE)
  }
  print(mean(MASE, na.rm =TRUE))
  par(mfrow = c(2,1))
  par(mar = c(3,1,1,0)+.1)
  plot(hdata$Month, hdata[[varName]], type = 'l')
  abline(v = 0, lty = 2)
  hist(MASE)
  return(MASE)
}

MASE_posAffect2 = naive2('posaffect')
MASE_negAffect2 = naive2('negaffect')
MASE_eafric2 = naive2('eafric')
MASE_easian2 = naive2('easian')
MASE_egend2 = naive2('egend')
MASE_iafric2 = naive2('iafric')
MASE_iasian2 = naive2('iasian')
MASE_igend2 = naive2('igend')
MASE_lifesat2 = naive2('lifesat')
MASE_ideoldem2 = naive2('ideoldem') # note a missing value
MASE_ideolrep2 = naive2('ideolrep') # note a missing value
MASE_polar2 = naive2('polar')

MASE_set2 = data.frame(MASE_posAffect2,MASE_negAffect2, MASE_eafric2,
MASE_easian2,MASE_egend2,MASE_iafric2,MASE_iasian2,MASE_igend2,MASE_lifesat2,
MASE_ideoldem2,MASE_ideolrep2,MASE_polar2)

# Model 3: naive random interval regression
# find the slope of change over a random interval of previous data
# extrapolate based on slope from last known data point
naive3 = function(varName){
  TrueValues = hdata[[varName]][hdata$Month > 0] # placeholder
  NaiveErrors = abs(diff(hdata[[varName]][hdata$Month < 0]))
  MASE = vector(mode = 'numeric', length = 10000)
  for(sim in 1:10000){
    randomIntervalStart = sample(3:39,1)
    randomIntervalEnd = sample(1:randomIntervalStart - 2,1)
    randomIntervalData = hdata[[varName]][hdata$Month >= -1*randomIntervalStart & 
                                           hdata$Month <= -1*randomIntervalEnd]
    m1 = lm(randomIntervalData ~ I(1:length(randomIntervalData)))
    Preds = hdata[[varName]][hdata$Month == -1] + m1$coefficients[2]*1:12
    Error = abs(Preds - TrueValues)
    MASE[sim] = mean(Error, na.rm = TRUE)/mean(NaiveErrors, na.rm = TRUE)
  }
  
  print(mean(MASE, na.rm =TRUE))
  par(mfrow = c(2,1))
  par(mar = c(3,1,1,0)+.1)
  plot(hdata$Month, hdata[[varName]], type = 'l')
  abline(v = 0, lty = 2)
  hist(MASE)
  return(MASE)
}

MASE_posAffect3 = naive3('posaffect')
MASE_negAffect3 = naive3('negaffect')
MASE_eafric3 = naive3('eafric')
MASE_easian3 = naive3('easian')
MASE_egend3 = naive3('egend')
MASE_iafric3 = naive3('iafric')
MASE_iasian3 = naive3('iasian')
MASE_igend3 = naive3('igend')
MASE_lifesat3 = naive3('lifesat')
MASE_ideoldem3 = naive3('ideoldem') # note a missing value
MASE_ideolrep3 = naive3('ideolrep') # note a missing value
MASE_polar3 = naive3('polar')

MASE_set3 = data.frame(MASE_posAffect3,MASE_negAffect3, MASE_eafric3,
                       MASE_easian3,MASE_egend3,MASE_iafric3,MASE_iasian3,MASE_igend3,MASE_lifesat3,
                       MASE_ideoldem3,MASE_ideolrep3,MASE_polar3)


# Model 3b: naive random interval regression
# find the slope of change over a random interval of previous data, ending at final month
# extrapolate based on slope from last known data point
naive3b = function(varName){
  TrueValues = hdata[[varName]][hdata$Month > 0] # placeholder
  NaiveErrors = abs(diff(hdata[[varName]][hdata$Month < 0]))
  MASE = vector(mode = 'numeric', length = 10000)
  for(sim in 1:10000){
    randomIntervalStart = sample(3:39,1)
    randomIntervalEnd = 1
    randomIntervalData = hdata[[varName]][hdata$Month >= -1*randomIntervalStart & 
                                            hdata$Month <= -1*randomIntervalEnd]
    m1 = lm(randomIntervalData ~ I(1:length(randomIntervalData)))
    Preds = hdata[[varName]][hdata$Month == -1] + m1$coefficients[2]*1:12
    Error = abs(Preds - TrueValues)
    MASE[sim] = mean(Error, na.rm = TRUE)/mean(NaiveErrors, na.rm = TRUE)
  }
  
  print(mean(MASE, na.rm =TRUE))
  par(mfrow = c(2,1))
  par(mar = c(3,1,1,0)+.1)
  plot(hdata$Month, hdata[[varName]], type = 'l')
  abline(v = 0, lty = 2)
  hist(MASE)
  return(MASE)
}

MASE_posAffect3b = naive3b('posaffect')
MASE_negAffect3b = naive3b('negaffect')
MASE_eafric3b = naive3b('eafric')
MASE_easian3b = naive3b('easian')
MASE_egend3b = naive3b('egend')
MASE_iafric3b = naive3b('iafric')
MASE_iasian3b = naive3b('iasian')
MASE_igend3b = naive3b('igend')
MASE_lifesat3b = naive3b('lifesat')
MASE_ideoldem3b = naive3b('ideoldem') # note a missing value
MASE_ideolrep3b = naive3b('ideolrep') # note a missing value
MASE_polar3b = naive3b('polar')


# Model 3c: scaled naive random interval regression
# first scale the data so that the starting point is 0, ending point is 1
# find the slope of change over a random interval of previous data
# extrapolate based on slope from last known data point
naive3c = function(varName){
  # scale prevValues
  prevValues = hdata[[varName]][hdata$Month < 0]
  if(prevValues[1] < prevValues[length(prevValues)]){
    b = -1/(prevValues[1] - prevValues[length(prevValues)])
  }else{
    b = 1/(prevValues[1] - prevValues[length(prevValues)])
  }
  a = -1*b*min(prevValues)

  allValues = a + b*hdata[[varName]]
  TrueValues = allValues[hdata$Month > 0]
  NaiveErrors = abs(diff(allValues[hdata$Month < 0]))
  MASE = vector(mode = 'numeric', length = 10000)
  for(sim in 1:10000){
    randomIntervalStart = sample(3:39,1)
    randomIntervalEnd = sample(1:randomIntervalStart - 2,1)
    randomIntervalData = allValues[hdata$Month >= -1*randomIntervalStart & 
                                            hdata$Month <= -1*randomIntervalEnd]
    m1 = lm(randomIntervalData ~ I(1:length(randomIntervalData)))
    Preds = allValues[hdata$Month == -1] + m1$coefficients[2]*1:12
    Error = abs(Preds - TrueValues)
    MASE[sim] = mean(Error, na.rm = TRUE)/mean(NaiveErrors, na.rm = TRUE)
  }
  
  print(mean(MASE, na.rm =TRUE))
  par(mfrow = c(2,1))
  par(mar = c(3,1,1,0)+.1)
  plot(hdata$Month, hdata[[varName]], type = 'l')
  abline(v = 0, lty = 2)
  hist(MASE)
  return(MASE)
}

MASE_posaffect3c = naive3c('posaffect')
MASE_negaffect3c = naive3c('negaffect')
MASE_eafric3c = naive3c('eafric')
MASE_easian3c = naive3c('easian')
MASE_egend3c = naive3c('egend')
MASE_iafric3c = naive3c('iafric')
MASE_iasian3c = naive3c('iasian')
MASE_igend3c = naive3c('igend')
MASE_lifesat3c = naive3c('lifesat')
MASE_ideoldem3c = naive3c('ideoldem') # note a missing value
MASE_ideolrep3c = naive3c('ideolrep') # note a missing value
MASE_polar3c = naive3c('polar')

vplotData = data.frame(MASE1_w1 = c(MASE_posaffect3c,MASE_negaffect3c, MASE_eafric3c,
                         MASE_easian3c,MASE_egend3c,MASE_iafric3c,MASE_iasian3c,MASE_igend3c,MASE_lifesat3c,
                         MASE_ideoldem3c,MASE_ideolrep3c,MASE_polar3c),
                       domain = rep(c('posaffect','negaffect', 'eafric',
                                    'easian','egend','iafric','iasian','igend','lifesat',
                                    'ideoldem','ideolrep','polar'), each = 10000))
vplotData$type = 'Regression Benchmark'



MASE_set3c = data.frame(MASE_posaffect3c,MASE_negaffect3c, MASE_eafric3c,
                       MASE_easian3c,MASE_egend3c,MASE_iafric3c,MASE_iasian3c,MASE_igend3c,MASE_lifesat3c,
                       MASE_ideoldem3c,MASE_ideolrep3c,MASE_polar3c)

psych::describe(MASE_set3c)
set3 = as.data.frame(t(apply(MASE_set3c, 2, function(x){Rmisc::CI(x, ci=0.95)})))
set3$domain = gsub('3c','',gsub('MASE_','',row.names(set3)))
names(set3) = c('upper.CL','emmean','lower.CL','domain')
set3$type<-"Regression Benchmark"

## EXAMINE EFFECTS OF UPDATING FOR PHASE I PREDICTIONS AMONG ACADEMICS
model.phase1.base<-  glmer(MASE1_w1~domain+(1|ResponseId), data=phase1_exp, family="poisson")

ggeffects::ggpredict(model.phase1.base,"domain") #compare to ggpredict effects (using predict())

#raw means
phase1_exp %>%
  group_by(domain) %>%
  summarise_at(vars(MASE1_w1), list(name = mean))


data.May<-as.data.frame(emmeans(model.phase1.base, ~|domain, adjust = "none")$emmeans) #nonsig


data.May$type<-"May participants"
data.wb3<-rbind(data.May[,c('domain','emmean','lower.CL','upper.CL','type')],set3)

library(ggplot2)
library(ggsci)


data.wb3 %>% 
  ggplot(aes(x = domain, y = emmean, colour = type, fill=type))+
  geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL), position=pd)+ theme_minimal(base_size = 14) +geom_hline(yintercept =1, linetype='dashed', color='red', 14)+
  theme(legend.position="bottom") +scale_color_d3()+scale_fill_d3()+  
  labs(colour = "",fill="", x="",y="MASE (M +/- 95%CI)") +scale_x_discrete(labels=labels)


#data driven and hybrid is better than lay people for igen (marginal), life satisfaction (sig)

phase1_exp %>% ggplot(aes(x = domain, y = MASE1_w1,colour = Method.code, fill=Method.code))+geom_violin()+
  theme(legend.position="bottom") +scale_color_d3()+scale_fill_d3()+ylim(0,NA)+facet_wrap(~domain, nrow=3, scale="free")+
  labs(colour = "Sample",fill="Sample", x="",y="MASE (M +/- 95%CI)")

phase1_exp.d<-phase1_exp[c("MASE1_w1","domain")]
phase1_exp.d$type<-"May participants"


combined_phase1<-rbind(phase1_exp.d,vplotData)

library(Hmisc)

combined_phase1 %>% ggplot(aes(x = domain, y = MASE1_w1,colour = type, fill=type))+geom_violin(alpha =.5)+
  theme(legend.position="bottom") +scale_color_d3()+scale_fill_d3()+ylim(0,NA)+facet_wrap(~domain, nrow=3, scale="free")+stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position=pd, size = 1.2)+
  labs(colour = "Sample",fill="Sample", x="",y="MASE (M +/- 95%CI)")

psych::describe.by(combined_phase1$MASE1_w1,group=combined_phase1[c("domain","type")])

#MUSINGS###
########################################################################################
# Model 3: ARIMA (1,0,0) fitting and simulation
library('forecast')
m3 = arima(hdata$posaffect[hdata$Month < 0],c(1,0,0))
MASE3 = vector(mode = 'numeric', length = 10000)
for(sim in 1:10000){
  Preds = arima.sim(list(ar = m3$coef["ar1"]),sd = sqrt(m3$sigma2), 12) + hdata$posaffect[hdata$Month == -1]
  Error = abs(Preds - TrueValues)
  MASE3[sim] = mean(Error)/mean(NaiveErrors)
}

# Model 3: ARIMA (1,0,0) fitting and simulation
library('forecast')
m3 = arima(hdata$posaffect[hdata$Month < 0],c(1,0,0))
MASE3 = vector(mode = 'numeric', length = 10000)
for(sim in 1:10000){
  Preds = arima.sim(list(ar = m3$coef["ar1"]),sd = sqrt(m3$sigma2), 12) + hdata$posaffect[hdata$Month == -1]
  Error = abs(Preds - TrueValues)
  MASE3[sim] = mean(Error)/mean(NaiveErrors)
}

# Model 4: Naive auto-ARIMA fitting
m4 = auto.arima(hdata$eafric[hdata$Month < 0])
test = summary(m4)
MASE3 = vector(mode = 'numeric', length = 10000)
for(sim in 1:10000){
  Preds = arima.sim(list(order = c(length(m4$model$phi), m4$model$Delta, length(m4$model$theta))),
                    sd = sqrt(m4$sigma2), 12)
  Error = abs(Preds - TrueValues)
  MASE3[sim] = mean(Error)/mean(NaiveErrors)
}

# Model 5: Naive regression approach 1
# Identify slope of change from a random interval of at least 3 points
# Extrapolate that slope from the last known data point

MASE = vector(mode = 'numeric', length = 10000)
for(sim in 1:10000){
  randomIntervalStart = sample(3:39,1)
  randomIntervalEnd = sample(1:randomIntervalStart,1)
  randomIntervalData = hdata$posaffect[hdata$Month >= -1*randomIntervalStart & 
                                         hdata$Month <= -1*randomIntervalEnd]
  m1 = lm(randomIntervalData ~ I(1:length(randomIntervalData)))
  Preds = hdata$posaffect[hdata$Month == -1] + m1$coefficients[2]*1:12
  Error = abs(Preds - TrueValues)
  MASE[sim] = mean(Error)/mean(NaiveErrors)
}
mean(MASE, na.rm = TRUE)
hist(MASE)

# Model 6: Naive regression approach 2
# Identify slope of change and intercept from a random interval of at least 3 points
# Extrapolate that slope from the intercept of the random interval rather than last data point

MASE = vector(mode = 'numeric', length = 10000)
for(sim in 1:10000){
  randomIntervalStart = sample(3:39,1)
  randomIntervalEnd = sample(1:randomIntervalStart,1)
  randomIntervalData = hdata$posaffect[hdata$Month >= -1*randomIntervalStart & 
                                         hdata$Month <= -1*randomIntervalEnd]
  m1 = lm(randomIntervalData ~ I(1:length(randomIntervalData)))
  Preds = m1$coefficients[1] + m1$coefficients[2]*1:12
  Error = abs(Preds - TrueValues)
  MASE[sim] = mean(Error)/mean(NaiveErrors)
}
mean(MASE, na.rm = TRUE)
hist(MASE)
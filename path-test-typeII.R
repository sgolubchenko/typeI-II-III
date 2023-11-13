typeIIData <- data.frame(read.csv('C:/Users/kinmi/Downloads/The University of Tsukuba/The University of Tsukuba/PhD/Research/seminars/231115//typeIIData.csv'))
library(dplyr)
typeIIData <- typeIIData %>% mutate_at(c('hincome'), ~(scale(.) %>% as.vector))
library(lavaan)
library(semPlot)
model2Test1 <- ' 
  happiness ~ female + married + child + bright_future + family_relation + work_well +
        satisfaction_area + satisfaction_home + downstream + insecure
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  child ~ married
  family_relation ~ married
  married ~ hincome
  insecure ~ hincome
  downstream ~ hincome
  hincome ~ daisotsu
  downstream ~ daisotsu
  insecure ~ daisotsu'
fit2Test1 <- sem(model2Test1, data=typeIIData)
smr2Test1 <- summary(fit2Test1, fit.measures=TRUE, standardized=TRUE)
model2Test2 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_area + satisfaction_home + downstream + insecure
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ hincome
  hincome ~ daisotsu
  downstream ~ daisotsu'
fit2Test2 <- sem(model2Test2, data=typeIIData)
smr2Test2 <- summary(fit2Test2, fit.measures=TRUE, standardized=TRUE)
path2Test2 <- semPaths(fit2Test2, layout = "tree", shapeLat="ellipse", whatLabels  = "stand", 
                      nDigits=3, shapeMan="square", sizeMan =8, 
                      sizeLat =8, sizeLat2 =8, style = "lisrel",
                      residScale=12, curve=2.5, optimizeLatRes=T,edge.color="black",
                      rotation = 2, edge.label.cex=1)
model2Test3 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu'
fit2Test3 <- sem(model2Test3, data=typeIIData)
smr2Test3 <- summary(fit2Test3, fit.measures=TRUE, standardized=TRUE)
model2Test4 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure + child
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu
  child ~ married'
fit2Test4 <- sem(model2Test4, data=typeIIData)
smr2Test4 <- summary(fit2Test4, fit.measures=TRUE, standardized=TRUE)
model2Test5 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure + living_alone
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu'
fit2Test5 <- sem(model2Test5, data=typeIIData)
smr2Test5 <- summary(fit2Test5, fit.measures=TRUE, standardized=TRUE)
model2Test6 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure + living_alone
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu'
fit2Test6 <- sem(model2Test6, data=typeIIData)
smr2Test6 <- summary(fit2Test6, fit.measures=TRUE, standardized=TRUE)
model2Test7 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure + living_alone
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu
  married ~ hincome
  insecure ~ hincome
  downstream ~ hincome
  hincome ~ daisotsu'
fit2Test7 <- sem(model2Test7, data=typeIIData)
smr2Test7 <- summary(fit2Test7, fit.measures=TRUE, standardized=TRUE)
model2Test8 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure + living_alone
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu
  downstream ~ hincome
  married ~ hincome
  hincome ~ daisotsu'
fit2Test8 <- sem(model2Test8, data=typeIIData)
smr2Test8 <- summary(fit2Test8, fit.measures=TRUE, standardized=TRUE)
model2Test9 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure + living_alone
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  family_relation ~ living_alone
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu
  downstream ~ hincome
  married ~ hincome
  hincome ~ daisotsu'
fit2Test9 <- sem(model2Test9, data=typeIIData)
smr2Test9 <- summary(fit2Test9, fit.measures=TRUE, standardized=TRUE)
model2Test10 <- ' 
  happiness ~ female + married + bright_future + family_relation + conf_health +
        satisfaction_home + downstream + insecure + living_alone
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  family_relation ~ living_alone
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu
  downstream ~ hincome
  married ~ hincome
  hincome ~ daisotsu'
fit2Test10 <- sem(model2Test10, data=typeIIData)
smr2Test10 <- summary(fit2Test10, fit.measures=TRUE, standardized=TRUE)
typeIIDataPca <- inner_join(typeIIData,pcaScores,by='sampleid')
#best fit - fit2Test9
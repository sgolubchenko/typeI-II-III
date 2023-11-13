typeIData <- data.frame(read.csv('C:/Users/kinmi/Downloads/The University of Tsukuba/The University of Tsukuba/PhD/Research/seminars/231115//typeIData.csv'))
library(dplyr)
typeIData <- typeIData %>% mutate_at(c('hincome'), ~(scale(.) %>% as.vector))
typeIData <- rename(typeIData, "sampleid" = "SAMPLEID")
library(lavaan)
library(semPlot)
modelTest1 <- ' 
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
fitTest1 <- sem(modelTest1, data=typeIData)
smrTest1 <- summary(fitTest1, fit.measures=TRUE, standardized=TRUE)
pathTest1 <- semPaths(fitTest1, layout = "tree", shapeLat="ellipse", whatLabels  = "stand", 
                          nDigits=3, shapeMan="square", sizeMan =8, 
                          sizeLat =8, sizeLat2 =8, style = "lisrel",
                          residScale=12, curve=2.5, optimizeLatRes=T,edge.color="black",
                          rotation = 2, edge.label.cex=1)
modelTest2 <- ' 
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
fitTest2 <- sem(modelTest2, data=typeIData)
smrTest2 <- summary(fitTest2, fit.measures=TRUE, standardized=TRUE)
pathTest2 <- semPaths(fitTest2, layout = "tree", shapeLat="ellipse", whatLabels  = "stand", 
                      nDigits=3, shapeMan="square", sizeMan =8, 
                      sizeLat =8, sizeLat2 =8, style = "lisrel",
                      residScale=12, curve=2.5, optimizeLatRes=T,edge.color="black",
                      rotation = 2, edge.label.cex=1)
pcaScores <- data.frame(read.csv('C:/Users/kinmi/Downloads/The University of Tsukuba/The University of Tsukuba/PhD/Research/seminars/231115//pcascores20-23.csv'))
typeIDataPca <- inner_join(typeIData,pcaScores,by='sampleid')
modelTest3 <- ' 
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
fitTest3 <- sem(modelTest3, data=typeIDataPca)
smrTest3 <- summary(fitTest3, fit.measures=TRUE, standardized=TRUE)
modelTest4 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  hincome ~ daisotsu
  downstream ~ daisotsu'
fitTest4 <- sem(modelTest4, data=typeIData)
smrTest4 <- summary(fitTest4, fit.measures=TRUE, standardized=TRUE)
modelTest5 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu'
fitTest5 <- sem(modelTest5, data=typeIData)
smrTest5 <- summary(fitTest5, fit.measures=TRUE, standardized=TRUE)
modelTest6 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure
  satisfaction_area ~ satisfaction_home + pride
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health + conf_elderly
  downstream ~ daisotsu'
fitTest6 <- sem(modelTest6, data=typeIData)
smrTest6 <- summary(fitTest6, fit.measures=TRUE, standardized=TRUE)
modelTest7 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure
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
fitTest7 <- sem(modelTest7, data=typeIData)
smrTest7 <- summary(fitTest7, fit.measures=TRUE, standardized=TRUE)
modelTest8 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure
  satisfaction_area ~ satisfaction_home
  satisfaction_home ~ house_own
  family_relation ~ married
  family_relation ~ living_alone
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu'
fitTest8 <- sem(modelTest8, data=typeIData)
smrTest8 <- summary(fitTest8, fit.measures=TRUE, standardized=TRUE)
modelTest9 <- ' 
  happiness ~ female + married + bright_future + family_relation + work_well + conf_health +
        satisfaction_home + downstream + insecure
  satisfaction_area ~ satisfaction_home + pride
  satisfaction_home ~ house_own
  family_relation ~ married
  insecure ~ conf_elderly
  bright_future ~ conf_health
  bright_future ~ conf_elderly
  downstream ~ daisotsu'
fitTest9 <- sem(modelTest9, data=typeIDataPca)
smrTest9 <- summary(fitTest9, fit.measures=TRUE, standardized=TRUE)
#best fit - fitTest5
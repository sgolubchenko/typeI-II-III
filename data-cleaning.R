pinfoData <- data.frame(read.csv("C:/Users/kinmi/Downloads/The University of Tsukuba/The University of Tsukuba/PhD/Research/livability_ranking/raw_data//pinfo_5years.csv"))
satisfactionData <- data.frame(read.csv("C:/Users/kinmi/Downloads/The University of Tsukuba/The University of Tsukuba/PhD/Research/livability_ranking/raw_data//q02-04_5years.csv"))
sentimentsData <- data.frame(read.csv("C:/Users/kinmi/Downloads/The University of Tsukuba/The University of Tsukuba/PhD/Research/livability_ranking/raw_data//q32_5years.csv"))
muntypes <- data.frame(read.csv("C:/Users/kinmi/Downloads/The University of Tsukuba/The University of Tsukuba/PhD/Research/livability_ranking/raw_data//muns_8types.csv"))
library(dplyr)
pinfo <- pinfoData %>%
  select(SAMPLEID,researchyear,jusho5,age10:married_dm,child_dm,gchild_dm,
         kyojunen:daisotsu_dm,q14_hincome,q15,q18_1,q20_1,q213,q23)
satisfaction <- satisfactionData %>%
  select(SAMPLEID,q2_60,q4_30)
sentiments <- sentimentsData %>%
  select(SAMPLEID,q32_003,q32_005:q32_008,q32_023,q32_031,q32_033,q32_034,q32_036,q32_087:q32_091)
pinfo <- pinfo[pinfo$researchyear != '2019', ]
pinfo$jusho5[pinfo$jusho5 >= 1101 & pinfo$jusho5 <= 1110] <- 1100
pinfo$jusho5[pinfo$jusho5 >= 4101 & pinfo$jusho5 <= 4105] <- 4100
pinfo$jusho5[pinfo$jusho5 >= 11101 & pinfo$jusho5 <= 11110] <- 11100
pinfo$jusho5[pinfo$jusho5 >= 12101 & pinfo$jusho5 <= 12106] <- 12100
pinfo$jusho5[pinfo$jusho5 >= 14101 & pinfo$jusho5 <= 14118] <- 14100
pinfo$jusho5[pinfo$jusho5 >= 14131 & pinfo$jusho5 <= 14153] <- 14130
pinfo$jusho5[pinfo$jusho5 >= 15101 & pinfo$jusho5 <= 15108] <- 15100
pinfo$jusho5[pinfo$jusho5 >= 22101 & pinfo$jusho5 <= 22103] <- 22100
pinfo$jusho5[pinfo$jusho5 >= 22131 & pinfo$jusho5 <= 22137] <- 22130
pinfo$jusho5[pinfo$jusho5 >= 23101 & pinfo$jusho5 <= 23116] <- 23100
pinfo$jusho5[pinfo$jusho5 >= 26101 & pinfo$jusho5 <= 26111] <- 26100
pinfo$jusho5[pinfo$jusho5 >= 27102 & pinfo$jusho5 <= 27128] <- 27100
pinfo$jusho5[pinfo$jusho5 >= 27141 & pinfo$jusho5 <= 27147] <- 27140
pinfo$jusho5[pinfo$jusho5 >= 28101 & pinfo$jusho5 <= 28111] <- 28100
pinfo$jusho5[pinfo$jusho5 >= 33101 & pinfo$jusho5 <= 33104] <- 33100
pinfo$jusho5[pinfo$jusho5 >= 34101 & pinfo$jusho5 <= 34108] <- 34100
pinfo$jusho5[pinfo$jusho5 >= 40101 & pinfo$jusho5 <= 40109] <- 40100
pinfo$jusho5[pinfo$jusho5 >= 40131 & pinfo$jusho5 <= 40137] <- 40130
pinfo$jusho5[pinfo$jusho5 >= 43101 & pinfo$jusho5 <= 43105] <- 43100
pinfo <- inner_join(pinfo,muntypes,by='jusho5')
pathData <- inner_join(pinfo,satisfaction,by='SAMPLEID')
pathData <- inner_join(pathData,sentiments,by='SAMPLEID')
samplesize <- pathData %>% count(jusho5)
samplesize <- samplesize[samplesize$n >= 30, ]
pathData <- inner_join(pathData,samplesize,by='jusho5')
typeIData <- pathData %>% filter(type == 'I')
typeIIData <- pathData %>% filter(type == 'II')
typeIIIData <- pathData %>% filter(type == 'III')
library(naniar)
typeIData <- typeIData %>% replace_with_na(replace = list(type = 'NA'))
typeIIData <- typeIIData %>% replace_with_na(replace = list(type = 'NA'))
typeIIIData <- typeIIIData %>% replace_with_na(replace = list(type = 'NA'))
which(is.na(typeIData$q14_hincome), arr.ind=TRUE)
typeIData <- na.omit(typeIData)
typeIIData <- na.omit(typeIIData)
typeIIIData <- na.omit(typeIIIData)
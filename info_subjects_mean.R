##############################
## Prominence Marking in PD ##
##           PLOS ONE       ##
## 02.01.2018 - Tabea Thies ##
##############################

## (1) Measurements of means and standard deviation (sd) for participant characteristics
## (2) T-Test Comparisons of several means
## (3) Correlation testing

## insert working directory
setwd("C:/Users/Tabea Thies/Desktop/Phonetik Institut/Paper_Article/KUS_Studie_PD_Daten/Versionen Paper_2nd try_Neuropsychologia/Data")

## read data file (csv)
data=read.csv("info_subjects.csv",header= T, strip.white=TRUE, sep = ";")


#########################################################################################

## (1)
## means and sd for table 1: general participant characteristics


## age
aggregate(Age ~ Group,data,mean)
aggregate(Age~ Group,data,sd)


## duration of disease (dur)
aggregate(dur ~ Group,data,mean)
aggregate(dur ~ Group,data,sd)

## levodopa daily dose (LEDD)
aggregate(LEDD ~ Group,data,mean)
aggregate(LEDD ~ Group,data,sd)


## Panda
aggregate(Panda ~ Group,data,mean)
aggregate(Panda~ Group,data,sd)


## means and sd for table 3: participant characteristics abaout motor, speech and cognitive functions


## UPDRS III (UPDRS)
aggregate(UPDRS ~ Group,data,mean)
aggregate(UPDRS ~ Group,data,sd)


## Hoehn & Yahr (H.Y)
aggregate(data$H.Y ~ Group,data,mean)
aggregate(H.Y~ Group,data,sd)


## Voice Handicap Index (VHI)
aggregate(VHI ~ Group,data,mean)
aggregate(VHI ~ Group,data,sd)


## Intelligibility (Intell)
aggregate(Intell ~ Group,data,mean)
aggregate(Intell ~ Group,data,sd)


## Trail Making Test (TMT)
aggregate(TMT ~ Group,data,mean)
aggregate(TMT~ Group,data,sd)

## TMT ohne Pat 13 (TMT = 280)
data_reduced =  data[-c(13), ] 
aggregate(TMT ~ Group,data_reduced,mean) ## new mean PAT: 73.4
aggregate(TMT~ Group,data_reduced,sd) ## new sd PAT: 44


## Biref Test of Attention (BTA)
aggregate(BTA ~ Group,data,mean)
aggregate(BTA ~ Group,data,sd)


## digit span (DS)
aggregate(DS ~ Group,data,mean)
aggregate(DS ~ Group,data,sd)


## (2)
## T-Tests for comparing group values and checking differences across them

## Age
t.test(Age~ Group,data) ## not significant: (t (-0.3) = 35, p = 0.7626)

## Cognition scores
t.test(TMT ~ Group,data) ## significant: (t (-2.7) = 23, p = 0.01235)

t.test(BTA ~ Group,data) ## not significant: (t (1.3) = 31, p = 0.1962)

t.test(DS ~ Group,data) ## not significant: (t (1.5) = 36, p = 0.1498)


## (3)
## Correlation testing

## subset only for patient group
Pat = subset (data, Group =="PAT")

shapiro.test(Pat$Intell) ## --> p = 0.2254 normal distributed
shapiro.test(Pat$UPDRS) ## --> p = 0.9723 normal distributed

## Intelligibility & UPDRS
cor.test(Pat$Intell, Pat$UPDRS, method = "pearson", conf.level = 0.95) ## sign. moderate, negative correlation (p = 0.01206), coef: -0.56








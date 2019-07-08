##############################
## Prominence Marking in PD ##
##     Neuropsychologia     ##
## 08.07.2019 - Tabea Thies ##
##############################

## (1) Measurements of means and standard deviation (sd) for participant characteristics
## (2) T-Test Comparisons of several means
## (3) Correlation testing

## insert working directory
setwd("")

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
aggregate(Panda ~ Group,data,sd)

## GDS
aggregate(GDS ~ Group,data,mean)
aggregate(GDS ~ Group,data,sd)


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



## (2)
## T-Tests for comparing group values and checking differences across them

## Age
t.test(Age ~ Group,data) ## not significant: (t (34.86) = -0.3, p = 0.7626)

## Cognition scores
## TMT Part A
t.test(TMTA ~ Group,data) ## significant: (t(34.45) = -4.0, p = 0.0003221)

## TMT B-A
t.test(TMTBA ~ Group,data) ## significant: (t(22.81) = -2.72, p = 0.01235)

## Brief Test of Attention
t.test(BTA ~ Group,data) ## not significant: (t (30.67) = 1.32, p = 0.1962)

## Digit Span
t.test(DS ~ Group,data) ## not significant: (t (35.99) = 1.47, p = 0.1498)


## (3)
## Correlation testing (reported in Table 9)

## subset only for patient group
Pat = subset (data, Group =="PAT")

## Intelligibility ~ UPDRS III
shapiro.test(Pat$Intell) ## --> p = 0.2254 normal distributed
shapiro.test(Pat$UPDRS) ## --> p = 0.9723 normal distributed

## --> both normal distributed method 'pearson'

cor.test(Pat$Intell, Pat$UPDRS, method = "pearson", conf.level = 0.95) ## sign. moderate, negative correlation (p = 0.01206), coef: -0.56


# Intelligibility ~ TMT B-A
shapiro.test(Pat$Intell) ## --> p = 0.2254 normal distributed
shapiro.test(Pat$TMTBA) ## --> p = 0.002603 not normal distributed

## --> method 'kendall'

cor.test(Pat$Intell, Pat$TMTBA, method = "kendall", conf.level = 0.95) ## no correlation (p = 0.09843), coef: -0.3

# UPDRS ~ TMT B-A
shapiro.test(Pat$UPDRS) ## --> p = 0.9723 normal distributed
shapiro.test(Pat$TMTBA) ## --> p = 0.002603 not normal distributed

## --> method 'kendall'

cor.test(Pat$UPDRS, Pat$TMTBA, method = "kendall", conf.level = 0.95) ## no correlation (p = 0.2713), coef: 0.19




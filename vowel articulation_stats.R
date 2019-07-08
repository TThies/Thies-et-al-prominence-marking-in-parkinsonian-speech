##############################
## Prominence Marking in PD ##
##           PLOS ONE       ##
## 04.01.2018 - Tabea Thies ##
##############################

## Analysis of vowel formants F1 and F2 and vowel articulation index

## (1) Mean vowel formants and VAI
## (2) Statistical anaylses
## (3) Testing correlation

## insert working directory
setwd("")

## read data file (csv)
data=read.csv("vowel_articulation.csv",header= T, strip.white=TRUE)


##############################################################################################


## (1)
## means and sd for table 4: Measurements for the formants F1 and F2 and the related vowel articulation index (VAI).

## Vowel formants /a/
aggregate(F1_a ~ acc_condition + group, data, mean)
aggregate(F1_a ~ acc_condition + group, data, sd)

aggregate(F2_a ~ acc_condition + group, data, mean)
aggregate(F2_a ~ acc_condition + group, data, sd)

## Vowel formants /i/
aggregate(F1_i ~ acc_condition + group, data, mean)
aggregate(F1_i ~ acc_condition + group, data, sd)

aggregate(F2_i ~ acc_condition + group, data, mean)
aggregate(F2_i ~ acc_condition + group, data, sd)

## Vowel formants /o/
aggregate(F1_o ~ acc_condition + group, data, mean)
aggregate(F1_o ~ acc_condition + group, data, sd)

aggregate(F2_o ~ acc_condition + group, data, mean)
aggregate(F2_o ~ acc_condition + group, data, sd)

## Vowel articulation index (VAI)
aggregate(VAI_meas ~ acc_condition + group, data, mean)
aggregate(VAI_meas ~ acc_condition + group, data, sd)


##############################################################

## (2)
## statistical analyses

## load package
library(lme4)

#############################################
## add new columns for influencing factors ##
#############################################

## neuen Faktor erstellen: TMT A

data[,"TMTA"] <- data[,"speaker"]
## Level umbenennen
levels(data[,"TMTA"]) <- list("20" = "P01", "80" = "P02",
                              "20" = "P03", "90" = "P04", "60" = "P05", 
                              "70" = "P06", "10" = "P07",
                              "10" = "P08", "10" = "P09", "10" = "P10",
                              "70" = "P11", "10" = "P12", "20" = "P13",
                              "20" = "P14", "30" = "P15", "10" = "P16",
                              "80" = "P17", "10" = "P18",  "20" = "P19",
                              "40" = "C01", "90" = "C02",
                              "90" = "C03", "90" = "C04", "90" = "C05", 
                              "50" = "C06", "90" = "C07", "80" = "C08",
                              "50" = "C09", "40" = "C10", "90" = "C11",
                              "70" = "C12", "80" = "C13", "50" = "C14",
                              "90" = "C15", "10" = "C16", "50" = "C17",
                              "80" = "C18", "80" = "C19")

## conert factor to numeric
data$TMTA = as.numeric(as.character(data$TMTA))

## neuen Faktor erstellen: TMT diff

data[,"TMTdiff"] <- data[,"speaker"]
## Level umbenennen
levels(data[,"TMTdiff"]) <- list("76" = "P01", "51" = "P02",
                                 "61" = "P03", "28" = "P04", "99" = "P05", 
                                 "54" = "P06", "110" = "P07",
                                 "126" = "P08", "28" = "P09", "45" = "P10",
                                 "90" = "P11", "52" = "P12", "280" = "P13",
                                 "134" = "P14", "29" = "P15", "58" = "P16",
                                 "24" = "P17", "182" = "P18",  "NA" = "P19",
                                 "41" = "C01", "18" = "C02",
                                 "35" = "C03", "35" = "C04", "27" = "C05", 
                                 "64" = "C06", "52" = "C07", "14" = "C08",
                                 "24" = "C09", "53" = "C10", "21" = "C11",
                                 "9" = "C12", "27" = "C13", "16" = "C14",
                                 "43" = "C15", "106" = "C16", "49" = "C17",
                                 "106" = "C18", "14" = "C19")

## conert factor to numeric
data$TMTdiff = as.numeric(as.character(data$TMTdiff))


## new column "TMTdiff2" = exclude TMT value of PD 13 (TMT = 280)

### TMT ändern - exclude PD 13 mit TMT (B-A) = 280

## neuen Faktor erstellen: TMT Differenz B-A

data[,"TMTdiff2"] <- data[,"speaker"]
## Level umbenennen
levels(data[,"TMTdiff2"]) <- list("76" = "P01", "51" = "P02",
                                  "61" = "P03", "28" = "P04", "99" = "P05", 
                                  "54" = "P06", "110" = "P07",
                                  "126" = "P08", "28" = "P09", "45" = "P10",
                                  "90" = "P11", "52" = "P12", "NA" = "P13",
                                  "134" = "P14", "29" = "P15", "58" = "P16",
                                  "24" = "P17", "182" = "P18",  "NA" = "P19",
                                  "41" = "C01", "18" = "C02",
                                  "35" = "C03", "35" = "C04", "27" = "C05", 
                                  "64" = "C06", "52" = "C07", "14" = "C08",
                                  "24" = "C09", "53" = "C10", "21" = "C11",
                                  "9" = "C12", "27" = "C13", "16" = "C14",
                                  "43" = "C15", "106" = "C16", "49" = "C17",
                                  "106" = "C18", "14" = "C19")

## conert factor to numeric
data$TMTdiff2 = as.numeric(as.character(data$TMTdiff2))

## neuen Faktor erstellen: BTA

data[,"BTA"] <- data[,"speaker"]
## Level umbenennen
levels(data[,"BTA"]) <- list("18" = "P01", "20" = "P02",
                             "20" = "P03", "20" = "P04", "18" = "P05", 
                             "20" = "P06", "18" = "P07",
                             "12" = "P08", "14" = "P09", "19" = "P10",
                             "16" = "P11", "NA" = "P12", "12" = "P13",
                             "18" = "P14", "19" = "P15", "20" = "P16",
                             "20" = "P17", "11" = "P18",  "14" = "P19",
                             "19" = "C01", "20" = "C02",
                             "19" = "C03", "19" = "C04", "19" = "C05", 
                             "20" = "C06", "19" = "C07", "20" = "C08",
                             "20" = "C09", "19" = "C10", "19" = "C11",
                             "18" = "C12", "19" = "C13", "19" = "C14",
                             "11" = "C15", "14" = "C16", "17" = "C17",
                             "18" = "C18", "20" = "C19")

## conert factor to numeric
data$BTA = as.numeric(as.character(data$BTA))

## neuen Faktor erstellen: digit span

data[,"digitspan"] <- data[,"speaker"]
## Level umbenennen
levels(data[,"digitspan"]) <- list("14" = "P01", "14" = "P02",
                                   "14" = "P03", "17" = "P04", "17" = "P05", 
                                   "16" = "P06", "20" = "P07",
                                   "16" = "P08", "12" = "P09", "15" = "P10",
                                   "15" = "P11", "12" = "P12", "8" = "P13",
                                   "17" = "P14", "12" = "P15", "24" = "P16",
                                   "18" = "P17", "10" = "P18",  "16" = "P19",
                                   "17" = "C01", "21" = "C02",
                                   "16" = "C03", "20" = "C04", "18" = "C05", 
                                   "16" = "C06", "15" = "C07", "17" = "C08",
                                   "20" = "C09", "12" = "C10", "19" = "C11",
                                   "21" = "C12", "17" = "C13", "15" = "C14",
                                   "15" = "C15", "9" = "C16", "16" = "C17",
                                   "14" = "C18", "26" = "C19")

## conert factor to numeric
data$digitspan = as.numeric(as.character(data$digitspan))


## z-standadise parameters of potential further influencing factors (UPDRS, TMT, disease duration, intelligibility)

data$TMTdiff_z <- scale(data$TMTdiff)
data$TMTdiff2_z <- scale(data$TMTdiff2)
data$TMTA_z <- scale(data$TMTA)
data$BTA_z <- scale(data$BTA)
data$digitspan_z <- scale(data$digitspan)
data$UPDRS_z <- scale(data$UPDRS)
data$Dur_z <- scale(data$Dur)
data$Intell_z <- scale(data$Intell)

################################
## (i) group - based analysis ##
################################

## Testing fixed factor: group
VAI.group.full <- lmer(VAI_meas ~ group + acc_condition + (1| speaker), data = data, REML=FALSE)
VAI.group.null <- lmer(VAI_meas ~ 1 + acc_condition + (1 |speaker), data = data, REML=FALSE)

anova(VAI.group.full, VAI.group.null)  ## effect of group X2(1) = 11.796 p= 0.0005935 ***
summary(VAI.group.full) ## smaller VAI in patients


## Testing fixed factor: accent condition
VAI.acc_condition.full <- lmer(VAI_meas ~ group + acc_condition + (1| speaker), data = data, REML=FALSE)
VAI.acc_condition.null <- lmer(VAI_meas ~ group + 1 + (1|speaker), data = data, REML=FALSE)

anova(VAI.acc_condition.full, VAI.acc_condition.null)  ## effect of accent conditions X2(1) = 13.324 p = 0.0002749 ***
summary(VAI.acc_condition.full) ## VAI greater in accented condition



############################################
## (ii) individual - differences analysis ##
############################################

## subset per group

PAT = subset(data, group == "Pat")
CON = subset(data, group == "Con")



##############
## patients ##
##############

## influence TMT A
patient.TMTA <- lmer(VAI_meas ~  TMTA_z + acc_condition + (1|speaker), data = na.exclude(PAT),  REML=FALSE)
patient.null <- lmer(VAI_meas ~  1 + acc_condition + (1|speaker), data = na.exclude(PAT), REML=FALSE)

anova(patient.TMTA, patient.null) ## X2(1) = 4.0144  ; p=0.04511 * --> no influence of TMT A


# influence TMT diff (B-A)
patient.TMTdiff <- lmer(VAI_meas ~  TMTdiff_z + acc_condition + (1|speaker), data = na.exclude(PAT),  REML=FALSE)

anova(patient.TMTdiff, patient.null) ## X2(1) = 0.8863   ; p=0.3465 --> no influence of TMT difference

# influence TMT diff (B-A)
patient.TMTdiff2 <- lmer(VAI_meas ~  TMTdiff2_z + acc_condition + (1|speaker), data = na.exclude(PAT),  REML=FALSE)

anova(patient.TMTdiff2, patient.null)  ## X2(1) = 0.8863   ; p=0.3465 --> no influence of TMT difference


# influence BTA
patient.BTA <- lmer(VAI_meas ~  BTA_z + acc_condition + (1|speaker), data = na.exclude(PAT),  REML=FALSE)

anova(patient.BTA, patient.null) ## X2(1) = 2.7232 ; p=0.0989 --> no influence of BTA


# influencce digit span
patient.DS <- lmer(VAI_meas ~  digitspan_z + acc_condition + (1|speaker), data = na.exclude(PAT),  REML=FALSE)

anova(patient.DS, patient.null) ## X2(1) = 0.0.1479 ; p=0.7006 --> no influence of digit span


## influence duration of disease
patient.Dur <- lmer(VAI_meas ~  Dur_z + acc_condition + (1 |speaker), data = na.exclude(PAT),  REML=FALSE)

anova(patient.Dur, patient.null) ## X2(1)=0.3002; p=0.0.5837 --> no influence of duration 


## influence of UPDRS
patient.UPDRS <- lmer(VAI_meas ~  UPDRS_z + acc_condition + (1 |speaker), data = na.exclude(PAT),  REML=FALSE)

anova(patient.UPDRS, patient.null) ## X2(1)=7.4147   ; p=0.006469 ** --> influence of motor impairment


## influence of intelligibility rating
patient.intell <- lmer(VAI_meas ~  Intell_z + acc_condition + (1 |speaker), data = na.exclude(PAT),  REML=FALSE)

anova(patient.intell, patient.null) ## X2(1)=0.6924  ; p=0.4054 --> no influence of intelligibility



##############
## controls ##
##############

## influence TMT A
control.TMTA <- lmer(VAI_meas ~  TMTA_z + acc_condition + (1|speaker), data = CON,  REML=FALSE)
control.null <- lmer(VAI_meas ~  1 + acc_condition + (1|speaker), data = CON, REML=FALSE)

anova(control.TMTA, control.null) ## X2(1) = 0.0032  ; p=0.9552 --> no influence of TMT A


# influence TMT diff (B-A)
control.TMTdiff <- lmer(VAI_meas ~  TMTdiff_z + acc_condition + (1|speaker), data = CON,  REML=FALSE)

anova(control.TMTdiff, control.null) ## X2(1) = 1.0226   ; p=0.3119 --> no influence of TMT difference


# influence BTA
control.BTA <- lmer(VAI_meas ~  BTA_z + acc_condition + (1|speaker), data = CON,  REML=FALSE)

anova(control.BTA, control.null) ## X2(1) = 0.5323  ; p=0.4656 --> no influence of BTA


# influencce digit span
control.DS <- lmer(VAI_meas ~  digitspan_z + acc_condition + (1|speaker), data = CON,  REML=FALSE)

anova(control.DS, control.null) ## X2(1) = 0.2815 ; p=0.5957 --> no influence of digit span




## (3) correlation Intell & vowel articulation index

shapiro.test(PAT$Intell) ## p-value 0.01479 --> normal distributed
shapiro.test(PAT$VAI_meas) ## p-value =  0.0005618 --> normal distributed

## --> to use parametric  correlations

cor.test(PAT$Intell, PAT$VAI_meas,  method = "pearson", conf.level = 0.95) ## r = 0.24, p = 0.1389



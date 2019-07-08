##############################
## Prominence Marking in PD ##
##           PLOS ONE       ##
## 04.01.2018 - Tabea Thies ##
##############################

## Analysis of tonal height of rising pitch accents

## (1) Mean tonal height measures
## (2) Statistical anaylses
## (3) Testing correlations

## insert working directory
setwd("")

## read data file (csv)
data=read.csv("tonalheight.csv",header= T, strip.white=TRUE)


##############################################################################################


## (1)
## means and sd for table 5: Values of tonal height

aggregate(Semitones ~ group, data, mean)
aggregate(Semitones ~ group, data, sd)


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


## neuen Faktor erstellen: UPDRS

data[,"UPDRS"] <- data[,"speaker"]
## Level umbenennen
levels(data[,"UPDRS"]) <- list("18" = "P01", "17" = "P02",
                                "15" = "P03", "7" = "P04", "8" = "P05", 
                                "28" = "P06", "22" = "P07",
                                "32" = "P08", "35" = "P09", "3" = "P10",
                                "29" = "P11", "25" = "P12", "40" = "P13",
                                "16" = "P14", "20" = "P15", "48" = "P16",
                                "28" = "P17", "31" = "P18",  "30" = "P19",
                                "NA" = "C01", "NA" = "C02",
                                "NA" = "C03", "NA"= "C04", "NA" = "C05", 
                                "NA" = "C06", "NA" = "C07", "NA" = "C08",
                                "NA" = "C09", "NA" = "C10", "NA" = "C11",
                                "NA" = "C12", "NA" = "C13", "NA" = "C14",
                                "NA" = "C15", "NA" = "C16", "NA" = "C17",
                                "NA" = "C18", "NA" = "C19")

## conert factor to numeric
data$UPDRS = as.numeric(as.character(data$UPDRS))

## neuen Faktor erstellen: disease duration

data[,"Dur"] <- data[,"speaker"]
## Level umbenennen
levels(data[,"Dur"]) <- list("1" = "P01", "3" = "P02",
                             "2" = "P03", "9" = "P04", "1" = "P05", 
                             "5" = "P06", "10" = "P07",
                             "9" = "P08", "5" = "P09", "1" = "P10",
                             "10" = "P11", "7" = "P12", "8" = "P13",
                             "5" = "P14", "5" = "P15", "3" = "P16",
                             "14" = "P17", "5" = "P18",  "9" = "P19",
                             "NA" = "C01", "NA" = "C02",
                             "NA" = "C03", "NA"= "C04", "NA" = "C05", 
                             "NA" = "C06", "NA" = "C07", "NA" = "C08",
                             "NA" = "C09", "NA" = "C10", "NA" = "C11",
                             "NA" = "C12", "NA" = "C13", "NA" = "C14",
                             "NA" = "C15", "NA" = "C16", "NA" = "C17",
                             "NA" = "C18", "NA" = "C19")

## conert factor to numeric
data$Dur = as.numeric(as.character(data$Dur))

## neuen Faktor erstellen: intelligibility

data[,"Intell"] <- data[,"speaker"]
## Level umbenennen
levels(data[,"Intell"]) <- list("7.25" = "P01", "8.50" = "P02",
                                "9" = "P03", "8.5" = "P04", "8.75" = "P05", 
                                "8.25" = "P06", "8" = "P07",
                                "7.75" = "P08", "8" = "P09", "9" = "P10",
                                "8" = "P11", "8.5" = "P12", "6.5" = "P13",
                                "8.75" = "P14", "7.20" = "P15", "8" = "P16",
                                "8.75" = "P17", "7.75" = "P18",  "7.25" = "P19",
                                "NA" = "C01", "NA" = "C02",
                                "NA" = "C03", "NA"= "C04", "NA" = "C05", 
                                "NA" = "C06", "NA" = "C07", "NA" = "C08",
                                "NA" = "C09", "NA" = "C10", "NA" = "C11",
                                "NA" = "C12", "NA" = "C13", "NA" = "C14",
                                "NA" = "C15", "NA" = "C16", "NA" = "C17",
                                "NA" = "C18", "NA" = "C19")

## conert factor to numeric
data$Intell = as.numeric(as.character(data$Intell))

## z-standadise parameters of potential further influencing factors (UPDRS, TMT, disease duration, intelligibility)

data$TMTdiff_z <- scale(data$TMTdiff)
data$TMTdiff2_z <- scale(data$TMTdiff2)
data$TMTA_z <- scale(data$TMTA)
data$BTA_z <- scale(data$BTA)
data$digitspan_z <- scale(data$digitspan)
data$UPDRS_z <- scale(data$UPDRS)
data$Dur_z <- scale(data$Dur)
data$Intell_z <- scale(data$Intell)


## column part of speech

data[,"Word"] <- data[,"utt"]

data$Word = as.factor(data$Word)

## Level umbenennen
levels(data[,"Word"]) <- list("Hose" = "1", "Dose" = "5",
                            "Hose" = "7", "Wiese" = "11", "Vase" = "12", 
                            "Mine" = "13", "Wade" = "14",
                            "Nase" = "15", "Dose" = "17", "Wiese" = "19",
                            "Nase" = "20", "Biene" = "23", "Biene" = "26",
                            "Bohne" = "28", "Mine" = "29", "Vase" = "30",
                            "Bohne" = "33", "Wade" = "35")


################################
## (i) group - based analysis ##
################################

## Testing fixed factor: group
TH.full <- lmer(TH ~ Group + (1| speaker) + (1|Word), data = data, REML=FALSE)
TH.null <- lmer(TH ~ 1 + (1|speaker)+ (1|Word), data = data, REML=FALSE)

anova(TH.full, TH.null)  ## no effect of group X2(1) = 0.3445 p= 0.5573


############################################
## (ii) individual - differences analysis ##
############################################

## subset per group

PAT = subset(data, Group == "patient")
CON = subset(data, Group == "control")


##############
## patients ##
##############

## influence TMT A
patient.TMTA <- lmer(TH ~  TMTA_z + (1|speaker)+ (1|Word) , data = na.exclude(PAT),  REML=FALSE)
patient.null <- lmer(TH ~  1 + (1|speaker)+ (1|Word), data = na.exclude(PAT), REML=FALSE)

anova(patient.TMTA, patient.null) ## X2(1) = 0.0798   ; p=0.7775 --> no influence of TMT A


# influence TMT diff (B-A)
patient.TMTdiff <- lmer(TH ~  TMTdiff_z + (1|speaker)+ (1|Word) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.TMTdiff, patient.null) ## X2(1) = 7.2313  ; p=0.007164 ** --> influence of TMT difference
summary(patient.TMTdiff) ## higher tonal height if slower TMT


# influence BTA
patient.BTA <- lmer(TH ~  BTA_z + (1|speaker)+ (1|Word) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.BTA, patient.null) ## X2(1) = 6.1781 ; p=0.01293 * --> no influence of BTA (by alpha level 0.0127)


# influencce digit span
patient.DS <- lmer(TH ~  digitspan_z + (1|speaker)+ (1|Word) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.DS, patient.null) ## X2(1) = 0.0369  ; p=0.8477 --> no influence of digit span


## influence duration of disease
patient.Dur <- lmer(TH ~  Dur_z + (1 |speaker)+ (1|Word) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.Dur, patient.null) ## X2(1)=2.0515; p=0.1521 --> no influence of duration 


## influence of UPDRS
patient.UPDRS <- lmer(TH ~  UPDRS_z + (1 |speaker)+ (1|Word), data = na.exclude(PAT),  REML=FALSE)

anova(patient.UPDRS, patient.null) ## X2(1)=0.0154   ; p=0.9012 --> no influence of motor impairment


## influence of intelligibility rating
patient.intell <- lmer(TH ~  Intell_z + (1 |speaker)+ (1|Word), data = na.exclude(PAT),  REML=FALSE)

anova(patient.intell, patient.null) ## X2(1)=1.5784  ; p=0.282 --> no influence of intelligibility



##############
## controls ##
##############

## influence TMT A
control.TMTA <- lmer(TH ~  TMTA_z + (1|speaker)+ (1|Word) , data = CON,  REML=FALSE)
control.null <- lmer(TH ~  1 + (1|speaker)+ (1|Word), data = CON, REML=FALSE)

anova(control.TMTA, control.null) ## X2(1) = 1.244  ; p= 0.2647--> no influence of TMT A


# influence TMT diff (B-A)
control.TMTdiff <- lmer(TH ~  TMTdiff_z + (1|speaker)+ (1|Word) , data = CON,  REML=FALSE)

anova(control.TMTdiff, control.null) ## X2(1) = 0.556  ; p=0.4559 --> no influence of TMT difference



# influence BTA
control.BTA <- lmer(TH ~  BTA_z + (1|speaker)+ (1|Word) , data = CON,  REML=FALSE)

anova(control.BTA, control.null) ## X2(1) = 1.7737 ; p=0.1829 --> no influence of BTA


# influencce digit span
control.DS <- lmer(TH ~  digitspan_z + (1|speaker)+ (1|Word) , data = CON,  REML=FALSE)

anova(control.DS, control.null) ## X2(1) = 0.3591  ; p=0.549 --> no influence of digit span


###############################################################################################


## (3)
## Correlation TMT & tonal hieght with and without outliers


## with outlier (PD 13)
patient.TMTdiffwith <- lmer(TH ~  TMTdiff_z  + (1|speaker) + (1| Word), data = PAT, REML=FALSE)

## without outlier
patient.TMTdiffwithout <- lmer(TH ~  TMTdiff2_z + (1|speaker)+ (1| Word), data = PAT,  REML=FALSE)

## comparison via AIC
summary(patient.TMTdiffwith) ## AIC = 1077.4
summary(patient.TMTdiffwithout) ## AIC = 1030.9 ## --> better model fit for linear relationship (lower AIC)


## Correlation

## checking normal distribution
## This p-value tells you what the chances are that the sample comes from a normal distribution. 
## The lower this value, the smaller the chance. 

## Correlation TMT B-A and tonal height

shapiro.test(PAT$TMTdiff) ## p-value < 2.2e-16 --> not normal distributed
shapiro.test(PAT$TMTdiff2) ## p-value = 1.939e-13 --> not normal distributed

shapiro.test(PAT$TH) ## p-value =  0.0005618 --> not normal distributed

## --> better to use non-parametric rank correlations (kenall)

## with outlier (PD 13)
cor.test(PAT$TMTdiff_z, PAT$TH,  method = "kendall", conf.level = 0.95) ## tau = 0.14, p = 0.002137


## without outlier
cor.test(PAT$TMTdiff2_z, PAT$TH, method = "kendall", conf.level = 0.95) ## tau = 0.17, p = 0.0001389



## correlation Intelligibility & tonal height

shapiro.test(PAT$Intell) ## p-value 1.041e-10 --> not normal distributed
shapiro.test(PAT$TH) ## p-value =  0.0005618 --> not normal distributed

## --> better to use non-parametric rank correlations (kenall)


cor.test(PAT$Intell, PAT$TH,  method = "kendall", conf.level = 0.95) ## tau = 0.16, p = 0.0003311



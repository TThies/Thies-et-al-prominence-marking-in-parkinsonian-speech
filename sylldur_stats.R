#############################
## Prominence Marking in PD ##
##           PLOS ONE       ##
## 04.01.2018 - Tabea Thies ##
##############################

## Analysis of syllable duration measures

## (1) Mean duration
## (2) Statistical anaylses

## insert working directory
setwd("C:/Users/Tabea Thies/Desktop/Phonetik Institut/Paper_Article/KUS_Studie_PD_Daten/Versionen Paper_2nd try_Neuropsychologia/Data")

## read data file (csv)
data=read.csv("sylldur.csv",header= T, strip.white=TRUE)


##############################################################################################


## (1)
## means and sd for table 5: values of sylldur

aggregate(Syll.dur ~ Akzent + Gruppe, data, mean)
aggregate(Syll.dur ~ Akzent + Gruppe, data, sd)


##############################################################


## (2)
## statistical analyses

## load package
library(lme4)

#############################################
## add new columns for influencing factors ##
#############################################

## neuen Faktor erstellen: TMT A

data[,"TMTA"] <- data[,"spk_age"]
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

## neuen Faktor erstellen: TMT B

data[,"TMTB"] <- data[,"spk_age"]
## Level umbenennen
levels(data[,"TMTB"]) <- list("10" = "P01", "50" = "P02",
                              "70" = "P03", "20" = "P04", "10" = "P05", 
                              "10" = "P06", "10" = "P07",
                              "10" = "P08", "40" = "P09", "10" = "P10",
                              "70" = "P11", "10" = "P12", "50" = "P13",
                              "10" = "P14", "50" = "P15", "40" = "P16",
                              "90" = "P17", "10" = "P18",  "NA" = "P19",
                              "30" = "C01", "90" = "C02",
                              "80" = "C03", "90" = "C04", "80" = "C05", 
                              "10" = "C06", "90" = "C07", "70" = "C08",
                              "20" = "C09", "30" = "C10", "90" = "C11",
                              "90" = "C12", "90" = "C13", "90" = "C14",
                              "90" = "C15", "20" = "C16", "50" = "C17",
                              "40" = "C18", "90" = "C19")

## conert factor to numeric
data$TMTB = as.numeric(as.character(data$TMTB))


## neuen Faktor erstellen: TMT diff

data[,"TMTdiff"] <- data[,"spk_age"]
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

data[,"TMTdiff2"] <- data[,"spk_age"]
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


## neuen Faktor erstellen: TMT Quatient B/A

data[,"TMTquo"] <- data[,"spk_age"]
## Level umbenennen
levels(data[,"TMTquo"]) <- list("2.90" = "P01", "2.89" = "P02",
                                  "4.05" = "P03", "1.70" = "P04", "4.41" = "P05", 
                                  "3.08" = "P06", "2.90" = "P07",
                                  "3.80" = "P08", "1.43" = "P09", "2.36" = "P10",
                                  "2.27" = "P11", "1.93" = "P12", "4.04" = "P13",
                                  "9.91" = "P14", "1.41" = "P15", "2.23" = "P16",
                                  "1.89" = "P17", "3.14" = "P18",  "NA" = "P19",
                                  "2.28" = "C01", "2.13" = "C02",
                                  "2.51" = "C03", "2.75" = "C04", "2.50" = "C05", 
                                  "2.83" = "C06", "2.73" = "C07", "1.64" = "C08",
                                  "1.96" = "C09", "2.43" = "C10", "2.11" = "C11",
                                  "1.26" = "C12", "1.90" = "C13", "1.41" = "C14",
                                  "2.30" = "C15", "2.29" = "C16", "2.17" = "C17",
                                  "3.94" = "C18", "1.44" = "C19")

## conert factor to numeric
data$TMTquo = as.numeric(as.character(data$TMTquo))

## neuen Faktor erstellen: BTA

data[,"BTA"] <- data[,"spk_age"]
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

data[,"digitspan"] <- data[,"spk_age"]
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

## neuen Faktor erstellen: verbal fluency

data[,"verbalf"] <- data[,"spk_age"]
## Level umbenennen
levels(data[,"verbalf"]) <- list("23" = "P01", "26" = "P02",
                                   "23" = "P03", "35" = "P04", "40" = "P05", 
                                   "22" = "P06", "10" = "P07",
                                   "17" = "P08", "15" = "P09", "32" = "P10",
                                   "16" = "P11", "20" = "P12", "10" = "P13",
                                   "18" = "P14", "20" = "P15", "18" = "P16",
                                   "23" = "P17", "16" = "P18",  "8" = "P19",
                                   "27" = "C01", "29" = "C02",
                                   "21" = "C03", "29" = "C04", "25" = "C05", 
                                   "22" = "C06", "20" = "C07", "20" = "C08",
                                   "18" = "C09", "22" = "C10", "38" = "C11",
                                   "38" = "C12", "19" = "C13", "19" = "C14",
                                   "27" = "C15", "22" = "C16", "20" = "C17",
                                   "18" = "C18", "20" = "C19")

## conert factor to numeric
data$verbalf = as.numeric(as.character(data$verbalf))

## neuen Faktor erstellen: phonematic fluency

data[,"phonf"] <- data[,"spk_age"]
## Level umbenennen
levels(data[,"phonf"]) <- list("7" = "P01", "16" = "P02",
                                 "14" = "P03", "10" = "P04", "12" = "P05", 
                                 "14" = "P06", "11" = "P07",
                                 "8" = "P08", "11" = "P09", "20" = "P10",
                                 "7" = "P11", "14" = "P12", "2" = "P13",
                                 "25" = "P14", "18" = "P15", "4" = "P16",
                                 "28" = "P17", "5" = "P18",  "11" = "P19",
                                 "18" = "C01", "10" = "C02",
                                 "12" = "C03", "21" = "C04", "10" = "C05", 
                                 "16" = "C06", "14" = "C07", "20" = "C08",
                                 "17" = "C09", "12" = "C10", "12" = "C11",
                                 "18" = "C12", "18" = "C13", "22" = "C14",
                                 "13" = "C15", "12" = "C16", "20" = "C17",
                                 "16" = "C18", "18" = "C19")

## conert factor to numeric
data$phonf = as.numeric(as.character(data$phonf))

## neuen Faktor erstellen: disease duration

data[,"Dur"] <- data[,"spk_age"]
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

data[,"Intell"] <- data[,"spk_age"]
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
data$TMTB_z <- scale(data$TMTB)
data$TMTquo_z <- scale(data$TMTquo)
data$BTA_z <- scale(data$BTA)
data$digitspan_z <- scale(data$digitspan)
data$verbalf_z <- scale(data$verbalf)
data$phonf_z <- scale(data$phonf)
data$UPDRS_z <- scale(data$UPDRS)
data$Dur_z <- scale(data$Dur)
data$Intell_z <- scale(data$Intell)


################################
## (i) Gruppe - based analysis ##
################################

## Testing fixed factor: Gruppe
sylldur.Gruppe.full <- lmer(Syll.dur ~ Gruppe + Akzent + (1| spk_age) , data = data, REML=FALSE)
sylldur.Gruppe.null <- lmer(Syll.dur ~ 1 + Akzent + (1|spk_age) , data = data, REML=FALSE)

anova(sylldur.Gruppe.full, sylldur.Gruppe.null)  ## no effect of Gruppe X2(1) = 0.0204    p = 0.8864


## Testing fixed factor: accent condition
sylldur.Akzent.full <- lmer(Syll.dur ~ Gruppe + Akzent + (1 spk_age) , data = data, REML=FALSE)
sylldur.Akzent.null <- lmer(Syll.dur ~ Gruppe + 1 + (1| spk_age) , data = data, REML=FALSE)

anova(sylldur.Akzent.full, sylldur.Akzent.null)  ## effect of accent condition X2(1) = 623.69  p < 2.2e-16 ***
summary(sylldur.Akzent.full) ## sylldur longer in accented Words


############################################
## (ii) individual - differences analysis ##
############################################

## subset per Gruppe

PAT = subset(data, Gruppe == "Pat")
CON = subset(data, Gruppe == "Con")


##############
## patients ##
##############

## influence TMT A
patient.TMTA <- lmer(Syll.dur ~  TMTA_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = na.exclude(PAT),  REML=FALSE)
patient.null <- lmer(Syll.dur ~  1 + Akzent + (1+Akzent|spk_age)+ (1|Wort), data = na.exclude(PAT), REML=FALSE)

anova(patient.TMTA, patient.null) ## X2(1) = 1.6138  ; p=0.204--> no influence of TMT A


# influence TMT B
patient.TMTB <- lmer(Syll.dur ~  TMTB_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.TMTB, patient.null) ## X2(1) = 2.7339  ; p=0.09824 --> no influence of TMT B


# influence TMT diff (B-A)
patient.TMTdiff <- lmer(Syll.dur ~  TMTdiff_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.TMTdiff, patient.null) ## X2(1) = 2.9098  ; p=0.08805 --> no influence of TMT difference



# influence TMT quatient (B/A)
patient.TMTquo <- lmer(Syll.dur ~  TMTquo_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.TMTquo, patient.null) ## X2(1) = 0.6436 ; p=0.4224 --> no influence of TMT quotient


# influence BTA
patient.BTA <- lmer(Syll.dur ~  BTA_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.BTA, patient.null) ## X2(1) = 1.4701 ; p=0.2253 --> no influence of BTA


# influencce digit span
patient.DS <- lmer(Syll.dur ~  digitspan_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.DS, patient.null) ## X2(1) = 0.3602 ; p=0.5484 --> no influence of digit span

# influencce verbal fluency
patient.verb <- lmer(Syll.dur ~  verbalf_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.verb, patient.null) ## X2(1) = 0.7022 ; p=0.4021 --> no influence of verbal fluency

# influencce phonematic fluency
patient.phon<- lmer(Syll.dur ~  phonf_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.phon, patient.null) ## X2(1) = 1.7902 ; p=0.1809 --> no influence of phonematic fluency


## influence duration of disease
patient.Dur <- lmer(Syll.dur ~  Dur_z + Akzent + (1+Akzent |spk_age)+ (1|Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.Dur, patient.null) ## X2(1)=0.4167 ; p=0.5186 --> no influence of duration 


## influence of UPDRS
patient.UPDRS <- lmer(Syll.dur ~  UPDRS_z + Akzent + (1+Akzent |spk_age)+ (1|Wort), data = na.exclude(PAT),  REML=FALSE)

anova(patient.UPDRS, patient.null) ## X2(1)=0.3921  ; p=0.5312 --> no influence of motor impairment


## influence of intelligibility rating
patient.intell <- lmer(Syll.dur ~  Intell_z + Akzent + (1+Akzent |spk_age)+ (1|Wort), data = na.exclude(PAT),  REML=FALSE)

anova(patient.intell, patient.null) ## X2(1)=0.2261  ; p=0.6344 --> no influence of intelligibility



##############
## controls ##
##############

## influence TMT A
control.TMTA <- lmer(Syll.dur ~  TMTA_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = CON,  REML=FALSE)
control.null <- lmer(Syll.dur ~  1 + Akzent + (1+Akzent|spk_age)+ (1|Wort), data = CON, REML=FALSE)

anova(control.TMTA, control.null) ## X2(1) = 0.5569  ; p= 0.4555 --> no influence of TMT A


# influence TMT B
control.TMTB <- lmer(Syll.dur ~  TMTB_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = CON,  REML=FALSE)

anova(control.TMTB, control.null) ## X2(1) = 0.011  ; p=0.9166 --> no influence of TMT B


# influence TMT diff (B-A)
control.TMTdiff <- lmer(Syll.dur ~  TMTdiff_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = CON,  REML=FALSE)

anova(control.TMTdiff, control.null) ## X2(1) = 0.2121  ; p=0.6451 --> no influence of TMT difference


# influence TMT quatient (B/A)
control.TMTquo <- lmer(Syll.dur ~  TMTquo_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = CON,  REML=FALSE)

anova(control.TMTquo, control.null) ## X2(1) = 0.0608 ; p=0.8053 --> no influence of TMT difference


# influence BTA
control.BTA <- lmer(Syll.dur ~  BTA_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = CON,  REML=FALSE)

anova(control.BTA, control.null) ## X2(1) = 0.1028 ; p=0.7485 --> no influence of BTA


# influencce digit span
control.DS <- lmer(Syll.dur ~  digitspan_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = CON,  REML=FALSE)

anova(control.DS, control.null) ## X2(1) = 0.7717  ; p=0.3797 --> no influence of digit span


# influencce verbal fluency
control.verb <- lmer(Syll.dur ~  verbalf_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = CON,  REML=FALSE)

anova(control.verb, control.null) ## X2(1) = 6927 ; p=0.1008 --> no influence of verbal fluency


# influencce phonematic fluency
control.phon<- lmer(Syll.dur ~  phonf_z + Akzent + (1+Akzent|spk_age)+ (1|Wort) , data = CON,  REML=FALSE)

anova(control.phon, control.null) ## X2(1) = 2.1321  ; p=0.1442 --> no influence of phonematic fluency
summary(control.phon)















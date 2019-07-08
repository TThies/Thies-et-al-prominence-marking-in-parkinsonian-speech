##############################
## Prominence Marking in PD ##
##     Neuropsychologia     ##
## 08.07.2018 - Tabea Thies ##
##############################

## Analysis of intensity measures

## (1) Mean intensity
## (2) Statistical anaylses with VIF's for fixed factors (collinearity testing)
## (3) Testing correlation with and without outlier (TMT)

## insert working directory
setwd("")

## read data file (csv)
data=read.csv("intensity.csv",header= T, strip.white=TRUE)


##############################################################################################


## (1)
## means and sd for table 5: values of intensity

aggregate(mean_vowel ~ acc_condition + group, data, mean)
aggregate(mean_vowel ~ acc_condition + group, data, sd)


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


############################
## check for collinearity ##
##  between fixed factors ##
############################

## Variation Influence Factor (vif)

vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }



################################
## (i) group - based analysis ##
################################

## Testing fixed factor: group
intensity.group.full <- lmer(mean_vowel ~ group + acc_condition + (1| speaker) + (1|word), data = data, REML=FALSE)
intensity.group.null <- lmer(mean_vowel ~ 1 + acc_condition + (1|speaker) + (1|word), data = data, REML=FALSE)

anova(intensity.group.full, intensity.group.null)  ## no effect of group X2(1) = 1.5732  p = 0.2097


## Testing fixed factor: accent condition
intensity.acc_condition.full <- lmer(mean_vowel ~ group + acc_condition + (1| speaker) + (1|word), data = data, REML=FALSE)
intensity.acc_condition.null <- lmer(mean_vowel ~ group + 1 + (1| speaker) + (1|word), data = data, REML=FALSE)

anova(intensity.acc_condition.full, intensity.acc_condition.null)  ## effect of accent condition X2(1) = 520.26  p < 2.2e-16 ***
summary(intensity.acc_condition.full) ## intensity lower in unaccented vowels

## collinearity
vif.lme(intensity.group.full) ## vif = 1.004


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
patient.TMTA <- lmer(mean_vowel ~  TMTA_z + acc_condition + (1+acc_condition|speaker)+ (1|word) , data = na.exclude(PAT),  REML=FALSE)
patient.null <- lmer(mean_vowel ~  1 + acc_condition + (1+acc_condition|speaker)+ (1|word), data = na.exclude(PAT), REML=FALSE)

anova(patient.TMTA, patient.null) ## X2(1) = 1.7576  ; p=0.1849 --> no influence of TMT A

vif.lme(patient.TMTA) ## vif = 1.000048 


# influence TMT diff (B-A)
patient.TMTdiff <- lmer(mean_vowel ~  TMTdiff_z + acc_condition + (1+acc_condition|speaker)+ (1|word) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.TMTdiff, patient.null) ## X2(1) = 8.1019  ; p=0.004422 ** --> influence of TMT difference
summary(patient.TMTdiff) ## higher intensity if slower TMT

vif.lme(patient.TMTdiff) ## vif =  1.000981


# influence BTA
patient.BTA <- lmer(mean_vowel ~  BTA_z + acc_condition + (1+acc_condition|speaker)+ (1|word) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.BTA, patient.null) ## X2(1) = 2.496 ; p=0.1141 --> no influence of BTA

vif.lme(patient.BTA) ## vif =  1.000645


# influencce digit span
patient.DS <- lmer(mean_vowel ~  digitspan_z + acc_condition + (1+acc_condition|speaker)+ (1|word) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.DS, patient.null) ## X2(1) = 0.3797 ; p=0.5377 --> no influence of digit span

vif.lme(patient.DS) ## vif =  1.000246

## influence duration of disease
patient.Dur <- lmer(mean_vowel ~  Dur_z + acc_condition + (1+acc_condition |speaker)+ (1|word) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.Dur, patient.null) ## X2(1)=0.1181; p=0.7311 --> no influence of duration 

vif.lme(patient.Dur) ## vif =  1.000014

## influence of UPDRS
patient.UPDRS <- lmer(mean_vowel ~  UPDRS_z + acc_condition + (1+acc_condition |speaker)+ (1|word), data = na.exclude(PAT),  REML=FALSE)

anova(patient.UPDRS, patient.null) ## X2(1)=0.0148  ; p=0.9033 --> no influence of motor impairment

vif.lme(patient.UPDRS) ## vif =  1.000004

## influence of intelligibility rating
patient.intell <- lmer(mean_vowel ~  Intell_z + acc_condition + (1+acc_condition |speaker)+ (1|word), data = na.exclude(PAT),  REML=FALSE)

anova(patient.intell, patient.null) ## X2(1)=1.5784  ; p=0.209 --> no influence of intelligibility

vif.lme(patient.intell) ## vif =  1.000093


##############
## controls ##
##############


## influence TMT A
control.TMTA <- lmer(mean_vowel ~  TMTA_z + acc_condition + (1+acc_condition|speaker)+ (1|word) , data = CON,  REML=FALSE)
control.null <- lmer(mean_vowel ~  1 + acc_condition + (1+acc_condition|speaker)+ (1|word), data = CON, REML=FALSE)

anova(control.TMTA, control.null) ## X2(1) = 1.174  ; p= 0.2786 --> no influence of TMT A

vif.lme(control.TMTA) ## vif =  1.000088

# influence TMT diff (B-A)
control.TMTdiff <- lmer(mean_vowel ~  TMTdiff_z + acc_condition + (1+acc_condition|speaker)+ (1|word) , data = CON,  REML=FALSE)

anova(control.TMTdiff, control.null) ## X2(1) = 0.0907  ; p=0.7633 --> no influence of TMT difference

vif.lme(control.TMTdiff) ## vif =  1.000056

# influence BTA
control.BTA <- lmer(mean_vowel ~  BTA_z + acc_condition + (1+acc_condition|speaker)+ (1|word) , data = CON,  REML=FALSE)

anova(control.BTA, control.null) ## X2(1) = 2.4227 ; p=0.1196 --> no influence of BTA

vif.lme(control.BTA) ## vif =  1.000113

# influencce digit span
control.DS <- lmer(mean_vowel ~  digitspan_z + acc_condition + (1+acc_condition|speaker)+ (1|word) , data = CON,  REML=FALSE)

anova(control.DS, control.null) ## X2(1) = 0.7346  ; p=0.3914 --> no influence of digit span

vif.lme(control.DS) ## vif =  1.000066


###############################################################################################


## (3)
## Correlation TMT & intensity with and without outliers

## mixed model:
## with outlier (PD 13) - OLD!
patient.TMTwith <- lmer(mean_vowel ~  TMT_z  + (1|speaker) + (1| word), data = PAT, REML=FALSE)

## without outlier - OLD!
patient.TMTwithout <- lmer(mean_vowel ~  TMT2_z + (1|speaker)+ (1| word), data = PAT,  REML=FALSE)

## comparison via AIC
summary(patient.TMTwith) ## AIC = 2565.8
summary(patient.TMTwithout) ## AIC = 2442.2 ## --> better model fit for linear relationship

############# 

## with outlier (PD 13)
patient.TMTdiffwith <- lmer(mean_vowel ~  TMTdiff_z  + (1|speaker) + (1| word), data = PAT, REML=FALSE)

## without outlier
patient.TMTdiffwithout <- lmer(mean_vowel ~  TMTdiff2_z + (1|speaker)+ (1| word), data = PAT,  REML=FALSE)

## comparison via AIC
summary(patient.TMTdiffwith) ## AIC = 2565.8
summary(patient.TMTdiffwithout) ## AIC = 2442.2 ## --> better model fit for linear relationship

## --> same results with new data!


## (3) Correlation

## checking normal distribution
## This p-value tells you what the chances are that the sample comes from a normal distribution. 
## The lower this value, the smaller the chance. 

## Correlation Trail Making test & intensity

shapiro.test(data$TMTdiff) ## p-value < 2.2e-16 --> not normal distributed
shapiro.test(data$TMTdiff2) ## p-value < 2.2e-16 --> not normal distributed

shapiro.test(data$mean_vowel) ## p-value = 0.000597 --> not normal distributed

## --> better to use non-parametric rank correlations (kenall)


## with outlier (PD 13)
cor.test(PAT$TMTdiff_z, PAT$mean_vowel,  method = "kendall", conf.level = 0.95) ## tau = 0.26, p < 2.2e-16

## without outlier
cor.test(PAT$TMTdiff2_z, PAT$mean_vowel, method = "kendall", conf.level = 0.95) ## tau = 0.29, p < 2.2e-16


## Correlation Intelligibility & intensity

shapiro.test(PAT$Intell) ## p-value < 2.2e-16 --> not normal distributed
shapiro.test(PAT$mean_vowel) ## p-value = 0.2449 -->  normal distributed

## --> better to use non-parametric rank correlations (kenall)

cor.test(PAT$Intell, PAT$mean_vowel,  method = "kendall", conf.level = 0.95) ## tau = 0.26, p = 1.677e-07

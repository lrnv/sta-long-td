# install.packages("lme4");install.packages("geepack"); 
require(lme4); require(nlme)

rdata<- read.delim('RIESBY_DAT.txt');
names(rdata) 
summary(rdata)

idx=which(is.na(rdata["HamD"])==1)
rdata=rdata[-idx,]
attach(rdata)

fWeek <-paste("J",rdata[,"Week"],sep="")
fEndog <-ifelse(rdata[,"Endog"]==0,"No","Yes")
subj <- paste("S",rdata[,"ID"],sep="")
rdata <-data.frame(subj,ID,fWeek,Week,fEndog,Endog,HamD)


# -------------------------------------------------------------
# ANOVA model 
# Modèle ANOVA 1 facteur 
#   + 1 facteur de répetition
# -------------------------------------------------------------
summary(lm(HamD~fWeek + fEndog, data=rdata))
summary(aov(HamD~fWeek + fEndog, data=rdata))


# -------------------------------------------------------------
# ANOVA model 
# Modèle ANOVA 1 facteur 
#   + 1 facteur de répetition 
#   + 1 effet sujet 
# -------------------------------------------------------------
summary(lmer(HamD~fWeek+ fEndog  + (1|subj), data=rdata))
summary(aov(HamD~fWeek + fEndog + Error(subj), data=rdata))

# -------------------------------------------------------------
# Note : aov fournit la decomposition de
# la variance
# 1 composante sujet 
#     (variabilité inter-patient)
# 1 composante residuelle 
#     (variabilité intra-patient)
# -------------------------------------------------------------

# -------------------------------------------------------------
# Modèle ANOVA 1 facteur + 1 facteur de répetition 
# + facteur d'interaction entre facteurs + 1  effet sujet (modèle saturé) 
# -------------------------------------------------------------
summary(lmer(HamD~fWeek + fEndog + fWeek:fEndog + (1|subj), data=rdata))
# summary(lmer(HamD~fWeek*fEndog + (1|subj), data=rdata))
# summary(lmer(HamD~fWeek + fEndog + fWeek*fEndog + (1|subj), data=rdata))


summary(aov(HamD~fWeek + fEndog + fWeek:fEndog+ Error(subj), data=rdata))


# -------------------------------------------------------------
# Note : aov fournit la decomposition de la variance
# la difficulté réside dans le choix de la residuelle 
# pour tester les différents effets fixes
# le patient etant emboite (nested) dans un niveau du facteur Endog  
# la residuelle pour tester ce facteur est la variablilité entre sujets
# pour le temps choisir la residuelle intra-patient car 
# le test est plus puissant
# -------------------------------------------------------------


# -------------------------------------------------------------
# Modèle ANOVA 1 facteur + 1 facteur de répetition 
# -------------------------------------------------------------
# l'interaction n'etant pas significative, on adopte le plus souvent 
# le modèle sans interaction
# -------------------------------------------------------------
summary(lmer(HamD~fWeek * fEndog + (1|subj), data=rdata))
summary(aov(HamD~fWeek + fEndog + fWeek*fEndog+ Error(subj), data=rdata))
summary(aov(HamD~fWeek + fEndog + Error(subj), data=rdata))



# Mixed Model 
# -------------------------------------------------------------
# Recodage de la variable semaine initalement 1,2,3 ....
# Objectif pouvoir interpreter l'intercep du modèle 
# en terme de moyenne prédite pour le bras de reference choisi
# lors de 1ere visite  
# -------------------------------------------------------------
# Random Intercept Model 
fit <- lmer(HamD~ Week + (1|subj))
summary(fit)

# pvaleurs pour les effets fixes
1-pnorm(abs(summary(fit)$coefficients[,3]))
# summary(fit)$coefficients[,3] ==  stastitique de Wald coef/se ~N(0,1)
# --------------------------------------------------------------

# -------------------------------------------------------------
# Random Slope and Intercept Model 
# Attention 1 seul coefficient par effet fixe dans le modèle
# 1 intercep + 1 coefficient pour la pente moyenne pour la variable Week
# Week = 0, 1, 2 ...
# -------------------------------------------------------------
fit <- lmer(HamD~ Week + (1+Week|subj))
summary(fit)

# pvaleurs pour les effets fixes
1-pnorm(abs(summary(fit)$coefficients[,3]))
# -------------------------------------------------------------
# Note : prendre la valeur absolue de la statistique pour un test bilateral
# -------------------------------------------------------------

# On ajoute une covariable Endog=0,1
# Random Slope and Intercept Model 
fit <- lmer(HamD~ Week + Endog + (1+Week|subj))
summary(fit)

# pvaleurs pour les effets fixes
1-pnorm(abs(summary(fit)$coefficients[,3]))

# -------------------------------------------------------------
# ANOVA (GEE solution)
# GEE permet de s'affranchir des hypothèses de variance covariance
# Il imoose de modéliser la variable du vecteur des réponses d'un individu
# pas d'effets alétoire (Working Model)
# Les resultats sont robustes à des écarts aux hyptothèses du Working Model
# -------------------------------------------------------------

# install.packages("geepack");
require(geepack)
summary(geese(HamD ~ fWeek + Endog + fWeek * Endog -1, id=ID, 
              data=rdata, corstr="exchangeable"))
summary(geese(HamD ~ fWeek + Endog + Week * Endog -1, id=ID, 
              data=rdata, corstr="independence"))

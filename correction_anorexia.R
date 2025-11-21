library(MASS)
data("anorexia")
summary(anorexia)
head(anorexia)
str(anorexia)
attach(anorexia)


# Q1: Différence entre les poids avant et 
# après traitement ? 

# Sol 1: H0 = moyenne après == moyenne avant 
#        Test de student pour données appairées. 
t.test(Prewt, Postwt, paired=TRUE, data=anorexia)

#  moyenne après = moyenne avant  <==> moyenne après -  moyenne avant = 0 

# Sol 2: (equivalent) Test de student classique sur
# différence==0: 
d = Postwt - Prewt
t.test(d, mu=0, data=anorexia)

# Sol 3: test intercept=0 de Wald de la 
# régression linéaire: 
fitlm=lm(d~1, data=anorexia)
summary(fitlm)

# Note: C'est 3 fois le même test. 


# Q2 Même chose par groupe: 
# Sol 1: 
t.test(Prewt[Treat == "Cont"], Postwt[Treat == "Cont"],
       paired=TRUE, data=anorexia)

t.test(Prewt[Treat == "CBT"], Postwt[Treat == "CBT"], 
       paired=TRUE, data=anorexia)

t.test(Prewt[Treat == "FT"], Postwt[Treat == "FT"], 
       paired=TRUE, data=anorexia)

# Sol 2 (diff=0)
d = Postwt - Prewt
t.test(d[Treat == "CBT"], mu=0, data=anorexia)
t.test(d[Treat == "Cont"], data=anorexia)
t.test(d[Treat == "FT"], data=anorexia)

# Sol 3: (effet moyen nul):
fitlm = lm(d~Treat-1, data=anorexia)
summary(fitlm)

# Auter chose: 
summary(aov(fitlm))

# --------------------------------------------------------------
# Note 1: Test de Student appairé par groupe = 
#         Test de Student sur les differences par groupe = 
#         Modèle d'Anlyse de Variance avec 1 facteur  
# -------------------------------------------------------------- 
# Note : Interet de la solution 3 
# Test global parmettant de gerer la multiplicité des tests 
# Demontrer une difference avec 3 tests conduit à une augmentation 
# de l'erreur de type 1
# -------------------------------------------------------------- 


# ############################################################## 
# Q3 : Existe t'il une diff?rence entre les traitements 
# ############################################################## 
# ---------------------------------------------------------------
# Hypoth?se : Difference entre 2 groupes 
# ---------------------------------------------------------------
d = Postwt - Prewt
t.test(d[Treat == "Cont"], d[Treat == "CBT"], data=anorexia)
t.test(d[Treat == "Cont"], d[Treat == "CBT"], data=anorexia, var.equal=T)

# --------------------------------------------------------------
# Note 1: par default hypoth?se de variances diff?rentes 
#         entre les 2 groupes (test de Welch)
# Note :  var.equal=T hypoth?se de variances ?gales 
#         hypoth?se classique d'un mod?le d'analyse de variance
#         (test de Student classique)
# -------------------------------------------------------------- 
fitlm=lm(d~Treat, data=anorexia)
summary(fitlm)
cbind(anorexia,model.matrix(fitlm))

# --------------------------------------------------------------
# Note 1: Mod?le d'Analyse de Variance avec 1 facteur en conservant 
#         l' intercept  dans le mod?le
#         Param?tres du mod?le 
#                        Intercept= Moyenne du groupe CBT 
#                        Difference control - CBT 
#                        Difference FT - CBT 
# -------------------------------------------------------------- 
# Note 2: 
#         Rejet d'une diff?rence au seuil de 5%
#         si test global (2 ddl) singificatif ? 5%
#         et la diff?rence observ?e est significative ? 5% 
# -------------------------------------------------------------- 
# -------------------------------------------------------------- 
# Note 3: bien choisir la r?ference = Control
# -------------------------------------------------------------- 
NewTreat = factor(Treat, levels=c("Cont", "CBT", "FT"))
summary(lm(d~NewTreat))

# ------------------------------------------------------------------
# Relation avec les analyses de variance classiques (sans r?p?tition)
# ------------------------------------------------------------------ 
summary(aov(d~Treat-1)); 
summary(aov(d~Treat));

# -------------------------------------------------------------- 
# Note 1 : 
#         Mod?le sans intercept 
#         Test global toutes les difference Avant Apres = 0
#         Mod?le avec intercept 
#         Test global difference Avant Apres entre les bras de traitment
# -------------------------------------------------------------- 
# Note 2 : Le choix de la reference n'a pas d'influence sur ces r?sultats
# ---------------------------------------------------------------- 

# ########################################################################## 
# Q4 : Prendre en compte une heterog?neite des poids des patients 
# ########################################################################## 

NewTreat = factor(Treat, levels=c("Cont", "CBT", "FT"))

summary(lm(d~Prewt + NewTreat, data=anorexia))

Prewtc = Prewt - mean(Prewt)
Prewtc = scale(Prewt, center = TRUE, scale = FALSE)

summary(lm(d~Prewtc + NewTreat, data=anorexia))
# -------------------------------------------------------------- 
# Note  : 
#         Mod?le avec poids initial  
#         Intercept effet pour un individu poids theorique nul !!!! 
#         diff?rence traitment experimetnal et controle ajust? sur le poids du patient
#         Prewt augmentation pour entre 2 patients pour une difference initiale de 1 kg 

#         Mod?le avec poids initial centr? sur la moyenne  
#         Intercept effet pour un individu de poids moyen !!!! 
#         diff?rence traitment experimetnal et controle ajust? sur le poids du patient
#         Prewt augmentation pour entre 2 patients pour une difference initiale de 1 kg 
# -------------------------------------------------------------- 

NewTreat = factor(Treat, levels=c("Cont", "CBT", "FT"))

summary(aov(d~Prewt + NewTreat, data=anorexia))
Prewtc=Prewt - mean(Prewt)
summary(aov(d~Prewtc + NewTreat, data=anorexia))
# -------------------------------------------------------------- 
# Note 2 : Centrer le poids ou non n'a pas d'unfluence sur les tests globaux
# ---------------------------------------------------------------- 




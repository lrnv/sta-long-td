## Partie 1: dataset Anorexia

library(MASS)
data("anorexia")
summary(anorexia)
head(anorexia)
str(anorexia)
attach(anorexia)


# Q1: Différence entre les poids avant et 
# après traitement ? 

# Solution 1: H0 = moyenne après == moyenne avant 
#        Test de student pour données appairées. 
t.test(Prewt, Postwt, paired=TRUE, data=anorexia)

#  moyenne après = moyenne avant  <==> moyenne après -  moyenne avant = 0 

# Solution 2: (equivalent) Test de student classique sur
# différence==0: 
d = Postwt - Prewt
t.test(d, mu=0, data=anorexia)

# Solution 3: test intercept=0 de Wald de la 
# régression linéaire: 
fitlm=lm(d~1, data=anorexia)
summary(fitlm)

# Note: C'est 3 fois le même test. 


# Q2 Même chose par groupe: 
# Solution 1: 
t.test(Prewt[Treat == "Cont"], Postwt[Treat == "Cont"],
       paired=TRUE, data=anorexia)

t.test(Prewt[Treat == "CBT"], Postwt[Treat == "CBT"], 
       paired=TRUE, data=anorexia)

t.test(Prewt[Treat == "FT"], Postwt[Treat == "FT"], 
       paired=TRUE, data=anorexia)

# Solution 2 (diff=0)
d = Postwt - Prewt
t.test(d[Treat == "CBT"], mu=0, data=anorexia)
t.test(d[Treat == "Cont"], data=anorexia)
t.test(d[Treat == "FT"], data=anorexia)

# Solution 3: (effet moyen nul):
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
# Q3 : Existe t'il une différence entre les traitements 
# ############################################################## 
# ---------------------------------------------------------------
# Hypothèse : Difference entre 2 groupes 
# ---------------------------------------------------------------
d = Postwt - Prewt
t.test(d[Treat == "Cont"], d[Treat == "CBT"], data=anorexia)
t.test(d[Treat == "Cont"], d[Treat == "CBT"], data=anorexia, var.equal=T)

# --------------------------------------------------------------
# Note 1: par default hypothèse de variances différentes 
#         entre les 2 groupes (test de Welch)
# Note :  var.equal=T hypothèse de variances égales 
#         hypothèse classique d'un modèle d'analyse de variance
#         (test de Student classique)
# -------------------------------------------------------------- 
fitlm=lm(d~Treat, data=anorexia)
summary(fitlm)
cbind(anorexia,model.matrix(fitlm))

# --------------------------------------------------------------
# Note 1: Modèle d'Analyse de Variance avec 1 facteur en conservant 
#         l' intercept  dans le modèle
#         Paramètres du modèle 
#                        Intercept= Moyenne du groupe CBT 
#                        Difference control - CBT 
#                        Difference FT - CBT 
# -------------------------------------------------------------- 
# Note 2: 
#         Rejet d'une différence au seuil de 5%
#         si test global (2 ddl) singificatif ? 5%
#         et la différence observée est significative ? 5% 
# -------------------------------------------------------------- 
# -------------------------------------------------------------- 
# Note 3: bien choisir la réference = Control
# -------------------------------------------------------------- 
NewTreat = factor(Treat, levels=c("Cont", "CBT", "FT"))
summary(lm(d~NewTreat))

# ------------------------------------------------------------------
# Relation avec les analyses de variance classiques (sans répétition)
# ------------------------------------------------------------------ 
summary(aov(d~Treat-1)); 
summary(aov(d~Treat));

# -------------------------------------------------------------- 
# Note 1 : 
#         Modèle sans intercept 
#         Test global toutes les difference Avant Apres = 0
#         Modèle avec intercept 
#         Test global difference Avant Apres entre les bras de traitment
# -------------------------------------------------------------- 
# Note 2 : Le choix de la reference n'a pas d'influence sur ces résultats
# ---------------------------------------------------------------- 

# ########################################################################## 
# Q4 : Prendre en compte une heterogéneite des poids des patients 
# ########################################################################## 

NewTreat = factor(Treat, levels=c("Cont", "CBT", "FT"))

summary(lm(d~Prewt + NewTreat, data=anorexia))

Prewtc = Prewt - mean(Prewt)
Prewtc = scale(Prewt, center = TRUE, scale = FALSE)

summary(lm(d~Prewtc + NewTreat, data=anorexia))
# -------------------------------------------------------------- 
# Note  : 
#         Modèle avec poids initial  
#         Intercept effet pour un individu poids theorique nul !!!! 
#         différence traitment experimetnal et controle ajusté sur le poids du patient
#         Prewt augmentation pour entre 2 patients pour une difference initiale de 1 kg 

#         Modèle avec poids initial centré sur la moyenne  
#         Intercept effet pour un individu de poids moyen !!!! 
#         différence traitment experimetnal et controle ajusté sur le poids du patient
#         Prewt augmentation pour entre 2 patients pour une difference initiale de 1 kg 
# -------------------------------------------------------------- 

NewTreat = factor(Treat, levels=c("Cont", "CBT", "FT"))

summary(aov(d~Prewt + NewTreat, data=anorexia))
Prewtc=Prewt - mean(Prewt)
summary(aov(d~Prewtc + NewTreat, data=anorexia))
# -------------------------------------------------------------- 
# Note 2 : Centrer le poids ou non n'a pas d'unfluence sur les tests globaux
# ---------------------------------------------------------------- 




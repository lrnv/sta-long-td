# TD SAT-LONG

Ce TD vous propose de traiter deux jeux de données. 

## Partie 1: dataset Anorexia

Vous pouvez récupérer le jeu de données avec le code R suivant: 

```r
# install.packages("MASS")
library(MASS)
data("anorexia")
summary(anorexia)
attach(anorexia)
```

Voici une courte description du jeu de données: 

```
Source : Hand, D. J., Daly, F., McConway, K., Lunn, D. and Ostrowski, E. eds(1993) A Handbook of Small Data Sets. Chapman & Hall, Data set 285 (p. 229)

N= 72 sujets (jeunes femmes anorexiques)
3 variables
1 facteur Traitement
  groupe contrôle : placebo (26)
  groupe expérimental 1 (29) : thérapie cognitive comportementale (CBT)
  groupe expérimental 2 (17) : thérapie familiale (FT)
2 variables
  Poids avant thérapie
  Poids après thérapie
```

Répondez aux questions suivantes: 

0. Analyse descriptive: affichez quelques lignes du jeu de données, les étudier. Vérifier les classes des variables, affichez des boxplots et des tableaux de contingence, etc..
1. Y as-t'il une différence dans les poids avant et après le traitement ? Utilisez des tests statistiques simples tirés du cours pour répondre à cette question.  
2. Même question, mais en utilisant une analyse de la variance.
3. Réessayez en prenant en compte l'hétérogénéité des poids des patients.

## Partie 2 : dataset Riesby

Le code suivant récupère le jeu de données: 

```r
# install.packages("lme4")
# install.packages("geepack"); 
# require(lme4)
# require(nlme)
library(lme4)

rdata<- read.delim('Preciser le chemin/RIESBY_DAT.txt');
names(rdata) 
summary(rdata)
idx=which(is.na(rdata["HamD"])==1)
rdata=rdata[-idx,]
attach(rdata)
fWeek <-paste("J",rdata[,"Week"],sep="")
fEndog <-ifelse(rdata[,"Endog"]==0,"No","Yes")
subj <- paste("S",rdata[,"ID"],sep="")
rdata <-data.frame(subj,ID,fWeek,Week,fEndog,Endog,HamD)
```

Voici une courte description du jeu de données: 
```
Source : Riesby et al. (1977)
N=66
Variable Réponse : HAM-D score (Day 0, Day 7, Day 14, Day 21, D28, Day 35)
Variables explicatives
  1 facteur Diagnostic (Dx)
    Dx=0 : endogène (37)
    Dx=1 : exogène (29)
  1 facteur temps
  Co-Variables
    Dosage plasmatique d’IMIPRAMINE (IMI)
    Dosage plasmatique de DESIPRAMINE (DMI) le metabolite
```

Répondez aux questions suivantes: 

0. (analyse descriptive) Affichez quelques lignes du jeu de données, les étudier. Vérifier les classes des variables, affichez un spaghetti plot, etc...
1. Proposez une analyse de la variance sur un modèle à 1 facteur + 1 facteur de répétition
2. Proposez une analyse de la variance sur un modèle à 1 facteur + facteur de répétition + 1 effet sujet. 
3. Proposez une analyse de la variance sur un modèle à 1 facteur + facteur de répétition + 1 effet sujet + facteurs d'interactions (modèle saturé).
4. Recodez la variable semaine pour pouvoir interpreter l'intercept du modèle en term de moyenne prédite pour le bras de référence choisi lors de la 1ere visite. Puis utilisez un modèle à effet mixte avec iontercet aléatoire, décrire le modèle et sa qualité (tests)
5. Avec un intercept et une pente aléatoire ?
6. Proposez une anova basée sur un modèle linéaire marginal (GEE)
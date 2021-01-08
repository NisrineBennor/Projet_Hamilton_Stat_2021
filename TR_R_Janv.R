install.packages("prettyR") 
install.packages("corrgram")
install.packages("corrplot") 
install.packages("dplyr")
install.packages("psy") 
install.packages("ggplot2") 
install.packages("Hmisc") 
install.packages("lme4")
install.packages("survival") 
install.packages("ltm") 
install.packages("psych")
install.packages("tidyverse")
install.packages("summarytools")
install.packages("reshape2")
install.packages("wesanderson")
install.packages("survminer")

### Choix et téléchargement des librairie indispensables par ordre de besoin :
library(prettyR) 
library(corrgram)
library(corrplot) 
library(dplyr)
library(psy) 
library(ggplot2) 
library(Hmisc) 
library(lme4)
library(survival) 
library(ltm) 
library(psych)
library(tidyverse)
library(survminer)
library(wesanderson)

##=====================================Data management=================================

###========Importation des fichiers===========

df_grp<-read.csv2("outils_groupe.csv",sep = ",")
df_hamilton<-read.csv2("outils_hdrs.csv",sep = ",")
## le fichier df_scl90 contient des "ND": 
df_scl90<-read.csv2("outils_autoeval.csv",sep=",",na.strings=c("NA","ND"))

##========Fusion des fichiers=====================

### Verification des données, colonnes, lignes
str(df_grp)
str(df_hamilton)
str(df_scl90)

### Recherche de données manquantes
sum(is.na(df_grp))
sum(is.na(df_hamilton))
sum(is.na(df_scl90))



###============== Données manquantes et aberrantes===============

#### Fichier df_Hamilton

### Fusion de HAMD16A et HAMD16B en HAMD16 :

## Données manquantes 
## Je constate que l'item 16 de l'échelle de Hamilton est scindé en 2(HAMD16A & HAMD16B) donc on le 
## fusionne en une seule colonne HAMD16:
df_hamilton$HAMD16 <- ifelse(is.na(df_hamilton$HAMD16A)==F,df_hamilton$HAMD16A,df_hamilton$HAMD16B)
head(df_hamilton)
### Suppression des 2 colonnes HAMD16A et HAMD16B
df_hamilton <- df_hamilton[,names(df_hamilton)!="HAMD16A"& names(df_hamilton)!="HAMD16B"] 
## Réorganiser l'odre de les colonnes: HAMD16 et HAMD17 
df_hamilton<-df_hamilton%>%select("NUMERO": "HAMD15", "HAMD16", "HAMD17")

### Valeurs abérrantes 
### je recherche les valeurs abérantes: pas de valeurs abérantes pour le fichier hamilton( scores des items entre 0 et 4)
apply(df_hamilton[,3:16],2,table,useNA="always")

## L'existence d'une seule valeur manquantes pour tout les items du Score de Hamilton laisse lace au doute s'il s'agit pas d'une valeur manquante récurrente 
## On cherche l'invividu qui pour une visite le Score Hamilton n'a pas été mesuré (valeur NA Conjointe): 
## patient numéro 128 
df_hamilton[is.na(df_hamilton$HAMD1), 1:19]


####======================Fichier df_scl90=======================================

## On constate l'existance de plusieurs valeurs abberantes qui normalement ne doit pas dépasser un score de 4 
## Et on trouve plusieurs valeurs manquantes également 
apply(df_scl90[,3:92],2,table,useNA="always")

## En analysant les résultats l'item Q4 présente plus de nomre de données aberrantes = 13 valeurs ce qui représente un proportion de 1.26% 

### Pour gérer l'ensemble des valeurs aberrantes, on va les considérer comme valeurs manquantes( on les transforme)
is.na(df_scl90[,3:92]) <- df_scl90[,3:92] > 4

## verification des valeurs aberrantes == 0 valeurs aberrantes 
df_scl90 %>% select("Q1":"Q90") %>%
  filter_all(all_vars(.> 4)) 


### Fusionner  des fichiers en format long 
df_intermediare <- merge(df_hamilton,df_scl90, by=c("NUMERO","VISIT"),all=TRUE) 
df_tot <- merge(df_intermediare,df_grp, by="NUMERO",all=TRUE)
## Réorganiser l'odre de les colonnes:
df_tot <- df_tot %>% select("NUMERO", "VISIT","GROUPE","HAMD1":"Q90")

### Verification du nouveau fichier "df_tot":
length(df_tot$NUMERO) ## 1053
length(unique(df_tot$NUMERO)) ## 146
length(df_tot$NUMERO) / length(unique(df_tot$NUMERO)) ### Moyenne de visite par patient  
sum(duplicated(df_tot)) ## aucune valeur dupliquée
str(df_tot)

### on regarde une 2ème fois les valeurs manquantes aprés fusion pour le score de SCL90 pour
## voir les valeurs manquantes existe pour le même sujet de Hamilton ( 19 valeurs manquantes récurrantes)
df_tot %>% select("Q1":"Q90") %>% filter_all(all_vars(is.na(.)))
NA_SCL90 <- df_tot[is.na(df_tot$Q11), c(1:2, 21:110)]
NA_SCL90



###=================Nombre d'individus dans Chaque groupe========================= 

###=================Nombre d'individus dans Chaque groupe========================= 
table(df_grp$GROUP)

### présentation graphique de la répatition des individus dans chaque groupe en fonction de leur Numéro 
par(mfrow=c(1,2))
hist(unique(df_tot$NUMERO[df_tot$GROUPE == 0]), main = "Répartion
     dans le Groupe 0",xlab = "Numéro patient", xlim =
       c(0,max(df_tot$NUMERO)+10),ylab = "", ylim = c(0,12), col =
       "lightskyblue1")

hist(unique(df_tot$NUMERO[df_tot$GROUPE == 1]), main = "Répartion
     dans le Groupe 1",xlab = "Numéro patient", xlim =
       c(0,max(df_tot$NUMERO)+10),ylab = "", ylim = c(0,12), col =
       "azure4")


###===============Nombre de Chaque visite pour chaque groupe(groupe1 et groupe0)======================= 
table(df_tot$GROUPE, df_tot$VISIT, deparse.level = 2) 

## En faisant le calcul , on découdre qu'on passant de J0 à J56 
## on passe de 146 à 120 patients et ce qui veut dire qu'on perd 17.8% de la populations

### ======================Nombre de patients à chaque visite========================
df_hamilton$VISIT<-ordered(df_hamilton$VISIT,levels=c("J0","J4","J7","J14","J21","J28","J42","J56"))
tab<-table(df_hamilton$VISIT)
barplot(146-tab,main = "Nombre de perdus de vues à chaque visite",ylim = c(0,30),
        ylab = "Nombre de perdus de vues à chaque visite",col = "lightskyblue3")

###====================Echèlle de Hamilton=========================

## Création d'une variable de scrore de Hamilton des 17 Questions d'une visite 
df_tot$HDRSScore <-apply(df_tot[,4:20],1,sum, na.rm=TRUE)
## Réorganisation d'ordre de la variable dans le dataframe 
df_tot <- df_tot %>% select("NUMERO":"HAMD17","HDRSScore","Q1":"Q90")

### on crée des 2 sous-groupes  entre les 2 visites J0 et J56
HDRSScore_J0 <- df_tot$HDRSScore[df_tot$VISIT=="J0"]
HDRSScore_J56 <- df_tot$HDRSScore[df_tot$VISIT=="J56"]

summary(HDRSScore_J0)
summary(HDRSScore_J56)

###============ Repartition de la somme du Score Hamilton à J0 et J56 en fonction des 2 groupes 0 et 1 ============
par(mfrow=c(2,2))
hist(HDRSScore_J0[df_tot$GROUPE=="0"],main = "Répartition du score Hamilton à J0/Groupe0",
     xlab = "Score de Hamilton",ylim=c(0,40),
     col = "lightskyblue1",ylab = "Effectif")

hist(HDRSScore_J0[df_tot$GROUPE=="1"],main = "Répartition du score Hamilton à J0/Groupe1",
     xlab = "Score de Hamilton",ylim=c(0,40),
     col = "lightskyblue1",ylab = "Effectif")

hist(HDRSScore_J56[df_tot$GROUPE=="0"],main = "Répartition du score Hamilton à J56/Groupe0",
     xlab = "Score de Hamilton",ylim=c(0,40),
     col = "palegreen1",ylab = "Effectif")

hist(HDRSScore_J56[df_tot$GROUPE=="1"],main = "Répartition du score Hamilton à J56/Groupe",
     xlab = "Score de Hamilton",,ylim=c(0,40),
     col = "palegreen1",ylab = "Effectif")


####======================Distribution de la somme du Score Hamilton à J0 et J56================

ggplot(df_tot[df_tot$VISIT == "J0"|df_tot$VISIT == "J56", ], aes(x = HDRSScore, fill=VISIT)) +
  geom_histogram(aes(y =..density..), alpha = 1,color="white",bins=30) +
  geom_density(alpha =0.3,color="blue")+
  theme_minimal()+ theme(axis.line = element_line(size = 1,linetype = "dotted",colour = "navyblue"))+
  facet_wrap(~VISIT)+facet_wrap(~VISIT, ncol = 1)+theme(strip.text = element_text(colour="navyblue", size=10,face = "bold"))+
  scale_fill_manual(values = c("lightskyblue1", "palegreen1"))+
  ggtitle("Distribution de la somme du Score Hamilton à J0 et J56")


### ===================Fichier Dénormalisé(format large)=======================
HDRS_wide<-reshape(df_hamilton,direction="wide",idvar="NUMERO",timevar="VISIT")
ncol(HDRS_wide)
nrow(HDRS_wide)
str(HDRS_wide)


#OU en utilisant somme du Score Hamilton==========
library(reshape2) # Chargement de la librairie 
df_tot_wide<- dcast(df_tot, NUMERO + GROUPE ~ VISIT, value.var = "HDRSScore")
ncol(df_tot_wide)
nrow(df_tot_wide)
str(df_tot_wide)

####============================VALIDATON DE L'ÉCHELLE=================================

####====Analyse d'items: effets planchées et plafond, redondance ==========
###=====recherche d'effets plancher, plafond et données manquantes==========

### échelle hamilton 17 items à J0
par(mar=c(2,2,2,2))
par(mfrow=c(4,6))
hdrs_j0<-c("HAMD1.J0","HAMD2.J0","HAMD3.J0","HAMD4.J0","HAMD5.J0","HAMD6.J0","HAMD7.J0","HAMD8.J0" ,"HAMD9.J0","HAMD10.J0","HAMD11.J0","HAMD12.J0","HAMD13.J0","HAMD14.J0","HAMD15.J0","HAMD16.J0","HAMD17.J0")
for (i in hdrs_j0){barplot(table(HDRS_wide[,i], useNA = "always"), col = "lightskyblue1",names.arg = c(names(table(HDRS_wide[,i])),"NA"), ylim = c(0,120), main =i, sub = "graphique")}

### échelle hamilton 17 items à J56
par(mar=c(2,2,2,2))
par(mfrow=c(4,6))
hdrs_j56<-c("HAMD1.J56","HAMD2.J56","HAMD3.J56","HAMD4.J56","HAMD5.J56","HAMD6.J56","HAMD7.J56","HAMD8.J56","HAMD9.J56","HAMD10.J56","HAMD11.J56","HAMD12.J56","HAMD13.J56","HAMD14.J56","HAMD15.J56","HAMD16.J56","HAMD17.J56")
for (i in hdrs_j56){barplot(table(HDRS_wide[,i], useNA = "always"), col = c("palegreen1") ,names.arg = c(names(table(HDRS_wide[,i])),"NA"), ylim = c(0,120), main =i)}


######=============ANALYSE DES CORRELATIONS ENTRE LES ITEMS(redondance)=================

### recherche de la redondance des items: on constate que aucune correlation ne dépasse 0.44 à J0
hdrs_j0 <- c("HAMD1.J0", "HAMD2.J0","HAMD3.J0", "HAMD4.J0", "HAMD5.J0","HAMD6.J0",
         "HAMD7.J0", "HAMD8.J0", "HAMD9.J0", "HAMD10.J0", "HAMD11.J0", "HAMD12.J0",
         "HAMD13.J0", "HAMD14.J0", "HAMD15.J0", "HAMD16.J0", "HAMD17.J0")

COR_hdrs_J0 <- cor(subset(HDRS_wide[,hdrs_j0]),use = "complete.obs")

corrplot(COR_hdrs_J0, method = "color", addCoef.col = "cornsilk1", type="upper", order="hclust",
         col = wes_palette(name = "FantasticFox1", type = "discrete"), tl.col = "black", tl.srt = 45, diag = FALSE)

### ====== On répete la même opération pour J56
hdrs_j56<-c("HAMD1.J56", "HAMD2.J56","HAMD3.J56", "HAMD4.J56", "HAMD5.J56","HAMD6.J56",
             "HAMD7.J56", "HAMD8.J56", "HAMD9.J56", "HAMD10.J56", "HAMD11.J56", "HAMD12.J56",
             "HAMD13.J56", "HAMD14.J56", "HAMD15.J56", "HAMD16.J56", "HAMD17.J56")

COR_hdrs_J56<-cor(na.omit(subset(HDRS_wide[,hdrs_j56])))

corrplot(COR_hdrs_J56, method = "color", addCoef.col = "cornsilk1", type="upper",
         order="hclust",col = wes_palette(name = "FantasticFox1", type = "discrete"),
         tl.col = "black", tl.srt = 45, diag = FALSE)

### ======================== ACP======================
## On réalise une ACP pour estimer le nombre de dimensions du score total de Hamilton
### Pour JO
mdspca(HDRS_wide[,hdrs_j0])

### Pour J56
mdspca(HDRS_wide[,hdrs_j56])


### ============CLASSIFICATION HIERARCHIQUE=============
## On utilise cette technique pour vocation de détérminer de façon homogème 
## l'existance de lien entre nos différents items. l'ideé est la présentation par arbre 

## POUR J0
cah<-hclust(dist(t(scale(HDRS_wide[,hdrs_j0]))),method = "ward.D")
plot(cah,xlab="",ylab = "",main="Classification hiérarchique de SCore de hamilton à J0")


## POUR J56
cah2<-hclust(dist(t(scale(HDRS_wide[,hdrs_j56]))),method = "ward.D")
plot(cah2,xlab="",ylab = "",main="Classification hiérarchique de SCore de hamilton à J56")


####======== Évaluation du caractère du score de Hamilton à J0 et J56 (Unidimensionnel????)=======
### Structure dimensionnelle 
## POUR J0
scree.plot(HDRS_wide[,hdrs_j0], simu =20, title = "Scree Plot des items de Hamilton à J0")
## POUR J56
scree.plot(HDRS_wide[,hdrs_j56], simu =20, title = "Scree Plot des items de Hamilton à J56")

fa.parallel(HDRS_wide[,hdrs_j0], main = "Diag. valeures propres avec données simulées à J0",fa="pc", n.iter = 100) 
fa.parallel(HDRS_wide[,hdrs_j56], main = "Diag. valeures propres avec données simulées à J56",fa="pc", n.iter = 100) 



###================================= Analyse factorielle =========================================

## POUR J0 = à 3 facteurs 
print(factanal(na.omit(HDRS_wide[,hdrs_j0]),factors=3,rotation="varimax"),cutoff=0)

## POUR J56 = à 1 facteur 
print(factanal(na.omit(HDRS_wide[,hdrs_j56]),factors=1,rotation="varimax"),cutoff=0)


###================================= Cronbach: =========================================
## Evaluation de la consistance interne (Coeficien alpha de cronbach)
## POUR J0
cronbach(na.omit(HDRS_wide[,hdrs_j0])) ## $alpha= 0.45594
## POUR J0
cronbach(na.omit(HDRS_wide[,hdrs_j56])) ### $alpha = 0.819954


### ===== Validité Concourante et divergente  : Corrélation score total de Hamilton avec sous dimensions de SCL_90 ========

## On créer des sous-échelles , sommes des items composant différents symptômes
## Cependant, à cause des valeurs manquantes,
## on s'affranchit des problèmes causés par la somme et calculant des moyennes

df_pool<- df_tot
df_pool <- mutate(df_tot, SCL_somatisation = Q1 + Q4 + Q12 + Q27 + Q40 + Q42 + Q48 + Q49 + Q52 + Q53 + Q56 + Q58,
                  SCL_obsession = Q3 + Q9 + Q10 + Q28 + Q38 + Q45 + Q46 + Q51 + Q55 + Q65,
                  SCL_vulnerabilite = Q6 + Q21 + Q34 + Q36 + Q37 + Q41 + Q61 + Q69 + Q73,
                  SCL_depression = Q5 + Q14 + Q15 + Q20 + Q22 + Q26 + Q29 + Q30 + Q31 + Q32 + Q54 + Q71 + Q79, SCL_anxiete = Q2 + Q17 + Q23 + Q33 + Q39 + Q57 + Q72 + Q78 + Q80 + Q86,
                  SCL_hostilite = Q11 + Q24 + Q63 + Q67 + Q74 + Q81,
                  SCL_phobies = Q13 + Q25 + Q47 + Q70 + Q75 + Q82 + Q50,
                  SCL_parano = Q8 + Q18 + Q43 + Q68 + Q76 + Q83,
                  SCL_psycho = Q7 + Q16 + Q35 + Q62 + Q77 + Q84 + Q85 + Q87 + Q90 + Q88,
                  SCL_divers = Q19 + Q44 + Q59 + Q60 + Q64 + Q66 + Q89)


## On décompte le nombres de NA
sous_echelles_SCL <- c("SCL_somatisation", "SCL_obsession", "SCL_vulnerabilite", "SCL_depression", "SCL_anxiete",
                       "SCL_hostilite", "SCL_phobies", "SCL_parano", "SCL_psycho", "SCL_divers")


apply((df_pool [df_pool $VISIT == "J0",][,sous_echelles_SCL]), 2, function(y) sum(is.na(y))) 
apply((df_pool [df_pool $VISIT == "J56",][,sous_echelles_SCL]), 2, function(y) sum(is.na(y)))

## On regarde les correspondances entre les sous-échelles du SCL-90 et le score total de Hamilton

COR_SCL_Hamilton_J0 <- cor(subset(df_pool[df_pool$VISIT == "J0",], select = c("HDRSScore", sous_echelles_SCL)), use = "complete.obs", method = "spearman")
corrplot(COR_SCL_Hamilton_J0, method = "color", addCoef.col = "azure1", type="upper", order="alphabet",
         col = wes_palette(name = "Darjeeling1", type = "discrete"), tl.col = "black", tl.srt = 70, diag = FALSE)


COR_SCL_Hamilton_J56 <- cor(subset(df_pool[df_pool$VISIT == "J56",], select = c("HDRSScore", sous_echelles_SCL)), use = "complete.obs", method = "spearman")
corrplot(COR_SCL_Hamilton_J56, method = "color", addCoef.col = "azure1", type="upper", order="alphabet",
         col = wes_palette(name = "Darjeeling1", type = "discrete"), tl.col = "black", tl.srt = 70, diag = FALSE)

##======================================================

### Validité concourante à J0 = ACP
fpca(HDRSScore_J0~SCL_somatisation+SCL_obsession+SCL_vulnerabilite+SCL_depression+SCL_anxiete+
     SCL_hostilite+SCL_phobies+SCL_parano+SCL_psycho+SCL_divers,data=df_pool [df_pool $VISIT == "J0",])

### Validité concourante à J56 = ACP
fpca(HDRSScore_J56~SCL_somatisation+SCL_obsession+SCL_vulnerabilite+SCL_depression+SCL_anxiete+
       SCL_hostilite+SCL_phobies+SCL_parano+SCL_psycho+SCL_divers,data=df_pool [df_pool $VISIT == "J56",])

###================================QUESTION2=========================================================

###========== Analyse de la réponse au traitement : LOCF ==================================
## On remplace les valeurs manquantes par les dernières valeurs connues
# On remplace les données manquantes avec l'option locf
hdrsgroupe<-merge(HDRS_wide, df_grp, by= "NUMERO") 
sum(duplicated(hdrsgroupe))
names(hdrsgroupe)
hdrsgroupe$HDRSScore.J0 <- rowSums(hdrsgroupe[2:18])
hdrsgroupe$HDRSScore.J56<- rowSums(hdrsgroupe[121:137])
HDRS_wide_locf <- subset(hdrsgroupe, select=c("NUMERO","HDRSScore.J0", "HDRSScore.J56" , "GROUPE"))
HDRS_wide_locf2<-HDRS_wide_locf[order(HDRS_wide_locf$GROUPE, HDRS_wide_locf$NUMERO),] 
library(zoo)
HDRS_wide_locf2 <- t(na.locf(zoo(t(HDRS_wide_locf))))
HDRS_wide_locf2<- data.frame(HDRS_wide_locf2)
## création d'une variable différence entre les deux scores
## On créer une variable =  différences entre J0 et J56
HDRS_wide_locf2$difference <- HDRS_wide_locf2$HDRSScore.J0 - HDRS_wide_locf2$HDRSScore.J56

## On regarde les conditions de validité du test t de Student

## Distribution des différences entre J0 et J56
ggplot(HDRS_wide_locf2, aes(x = difference)) +
  geom_histogram(aes(y=..density..), bins = 20, fill = wes_palette(n = 1, name = "Moonrise1"),
                 colour = "black")+ 
  geom_density(alpha = 0.3, color="blue") +
  theme_minimal()+
  labs(title = "Distribution des différences entre J0 et J56",
       x = "Différence entre J0 et J56", y = "Fréquence") ## ça suit pas une loi normale

## Estimation par rapport à la loi normale par visualisation du qqnorm et de la qqline
qqnorm(HDRS_wide_locf2$difference, main = "Quantile-Quantile plot de la différence entre J0 et J56")
qqline(HDRS_wide_locf2$difference)

## On teste l'égalité des variances
by(HDRS_wide_locf2$difference,HDRS_wide_locf$GROUPE,sd,na.rm=TRUE)

## On compare la moyenne des différences
by(HDRS_wide_locf2$difference, HDRS_wide_locf2$GROUPE, mean)
table( HDRS_wide_locf2$GROUPE)  
t.test(HDRS_wide_locf2$difference~HDRS_wide_locf2$GROUPE, var.equal=TRUE)
boxplot(HDRS_wide_locf2$difference~HDRS_wide_locf2$GROUPE, ylab = "Difference du score de Hamilton entre J0 et J56", xlab="Groupe",main="Diminution du score HDRS entre J0 et J56 selon le
groupe",col=c("lightskyblue1","azure4"))


####====================MODELE MIXTE======================================

df_tot$GROUPE <- factor(df_tot$GROUPE)
ggplot(df_tot, aes(x = VISIT, y = HDRSScore, group = GROUPE, colour = GROUPE)) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, position = position_dodge(.20)) +
  stat_summary(fun.y = mean, geom = "line") +
  theme_minimal()+
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1), geom = "errorbar", width = .3, position = position_dodge(.20)) + 
  scale_x_discrete(limits = c("J0", "J4", "J7", " ", "J14", " ", "J21", " ", "J28", " ", " ", " ", "J42", " ", " ", " ", "J56"),
                   breaks = c("J0", "J4", "J7", "J14", "J21", "J28", "J42", "J56")) +
  scale_colour_manual(values = c("lightskyblue2","azure4")) + theme(legend.position = "top")
## On réalise un premier modèle en mettant fixe Groupe et Visite, et aléatoire les sujets
modele_mixte_simple <- lmer(HDRSScore ~ GROUPE + VISIT + (1|NUMERO), data = df_tot) 
summary(modele_mixte_simple)
par(mfrow = c(1, 2))
hist(resid(modele_mixte_simple), breaks = 20, col = "palegreen1", main = "Résidus du modèle mixte simple", xlab = "Résidus") 
qqnorm(resid(modele_mixte_simple))
qqline(resid(modele_mixte_simple))
## On réalise un deuxième modèle en ajoutant une interaction entre groupe et visite
modele_mixte_interaction <- lmer(HDRSScore ~ GROUPE * VISIT + (1|NUMEROs), data = df_tot) 
summary(modele_mixte_interaction)
par(mfrow = c(1, 2))
hist(resid(modele_mixte_interaction), breaks = 20, col = "palegreen1", main = "Résidus du modèle mixte avec interaction", xlab = "Résidus") 
qqnorm(resid(modele_mixte_interaction))
qqline(resid(modele_mixte_interaction))
## On teste l'ajout de l'interaction sur le modèle 
anova(modele_mixte_simple, modele_mixte_interaction, test = "Chisq")



###================================QUESTION3=========================================================
##====## Creation de la variable binaire guérison : diminution > 50% du HDRS à J0:
## On créer la variable reponse binaire que l'on met en NA
df_tot_denormalise_binaire <- df_tot_wide %>% mutate(reponse_binaire = NA)
## On créer la variable réponse seuil qui est la moitié de la valeur de J0 
df_tot_denormalise_binaire <- df_tot_denormalise_binaire %>% mutate(seuil_reponse = J0 / 2)
##  mettre la variable à 1 si on a une diminution de 50 % du score à J0 
for (k in 1:length(df_tot_denormalise_binaire$NUMERO)) {
  for (l in jours_visites) {
    if (!is.na(df_tot_denormalise_binaire[k, l]) & df_tot_denormalise_binaire[k, l] <= df_tot_denormalise_binaire[k, "seuil_reponse"] & is.na(df_tot_denormalise_binaire[k, "reponse_binaire"])) {
      df_tot_denormalise_binaire[k, "reponse_binaire"] <- l }
  } }
## changer la var reponse_binaire en int puisqu'elle en Chr
for (k in 1:length(df_tot_denormalise_binaire$NUMERO)) {
  df_tot_denormalise_binaire[k, "reponse_binaire"] <- str_extract_all(df_tot_denormalise_binaire[k, "reponse_binaire"], "\\d+") }
df_tot_denormalise_binaire$reponse_binaire <- as.integer(df_tot_denormalise_binaire$reponse_binaire) 
summary(df_tot_denormalise_binaire)
## On créer un objet survfit
survie_bin <- survfit(Surv(time = reponse_binaire) ~ GROUPE, data = df_tot_denormalise_binaire) 
summary(survie_bin)
summary(survie_bin)$table
ggsurvplot(survie_bin, pval = TRUE, conf.int = TRUE, risk.table = TRUE, risk.table.col = "strata", linetype = "strata",
           surv.median.line = "v", palette = c("lightskyblue1", "azure4"),main="Courbe de guérison dans le temps",
           xlab = "Time in day", risk.table.y.text = FALSE, legend.labs = c("Groupe 0", "Groupe 1"))
survie_bin_difference <- survdiff(Surv(time = reponse_binaire) ~ GROUPE, data = df_tot_denormalise_binaire) 
survie_bin_difference


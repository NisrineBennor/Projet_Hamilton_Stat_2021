# Projet_Hamilton_Stat_2021

Dans une étude d’épidémiologie clinique, 146 patients déprimés ont été évalués à J0 (début du traitement), J4, J7, J14, J21, J28, J42 et J56, à l’aide de deux outils : un outil d’hétéro- évaluation (échelle de dépression de Hamilton ou HDRS) ; et un outil d’auto-évaluation (SCL 90 pour Symptom distress Check-List de 90 items).
Le jeu de données étudié est composé de 3 fichiers csv :
* Un fichier séparant les patients en deux groupes hommes et femmes (groupe 1 et groupe0), c’est l’outils groupe.
* Un fichier de données sur les résultats de l’hétéro-évaluation (échelle de Hamilton HDRS) pour chaque visite : c’est l’outils hrds de 1967 modifié à 17 questions (HDRS- 17), cette échelle est issue d’un questionnaire à choix multiples, mesurant la sévérité des symptômes associées à la dépression, le clinicien opte pour l’une des réponses des 3 à 5 réponses proposés en examinant son malade, ces réponses sont classées par ordre croissant de sévérité.
* Et un fichier de données sur les résultats de l’auto-évaluation (SCL 90 R) pour chaque visite : « outils autoeval ». Comme son nom l’indique, cette échelle comporte 90 descriptions courtes des plaintes et symptômes du patient.

L’objectif de ce travail est de déterminer si les patients du groupe 1 répondent mieux au traitement que les patients du groupe 0, d’après l’échelle de dépression de Hamilton qui est évaluée dans le temps sur plusieurs consultations, ces consultations ont été programmées à l’avance à J0.

Après une revalidation rapide de l’échelle de dépression de Hamilton, des analyses sont
effectuées afin de déterminer l’impact du groupe dans la réponse au traitement.
La revalidation de l’échelle de dépression de Hamilton (HDRS) a été réalisée sur les scores des temps J0 et J56.
Après une analyse descriptive du jeu de données, la structure dimensionnelle de l’échelle a été étudiée par des diagrammes de valeurs propres et des analyses factorielles. La consistance interne a été évaluée par le calcul des coefficients de Cronbach.
Enfin, la validité concourante de l’échelle a été appréciée à l’aide de corrélations avec un autre score (SCL 90 R : score d’autoévaluation) et d’une analyse en composante principale focalisée.
L’impact du groupe dans la réponse au traitement à partir du score de l’échelle de Hamilton a été évalué selon trois approches successives :
o Par une approche LOCF (Last Observation Carried Forward),
o Puis par un modèle mixte,
o Et enfin, en considérant le critère binaire censuré « réponse au traitement »
défini par une chute de 50% à l’échelle de Hamilton par rapport à J0 (début de traitement).
Toutes les analyses statistiques ont été réalisées avec le logiciel R. studio (version1.0.16).



Pour mieux comprendre les techniques et voir les résultats obtenus, veuillez lire le fichier pdf
  

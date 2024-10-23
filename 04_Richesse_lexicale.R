#######################################
#### Évaluer la richesse lexicale ####
#######################################

# L'évaluation de la variété ou « richesse » lexicale est souvent mobilisée en humanités numériques pour caractériser et comparer les parties d'un même texte ou encore des collections de textes
# de différents auteurs, périodes ou sous-genres. C'est également une donnée qui est souvent prise en compte dans les disputes entourant l'attribution de textes anonymes à des écrivains connus.
# L'une des applications célèbres de la mesure de la variété lexicale a été faite sur l'oeuvre entière d'Agatha Christie, en appui à la thèse d'une atteinte de la maladie d'Alzheimer (Lancashire, 2009 et 2011).
# Une diminution progressive et notable de la variété lexicale est en effet observée dans les romans de la romancière britannique au fil des décennies.
# Nous allons explorer dans cet atelier une mesure de la variété lexicale, la taille relative du vocabulaire (TTR, pour "type-token ratio").
# Nous allons également mesurer la complexité et la profondeur des phrases que composent des textes.
# Selon la question de recherche et le découpage du corpus, ces mesures peuvent bien entendu révéler ou faire apparaître des phénomènes très différents.

# Dans un premier temps, nous allons utiliser le cas très commode du roman Maria Chapdelaine, déjà découpé en chapitres.
# Nous allons ensuite observer les mêmes mesures pour les nouvelles littéraires de la revue XYZ.

#### Chargement des modules supplémentaires ----
inst_ext_f <- function(extension) {
  if(!extension %in% rownames(installed.packages())) {
    install.packages(extension, dependencies = TRUE)
  }
  require(extension, character.only = TRUE)
}

extensions <- c("stringr",
                "data.table",
                "ggplot2",
                "tibble",
                "dplyr",
                "udpipe",
                "spacyr",
                "tokenizers",
                "ggrepel",
                "stylo")                             

sapply(extensions, inst_ext_f)

rm(list = ls())

### Importation d'une structure de données déjà traitée ----

# La structure de données à importer est le tableau dans lequel nous avons emmagasiné les mots et lemmes du roman Maria Chapdelaine.
maria_df <- readRDS("donnees/maria_traite.RDS")

View(maria_df)

# Nous allons maintenant calculer le ratio "type-token" en utilisant les valeurs de la colonne "token".
# On se souvient que cette colonne contient les mots en bas de casse de chacun des chapitres, moins les mots fonctionnels.
# La première étape consiste à créer une table des mots de ces chapitres et à diviser le nombre d'occurrences de chaque forme par le nombre total de mots du chapitre.
# Ainsi, la valeur d'une forme donnée (ex.: "maria") ne dépendra pas de la longueur du chapitre, c'est-à-dire du nombre total de mots d'un chapitre.

# 1) Création d'une fonction pour prétraiter le texte et en mesurer la longueur.

pretraitement_f <- function(x) {
  
  # Prétraitement
  texte_net <- tolower(x)                                                         # Remplacement des majuscules par des minuscules
  texte_net <- gsub("[\\W[:punct:]]", " ", texte_net)                             # Remplacement de tout ce qui n'est pas une lettre d'un mot par une espace simple.
  texte_net <- gsub("\\s+", " ", texte_net)                                       # On remplace ici les espaces multipes (\\s+) par une espace simple.
  
  # Segmentation du texte
  texte_net_v <- strsplit(texte_net, " ") |> unlist()                             # Séparation du texte à partir d'une espace
  texte_net_v <- texte_net_v[!texte_net_v %in% tm::stopwords(kind = "fr")]        # Élimination des mots fonctionnels
  
  return(texte_net_v)                                                             # On explicite ici que la fonction doit retourner le résultat des étapes qui précèdent, soit un vecteur dont chaque élément est un mot.
  
}

# 2) Création d'une deuxième fonction qui utilise la première et qui, ensuite, calcule la variété du vocabulaire (TTR).
ttr_f <- function(x){
  
  texte_net_v <- pretraitement_f(x)                                               # Application au texte fourni en entrée de la fonction précédente
  
  longueur_v <- length(texte_net_v)                                               # Calcul du nombre total de mots dans le vecteur
  
  # Calcul du ttr
  mots_uniques_v <- length(unique(texte_net_v))                                   # Calcul du nombre de mots uniques dans le même vecteur
  ttr_v <- mots_uniques_v/longueur_v                                              # Division de la première valeur par la seconde
  return(ttr_v)                                                                   # Renvoi de cette valeur, comprise entre 0 et 1
}

# Test de la fonction sur une phrase simple.
phrase <- "Il était une foi un petit chaperon rouge. Il était notable que ce chaperon rouge aimait, par-dessus tout, être auprès de sa mère-grand."

pretraitement_f(phrase)
ttr_f(phrase)
# Le résultat indique que 85.7% des mots composant cette phrase sont uniques.

# Autre phrase qui ne contient que des mots uniques:
phrase2 <- "C'était un grand vaisseau taillé dans l'or massif."
ttr_f(phrase2)
# Résultat de 1, indiquant que 100% des mots sont uniques.

# Dernière phrase ne contenant que des mots répétés:
phrase3 <- "da da da da da da da da da da."
ttr_f(phrase3)
# Les 10 mots de cette "phrase" sont identiques, ce qui donne un résultat TTR de 10%.

# 3) Ci-dessous, nous ajoutons à notre tableau principal une colonne où nous indiquons, pour chacun des chapitres, le nombre de mots du texte prétraité.
maria_df$longueur <- lapply(maria_df$texte, function(x) length(pretraitement_f(x)))

# 4) Et maintenant, nous ajoutons une colonne où, pour chaque chapitre, nous indiquons le TTR.

maria_df$ttr <- lapply(maria_df$texte, ttr_f)

View(maria_df)
# Que remarquez-vous?



# Y a-t-il un lien direct entre la longueur d'un chapitre et son TTR? Projetons les deux séries de valeurs dans un diagramme à points
plot(maria_df$longueur, maria_df$ttr)

# Que remarquez-vous? 

# Nous avons fait la connaissance du test de Pearson dans le dernier cours. On peut l'appliquer à notre jeu de données
# pour voir si, comme le laisse croire le diagramme, il y a bien une forte corrélation négative entre la longueur d'un document et son TTR.
cor.test(unlist(maria_df$longueur), unlist(maria_df$ttr))

# Le test de corrélation confirme ce lien très fort et inversé entre les variables.
# Comment neutraliser cet effet de la longueur des chapitres (ou des textes à comparer en général)?

# Il existe plusieurs solutions pour faire cela.
# L'une d'elle consiste à segmenter les chapitres en un nombre de mots fixes, par exemple 100 mots, de mener ensuite une analyse TTR sur chacun des segments, puis d'en faire la moyenne.
# La fonction ci-dessous permettra de faire cette opération.

# Pour être clair, si un chapitre comporte 350 mots, la fonction calculera le TTR sur quatre segments:
# [1:100][101:200][201:300][301:350]. La moyenne des quatre résultats sera retourné.

moyenne_segments_ttr_f <- function(x, nbre_mots = 100) {
  texte_net_v <- pretraitement_f(x)                                              # Prétraitement du texte fourni en entrée
  n <- length(texte_net_v)                                                       # Calcul de la longueur du vecteur renvoyé par la fonction, donc du nombre de mots total
  
  # Si le texte est trop court pour être segmenté, retourner un TTR simple.
  if (n < nbre_mots) {
    return(ttr_f(x))
  }
  
  # Calcul de la moyenne TTR en segmentant le texte
  segments <- split(texte_net_v, ceiling(seq_along(texte_net_v)/nbre_mots))      # Segmentation du vecteur en groupes de 100 mots
  ttrs <- sapply(segments, function(segment) {                                   # Calcul du ttr pour chacun des segments
    length(unique(segment)) / length(segment)
  })

  # Retourner la moyenne
  return(mean(ttrs))
}

# On applique cette fonction à nos chapitres et on inscrit le résultat dans une nouvelle colonne.
maria_df$msttr <- sapply(maria_df$texte, moyenne_segments_ttr_f)

View(maria_df)





#### Complexité syntaxique ----
# Avant d'appliquer la TTR à un corpus plus conséquent, voyons comment on pourrait mesurer la complexité des phrases d'un texte.
# La mesure la plus simple et la moins coûteuse à produire du point de vue computationnel est le nombre moyen de mots que contiennent les phrases d'un document.
# On peut mesurer cela avec une simple expression régulière.

# Fonction pour mesurer la longueur des phrases d'une chaîne de caractères
longueur_phrase_f <- function(x) {
  # Segmentation du texte en phrases
  phrases_v <- strsplit(x, split = "(?<=\\.)\\s", perl = TRUE) |> unlist()       # Séparation du texte en phrases en utilisant le point suivi d'une espace.
  phrases_v <- phrases_v[!phrases_v == ""]                                       # Élimination des éléments vides du vecteur de phrases.
  
  # Calcul de la longueur de chaque phrase (nombre de mots)
  longueur_v <- sapply(phrases_v, function(phrase) {
    mots_v <- strsplit(phrase, split = "\\W+") |> unlist()                       # On segmente chaque phrase en mots, en utilisant les non-lettres comme séparateurs.
    mots_v <- mots_v[mots_v != ""]                                               # On élimine les chaînes vides résultant de la segmentation.
    return(length(mots_v))                                                       # Retourne la longueur de chaque phrase (nombre de mots).
  })
  
  # Calcul de la longueur moyenne des phrases, arrondie à 2 décimales
  longueur_moyenne <- round(mean(longueur_v), 2)
  
  return(longueur_moyenne)                                                       # Retourne la longueur moyenne des phrases.
}

longueur_phrase_f <- function(x) {
  # Segmentation du texte en phrases
  phrases_v <- strsplit(x, split = "(?<=\\.)\\s", perl = TRUE) |> unlist()       # Séparation du texte en phrases en utilisant le point suivi d'une espace.
  phrases_v <- phrases_v[!phrases_v == ""]                                       # Élimination des éléments vides du vecteur de phrases.
  
  # Création d'un vecteur pour stocker les longueurs des phrases
  longueur_moyenne_l <- vector(mode = "list", length = length(phrases_v))         # Ce vecteur est inutile ici, on peut calculer directement la longueur de chaque phrase.
  
  # Calcul de la longueur de chaque phrase (nombre de mots)
  longueur_v <- sapply(phrases_v, function(phrase) {
    mots_v <- strsplit(phrase, split = "\\W+") |> unlist()                       # On segmente chaque phrase en mots, en utilisant les non-lettres comme séparateurs.
    mots_v <- mots_v[mots_v != ""]                                               # On élimine les chaînes vides résultant de la segmentation.
    return(length(mots_v))                                                       # Retourne la longueur de chaque phrase (nombre de mots).
  })
  
  # Calcul de la longueur moyenne des phrases, arrondie à 2 décimales
  longueur_moyenne <- round(mean(longueur_v), 2)
  
  return(longueur_moyenne)                                                       # Retourne la longueur moyenne des phrases.
}

# On peut ajouter au tableau principal cette mesure pour chacun des chapitres
maria_df$phrase_lng_moy <- sapply(maria_df$texte, longueur_phrase_f)

View(maria_df)


# La complexité syntaxique peut également être mesurée par le nombre de propositions que contient une phrase.
# Pour obtenir cette mesure, il faut d'abord procéder à l'annotation morpho-syntaxique d'un document.
# On a vu déjà la manière de faire cela avec l'extension `udpipe`. On utilisera à nouveau cette extension ici et on se servira des étiquettes de relations des propositions `dep_rel`. 

# Chargement du modèle udpipe
modele_fr <- udpipe_load_model("modele_langage/french-gsd-ud-2.5-191206.udpipe")

# Fonction pour mesurer le nombre moyen de propositions dépendantes par phrase:
mesurer_densite_syntaxique_f <- function(texte, modele_udpipe = modele_fr) {
  
  # Annoter le texte avec udpipe.
  annotations <- udpipe_annotate(object = modele_udpipe, x = texte)
  annotations <- as.data.frame(annotations)
  
  # Compter les propositions dans la phrase.
  nbr_prop_sub <- sum(annotations$dep_rel %in% c("csubj", "expl:subj", "advcl", "acl", "xcomp", "relcl", "acl:relcl", "ccomp"))
  nbre_phrases <- max(annotations$sentence_id)
  nbre_prop_rel <- nbr_prop_sub/nbre_phrases

  return(nbre_prop_rel)
}

# On peut appliquer la fonction ci-dessus à une phrase test:
mesurer_densite_syntaxique_f(texte = "L'évaluation de la variété ou de la richesse lexicale est souvent mobilisée en humanités numériques pour caractériser et comparer les parties d'un même texte, qui peut être un texte littéraire, mais non forcément, ou encore des collections de textes de différents auteurs, périodes ou sous-genres.")

# Le résultat est de "3", c'est-à-dire que trois propositions sont dépendantes de la principale:
# 1. "pour caractériser": advcl (proposition adverbiale)
# 2. "qui peut": acl:relcl (proposition relative)
# 3. "textes": xcomp (proposition complément d'objet non fini)


# Pour connaître la signification de chaque étiquette retenue dans la fonction ci-dessus ("csubj", "expl:subj", "advcl", "acl", "xcomp", "relcl", "acl:relcl", "ccomp"), vous pouvez interroger votre LLM préféré en précisant qu'il s'agit de "Universal Dependencies (UD)".

# On peut maintenant mesurer la profondeur relative des phrases de chacun des chapitres du roman
maria_df$phrase_densite_moy <- sapply(maria_df$texte, mesurer_densite_syntaxique_f, modele_udpipe = modele_fr)

# Pour l'ensemble du roman, la mesure est la suivante:
mean(maria_df$phrase_densite_moy)



# Enfin, on pourrait mesurer la complexité d'une phrase en fonction de sa profondeur.
# La profondeur syntaxique mesure la complexité d’une phrase en évaluant combien de niveaux de relations grammaticales séparent chaque mot (ou “token”) de la racine syntaxique de la phrase. 
# Cela revient à quantifier le nombre de dépendances grammaticales qu'il faut traverser pour atteindre la racine (généralement le verbe principal ou un élément central).
# Autrement dit Profondeur syntaxique d’un mot correspond au nombre de relations ou de nœuds qu’il faut traverser pour aller de ce mot à la racine de la phrase. 
# Si un mot est directement relié à la racine, sa profondeur est faible (profondeur 1). 
# Si un mot est éloigné de la racine, sa profondeur est plus grande.

# Fonction pour mesurer la profondeur syntaxique d'un texte avec data.table
# Note: les deux fonctions suivantes ont été créées avec l'assistance de OpenAI, ChatGPT-4o, version d'octobre 2024.

mesurer_profondeur_syntaxique_dt <- function(texte, modele_udpipe = modele_fr) {
  
  # Annoter le texte avec udpipe
  annotations <- udpipe_annotate(object = modele_udpipe, x = texte)
  annotations <- as.data.table(as.data.frame(annotations)) # Conversion en data.table
  
  # Initialisation de la profondeur syntaxique
  annotations[, profondeur := 0] # Crée une colonne 'profondeur'
  
  # Calculer la profondeur pour chaque token dans chaque phrase
  annotations[, profondeur := sapply(token_id, function(tok_id) {
    calculer_profondeur_individuelle_dt(tok_id, head_token_id, token_id)
  }), by = sentence_id]
  
  # Moyenne des profondeurs par phrase
  resultats_profondeur <- annotations[, .(profondeur_moyenne = mean(profondeur)), by = sentence_id]
  
  return(resultats_profondeur$profondeur_moyenne)
}

# Fonction auxiliaire pour calculer la profondeur d'un token individuel
calculer_profondeur_individuelle_dt <- function(token_id_actuel, head_token_id, token_id) {
  profondeur <- 0
  current_token <- token_id_actuel
  
  # Remonter jusqu'à la racine (ROOT) ou jusqu'à ce qu'il n'y ait plus de dépendance
  while (current_token != 0 && !is.na(current_token)) {
    profondeur <- profondeur + 1
    
    # Obtenir le parent du token actuel
    parent_token <- head_token_id[token_id == current_token]
    
    # S'assurer que parent_token est un seul élément
    if (length(parent_token) > 1) {
      parent_token <- parent_token[1]
    }
    
    # Vérifier si le parent est valide
    if (is.na(parent_token) || parent_token == 0) {
      break
    }
    
    # Mettre à jour le token courant
    current_token <- parent_token
  }
  
  return(profondeur - 1) # On soustrait 1 car la racine est comptée comme une profondeur 0
}


# Exemple de texte
texte_exemple <- c("Victor Hugo est né en 1802. L'évaluation de la richesse lexicale est importante en humanités numériques.")

# Calcul de la profondeur syntaxique
mesurer_profondeur_syntaxique_dt(texte_exemple, modele_fr)


# On calcule maintenant la profondeur moyenne des phrases et on ajoute cette donnée au tableau
for(i in 1:nrow(maria_df)){
  mesure_temp <- mesurer_profondeur_syntaxique_dt(maria_df$texte[i], modele_fr)
  maria_df$phrase_profondeur_moy[i] <- mean(mesure_temp, 2)
}

# On peut observer le jeu de données
View(maria_df)

# Sauvegarde du jeu de données sous forme de fichier .csv
fwrite(maria_df, "donnees/maria_enrichie.csv")

#### Application sur une collection de documents: le corpus de nouvelles littéraires XYZ ----

# Les documents que nous allons exploiter maintenant proviennent de la revue de nouvelles littéraires _XYZ_.
# La table que nous allons importer est le fruit d'un travail de plusieurs dizaines d'heures réalisé par Julien Vallières Gingras et Yuchen Shi. 
# Les textes ont été télécharchés en format pdf depuis Érudit; ils ont été océrisés et nettoyés, et leurs métadonnées ont été intégrées à un tableau de données.

# La table importée contient 564 nouvelles littéraires écrites par 318 écrivains entre 2012 et 2022.

# Importation de la table dans l'environnement et modification mineure des colonnes
xyz_dt <- data.table::fread("donnees/combined_table_net_xyz.csv")
xyz_dt[, `:=`(
  periode = str_extract(annee, "^[[:alpha:]]+"),
  annee = str_extract(annee, "\\d+"),
  longueur = sapply(texte, function(x) length(pretraitement_f(x)))
  )
  ]

View(xyz_dt)

# Le défi que nous allons nous donner maintenant est de trouver :

# 1) le texte qui possède le ratio TTR le plus élevé;
# 2) l'auteur écrivant les phrases les plus complexes.

# Les 564 textes étant déjà intégrés à un un tableau de données (dataframe), le calcul du TTR sera particulièrement aisé! 
# Il s'agit d'appliquer à chaque document de la collection la fonction que nous avons créée ci-dessus, d'extraire le TTR le plus élevé, puis de lire le texte!

xyz_dt$ttr <- sapply(xyz_dt$texte, ttr_f)

# Vous pouvez visualiser le résultat dans le tableau avec View(xyz_dt).

# La fonction `summary()` renverra des statistiques sur les valeurs produites par notre fonction de calcul
summary(xyz_dt$ttr)


# À quoi ressemble le texte ayant le ttr le plus élevé?
xyz_dt[ttr == max(ttr), .(auteur, annee, titre, texte)]

# On remarque que le texte dont le TTR est le plus élevé est également très court.

# Y a-t-il une corrélation entre la longueur des textes et les valeurs TTR? À nouveau, nous pouvons projeter ces valeurs dans un diagramme à points.
plot(xyz_dt$longueur, xyz_dt$ttr)

# Trouvons maintenant l'auteur avec le TTR le plus élevé et celui avec le TTR le moins élevé. Ajoutons-y l'auteur du texte le plus long!
auteur_max_min_ttr <- xyz_dt[ttr == max(ttr) | ttr == min(ttr) | longueur == max(longueur), .(auteur, longueur, ttr)]
 
# La ligne de code ci-dessus va permettre de repérer ces trois auteurs dans le diagramme ci-dessous, plus raffiné.

ggplot(xyz_dt, aes(x = longueur, y = ttr)) +
  geom_point() +
  geom_text_repel(data = auteur_max_min_ttr, 
                  aes(label = auteur), 
                  size = 4, 
                  nudge_x = 0.05, # Décale légèrement l'étiquette horizontalement
                  nudge_y = 0.05, # Décale légèrement l'étiquette verticalement
                  arrow = arrow(length = unit(0.02, "npc"), type = "closed")) + # Ajoute une flèche
  xlab("Longueur du texte") +
  ylab("TTR") +
  ggtitle("Corrélation entre la longueur du texte et la variété lexicale (TTR)") + 
  stat_smooth(method = "loess")+
  theme_light()


# Voyons maintenant quel auteur détient les phrases les plus complexes.
# Pour cela, nous avons deux mesures: la moyenne des phrases comprenant le plus de propositions (densité) et la moyenne des phrases ayant la plus grande profondeur.


# Commençons par mesurer la densité syntaxique moyenne des phrases de chaque texte (l'opération peut prendre plusieurs minutes avec un ordinateur récent) :
xyz_dt[, phrase_densite := lapply(texte, mesurer_densite_syntaxique_f)]

# Mesurons maintenant la profondeur syntaxique moyenne des phrases de chaque texte (l'opération peut prendre plusieurs minutes avec un ordinateur récent) :
xyz_dt[, phrase_profondeur:=lapply(texte, mesurer_profondeur_syntaxique_dt)]

xyz_dt[, phrase_profondeur:=lapply(phrase_profondeur, mean)]

View(xyz_dt)

# Il ne reste plus qu'à effectuer la moyenne de chaque mesure selon les auteurs et de créer un tableau avec le résultat!
# D'abord, combinons les résultats par auteur,
xyz_phrase_densite_dt <- xyz_dt[, .(moyenne_densite = round(mean(as.numeric(phrase_densite)), 2)), by = auteur][order(-moyenne_densite)]
xyz_phrase_profondeur_dt <- xyz_dt[, .(moyenne_profondeur = round(mean(as.numeric(phrase_profondeur)), 2)), by = auteur][order(-moyenne_profondeur)]

xyz_phrase_complexite_dt <- merge.data.table(xyz_phrase_densite_dt, xyz_phrase_profondeur_dt, by = "auteur")

# Le résultat est une table avec 
# 1) le nom de chaque auteur; 
# 2) la densité moyenne pour tous les textes de chaque auteur; 
# 3) la profondeur moyenne pour tous les textes de chaque auteur.
View(xyz_phrase_complexite_dt)

# Y a-t-il une corrélation entre les deux mesures de la complexité des phrases?

auteurs_top5 <- xyz_phrase_complexite_dt[order(-moyenne_profondeur)][1:5, .(auteur, moyenne_profondeur, moyenne_densite)]
ggplot(xyz_phrase_complexite_dt, aes(x = moyenne_densite, y = moyenne_profondeur)) +
  geom_point() +
  geom_text_repel(data = auteurs_top5,
                  aes(label = auteur),
                  size = 4,
                  nudge_x = 0.05, # Décale légèrement l'étiquette horizontalement
                  nudge_y = 0.05, # Décale légèrement l'étiquette verticalement
                  arrow = arrow(length = unit(0.02, "npc"), type = "closed")) + # Ajoute une flèche
  xlab("Densité des phrases") +
  ylab("Profondeur des phrases") +
  ggtitle("Corrélation entre la densité et la profondeur des phrases") + 
  stat_smooth(method = "loess")+
  theme_light()


# On peut maintenant retourner aux textes et observer les raisons qui font de ces auteurs des cas aberrants.
# Dans le code ci-desous, écrivez le nom d'un auteur, exécutez le code et observez le ou les textes qui sont produits dans la console.


xyz_dt[auteur == "Danus", .(auteur, texte)]

saveRDS(xyz_dt, "donnees/xyz_enrichie_rich_lex.RDS")
# Quelle pourrait être la prochaine étape? Tous les cas aberrants sont des auteurs masculins. On pourrait vérifier la corrélation entre le genre de l'auteur et les variables.
# On pourrait également ajouter les âges et vérifier s'il y a une relation statistiquement significative entre les variables mesurées et les groupes d'âge.



#### Bibliographie ----
# Benoit, Kenneth et Akitaka Matsuo. (2023). Spacyr: Wrapper to the 'spaCy' 'NLP' Library. https://spacyr.quanteda.io
# Lancashire, Ian & Hirst, Graeme. (2009). Vocabulary Changes in Agatha Christie's Mysteries as an Indication of Dementia: A Case Study. 19Th Annual Rotman Research Institute Conference, Cognitive Aging: Research and Practice. 
# Xuan Le, Ian Lancashire, Graeme Hirst et Regina Jokel. (2011). Longitudinal detection of dementia through lexical and syntactic changes in writing: a case study of three British novelists. Literary and Linguistic Computing, Volume 26, Issue 4, December 2011, p. 435–461, https://doi.org/10.1093/llc/fqr013
# Jockers, Matthew, L. et Rosamond Thalken. 2020. Text Analysis With R For Students in Literature, Springer.

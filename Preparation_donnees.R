
#### SEGMENTATION DU TEXTE EN PARAGRAPHES ----
library(tidyverse)

# 1. Chargement du fichier texte
text_raw <- read_file("textes_bruts/Passeuse3_4.txt")

# 2. Nettoyage initial (optionnel)
text_clean <- text_raw |> 
  str_replace_all("\\s+", " ") |>  # Normaliser les espaces
  str_trim()

# 3. Segmentation en phrases
# Découpe sur les points suivis d'une majuscule
sentences <- text_clean |> 
  str_split("(?<=\\.)\\s+(?=[A-ZÀÂÇÉÈÊËÏÎÔÙÛÜŸÆŒ])") |> 
  unlist()

# 4. Création de paragraphes artificiels (par exemple, 5 phrases par paragraphe)
sentences_per_paragraph <- 5

paragraphs_df <- tibble(sentence = sentences) |> 
  mutate(
    sentence_id = row_number(),
    paragraph_id = ceiling(sentence_id / sentences_per_paragraph)
  ) |> 
  group_by(paragraph_id) |> 
  summarise(
    paragraph_text = paste(sentence, collapse = " "),
    .groups = "drop"
  )

# 5. Identification des parties (si votre texte contient des marqueurs)
# S'il n'en contient pas, vous pouvez en intégrer dans le texte brut avant de
# le charger dans l'environnement, puis utiliser les noms de ces parties 
# dans le code ci-dessous pour segmenter les paragraphes par parties.
# S'il n'y a pas de partie dans le texte, inutile d'exécuter le code ci-dessous.
paragraphs_with_parts <- paragraphs_df |> 
  mutate(
    partie = case_when(
      str_detect(paragraph_text, "###Tome 3###") ~ "Partie_1",
      str_detect(paragraph_text, "###Tome 4###") ~ "Partie_2",
      #str_detect(paragraph_text, "TROISIÈME PARTIE") ~ "Partie_3",
      TRUE ~ NA_character_
    )
  ) |> 
  fill(partie, .direction = "down") |>  # Propager la partie vers le bas
  filter(!is.na(partie)) |>  # Retirer les lignes avant la première partie
  group_by(partie) |> 
  mutate(paragraph_id_in_part = row_number()) |> 
  ungroup() |> 
  mutate(doc_id = paste(partie, paragraph_id_in_part, sep = "_")) |> 
  select(doc_id, partie, paragraph_id_in_part, paragraph_text)



#### ANNOTATION MORPHOSYNTAXIQUE ---- 
# library(udpipe)
# 
# # 1. Téléchargement du modèle de langue française (si non présent)
# ud_model_path <- "french-gsd-ud-2.5-191206.udpipe"
# if (!file.exists(ud_model_path)) {
#   udpipe_download_model(language = "french-gsd", model_dir = getwd())
# }
# 
# # 2. Chargement du modèle
# ud_model <- udpipe_load_model(file = ud_model_path)
# 
# # 3. Annotation du texte de nos paragraphes
# # On utilise les colonnes 'doc_id' et 'paragraph_text' de notre table 'paragraphs_df'
# annotations <- udpipe_annotate(ud_model,
#                                x = paragraphs_with_parts$paragraph_text,
#                                doc_id = paragraphs_with_parts$doc_id)
# 
# # 4. Conversion du résultat en un format 'tibble' pour une manipulation aisée
# annotations_df <- as_tibble(annotations)
# 
# # 5. Rajout de l'information 'partie' dans notre table d'annotations
# # On extrait la partie depuis le doc_id (ex: "Partie_1_1" -> "Partie_1")
# annotations_df <- annotations_df |>
#   mutate(partie = str_extract(doc_id, "Partie_[1-2]"))
# 
# saveRDS(annotations_df, "donnees/annotations_df.RDS")

annotations_df <- readRDS("donnees/annotations_df.RDS")

#### FILTRAGE DES TABLES ET CRÉATION DES NOEUDS ----
library(data.table)
library(lsa)
# Définition des catégories grammaticales qui nous intéressent
pos_of_interest <- c("NOUN", "PROPN", "ADJ", "VERB")

# Filtrage des annotations pour ne garder que les lemmes pertinents
lemmes_interessants <- annotations_df |>
  filter(upos %in% pos_of_interest)

nodes_table_auto <- lemmes_interessants |>
  group_by(lemma, partie) |>
  summarise(
    Total_Frequency = n(),
    POS_Category = names(which.max(table(upos)))
  ) |>
  ungroup()

# Insérer ici les lemmes non pertinents (à éliminer)
nodes_table_auto <- nodes_table_auto |> filter(!lemma %in% c("voici",
                                                             "voilà",
                                                             "ser",
                                                             "riir",
                                                             "ir",
                                                             "ter",
                                                             "sere",
                                                             "-A",
                                                             "il",
                                                             "ire",
                                                             "lure",
                                                             "donn",
                                                             "son",
                                                             "adieu"))


setDT(nodes_table_auto)

# Filtrage des noeuds (lemmes) par seuil de fréquence
nodes_table <- nodes_table_auto |>
  filter(Total_Frequency > 15) |>
  rename(Id = lemma) |>
  mutate(Label = Id) |>
  select(Id, Label, POS_Category, Total_Frequency, partie) |>
  arrange(desc(Total_Frequency)) |> 
  # 6. Supprimer les mots trop communs (ex: "avoir", "dire")
  filter(!Label %in% lsa::stopwords_fr)

# Affichage des premières lignes de notre table de nœuds
cat("Aperçu de la table des nœuds (sommets) :\n")
head(nodes_table) |> print()

cat(paste("\nNombre total de nœuds après filtrage :", nrow(nodes_table), "\n"))


#### CRÉATION DE LA TABLE DES LIENS (COOCCURRENCES) ----
# On ne garde que les annotations correspondant à nos nœuds sélectionnés
annotations_filtered <- lemmes_interessants |>
  filter(lemma %in% nodes_table$Id)

# Pour créer les paires, on joint la table à elle-même
# On prend tous les mots d'une même phrase et on les associe
# La condition `lemma.x < lemma.y` évite les doublons (a,b)/(b,a) et les boucles (a,a)
paired_lemmes <- annotations_filtered |>
  select(doc_id, sentence_id, lemma, partie) |>
  inner_join(
    annotations_filtered |>
      select(doc_id, sentence_id, lemma),
    by = c("doc_id", "sentence_id"),
    relationship = "many-to-many"
  ) |>
  filter(lemma.x < lemma.y)

# Création de la table des liens (arêtes)
edges_table <- paired_lemmes |>
  # 1. On groupe par paire de lemmes et par partie du roman
  group_by(Source = lemma.x, Target = lemma.y, Partie = partie) |>
  # 2. On compte le nombre de cooccurrences pour obtenir le poids
  summarise(Weight = n(), .groups = "drop") |>
  # 3. On s'assure d'avoir des colonnes bien nommées et dans le bon ordre
  select(Source, Target, Weight, Partie) |>
  arrange(desc(Weight)) # On ordonne par poids décroissant

# Affichage des premières lignes de notre table de liens
cat("Aperçu de la table des liens (arêtes) :\n")
head(edges_table) |> print()

cat(paste("\nNombre total de liens uniques (cooccurrence-partie) :", nrow(edges_table), "\n"))



#### Sauvegarde des données pour Gephi ou autre ----
library(readr)
# Création d'un dossier pour stocker les résultats s'il n'existe pas
if (!dir.exists("Resultats")) {
  dir.create("Resultats")
}

# Sauvegarde des fichiers CSV
write_csv(nodes_table, "Resultats/Passeuse_3_4_nodes.csv")
write_csv(edges_table, "Resultats/Passeuse_3_4_edges.csv")

cat("Les fichiers 'illusions-perdues-nodes.csv' et 'illusions-perdues-edges.csv' ont été créés dans le dossier 'Gephi_data/'.\n")

write_csv(nodes_table[partie == "Partie_1"], "Resultats/Passeuse_nodes_3.csv")
write_csv(nodes_table[partie == "Partie_2"], "Resultats/Passeuse_nodes_4.csv")

write_csv(edges_table[edges_table$Partie == "Partie_1",], "Resultats/Passeuse_edges_3.csv")
write_csv(edges_table[edges_table$Partie == "Partie_2",], "Resultats/Passeuse_edges_4.csv")




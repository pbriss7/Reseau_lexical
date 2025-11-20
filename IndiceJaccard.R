#################################################
#### Test de similarité (mesure de Jaccard) #####
#################################################

# Pourquoi un test de Jaccard?
#   Le coefficient de Jaccard est utilisé ici pour mesurer la similarité de voisinage du personnage "Alric" entre les deux tomes. Plus précisément, il calcule:
#   Jaccard = |voisins_t1 ∩ voisins_t2| / |voisins_t1 ∪ voisins_t2|
#     Pertinence pour l'analyse lexicale:
#     
#     Il mesure la stabilité du contexte lexical d'un terme entre deux parties du roman
# Un Jaccard élevé (proche de 1) = le terme co-occur avec les mêmes mots dans les deux parties
# Un Jaccard faible (proche de 0) = le terme change de contexte lexical

library(igraph)
library(dplyr)

# ---- Paramètres ----
nodes_file <- "Resultats/Passeuse_3_4_nodes.csv"
edges_file <- "Resultats/Passeuse_3_4_edges.csv"
target_word <- "Alric"  # Modifiez selon le mot à analyser

# ---- Vérification des fichiers ----
if(!file.exists(nodes_file)) stop(paste("❌ Fichier manquant:", nodes_file))
if(!file.exists(edges_file)) stop(paste("❌ Fichier manquant:", edges_file))

# ---- Import ----
nodes <- read.csv(nodes_file, stringsAsFactors = FALSE)
edges <- read.csv(edges_file, stringsAsFactors = FALSE)

# ---- Vérification colonnes ----
if(!all(c("Source","Target","Weight","Partie") %in% colnames(edges))) {
  stop("❌ Colonnes manquantes dans edges. Attendu: Source, Target, Weight, Partie")
}
if(!all(c("Id","Label","partie") %in% colnames(nodes))) {
  stop("❌ Colonnes manquantes dans nodes. Attendu: Id, Label, partie")
}

# ---- Normalisation ----
edges <- edges %>% 
  rename(source = Source, target = Target, weight = Weight) %>%
  mutate(source = trimws(source), target = trimws(target))

nodes <- nodes %>% 
  rename(id = Id, label = Label) %>%
  mutate(label = trimws(label))

# ---- Séparation par partie ----
edges_t1 <- edges %>% filter(Partie == "Partie_1")
edges_t2 <- edges %>% filter(Partie == "Partie_2")
nodes_t1 <- nodes %>% filter(partie == "Partie_1")
nodes_t2 <- nodes %>% filter(partie == "Partie_2")

message("📊 Partie 1: ", nrow(edges_t1), " arêtes, ", nrow(nodes_t1), " nœuds")
message("📊 Partie 2: ", nrow(edges_t2), " arêtes, ", nrow(nodes_t2), " nœuds")

# ---- Création graphes ----
g1 <- graph_from_data_frame(edges_t1, directed = FALSE)
g2 <- graph_from_data_frame(edges_t2, directed = FALSE)

# Enrichir avec les attributs des nœuds (méthode compatible igraph)
# Note: on garde ces infos dans des data frames séparés pour éviter les conflits igraph
nodes_info_t1 <- nodes_t1 %>% 
  filter(label %in% V(g1)$name) %>%
  select(label, everything(), -id, -partie)

nodes_info_t2 <- nodes_t2 %>% 
  filter(label %in% V(g2)$name) %>%
  select(label, everything(), -id, -partie)

# Optionnel: ajouter uniquement les attributs essentiels sans conflit
# (POS_Category et Total_Frequency causent des problèmes avec certaines versions d'igraph)
# Si vous en avez besoin, vous pouvez les joindre plus tard via nodes_info_t1/t2

message("🔗 Graphe 1: ", vcount(g1), " nœuds, ", ecount(g1), " arêtes")
message("🔗 Graphe 2: ", vcount(g2), " nœuds, ", ecount(g2), " arêtes")

# ---- Vérification mot cible ----
if (!(target_word %in% V(g1)$name)) {
  stop(paste("❌", target_word, "absent dans Partie 1"))
}
if (!(target_word %in% V(g2)$name)) {
  stop(paste("❌", target_word, "absent dans Partie 2"))
}

# ---- Extraction des voisins ----
neighbors_t1 <- V(g1)[neighbors(g1, target_word)]$name
neighbors_t2 <- V(g2)[neighbors(g2, target_word)]$name

message("\n📍 Analyse pour: ", target_word)
message("👥 Voisins Partie 1 (", length(neighbors_t1), "): ", 
        paste(head(neighbors_t1, 10), collapse=", "))
message("👥 Voisins Partie 2 (", length(neighbors_t2), "): ", 
        paste(head(neighbors_t2, 10), collapse=", "))

# ---- Extraire arêtes du mot cible ----
edges_char_t1 <- edges_t1 %>% filter(source == target_word | target == target_word)
edges_char_t2 <- edges_t2 %>% filter(source == target_word | target == target_word)

message("📈 Arêtes Partie 1: ", nrow(edges_char_t1))
message("📈 Arêtes Partie 2: ", nrow(edges_char_t2))

# ---- Normalisation des arêtes (ordre alphabétique) ----
normalize_edges <- function(df) {
  if(nrow(df) == 0) return(df)
  df %>% rowwise() %>%
    mutate(source_norm = min(source, target),
           target_norm = max(source, target)) %>%
    ungroup()
}

edges_char_t1 <- normalize_edges(edges_char_t1)
edges_char_t2 <- normalize_edges(edges_char_t2)

# ---- Fusion et comparaison des arêtes ----
edges_compare <- full_join(
  edges_char_t1 %>% select(source_norm, target_norm, weight),
  edges_char_t2 %>% select(source_norm, target_norm, weight),
  by = c("source_norm", "target_norm"),
  suffix = c("_t1", "_t2")
)

if(nrow(edges_compare) == 0) {
  message("⚠️ Aucune arête trouvée pour ce mot")
  edges_compare <- data.frame(
    source_norm = target_word,
    target_norm = NA,
    weight_t1 = NA,
    weight_t2 = NA,
    delta_weight = NA
  )
} else {
  edges_compare <- edges_compare %>% 
    mutate(
      weight_t1 = replace_na(weight_t1, 0),
      weight_t2 = replace_na(weight_t2, 0),
      delta_weight = weight_t2 - weight_t1
    )
}

# ---- Calcul des métriques ----
metrics <- function(g) {
  data.frame(
    name = V(g)$name,
    degree = degree(g),
    betweenness = betweenness(g),
    closeness = closeness(g)
  )
}

m1 <- metrics(g1)
m2 <- metrics(g2)
char_m1 <- m1 %>% filter(name == target_word)
char_m2 <- m2 %>% filter(name == target_word)

# Détection de communautés
comm1 <- cluster_louvain(g1)
comm2 <- cluster_louvain(g2)
modularity_t1 <- modularity(comm1)
modularity_t2 <- modularity(comm2)
char_comm1 <- membership(comm1)[which(V(g1)$name == target_word)]
char_comm2 <- membership(comm2)[which(V(g2)$name == target_word)]

# ---- Calcul du coefficient de Jaccard ----
jaccard <- if(length(union(neighbors_t1, neighbors_t2)) > 0) {
  length(intersect(neighbors_t1, neighbors_t2)) / length(union(neighbors_t1, neighbors_t2))
} else {
  NA
}

message("\n🎯 Coefficient de Jaccard: ", round(jaccard, 3))
message("   → Interprétation: ", 
        ifelse(is.na(jaccard), "Aucun voisin", 
               ifelse(jaccard > 0.7, "Contexte très stable",
                      ifelse(jaccard > 0.4, "Contexte partiellement stable",
                             "Contexte fortement modifié"))))

# ---- Tableau de comparaison ----
comparison <- data.frame(
  Metric = c("Degree", "Betweenness", "Closeness", "Community", 
             "Modularity", "Jaccard"),
  Partie_1 = c(char_m1$degree, char_m1$betweenness, char_m1$closeness, 
               char_comm1, modularity_t1, NA),
  Partie_2 = c(char_m2$degree, char_m2$betweenness, char_m2$closeness, 
               char_comm2, modularity_t2, jaccard),
  Delta = c(char_m2$degree - char_m1$degree,
            char_m2$betweenness - char_m1$betweenness,
            char_m2$closeness - char_m1$closeness,
            NA, modularity_t2 - modularity_t1, NA)
)

# ---- Analyse des voisins ----
voisins_communs <- intersect(neighbors_t1, neighbors_t2)
voisins_perdus <- setdiff(neighbors_t1, neighbors_t2)
voisins_nouveaux <- setdiff(neighbors_t2, neighbors_t1)

# ---- Export ----
write.csv(comparison, "comparison_metrics.csv", row.names = FALSE)
write.csv(edges_compare, "comparison_edges.csv", row.names = FALSE)

# Export analyse voisins
voisins_analysis <- data.frame(
  Type = c(rep("Commun", length(voisins_communs)),
           rep("Perdu", length(voisins_perdus)),
           rep("Nouveau", length(voisins_nouveaux))),
  Voisin = c(voisins_communs, voisins_perdus, voisins_nouveaux)
)
write.csv(voisins_analysis, "comparison_neighbors.csv", row.names = FALSE)

message("\n✅ Export terminé:")
message("   • comparison_metrics.csv (métriques)")
message("   • comparison_edges.csv (évolution des connexions)")
message("   • comparison_neighbors.csv (analyse des voisins)")

# ---- Affichage résultats ----
cat("\n╔═══════════════════════════════════════════════╗\n")
cat("║  RÉSULTATS POUR:", target_word, "                ║\n")
cat("╚═══════════════════════════════════════════════╝\n\n")

cat("=== MÉTRIQUES DE CENTRALITÉ ===\n")
print(comparison, row.names = FALSE)

cat("\n=== ÉVOLUTION DES CONNEXIONS (Top 10) ===\n")
top_changes <- edges_compare %>% 
  arrange(desc(abs(delta_weight))) %>% 
  head(10) %>%
  select(target_norm, weight_t1, weight_t2, delta_weight)
print(top_changes, row.names = FALSE)

cat("\n=== ANALYSE DES VOISINS ===\n")
cat("Voisins communs aux 2 parties (", length(voisins_communs), "):\n")
if(length(voisins_communs) > 0) {
  cat("  ", paste(head(voisins_communs, 15), collapse = ", "), "\n")
} else {
  cat("  Aucun\n")
}

cat("\nVoisins perdus en Partie 2 (", length(voisins_perdus), "):\n")
if(length(voisins_perdus) > 0) {
  cat("  ", paste(head(voisins_perdus, 15), collapse = ", "), "\n")
} else {
  cat("  Aucun\n")
}

cat("\nNouveaux voisins en Partie 2 (", length(voisins_nouveaux), "):\n")
if(length(voisins_nouveaux) > 0) {
  cat("  ", paste(head(voisins_nouveaux, 15), collapse = ", "), "\n")
} else {
  cat("  Aucun\n")
}

cat("\n╔═══════════════════════════════════════════════╗\n")
cat("║  INTERPRÉTATION RAPIDE                        ║\n")
cat("╚═══════════════════════════════════════════════╝\n")
cat("• Jaccard =", round(jaccard, 3), "→", 
    ifelse(is.na(jaccard), "pas de voisins",
           ifelse(jaccard > 0.7, "contexte TRÈS stable",
                  ifelse(jaccard > 0.4, "contexte PARTIELLEMENT stable",
                         "contexte FORTEMENT modifié"))), "\n")
cat("• Degree Δ =", comparison$Delta[1], "→", 
    ifelse(comparison$Delta[1] > 0, "plus de co-occurrences", "moins de co-occurrences"), "\n")
cat("• Betweenness Δ =", round(comparison$Delta[2], 2), "→", 
    ifelse(comparison$Delta[2] > 0, "rôle de pont accru", "rôle de pont diminué"), "\n")
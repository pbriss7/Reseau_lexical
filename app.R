#################################################################
# APPLICATION SHINY POUR L'EXPLORATION DU RÉSEAU LEXICAL        #
# AVEC FILTRES AVANCÉS ET PROJECTION DE PERSONNAGES             #
#################################################################

# IMPORTANT : L'application suppose que `nodes_table` et `edges_table` existent.
# Vous devez avoir exécuté le code du fichier `Consolidation_connaissances_reseau.qmd` avant de lancer l'app.

# --- 1. Prérequis et chargement des bibliothèques ---

# install.packages(c("shiny", "visNetwork", "dplyr", "igraph", "shinythemes"))
library(shiny)
library(visNetwork)
library(dplyr)
library(igraph)
library(shinythemes)

library(data.table)
nodes_table <- fread("donnees/Passeuse_3_4_nodes.csv")
edges_table <- fread("donnees/Passeuse_3_4_edges.csv")

# --- Pré-calcul des valeurs pour les filtres ---
if (exists("nodes_table") && exists("edges_table")) {
  propn_list <- nodes_table %>% 
    filter(POS_Category == "PROPN") %>% 
    arrange(desc(Total_Frequency)) %>%
    pull(Id)
  
  # Liste des autres catégories
  other_pos_list <- c("NOUN", "ADJ", "VERB")
  
  max_node_freq <- max(nodes_table$Total_Frequency)
  max_edge_weight <- max(edges_table$Weight) 
} else {
  # Valeurs par défaut pour éviter un crash
  propn_list <- c("Arya", "Abel", "Lyncée")
  other_pos_list <- c("NOUN", "ADJ", "VERB") 
  max_node_freq <- 1000
  max_edge_weight <- 100
}

# --- 2. Définition de l'interface utilisateur (UI) ---

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Explorateur du réseau lexical d'Illusions Perdues"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtres principaux"),
      
      # Contrôle 1a: projection de noms propres
      selectizeInput(
        "propn_select", "1a. Projeter des noms propres (personnages, lieux) :",
        choices = propn_list,
        selected = "lucien", # Valeur par défaut
        multiple = TRUE
      ),
      
      # Contrôle 1b: autres catégories grammaticales
      checkboxGroupInput(
        "other_pos_categories", "1b. Dans un contexte de concepts/actions/qualités :",
        choices = other_pos_list,
        selected = "NOUN" # Valeur par défaut
      ),
      
      # Contrôle 2: parties du roman
      checkboxGroupInput(
        "parties", "2. Partie(s) du roman :",
        choices = c("Partie_1",
                    "Partie_2"
                    #,"Partie_3"
                    ), selected = "Partie_1"
      ),
      
      hr(),
      h4("Filtres de densité"),
      
      # Les filtres de densité restent les mêmes
      sliderInput("top_n_edges", "3. Afficher les N liens les plus forts :", min = 10, max = 500, value = 100, step = 10),
      sliderInput("node_freq", "4. Fréquence des lemmes (nœuds) :", min = 1, max = max_node_freq, value = c(1, max_node_freq), step = 10),
      sliderInput("edge_weight", "5. Poids des cooccurrences (liens) :", min = 1, max = max_edge_weight, value = c(1, max_edge_weight), step = 1)
    ),
    
    mainPanel(
      width = 9,
      visNetworkOutput("network_plot", height = "85vh")
    )
  )
)


# --- 3. Définition du serveur (logique de l'application) ---

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    # Sécurité: on requiert au moins une partie et au moins une catégorie sélectionnée (soit PROPN, soit autre)
    req(input$parties, (length(input$propn_select) > 0 | length(input$other_pos_categories) > 0))
    
    # --- 1. Définition du "pool" de nœuds autorisés ---
    
    allowed_nodes_ids <- nodes_table %>% 
      filter(
        # On garde les PROPN sélectionnés OU les nœuds appartenant aux autres catégories cochées
        Id %in% input$propn_select | POS_Category %in% input$other_pos_categories
      ) %>%
      pull(Id)
    
    # --- 2. Filtrage des ARÊTES basé sur le pool de nœuds ---
    
    edges_subset <- edges_table %>%
      # On filtre par partie et on agrège
      filter(Partie %in% input$parties) %>%
      group_by(Source, Target) %>%
      summarise(Weight = sum(Weight), .groups = 'drop') %>%
      # important : on ne garde que les liens dont les deux extrémités sont dans notre pool de nœuds autorisés
      filter(Source %in% allowed_nodes_ids & Target %in% allowed_nodes_ids)
    
    # --- 3. Application des filtres de densité ---
    
    edges_filtered <- edges_subset %>%
      filter(Weight >= input$edge_weight[1] & Weight <= input$edge_weight[2]) %>%
      arrange(desc(Weight)) %>%
      head(input$top_n_edges)
    
    active_nodes_ids <- unique(c(edges_filtered$Source, edges_filtered$Target))
    
    # On filtre la table de nœuds initiale
    nodes_filtered <- nodes_table %>%
      filter(
        Id %in% active_nodes_ids,
        Total_Frequency >= input$node_freq[1] & Total_Frequency <= input$node_freq[2]
      )
    
    final_edges <- edges_filtered %>%
      filter(Source %in% nodes_filtered$Id & Target %in% nodes_filtered$Id)
    
    list(nodes = nodes_filtered, edges = final_edges)
  })
  
  # Rendu
  output$network_plot <- renderVisNetwork({
    
    data <- filtered_data()
    vis_nodes <- data$nodes
    vis_edges <- data$edges
    
    if (nrow(vis_nodes) == 0 || nrow(vis_edges) == 0) {
      return(visNetwork(nodes = data.frame(id = 1, label = "Aucun réseau à afficher.\nEssayez d'élargir vos filtres.", shape = "text")))
    }
    
    vis_nodes <- vis_nodes %>% rename(id = Id, label = Label, value = Total_Frequency, group = POS_Category)
    vis_edges <- vis_edges %>% rename(from = Source, to = Target, value = Weight)
    
    visNetwork(nodes = vis_nodes, edges = vis_edges, main = "Réseau lexical d'Illusions Perdues") %>%
      visNodes(shape = "dot", font = list(size = 18)) %>%
      visEdges(smooth = FALSE, color = list(color = "#CECECE", highlight = "#2B7CE9")) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -50)) %>%
      visLayout(randomSeed = 123) %>%
      visLegend(useGroups = TRUE, width = 0.1, main = "Catégories")
  })
}


# --- 4. Lancement de l'application ---
shinyApp(ui = ui, server = server)
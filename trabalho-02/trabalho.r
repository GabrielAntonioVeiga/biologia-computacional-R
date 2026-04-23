#' Script - Exercício 2 (ggraph + comunidades com separação controlada)

library(Matrix)
library(plyr)
library(dplyr)
library(igraph)
library(ggplot2)
library(ggraph)
library(stringr)
library(RColorBrewer)
library(rstudioapi)

# --- Etapa 1: Importação ---
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_dir)

mat     <- readMM("bio-celegans/bio-celegans.mtx")
edges   <- summary(mat)

network <- graph_from_edgelist(as.matrix(edges[,1:2]), directed = FALSE)
network <- simplify(network, remove.multiple = TRUE, remove.loops = TRUE)

# --- Etapa 2: Métricas ---
deg  <- degree(network)

# --- Etapa 3: Comunidades ---
set.seed(42)
comunidades <- cluster_louvain(network)
memb <- membership(comunidades)

# --- Top hubs ---
top5_degree_ids <- order(deg, decreasing = TRUE)[1:5]

# --- Atributos ---
V(network)$grau <- deg
V(network)$comunidade <- as.factor(memb)

V(network)$tipo_no <- ifelse(
  seq_len(vcount(network)) %in% top5_degree_ids,
  "Hub",
  "Normal"
)

V(network)$stroke_hub <- ifelse(
  V(network)$tipo_no == "Hub",
  2,
  0.5
)

V(network)$label <- ifelse(
  V(network)$tipo_no == "Hub",
  paste0("v", seq_len(vcount(network))),
  NA
)

# ============================================================
# 🔥 LAYOUT COM SEPARAÇÃO SUAVE
# ============================================================

# 1. Contrair grafo por comunidade
g_cluster <- contract(network, memb)
g_cluster <- simplify(g_cluster)

# 2. Layout FR no grafo reduzido
set.seed(42)
layout_cluster <- layout_with_fr(g_cluster)

# 🔥 3. REDUZIR distância entre comunidades
layout_cluster <- layout_cluster * 0.08  # <<< AQUI controla separação

# 4. Expandir para nós originais
layout_expanded <- layout_cluster[memb, ]

# 🔥 5. Pequeno espalhamento interno (bem leve agora)
set.seed(42)
layout_expanded <- layout_expanded +
  matrix(rnorm(length(layout_expanded), sd = 0.015), ncol = 2)

# ============================================================
# Converter para ggraph
# ============================================================

layout_df <- create_layout(network, layout = "manual",
                           x = layout_expanded[,1],
                           y = layout_expanded[,2])

# ============================================================
# Plot
# ============================================================

ggraph(layout_df) +
  
  geom_edge_link(alpha = 0.2, color = "gray50") +
  
  geom_node_point(aes(
    size = grau,
    fill = comunidade,
    stroke = stroke_hub
  ),
  shape = 21,
  color = "black"
  ) +
  
  geom_node_text(aes(label = label),
                 repel = TRUE,
                 size = 3) +
  
  scale_size(range = c(2, 8)) +
  
  scale_fill_manual(
    values = colorRampPalette(
      brewer.pal(min(length(unique(V(network)$comunidade)), 12), "Set3")
    )(length(unique(V(network)$comunidade)))
  ) +
  
  theme_void() +
  
  ggtitle("C. elegans – Comunidades (Louvain)") +
  
  labs(
    subtitle = "Separação suave entre comunidades | Borda = Hub",
    fill = "Comunidade",
    size = "Grau"
  )
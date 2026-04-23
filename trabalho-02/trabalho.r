#' Este script acompanha o "Exercício 2", carregando no RStudio os dados 
#' indicados na tarefa EaD2.

library(Matrix)
library(plyr)
library(dplyr)
library(igraph)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(RGraphSpace)
library(rstudioapi)

# --- Etapa 1: Importação e Limpeza ---
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_dir)

mat     <- readMM("bio-celegans/bio-celegans.mtx")
edges   <- summary(mat)

network <- graph_from_edgelist(as.matrix(edges[,1:2]), directed = FALSE)
network <- simplify(network, remove.multiple = TRUE, remove.loops = TRUE)

# --- Etapa 2: Caracterização Topológica ---
cat("--- Indicadores Globais ---\n")
cat("Ordem (Vértices):",           vcount(network), "\n")
cat("Tamanho (Arestas):",          ecount(network), "\n")
cat("Densidade:",                 edge_density(network), "\n")
cat("Diâmetro:",                  diameter(network), "\n")
cat("Transitividade (Clustering):", transitivity(network, type = "global"), "\n")

# ============================================================
# ETAPA 3: Identificação de Atores-chave (Hubs)
# ============================================================

deg        <- degree(network)
betw       <- betweenness(network, normalized = TRUE)
close      <- closeness(network, normalized = TRUE)
eigen_cent <- eigen_centrality(network)$vector

top5_degree_ids <- order(deg,  decreasing = TRUE)[1:5]
top5_betw_ids   <- order(betw, decreasing = TRUE)[1:5]

cat("\n--- Top 5 por Grau ---\n")
for (i in top5_degree_ids)
  cat(sprintf("  Vértice %d: grau = %d\n", i, deg[i]))

cat("\n--- Top 5 por Betweenness ---\n")
for (i in top5_betw_ids)
  cat(sprintf("  Vértice %d: betweenness = %.4f\n", i, betw[i]))

hub_ids <- union(top5_degree_ids, top5_betw_ids)

df_hubs <- data.frame(
  No          = hub_ids,
  Grau        = deg[hub_ids],
  Betweenness = round(betw[hub_ids],        4),
  Closeness   = round(close[hub_ids],       4),
  Eigenvector = round(eigen_cent[hub_ids],  4),
  row.names   = NULL
) %>% arrange(desc(Grau))

cat("\n--- Tabela de Centralidades dos Atores-chave ---\n")
print(df_hubs)

cat("\n--- Análise Estrutural ---\n")
for (i in hub_ids) {
  g    <- deg[i]
  b    <- betw[i]
  tipo <- dplyr::case_when(
    g > median(deg) & b > median(betw)  ~ "Conector Global (Hub + Ponte)",
    g > median(deg) & b <= median(betw) ~ "Hub Local (alta conectividade local)",
    g <= median(deg) & b > median(betw) ~ "Ponte Estrutural (articulador)",
    TRUE                                ~ "Nó Periférico"
  )
  cat(sprintf("  Vértice %d | Grau=%d | Betw=%.4f => %s\n", i, g, b, tipo))
}

# ============================================================
# ETAPA 4: Detecção de Comunidades e Visualização
# ============================================================

set.seed(42)
comunidades <- cluster_louvain(network)

cat("\n--- Resultado da Detecção de Comunidades (Louvain) ---\n")
cat("Número de comunidades:", length(comunidades), "\n")
cat("Modularidade:", round(modularity(comunidades), 4), "\n")
print(sort(table(membership(comunidades)), decreasing = TRUE))

# --- Atributos visuais nos vértices ---
n_com  <- length(comunidades)
paleta <- colorRampPalette(brewer.pal(min(n_com, 12), "Set3"))(n_com)

# Cor por comunidade
V(network)$nodeColor <- paleta[membership(comunidades)]

# Tamanho proporcional ao grau
deg_norm <- (deg - min(deg)) / (max(deg) - min(deg))
V(network)$nodeSize <- 1 + deg_norm * 9

# Destacar top-5 hubs com borda dourada
top5_ids <- order(deg, decreasing = TRUE)[1:5]
V(network)$nodeBorderColor <- ifelse(
  seq_len(vcount(network)) %in% top5_ids, "gold", NA
)
V(network)$nodeBorderWidth <- ifelse(
  seq_len(vcount(network)) %in% top5_ids, 2, 0.5
)

# Rótulo apenas para os top-5
V(network)$nodeLabel <- ifelse(
  seq_len(vcount(network)) %in% top5_ids,
  paste0("v", seq_len(vcount(network))),
  NA
)

# --- Criar objeto GraphSpace com layout Fruchterman-Reingold ---
set.seed(42)
gs <- GraphSpace(network, layout = layout_with_fr(network))

# --- Plotar (CORRIGIDO) ---
p <- plotGraphSpace(gs)

# Adicionar título (opcional)
p +
  ggtitle("C. elegans – Detecção de Comunidades (Louvain)") +
  annotate("text", x = Inf, y = Inf, label = "Cores: Comunidades (Louvain)",
           hjust = 1.1, vjust = 2, size = 4) +
  annotate("text", x = Inf, y = Inf, label = "Tamanho: Grau do vértice",
           hjust = 1.1, vjust = 4, size = 4) +
  annotate("text", x = Inf, y = Inf, label = "Borda dourada: Top 5 hubs",
           hjust = 1.1, vjust = 6, size = 4)
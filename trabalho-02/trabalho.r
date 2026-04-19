#' Este script acompanha o "Exercício 2", carregando no RStudio os dados 
#' indicados na tarefa EaD2.

library(Matrix)
library(plyr)
library(dplyr)
library(igraph)
library(ggplot2)
library(stringr)
library(RColorBrewer)

# --- Etapa 1: Importação e Limpeza ---
# Carregando os dados
mat <- readMM("/home/veiga/Downloads/bio-celegans/bio-celegans.mtx")

edges <- summary(mat)

# Criando o grafo
network <- graph_from_edgelist(as.matrix(edges[,1:2]), directed = FALSE)

# Simplificação: Remove loops e arestas múltiplas (Essencial para métricas limpas)
network <- simplify(network, remove.multiple = TRUE, remove.loops = TRUE)

# --- Etapa 2: Caracterização Topológica ---
cat("--- Indicadores Globais ---\n")
cat("Ordem (Vértices):", vcount(network), "\n")
cat("Tamanho (Arestas):", ecount(network), "\n")
cat("Densidade:", edge_density(network), "\n")
cat("Diâmetro:", diameter(network), "\n")
cat("Transitividade (Clustering):", transitivity(network, type = "global"), "\n")
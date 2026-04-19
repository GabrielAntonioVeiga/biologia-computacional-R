#' Este script acompanha o "Exercício 2", carregando no RStudio os dados 
#' indicados na tarefa EaD2.

library(dplyr)
library(ggplot2)
library(stringr)
library(plyr)
library(RColorBrewer)
library(igraph)
library(RGraphSpace)

# --- Etapa 1: Importação e Limpeza ---
# Carregando os dados
mousedata <- read.table("bio-mouse-gene/bio-mouse-gene.edges", header = FALSE)

# Criando o grafo
network <- graph_from_edgelist(as.matrix(mousedata[,1:2]), directed = FALSE)

# Simplificação: Remove loops e arestas múltiplas (Essencial para métricas limpas)
network <- simplify(network, remove.multiple = TRUE, remove.loops = TRUE)

# --- Etapa 2: Caracterização Topológica ---
cat("--- Indicadores Globais ---\n")
cat("Ordem (Vértices):", vcount(network), "\n")
cat("Tamanho (Arestas):", ecount(network), "\n")
cat("Densidade:", edge_density(network), "\n")
cat("Diâmetro:", diameter(network), "\n")
cat("Transitividade (Clustering):", transitivity(network, type = "global"), "\n")
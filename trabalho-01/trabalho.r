library(igraph)
library(ggraph)
library(tidyverse)
library(gridExtra) # Biblioteca para adicionar a tabela na imagem

# --- 1. Importação e Preparação ---
df <- read_csv("/home/veiga/Downloads/biologia-computacional/trabalho-01/WHO-COVID-19-global-table-data.csv")

df_clean <- df %>%
  filter(Name != "Global") %>% 
  select(pais = Name, regiao = `WHO Region`, casos = `Cases - cumulative total`) %>%
  mutate(regiao = ifelse(is.na(regiao) | regiao == "Other", "Other/NA", regiao),
         casos_log = log(casos + 1)) 

# --- 2. Preparar a Tabela de Destaques (O que você pediu) ---
tabela_destaques <- df_clean %>%
  filter(regiao != "Other/NA") %>%
  group_by(regiao) %>%
  summarise(
    `Mais Casos` = pais[which.max(casos)],
    `Menos Casos` = pais[which.min(casos)]
  ) %>%
  rename(Regiao = regiao)

# --- 3. Criar o Grafo ---
edges <- df_clean %>% select(pais, regiao)
rede_covid <- graph_from_data_frame(edges, directed = FALSE)
v_names <- V(rede_covid)$name
V(rede_covid)$grupo <- ifelse(v_names %in% df_clean$regiao, "Região", "País")
V(rede_covid)$tamanho <- df_clean$casos_log[match(v_names, df_clean$pais)]
V(rede_covid)$tamanho[is.na(V(rede_covid)$tamanho)] <- max(df_clean$casos_log) * 1.3

# --- 4. Gerar o Gráfico Principal ---
grafico_redes <- ggraph(rede_covid, layout = "fr") + 
  geom_edge_link(alpha = 0.05, color = "gray60") + 
  geom_node_point(aes(color = grupo, size = tamanho), show.legend = TRUE) +
  geom_node_text(aes(label = ifelse(grupo == "Região", name, NA)), 
                 repel = TRUE, fontface = "bold", size = 4, color = "gray10") +
  scale_size_continuous(range = c(1, 10), guide = "none") +
  scale_color_manual(values = c("País" = "dodgerblue", "Região" = "firebrick")) +
  theme_void() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "bottom") +
  labs(title = "Análise de Redes COVID-19 por Região da OMS",
       subtitle = "O tamanho dos nós reflete o volume de casos (escala logarítmica)",
       color = "Legenda:")

# --- 5. Unir tudo em uma única imagem ---
# Criamos um tema visual para a tabela
tema_tabela <- ttheme_minimal(
  core = list(fg_params = list(cex = 0.8)),
  colhead = list(fg_params = list(cex = 0.9, fontface = "bold"))
)

tabela_plot <- tableGrob(tabela_destaques, rows = NULL, theme = tema_tabela)

# Salvar unindo o gráfico e a tabela
png("trabalho_final_completo.png", width = 1200, height = 1400, res = 120, bg = "white")
grid.arrange(grafico_redes, tabela_plot, ncol = 1, heights = c(3, 1))
dev.off()
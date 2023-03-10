
## An?lise de Correspond?ncia M?ltipla

# MBA DSA USP ESALQ
# Prof. Wilson Tarantin Jr.

# Fonte: https://www.kaggle.com/code/jiagengchang/heart-disease-multiple-correspondence-analysis

# Instala??o e carregamento dos pacotes utilizados
pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel",
             "knitr", "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap", 
             "ade4",
             "readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Importando a base de dados
dados_apt <- read.csv("df_apart_cat_r.csv", sep=';', encoding = 'UTF-8')


# A fun??o para a cria??o da ACM pede que sejam utilizados "fatores"
dados_apt <- as.data.frame(unclass(dados_apt), stringsAsFactors=TRUE)
dados_apt[] = lapply(dados_apt, as.factor)

# Tabelas de conting?ncia (todas apresentam associa??o com alguma vari?vel?)
sjt.xtab(var.row = dados_apt$QUARTO,
         var.col = dados_apt$PRECO,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_apt$GARAGENS,
         var.col = dados_apt$PRECO,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_apt$BANHEIROS,
         var.col = dados_apt$PRECO,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_apt$AREA,
         var.col = dados_apt$PRECO,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_apt$BAIRRO,
         var.col = dados_apt$PRECO,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

# Vamos gerar a ACM
#ACM <- dudi.acm(dados_apt, scannf = FALSE)

ACM <- dudi.acm(dados_apt, scannf = FALSE, nf = 3)

# Analisando as vari?ncias de cada dimens?o
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por vari?vel
quant_categorias <- apply(dados_apt,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padr?o obtidas por meio da matriz bin?ria
df_ACM <- data.frame(ACM$c1, Vari?vel = rep(names(quant_categorias),
                                            quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Vari?vel)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimens?o 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimens?o 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# Poder?amos fazer o mapa com as coordenadas obtidas por meio da matriz de Burt

# Consolidando as coordenadas-padr?o obtidas por meio da matriz de Burt
df_ACM_B <- data.frame(ACM$co, Vari?vel = rep(names(quant_categorias),
                                              quant_categorias))

# Plotando o mapa perceptual
df_ACM_B %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Vari?vel)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimens?o 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimens?o 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# ? poss?vel obter as coordenadas das observa??es
df_coord_obs <- ACM$li

# Plotando o mapa perceptual
df_coord_obs %>%
  ggplot(aes(x = Axis1, y = Axis2, color = dados_apt$Doen?a_Card)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimens?o 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimens?o 2:", paste0(round(perc_variancia[2], 2), "%")),
       color = "Doen?a Card?aca") +
  theme_bw()



# Mapa perceptual em 3D (3 primeiras dimens?es)
ACM_3D <- plot_ly()

# Adicionando as coordenadas
ACM_3D <- add_trace(p = ACM_3D,
                    x = df_ACM$CS1,
                    y = df_ACM$CS2,
                    z = df_ACM$CS3,
                    mode = "text",
                    text = rownames(df_ACM),
                    textfont = list(color = "blue"),
                    marker = list(color = "red"),
                    showlegend = FALSE)

ACM_3D

# Fim!


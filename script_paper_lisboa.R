library(readxl)
library(factoextra)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(dplyr)
library(FactoMineR)
library(purrr)
library(tidyverse)
library(ggstatsplot)

path = "/home/leandro/Área de Trabalho/DOCUMENTOS/analise_lisboa_novo"
setwd(path)

#LISBOA
data_lisboa <- read_excel("analiseLisboa.xlsx")
data_lisboa <- select(data_lisboa, -`IL-12p40`)

#TEMPO ZERO LISBOA
data_lisboa_tempo_zero <- filter(data_lisboa, TEMPO == 0)
data_lisboa_tempo_zero <- as_tibble(data_lisboa_tempo_zero)
data_lisboa_tempo_zero_final <- apply(data_lisboa_tempo_zero[,7:21], 2, as.numeric)
#TEMPO SETE LISBOA
data_lisboa_tempo_sete <- filter(data_lisboa, TEMPO == 7)
data_lisboa_tempo_sete <- as_tibble(data_lisboa_tempo_sete)
data_lisboa_tempo_sete_final <- apply(data_lisboa_tempo_sete[,7:21], 2, as.numeric)

#Belo Horizonte
data_bh <- read_excel("analiseLisboa.xlsx", sheet = "bh")

#TEMPO ZERO BH
data_bh_tempo_zero <- filter(data_bh, TEMPO == 0)
data_bh_tempo_zero <- as_tibble(data_bh_tempo_zero)
data_bh_tempo_zero_final <- apply(data_bh_tempo_zero[,7:21], 2, as.numeric)
#TEMPO SETE BH
data_bh_tempo_sete <- filter(data_bh, TEMPO == 7)
data_bh_tempo_sete <- as_tibble(data_bh_tempo_sete)
data_bh_tempo_sete_final <- apply(data_bh_tempo_sete[,7:21], 2, as.numeric)


#PCA Lisboa TEMPO ZERO
res_pca_lisboa_zero <- PCA(data_lisboa_tempo_zero_final)

fig1 = fviz_pca_var(res_pca_lisboa_zero, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
) + ggtitle("PCA", subtitle = "Lisbon - patients at time 0")

# Contributions of variables to PC1
fig2 = fviz_contrib(res_pca_lisboa_zero, choice = "var", axes = 1, top = 15) +
  ggtitle("PCA Lisbon", subtitle = "Contributions of variables to PC1 - time 0")

fig3 = fviz_contrib(res_pca_lisboa_zero, choice = "var", axes = 2, top = 15) +
  ggtitle("PCA Lisbon", subtitle = "Contributions of variables to PC2 - time 0")


#PCA Lisboa TEMPO SETE
res_pca_lisboa_sete <- PCA(data_lisboa_tempo_sete_final)

fig4 = fviz_pca_var(res_pca_lisboa_sete, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + ggtitle("PCA", subtitle = "Lisbon - patients at time 7")

# Contributions of variables to PC1
fig5 = fviz_contrib(res_pca_lisboa_sete, choice = "var", axes = 1, top = 15) +
  ggtitle("PCA Lisbon", subtitle = "Contributions of variables to PC1 - time 7")

fig6 = fviz_contrib(res_pca_lisboa_sete, choice = "var", axes = 2, top = 15) +
  ggtitle("PCA Lisbon", subtitle = "Contributions of variables to PC2 - time 7")

# PLOTS COMBINADOS LISBOA
combine_plots(
  list(fig1, fig4),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    #title = "",
    #caption = "Source: Gapminder Foundation"
  )
)

combine_plots(
  list(fig2, fig3, fig5, fig6),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    #title = "",
    #caption = "Source: Gapminder Foundation"
  )
)

#PCA BELO HORIZONTE TEMPO ZERO
res_pca_bh_zero <- PCA(data_bh_tempo_zero_final)

fig7 = fviz_pca_var(res_pca_bh_zero, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + ggtitle("PCA", subtitle = "Belo Horizonte - patients at time 0")

# Contributions of variables to PC1
fig8 = fviz_contrib(res_pca_bh_zero, choice = "var", axes = 1, top = 15) +
  ggtitle("PCA Belo-Horizonte", subtitle = "Contributions of variables to PC1 - time 0")
fig9 = fviz_contrib(res_pca_bh_zero, choice = "var", axes = 2, top = 15) +
  ggtitle("PCA Belo-Horizonte", subtitle = "Contributions of variables to PC2 - time 0")

#PCA BELO HORIZONTE TEMPO SETE
res_pca_bh_sete <- PCA(data_bh_tempo_sete_final)

fig10 = fviz_pca_var(res_pca_bh_sete, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + ggtitle("PCA", subtitle = "Belo Horizonte - patients at time 7")

# Contributions of variables to PC1
fig11 = fviz_contrib(res_pca_bh_sete, choice = "var", axes = 1, top = 15) +
  ggtitle("PCA Belo-Horizonte", subtitle = "Contributions of variables to PC1 - time 7")

fig12 = fviz_contrib(res_pca_bh_sete, choice = "var", axes = 2, top = 15) +
  ggtitle("PCA Belo-Horizonte", subtitle = "Contributions of variables to PC2 - time 7")

# PLOTS COMBINADOS LISBOA
combine_plots(
  list(fig7, fig10),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    #title = "",
    #caption = "Source: Gapminder Foundation"
  )
)

combine_plots(
  list(fig8, fig9, fig11, fig12),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    #title = "",
    #caption = "Source: Gapminder Foundation"
  )
)

library(readxl)
library(dplyr)
library(UncertainInterval)
library(stringr)
library(ggplot2)

setwd("/home/leandro/Área de Trabalho/DOCUMENTOS/Cecília")
data = read.csv("dados_para_analise.csv")


data$GLR = as.numeric(data$GLR)
data$MLR = as.numeric(data$MLR)
data$NLR = as.numeric(data$NLR)
data$PLT_x_LYM = as.numeric(data$PLT_x_LYM)
data$hemoglobina = as.numeric(data$hemoglobina)
data$monocitos_absoluto = as.numeric(data$monocitos_absoluto)

data$GRUPO = as.factor(data$GRUPO)

covid_negativo = data %>%
  filter(
    GRUPO %in% c("COVID", "NEGATIVO")
    )

covid_negativo_replaced_group = covid_negativo %>%
  mutate(
         GRUPO = str_replace_all(GRUPO, "COVID", "1"),
         GRUPO = str_replace_all(GRUPO, "NEGATIVO", "0")
    )

covid_negativo_replaced_group$GRUPO = as.numeric(
  covid_negativo_replaced_group$GRUPO
)

# GLR
covid_negativo_replaced_group <- filter(
  covid_negativo_replaced_group, GLR < 10
)

ref <- covid_negativo_replaced_group$GRUPO
test <- covid_negativo_replaced_group$GLR

TG.ROC(ref,
       test,
      # Se.criterion = 0.95,
      # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC GLR")

# PLR
covid_negativo_plr <- filter(
  covid_negativo_replaced_group, PLT_x_LYM < 130
)

ref_plr <- covid_negativo_plr$GRUPO
test_plr <- covid_negativo_plr$PLT_x_LYM

TG.ROC(ref_plr,
       test_plr,
      # Se.criterion = 0.95,
       model='binormal', plot=TRUE,
       position.legend = "right") + title("TG-ROC PLR")

# HGB
ref_hgb <- covid_negativo_replaced_group$GRUPO
test_hgb <- covid_negativo_replaced_group$hemoglobina

TG.ROC(ref_hgb,
       test_hgb,
      # Se.criterion = 0.95,
       model='binormal', plot=TRUE,
       position.legend = "right") + title("TG-ROC HGB")

# MONOCITOS
ref_monocitos <- covid_negativo_replaced_group$GRUPO
monocitos_relativo <- covid_negativo_replaced_group$monocitos_absoluto / 
  sum(covid_negativo_replaced_group$monocitos_absoluto)
test_monocitos <- monocitos_relativo

TG.ROC(ref_monocitos,
       test_monocitos,
       # Se.criterion = 0.95,
       model='none', plot=TRUE,
       position.legend = "right") + title("TG-ROC Monocitos %")

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



# COVID INTERNADOS X COVID LEVE ----------------------------------------------
#----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
covid_internado_e_covid_leve <- data %>% filter(
  classificacao %in% c("Internado", "Leve")
)

covid_internado_e_covid_leve_replaced <- covid_internado_e_covid_leve %>%
  mutate(
    classificacao = str_replace_all(classificacao, "Internado", "1"),
    classificacao = str_replace_all(classificacao, "Leve", "0")
  )

covid_internado_e_covid_leve_replaced$classificacao <-
  as.numeric(covid_internado_e_covid_leve_replaced$classificacao)

monocitos_relativo_covid_internado_e_leve <-
  covid_internado_e_covid_leve_replaced$monocitos_absoluto / 
  sum(covid_internado_e_covid_leve_replaced$monocitos_absoluto)
test_monocitos_relativo_covid_internado_e_leve <- monocitos_relativo_covid_internado_e_leve



# GLR ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
covid_internado_e_covid_leve_reduced <- filter(
  covid_internado_e_covid_leve_replaced, GLR < 13
)

ref_internado_e_leve <- covid_internado_e_covid_leve_reduced$classificacao
test_internado_e_leve <- covid_internado_e_covid_leve_reduced$GLR

TG.ROC(ref_internado_e_leve,
       test_internado_e_leve,
      # Se.criterion = 0.95,
      # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC GLR", sub = "Leve x COVID19 Internados")



# PLR ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
covid_internado_e_covid_leve_reduced_plr <- filter(
  covid_internado_e_covid_leve_replaced, PLT_x_LYM < 80
)

ref_internado_e_leve_plr <- covid_internado_e_covid_leve_reduced_plr$classificacao
test_internado_e_leve_plr <- covid_internado_e_covid_leve_reduced_plr$PLT_x_LYM

TG.ROC(ref_internado_e_leve_plr,
       test_internado_e_leve_plr,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC PLR", sub = "Leve x COVID19 Internados")



# HGB ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
ref_internado_e_leve_hgb <- covid_internado_e_covid_leve_replaced$classificacao
test_internado_e_leve_hgb <- covid_internado_e_covid_leve_replaced$hemoglobina

TG.ROC(ref_internado_e_leve_hgb,
       test_internado_e_leve_hgb,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC HGB", sub = "Leve x COVID19 Internados")



# Monocitos ------------------------------------------------------------------
#-----------------------------------------------------------------------------

ref_internado_e_leve_monocitos <- covid_internado_e_covid_leve_replaced$classificacao

TG.ROC(ref_internado_e_leve_monocitos,
       monocitos_relativo_covid_internado_e_leve,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC Monócitos %", sub = "Leve x COVID19 Internados")



# COVID INTERNADOS X CONTROLE NEGATIVO ---------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
covid_internado_e_controle_negativo <- data %>% filter(
  classificacao %in% c("Internado", "NEGATIVO")
)

covid_internado_e_controle_negativo_replaced <- covid_internado_e_controle_negativo %>%
  mutate(
    classificacao = str_replace_all(classificacao, "Internado", "1"),
    classificacao = str_replace_all(classificacao, "NEGATIVO", "0")
  )

covid_internado_e_controle_negativo_replaced$classificacao <-
  as.numeric(covid_internado_e_controle_negativo_replaced$classificacao)

monocitos_covid_internado_e_controle_negativo <-
  covid_internado_e_controle_negativo_replaced$monocitos_absoluto / 
  sum(covid_internado_e_controle_negativo_replaced$monocitos_absoluto)
test_covid_internado_e_controle_negativo <- monocitos_covid_internado_e_controle_negativo


# GLR ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#covid_internado_e_controle_negativo_reduced <- filter(
#  covid_internado_e_controle_negativo, GLR < 13
#)

ref_internado_e_controle_negativo <- covid_internado_e_controle_negativo_replaced$classificacao
test_internado_e_controle_negativo <- covid_internado_e_controle_negativo_replaced$GLR

TG.ROC(ref_internado_e_controle_negativo,
       test_internado_e_controle_negativo,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC GLR", sub = "Controle Negativo x COVID19 Internados")



# PLR ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
covid_internado_e_controle_negativo_reduced_plr <- filter(
  covid_internado_e_controle_negativo_replaced, PLT_x_LYM < 100
)

ref_internado_e_controle_negativo_plr <- covid_internado_e_controle_negativo_reduced_plr$classificacao
test_internado_e_controle_negativo_plr <- covid_internado_e_controle_negativo_reduced_plr$PLT_x_LYM

TG.ROC(ref_internado_e_controle_negativo_plr,
       test_internado_e_controle_negativo_plr,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC PLR", sub = "Controle Negativo x COVID19 Internados")



# HGB ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
ref_internado_e_controle_negativo_hgb <- covid_internado_e_controle_negativo_reduced_plr$classificacao
test_internado_e_controle_negativo_hgb <- covid_internado_e_controle_negativo_reduced_plr$hemoglobina

TG.ROC(ref_internado_e_controle_negativo_hgb,
       test_internado_e_controle_negativo_hgb,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC HGB", sub = "Controle Negativo x COVID19 Internados")



# Monocitos ------------------------------------------------------------------
#-----------------------------------------------------------------------------

ref_internado_e_controle_negativo_monocitos <- covid_internado_e_controle_negativo_replaced$classificacao

TG.ROC(ref_internado_e_controle_negativo_monocitos,
       monocitos_covid_internado_e_controle_negativo,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC Monócitos %", sub = "Controle Negativo x COVID19 Internados")



# LEVE E CONTROLE NEGATIVO ---------------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
covid_leve_e_controle_negativo <- data %>% filter(
  classificacao %in% c("Leve", "NEGATIVO")
)

covid_leve_e_controle_negativo_replaced <- covid_leve_e_controle_negativo %>%
  mutate(
    classificacao = str_replace_all(classificacao, "Leve", "1"),
    classificacao = str_replace_all(classificacao, "NEGATIVO", "0")
  )

covid_leve_e_controle_negativo_replaced$classificacao <-
  as.numeric(covid_leve_e_controle_negativo_replaced$classificacao)

monocitos_covid_leve_e_controle_negativo_replaced <-
  covid_leve_e_controle_negativo_replaced$monocitos_absoluto / 
  sum(covid_leve_e_controle_negativo_replaced$monocitos_absoluto)
test_covid_leve_e_controle_negativo_replaced <- monocitos_covid_leve_e_controle_negativo_replaced

# PLR ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
covid_leve_e_controle_negativo_reduced_plr <- filter(
  covid_leve_e_controle_negativo_replaced, PLT_x_LYM < 110
)

ref_leve_e_controle_negativo_plr <- covid_leve_e_controle_negativo_reduced_plr$classificacao
test_leve_e_controle_negativo_plr <- covid_leve_e_controle_negativo_reduced_plr$PLT_x_LYM

TG.ROC(ref_leve_e_controle_negativo_plr,
       test_leve_e_controle_negativo_plr,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC PLR", sub = "Controle Negativo x Leve")

# GLR ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
ref_leve_e_controle_negativo_glr <- covid_leve_e_controle_negativo_replaced$classificacao
test_leve_e_controle_negativo_glr <- covid_leve_e_controle_negativo_replaced$GLR

TG.ROC(ref_leve_e_controle_negativo_glr,
       test_leve_e_controle_negativo_glr,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC GLR", sub = "Controle Negativo x Leve")


# HGB ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
ref_leve_e_controle_negativo_hgb <- covid_leve_e_controle_negativo_replaced$classificacao
test_leve_e_controle_negativo_hgb <- covid_leve_e_controle_negativo_replaced$hemoglobina

TG.ROC(ref_leve_e_controle_negativo_hgb,
       test_leve_e_controle_negativo_hgb,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC HGB", sub = "Controle Negativo x Leve")



# Monocitos ------------------------------------------------------------------
#-----------------------------------------------------------------------------

ref_leve_e_controle_negativo_monocitos <- covid_leve_e_controle_negativo_replaced$classificacao

TG.ROC(ref_leve_e_controle_negativo_monocitos,
       monocitos_covid_leve_e_controle_negativo_replaced,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC Monócitos %", sub = "Controle Negativo x Leve")



# negativo e internado evoluiu -----------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

data_II <- read.csv("dados_para_analise_II.csv")

covid_negativo_e_internado_evoluiu <- data_II %>% filter(
  classificacao %in% c("NEGATIVO", "IE")
)

covid_negativo_e_internado_evoluiu_replaced <- covid_negativo_e_internado_evoluiu %>%
  mutate(
    classificacao = str_replace_all(classificacao, "IE", "1"),
    classificacao = str_replace_all(classificacao, "NEGATIVO", "0")
  )

covid_negativo_e_internado_evoluiu_replaced$classificacao <-
  as.numeric(covid_negativo_e_internado_evoluiu_replaced$classificacao)

monocitos_covid_negativo_e_internado_evoluiu <-
  covid_negativo_e_internado_evoluiu_replaced$monocitos_absoluto / 
  sum(covid_negativo_e_internado_evoluiu_replaced$monocitos_absoluto)
test_covid_negativo_e_internado_evoluiu_replaced <- covid_negativo_e_internado_evoluiu_replaced

# PLR ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#covid_covid_negativo_e_internado_evoluiu_plr <- filter(
#  covid_covid_negativo_e_internado_evoluiu, PLT_x_LYM < 110
#)

ref_covid_negativo_e_internado_evoluiu_plr <- covid_negativo_e_internado_evoluiu_replaced$classificacao
test_covid_negativo_e_internado_evoluiu_plr <- as.numeric(covid_negativo_e_internado_evoluiu_replaced$PLT_x_LYM)

TG.ROC(ref_covid_negativo_e_internado_evoluiu_plr,
       test_covid_negativo_e_internado_evoluiu_plr,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC PLR", sub = "Controle Negativo x Internado (Evoluiu)")

# GLR ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
ref_covid_negativo_e_internado_evoluiu_glr <- covid_negativo_e_internado_evoluiu_replaced$classificacao
test_covid_negativo_e_internado_evoluiu_glr <- covid_negativo_e_internado_evoluiu_replaced$GLR

TG.ROC(ref_covid_negativo_e_internado_evoluiu_glr,
       test_covid_negativo_e_internado_evoluiu_glr,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC GLR", sub = "Controle Negativo x Internado (Evoluiu)")



# HGB ------------------------------------------------------------------------
#-----------------------------------------------------------------------------
ref_covid_negativo_e_internado_evoluiu_hgb <- covid_negativo_e_internado_evoluiu_replaced$classificacao
test_covid_negativo_e_internado_evoluiu_hgb <- covid_negativo_e_internado_evoluiu_replaced$hemoglobina

TG.ROC(ref_covid_negativo_e_internado_evoluiu_hgb,
       test_covid_negativo_e_internado_evoluiu_hgb,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC HGB", sub = "Controle Negativo x Internado (Evoluiu)")



# Monocitos ------------------------------------------------------------------
#-----------------------------------------------------------------------------

ref_covid_negativo_e_internado_evoluiu_monocitos <- covid_negativo_e_internado_evoluiu_replaced$classificacao

TG.ROC(ref_covid_negativo_e_internado_evoluiu_monocitos,
       monocitos_covid_negativo_e_internado_evoluiu,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC Monócitos %", sub = "Controle Negativo x Internado (Evoluiu)")

# LEVE E INTERNADO EVOLUIU -----------------------------------------------
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

leve_e_internado_evoluiu <- data_II %>% filter(
  classificacao %in% c("Leve", "IE")
)

leve_e_internado_evoluiu_replaced <- leve_e_internado_evoluiu %>%
  mutate(
    classificacao = str_replace_all(classificacao, "IE", "1"),
    classificacao = str_replace_all(classificacao, "Leve", "0")
  )

leve_e_internado_evoluiu_replaced$classificacao <-
  as.numeric(leve_e_internado_evoluiu_replaced$classificacao)

leve_e_internado_evoluiu_replaced$PLT_x_LYM <- as.numeric(leve_e_internado_evoluiu_replaced$PLT_x_LYM)

monocitos_leve_e_internado_evoluiu <-
  leve_e_internado_evoluiu$monocitos_absoluto / 
  sum(leve_e_internado_evoluiu$monocitos_absoluto)
test_leve_e_internado_evoluiu_replaced <- monocitos_leve_e_internado_evoluiu

# PLR -------------------------------------------------------------------------

leve_e_internado_evoluiu_replaced_reduced_plr <- filter(
  leve_e_internado_evoluiu_replaced, PLT_x_LYM < 110
)

ref_leve_e_internado_evoluiu_plr <- leve_e_internado_evoluiu_replaced_reduced_plr$classificacao
test_leve_e_internado_evoluiu_plr <- leve_e_internado_evoluiu_replaced_reduced_plr$PLT_x_LYM

TG.ROC(ref_leve_e_internado_evoluiu_plr,
       test_leve_e_internado_evoluiu_plr,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC PLR", sub = "Leve x Internado (Evoluiu)")

# GLR -------------------------------------------------------------------------

ref_leve_e_internado_evoluiu_glr <- leve_e_internado_evoluiu_replaced$classificacao
test_leve_e_internado_evoluiu_glr <- leve_e_internado_evoluiu_replaced$GLR

TG.ROC(ref_leve_e_internado_evoluiu_glr,
       test_leve_e_internado_evoluiu_glr,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC GLR", sub = "Leve x Internado (Evoluiu)")

# HGB -------------------------------------------------------------------------

ref_leve_e_internado_evoluiu_hgb <- leve_e_internado_evoluiu_replaced$classificacao
test_leve_e_internado_evoluiu_hgb <- leve_e_internado_evoluiu_replaced$hemoglobina

TG.ROC(ref_leve_e_internado_evoluiu_hgb,
       test_leve_e_internado_evoluiu_hgb,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC HGB", sub = "Leve x Internado (Evoluiu)")

# MONOCITOS -------------------------------------------------------------------

ref_leve_e_internado_evoluiu_replaced_monocitos <- leve_e_internado_evoluiu_replaced$classificacao

TG.ROC(ref_leve_e_internado_evoluiu_replaced_monocitos,
       monocitos_leve_e_internado_evoluiu,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC Monócitos %", sub = "Leve x Internado (Evoluiu)")

# GRAVES E INTERNADO EVOLUIU -----------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

internado_e_internado_evoluiu <- data_II %>% filter(
  classificacao %in% c("Internado", "IE")
)

internado_e_internado_evoluiu_replaced <- internado_e_internado_evoluiu %>%
  mutate(
    classificacao = str_replace_all(classificacao, "Internado", "1"),
    classificacao = str_replace_all(classificacao, "IE", "0")
  )

internado_e_internado_evoluiu_replaced$classificacao <-
  as.numeric(internado_e_internado_evoluiu_replaced$classificacao)

internado_e_internado_evoluiu_replaced$PLT_x_LYM <- as.numeric(internado_e_internado_evoluiu_replaced$PLT_x_LYM)

monocitos_internado_e_internado_evoluiu <-
  internado_e_internado_evoluiu$monocitos_absoluto / 
  sum(internado_e_internado_evoluiu$monocitos_absoluto)
test_internado_e_internado_evoluiu_replaced <- monocitos_internado_e_internado_evoluiu

# plr ------------------------------------------------------------------------

internado_e_internado_evoluiu_replaced_reduced_plr <- filter(
  internado_e_internado_evoluiu_replaced, PLT_x_LYM < 70
)

ref_internado_e_internado_evoluiu_plr <- internado_e_internado_evoluiu_replaced_reduced_plr$classificacao
test_internado_e_internado_evoluiu_plr <- internado_e_internado_evoluiu_replaced_reduced_plr$PLT_x_LYM

TG.ROC(ref_internado_e_internado_evoluiu_plr,
       test_internado_e_internado_evoluiu_plr,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC PLR", sub = "Internado x Internado (Evoluiu)")

# glr ------------------------------------------------------------------------

ref_internado_e_internado_evoluiu_glr <- internado_e_internado_evoluiu_replaced$classificacao
test_internado_e_internado_evoluiu_glr <- internado_e_internado_evoluiu_replaced$GLR

TG.ROC(ref_internado_e_internado_evoluiu_glr,
       test_internado_e_internado_evoluiu_glr,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC GLR", sub = "Internado x Internado (Evoluiu)")

# hgb ------------------------------------------------------------------------

ref_internado_e_internado_evoluiu_hgb <- internado_e_internado_evoluiu_replaced$classificacao
test_internado_e_internado_evoluiu_hgb <- internado_e_internado_evoluiu_replaced$hemoglobina

TG.ROC(ref_internado_e_internado_evoluiu_hgb,
       test_internado_e_internado_evoluiu_hgb,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC HGB", sub = "Internado x Internado (Evoluiu)")

# monocitos ------------------------------------------------------------------------

ref_internado_e_internado_evoluiu_monocitos <- internado_e_internado_evoluiu_replaced$classificacao

TG.ROC(ref_internado_e_internado_evoluiu_monocitos,
       monocitos_internado_e_internado_evoluiu,
       # Se.criterion = 0.95,
       # Sp.criterion = 0.95,
       model='binormal', plot=T,
       position.legend = "right") + title("TG-ROC Monocitos %", sub = "Internado x Internado (Evoluiu)")

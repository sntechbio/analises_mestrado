library(dplyr)
library(stringr)
library(tidymodels)
library(recipes)
library(ggplot2)

path = "/home/leandro/Documentos/SNTechBio/Stephanie"
setwd(path)

data = read.csv("gene_presence_absence.csv")
host = read.csv("Saureus_host.csv")

binary_data <- data[,15:451]

binary_data[binary_data == ""] <- NA
binary_data[!is.na(binary_data)] <- "1"
binary_data[is.na(binary_data)] <- "0"

binary_data_t = as.data.frame(
  t(binary_data)
  )

names(binary_data_t) <- data$Gene

binary_data_t_convert_numeric <- as.data.frame(
  apply(
    binary_data_t, 2, as.numeric)
  )

binary_data_t_convert_numeric["host"] <- host$Host

binary_data_t_convert_numeric$host <- str_replace_all(
  binary_data_t_convert_numeric$host,
                pattern=" ",
                repl=""
  )

pca_rec = recipe(~ ., data = binary_data_t_convert_numeric) %>%
  update_role(host, new_role="id") %>%
  step_pca(all_predictors())

pca_prep = prep(pca_rec)
pca_prep

juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = host)) +
  geom_point(aes(color = host), alpha = 0.7, size = 2) +
#  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

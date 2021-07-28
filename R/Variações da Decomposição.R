## Testando Novas Variacoes da Decomposicao
# Author: Rafaella Carnevali
# Build under R version 4.1.0

options(scipen = 9999)
source("R/Funcoes.r")

## Pacotes
.packages = c("devtools", "stringr", "foreign", "Hmisc",
              "scales", "zoo", "janitor", "ggplot2", "tidyr",
              "dplyr", "data.table", "gt")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0)
  install.packages(.packages[!.inst],dependencies = T)

# Load packages into session
lapply(.packages, require, character.only = T)

#devtools::install_github("josehcms/fertestr")
library(fertestr)

##----------------------------------------------------------------------------------------------------------
## ATENCAO

# Para obter os dados neste script, primeiro rode o "2. Criando SINASC fecundidade 2000-2019.R 
  # e depois 4.Estimando Decomposição.R"

##----------------------------------------------------------------------------------------------------------

## MANTENDO A RMM CONSTANTE E MUDANDO A FECUNDIDADE

## 1) Criando o novo banco de dados
RMM.Const <- TFT.2000.2019

RMM.Const$RMM.Ibge[RMM.Const$Ano == 2019] <- RMM.Const$RMM.Ibge[RMM.Const$Ano == 2009]
RMM.Const$RMM.Ibge[RMM.Const$Ano == 2014] <- RMM.Const$RMM.Ibge[RMM.Const$Ano == 2009]

RMM.Const %>%
  filter(Ano == 2009 | Ano == 2019,
         Estado == 99) %>%
  View()
  
################################

## 2) Estimando em loop decomposicao 2009-2019 com RMM constante ##

UnidFed <- unique(RMM.Const$sigla)

datalist <- list()

for (i in UnidFed) {
  
  print(paste("Processing", i, sep = " "))
  
  dat <- Decomp.MMR(RMM.Const, 2009, 2014, 2019, i, ajuste = T)
  datalist[[i]] <- dat # add it to your list

}

Decomp.RMM.Const <- do.call(rbind, datalist)


rm(datalist, dat)




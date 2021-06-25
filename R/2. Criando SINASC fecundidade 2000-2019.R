# Criando as bases de fecundidade Sinasc 2000-2019
  # Fonte: Nascimentos Sinasc & Mulheres DataSUS (2000-2009) e Projeções IBGE (2010-2019) 
# Author: Rafaella Carnevali
# Build under R version 4.1.0

options(scipen = 9999)

## Pacotes
.packages = c("devtools", "stringr", "foreign", "Hmisc",
              "janitor", "ggplot2", "tidyr",
              "dplyr", "data.table")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0)
  install.packages(.packages[!.inst],dependencies = T)

# Load packages into session
lapply(.packages, require, character.only = T)

## Importando os dados
nascimentos.W.2000.2010 <- read.csv('data/Nascimentos BR e UFs (wide) - 2000-2010.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
mulheres.W.2000.2010 <- read.csv('data/Pop Feminina BR e UFs - 2000-2010.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')

nascimentos.W.2011.2019 <- read.csv('data/Nascimentos BR e UFs (wide) - 2011-2019.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
mulheres.W.2011.2019 <- read.csv('data/Pop Feminina BR e UFs - 2011-2019.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')

##----------------------------------------------------------------------------------------------------------

## Juntando Nascimentos e Mulheres - 2000 até 2010 - Dados Sinasc e Estimações de pop IBGE

nascimentos.2000.2010 <- nascimentos.W.2000.2010 %>%
  pivot_longer(!c(UF, Ano), names_to = "idade",  values_to = "Filhos.tidos.no.ultimo.ano") %>%
  mutate(Filhos.tidos.no.ultimo.ano = round(Filhos.tidos.no.ultimo.ano), 
         idade = as.numeric(substr(idade, start = 2, stop = 3)),
         Grupos.de.idade = case_when(idade == 10 ~ "10 a 14 anos",
                                     idade == 15 ~ "15 a 19 anos",
                                     idade == 20 ~ "20 a 24 anos",
                                     idade == 25 ~ "25 a 29 anos",
                                     idade == 30 ~ "30 a 34 anos",
                                     idade == 35 ~ "35 a 39 anos",
                                     idade == 40 ~ "40 a 44 anos",
                                     idade == 45 ~ "45 a 49 anos",
                                     idade == 50 ~ "50 a 54 anos",
                                     idade == 55 ~ "55 a 59 anos")) %>%
  select(UF, Ano, Grupos.de.idade, Filhos.tidos.no.ultimo.ano, idade)


mulheres.2000.2010 <- mulheres.W.2000.2010 %>%
  pivot_longer(!c(UF, Grupos.de.idade), names_to = "Ano",  values_to = "Mulheres") %>%
  mutate(Mulheres = round(Mulheres), 
         Ano = as.numeric(substr(Ano, start = 2, stop = 5)),
         idade = as.numeric(substr(Grupos.de.idade, start = 1, stop = 2))) %>%
  select(UF, Ano, Grupos.de.idade, Mulheres, idade) 


fecundidade.2000.2010 <- mulheres.2000.2010 %>%
  left_join(nascimentos.2000.2010, by = c("UF", "Ano", "Grupos.de.idade", "idade")) %>%
  select(UF, Ano, Grupos.de.idade, Mulheres, Filhos.tidos.no.ultimo.ano)

rm(nascimentos.W.2000.2010, mulheres.W.2000.2010, nascimentos.2000.2010, mulheres.2000.2010)

################################

## Juntando Nascimentos e Mulheres - 2011 até 2019 - Dados Sinasc e Estimações de pop IBGE

nascimentos.2011.2019 <- nascimentos.W.2011.2019 %>%
  pivot_longer(!c(UF, Ano), names_to = "idade",  values_to = "Filhos.tidos.no.ultimo.ano") %>%
  mutate(Filhos.tidos.no.ultimo.ano = round(Filhos.tidos.no.ultimo.ano), 
         idade = as.numeric(substr(idade, start = 2, stop = 3)),
         Grupos.de.idade = case_when(idade == 10 ~ "10 a 14 anos",
                                     idade == 15 ~ "15 a 19 anos",
                                     idade == 20 ~ "20 a 24 anos",
                                     idade == 25 ~ "25 a 29 anos",
                                     idade == 30 ~ "30 a 34 anos",
                                     idade == 35 ~ "35 a 39 anos",
                                     idade == 40 ~ "40 a 44 anos",
                                     idade == 45 ~ "45 a 49 anos",
                                     idade == 50 ~ "50 a 54 anos",
                                     idade == 55 ~ "55 a 59 anos")) %>%
  select(UF, Ano, Grupos.de.idade, Filhos.tidos.no.ultimo.ano, idade)


mulheres.2011.2019 <- mulheres.W.2011.2019 %>%
  pivot_longer(!c(UF, Grupos.de.idade), names_to = "Ano",  values_to = "Mulheres") %>%
  mutate(Mulheres = round(Mulheres), 
         Ano = as.numeric(substr(Ano, start = 2, stop = 5)),
         idade = as.numeric(substr(Grupos.de.idade, start = 1, stop = 2))) %>%
  select(UF, Ano, Grupos.de.idade, Mulheres, idade) 


fecundidade.2011.2019 <- mulheres.2011.2019 %>%
  left_join(nascimentos.2011.2019, by = c("UF", "Ano", "Grupos.de.idade", "idade")) %>%
  select(UF, Ano, Grupos.de.idade, Mulheres, Filhos.tidos.no.ultimo.ano)

rm(nascimentos.W.2011.2019, mulheres.W.2011.2019, nascimentos.2011.2019, mulheres.2011.2019)

##----------------------------------------------------------------------------------------------------------
## Organizando o Banco de Dados

fecundidade.2000.2010 <- fecundidade.2000.2010 %>%
  mutate(idade = as.numeric(substr(Grupos.de.idade, start = 1, stop = 2)),
         Estado = case_when(UF == "Rondônia" ~ 11,
                            UF == "Acre" ~ 12,
                            UF == "Amazonas" ~ 13,
                            UF == "Roraima" ~ 14,
                            UF == "Pará" ~ 15,
                            UF == "Amapá" ~ 16,
                            UF == "Tocantins" ~ 17,
                            UF == "Maranhão" ~ 21,
                            UF == "Piauí" ~ 22,
                            UF == "Ceará" ~ 23,
                            UF == "Rio Grande do Norte" ~ 24,
                            UF == "Paraíba" ~ 25,
                            UF == "Pernambuco" ~ 26,
                            UF == "Alagoas" ~ 27,
                            UF == "Sergipe" ~ 28,
                            UF == "Bahia" ~ 29,
                            UF == "Minas Gerais" ~ 31,
                            UF == "Espírito Santo" ~ 32,
                            UF == "Rio de Janeiro" ~ 33,
                            UF == "São Paulo" ~ 35,
                            UF == "Paraná" ~ 41 ,
                            UF == "Santa Catarina" ~ 42,
                            UF == "Rio Grande do Sul" ~ 43,
                            UF == "Mato Grosso do Sul" ~ 50,
                            UF == "Mato Grosso" ~ 51,
                            UF == "Goiás" ~ 52,
                            UF == "Distrito Federal" ~ 53,
                            UF == "Brasil" ~ 99),
         region = case_when(11 <= Estado & Estado <= 17 ~ "Norte",
                            21 <= Estado & Estado <= 29 ~ "Nordeste",
                            (31 <= Estado & Estado <= 33) | Estado == 35 ~ "Sudeste",
                            41 <= Estado & Estado <= 43 ~ "Sul",
                            50 <= Estado & Estado <= 53 ~ "Centro-Oeste",
                            Estado > 55 ~ "Brasil"),
         sigla = case_when(UF == "Rondônia" ~ "RO",
                           UF == "Acre" ~ "AC",
                           UF == "Amazonas" ~ "AM",
                           UF == "Roraima" ~ "RR",
                           UF == "Pará" ~ "PA",
                           UF == "Amapá" ~ "AP",
                           UF == "Tocantins" ~ "TO",
                           UF == "Maranhão" ~ "MA",
                           UF == "Piauí" ~ "PI",
                           UF == "Ceará" ~ "CE",
                           UF == "Rio Grande do Norte" ~ "RN",
                           UF == "Paraíba" ~ "PB",
                           UF == "Pernambuco" ~ "PE",
                           UF == "Alagoas" ~ "AL",
                           UF == "Sergipe" ~ "SE",
                           UF == "Bahia" ~ "BA",
                           UF == "Minas Gerais" ~ "MG",
                           UF == "Espírito Santo" ~ "ES",
                           UF == "Rio de Janeiro" ~ "RJ",
                           UF == "São Paulo" ~ "SP",
                           UF == "Paraná" ~ "PR",
                           UF == "Santa Catarina" ~ "SC",
                           UF == "Rio Grande do Sul" ~ "RS",
                           UF == "Mato Grosso do Sul" ~ "MS",
                           UF == "Mato Grosso" ~ "MT",
                           UF == "Goiás" ~ "GO",
                           UF == "Distrito Federal" ~ "DF",
                           UF == "Brasil" ~ "BR"),
         ASFR = Filhos.tidos.no.ultimo.ano/Mulheres) %>%
  filter(idade >=  15,
         idade <= 45)

################################

fecundidade.2011.2019 <- fecundidade.2011.2019 %>%
  mutate(Filhos.tidos.no.ultimo.ano = round(Filhos.tidos.no.ultimo.ano),
         Estado = case_when(UF == "Rondônia" ~ 11,
                            UF == "Acre" ~ 12,
                            UF == "Amazonas" ~ 13,
                            UF == "Roraima" ~ 14,
                            UF == "Pará" ~ 15,
                            UF == "Amapá" ~ 16,
                            UF == "Tocantins" ~ 17,
                            UF == "Maranhão" ~ 21,
                            UF == "Piauí" ~ 22,
                            UF == "Ceará" ~ 23,
                            UF == "Rio Grande do Norte" ~ 24,
                            UF == "Paraíba" ~ 25,
                            UF == "Pernambuco" ~ 26,
                            UF == "Alagoas" ~ 27,
                            UF == "Sergipe" ~ 28,
                            UF == "Bahia" ~ 29,
                            UF == "Minas Gerais" ~ 31,
                            UF == "Espírito Santo" ~ 32,
                            UF == "Rio de Janeiro" ~ 33,
                            UF == "São Paulo" ~ 35,
                            UF == "Paraná" ~ 41 ,
                            UF == "Santa Catarina" ~ 42,
                            UF == "Rio Grande do Sul" ~ 43,
                            UF == "Mato Grosso do Sul" ~ 50,
                            UF == "Mato Grosso" ~ 51,
                            UF == "Goiás" ~ 52,
                            UF == "Distrito Federal" ~ 53,
                            UF == "Brasil" ~ 99),
         region = case_when(11 <= Estado & Estado <= 17 ~ "Norte",
                            21 <= Estado & Estado <= 29 ~ "Nordeste",
                            (31 <= Estado & Estado <= 33) | Estado == 35 ~ "Sudeste",
                            41 <= Estado & Estado <= 43 ~ "Sul",
                            50 <= Estado & Estado <= 53 ~ "Centro-Oeste",
                            Estado > 55 ~ "Brasil"),
         sigla = case_when(UF == "Rondônia" ~ "RO",
                           UF == "Acre" ~ "AC",
                           UF == "Amazonas" ~ "AM",
                           UF == "Roraima" ~ "RR",
                           UF == "Pará" ~ "PA",
                           UF == "Amapá" ~ "AP",
                           UF == "Tocantins" ~ "TO",
                           UF == "Maranhão" ~ "MA",
                           UF == "Piauí" ~ "PI",
                           UF == "Ceará" ~ "CE",
                           UF == "Rio Grande do Norte" ~ "RN",
                           UF == "Paraíba" ~ "PB",
                           UF == "Pernambuco" ~ "PE",
                           UF == "Alagoas" ~ "AL",
                           UF == "Sergipe" ~ "SE",
                           UF == "Bahia" ~ "BA",
                           UF == "Minas Gerais" ~ "MG",
                           UF == "Espírito Santo" ~ "ES",
                           UF == "Rio de Janeiro" ~ "RJ",
                           UF == "São Paulo" ~ "SP",
                           UF == "Paraná" ~ "PR",
                           UF == "Santa Catarina" ~ "SC",
                           UF == "Rio Grande do Sul" ~ "RS",
                           UF == "Mato Grosso do Sul" ~ "MS",
                           UF == "Mato Grosso" ~ "MT",
                           UF == "Goiás" ~ "GO",
                           UF == "Distrito Federal" ~ "DF",
                           UF == "Brasil" ~ "BR"),
         idade = as.numeric(substr(Grupos.de.idade, start = 1, stop = 2)),
         ASFR = Filhos.tidos.no.ultimo.ano/Mulheres) %>% 
  filter(idade >=  15,
         idade <= 45)


fecundidade.2000.2019 <- rbind(fecundidade.2000.2010, fecundidade.2011.2019)

rm(fecundidade.2000.2010, fecundidade.2011.2019)
  
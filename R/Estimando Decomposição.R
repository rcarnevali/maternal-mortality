### Decomposição da MMR ###
# Author: Rafaella Carnevali
# Build under R version 4.0.3
options(scipen = 9999)

## Pacotes
.packages = c("devtools", "stringr", "foreign", "Hmisc",
              "scales", "zoo", "janitor", "ggplot2", "tidyr",
              "dplyr", "data.table")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0)
  install.packages(.packages[!.inst],dependencies = T)

# Load packages into session
lapply(.packages, require, character.only = T)

devtools::install_github("josehcms/fertestr")
library(fertestr)

##----------------------------------------------------------------------------------------------------------

## Importando dados de Populacao
fecundidade2000.2010 <- read.csv('data/Fecundidade 2000-2010 Censo.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
fecundidade2011.2018 <- read.csv('data/Fecundidade 2011-2018 Sinasc.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
Obitos.maternos2000.2018 <- read.csv('data/Prop Obitos Maternos 2000-2018.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
BR.Pop <- read.csv('data/Pop BR 00, 10 e 18.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')

##----------------------------------------------------------------------------------------------------------

## Organizando o Banco de Dados

################################
## 1) Censos 2000 e 2010 - Fecundidade idade 15-45 ##
fecundidade2000.2010 <- fecundidade2000.2010 %>%
  mutate(Estado = case_when(UF == "Rondônia" ~ 11,
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
         Parity = Filhos.Tidos.Nascidos.Vivos/Mulheres,
         ASFR = Filhos.tidos.no.ultimo.ano/Mulheres) %>%
  filter(idade >=  15,
         idade <= 45)

################################

## 2) SINASC - Fecundidade idade 15-45 ##
fecundidade2011.2018 <- fecundidade2011.2018 %>%
  rename (Grupos.de.idade = Idade) %>%
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

################################

## 3) Obitos Maternos ##
Obitos.maternos2000.2018 <- Obitos.maternos2000.2018 %>%
  group_by(UF) %>%
  mutate( #Obitos.maternos2000.2018,  
    across("Prop.Incremento", ~(
      function(x) {
        for(i in 1:length(x))
        {
          if(is.na(x[i]) & Ano[i] < 2009)
            x[i] <- x[Ano == 2009]
          
          else if(is.na(x[i]) & Ano[i] >= 2015)
            x[i] <- x[Ano == 2015]
          
        }
        return(x)
      } ) # end of function spec
      (.) ) # end of across spec
  ) %>% # end of mutate
  ungroup() %>%
  mutate(Obitos.Ajust = Obitos.Obs * (Prop.Incremento + 100) / 100,
         Fator = Obitos.Obs / Obitos.Ajust,
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
                           UF == "Brasil" ~ "BR")) %>%
  arrange(Estado, Ano)

##----------------------------------------------------------------------------------------------------------

## Calculando a TEF por diferentes métodos para UFs

################################
## 1) Gompertz ##
novo <- NULL
TEF.2000.2010 <- NULL

for (j in unique(fecundidade2000.2010$Ano)) {
  for (i in  unique(fecundidade2000.2010$Estado)) {
    cat(paste("\nProcessing UF", i, "ano", j, sep = " "))
    
    data <- fecundidade2000.2010 %>%
      filter(Estado == i,
             Ano == j)  
    
    Gompertz2000.2010 <- fertGompPF(ages = data$idade, P = data$Parity, asfr = data$ASFR, level = T, plot.diagnostic = F)[[1]]
    
    Gompertz2000.2010 <- Gompertz2000.2010 %>%
      mutate(Estado = i,
             Ano = j, 
             idade = as.numeric(substr(age.group, start = 1, stop = 2))) %>%
      filter(idade >= 15) %>%
      select(Estado, Ano, idade, asfr.adj) %>%
      rename(Gompertz = asfr.adj) %>%
      left_join(fecundidade2000.2010, by = c("Estado", "Ano", "idade"))  %>%
      select(Estado, UF, region, Ano, Grupos.de.idade, idade, Mulheres, Filhos.Tidos.Nascidos.Vivos, Filhos.tidos.no.ultimo.ano, 
             Parity, ASFR, Gompertz)
    
    TEF.2000.2010 <- rbind(Gompertz2000.2010, TEF.2000.2010)
    
    Gompertz.sem.P <- fertGompPF(ages = data$idade, asfr = data$ASFR, level = F, plot.diagnostic = F)[[1]]
    
    Gompertz.sem.P <- Gompertz.sem.P %>%
      mutate(Estado = i,
             Ano = j,
             idade = as.numeric(substr(age.group, start = 1, stop = 2))) %>%
      filter(idade >= 15) %>%
      select(Estado, Ano, idade, asfr.adj) %>%
      rename(Gompertz.sem.P = asfr.adj) %>%
      left_join(fecundidade2000.2010, by = c("Estado", "Ano", "idade"))  %>%
      select(Estado, UF, region, Ano, idade, Gompertz.sem.P)
    
    novo <- rbind(Gompertz.sem.P, novo)
  }
}

################################
## 2) Brass, Coale e Trussel ##
TEF.2000.2010 <- TEF.2000.2010 %>%
  left_join(novo, by = c("Estado", "Ano", "idade", "region", "UF")) %>%
  group_by(Ano, Estado) %>%
  mutate(Brass = unlist(do.call(rbind, lapply(fertBrassPF(ages = idade, P = Parity, asfr = ASFR)[1], as.data.frame)) %>%
                          select(adj_asfr)),
         Coale.Trussel = unlist(do.call(rbind, lapply(fertBrassPF.cltrss(ages = idade, P = Parity, asfr = ASFR)[1], as.data.frame)) %>%
                                  select(adj_asfr))) %>%
  select(UF, Grupos.de.idade, Ano, Estado, region, idade, Mulheres, Filhos.Tidos.Nascidos.Vivos, Filhos.tidos.no.ultimo.ano, 
         Parity, ASFR, Brass, Coale.Trussel, Gompertz, Gompertz.sem.P) %>%
  ungroup() %>%
  as.data.frame()

rm(novo, data, Gompertz.sem.P, Gompertz2000.2010)

################################
## 3) Calculando os ajustes de cada método por idade ##
TEF.2000.2010 <- TEF.2000.2010 %>%
  mutate(ratio.Obs.Brass = Brass/ASFR,
         ratio.Obs.CT = Coale.Trussel/ASFR,
         ratio.Obs.Gompertz = Gompertz/ASFR)

##----------------------------------------------------------------------------------------------------------

## Calculando a TFT 

################################
## 1) De 2000-2010 - Censos ## 
Total.2000.2010 <- fecundidade2000.2010 %>%
  group_by(Estado, UF, Ano) %>%
  summarise(Women = sum(Mulheres),
            Filhos.Tidos = sum(Filhos.Tidos.Nascidos.Vivos),
            Nascimentos = sum(Filhos.tidos.no.ultimo.ano),
            TFT = (sum(ASFR)*5)) %>%
  ungroup() %>%
  arrange(Estado, Ano)

################################
## 2) De 2011-2018 - SINASC ## 
Total.2011.2018 <- fecundidade2011.2018 %>%
  group_by(Estado, UF, Ano) %>%
  summarise(Women = sum(Mulheres),
            Nascimentos = sum(Filhos.tidos.no.ultimo.ano),
            TFT = (sum(ASFR)*5)) %>%
  ungroup() %>%
  arrange(Estado, Ano)

##----------------------------------------------------------------------------------------------------------

## Calculando TFT para Gompertz, Brass e Coale & Trussel

################################
## 1) De 2000-2010 - Censos ## 

TFT.2000.2010 <- NULL

for (j in unique(fecundidade2000.2010$Ano)) {
  for (i in unique(fecundidade2000.2010$Estado)) {
    cat(paste("\nProcessing", i, "ano", j, sep = " "))
    data <- fecundidade2000.2010 %>%
      filter(Estado == i,
             Ano == j) 
    
    Brass <- fertBrassPF(ages = data$idade, P = data$Parity, asfr = data$ASFR)[[3]]
    Coale.Trussel <- fertBrassPF.cltrss(ages = data$idade, P = data$Parity, asfr = data$ASFR)[[3]]
    Gompertz <- fertGompPF(ages = data$idade, P = data$Parity, asfr = data$ASFR, level = T, plot.diagnostic = F)[[2]]
    Gompertz.sem.P <- fertGompPF(ages = data$idade, asfr = data$ASFR, level = F, plot.diagnostic = F)[[2]]
    Gompertz.sem.P <- Gompertz.sem.P$TFR.adj
    novo <- cbind(Ano = j, Estado = i, Brass, Coale.Trussel, Gompertz, Gompertz.sem.P)
    TFT.2000.2010 <-  rbind(TFT.2000.2010, novo)
    
  }
}

TFT.2000.2010 <- TFT.2000.2010 %>%
  rename (Gompertz = TFR.adj) %>%
  right_join(Total.2000.2010, by = c("Ano", "Estado")) %>%
  select(Ano, UF, Estado, Women, Filhos.Tidos, Nascimentos, TFT, 
         Brass, Coale.Trussel, Gompertz, Gompertz.sem.P) %>%
  mutate(ratio.Obs.Brass = Brass/TFT,
         ratio.Obs.CT = Coale.Trussel/TFT,
         ratio.Obs.Gompertz = Gompertz/TFT,
         ratio.Obs.Gompertz.sem.P = Gompertz.sem.P/TFT) %>%
  ungroup()

rm(Brass, Coale.Trussel, Gompertz, Gompertz.sem.P, novo, data)

################################
## 3) De 2011-2018 - SINASC ##

TFT.2011.2018 <- NULL

for (j in unique(fecundidade2011.2018$Ano)) {
  for (i in unique(fecundidade2011.2018$Estado)) {
    cat(paste("\nProcessing", i, "ano", j, sep = " "))
    
    data <- fecundidade2011.2018 %>%
      filter(Estado == i,
             Ano == j) 
    
    Gompertz.TFT.11.18 <- fertGompPF(ages = data$idade, asfr = data$ASFR, level = F, plot.diagnostic = F)[[2]]
    novo <- cbind(Ano = j, Estado = i, Gompertz.TFT.11.18)
    TFT.2011.2018 <-  rbind(TFT.2011.2018, novo)
  }
}

TFT.2011.2018 <- TFT.2011.2018 %>%
  rename (Gompertz.sem.P = TFR.adj) %>%
  right_join(Total.2011.2018, by = c("Ano", "Estado")) %>%
  select(Ano, UF, Estado, Women, Nascimentos, TFT, Gompertz.sem.P)


rm(novo, Gompertz.TFT.11.18, data)
##----------------------------------------------------------------------------------------------------------

## Interpolando valores de Mulheres, Nascimentos, Filhos Tidos, TFT, Brass, Coale & Trussel, Gompertz e Gompertz sem Parturição

################################
## 1) De 2000-2010 - Censos ## 

Interp.Total.2000.2010 <- NULL

for (i in unique(TFT.2000.2010$Estado)) {
  cat(paste("\nProcessing", i, sep = " "))
  data <- TFT.2000.2010 %>%
    filter(Estado == i)  
  
  interpolacao <- as.data.frame(approx(data$Women, method = "linear", n = 11)[[2]])
  Interpolando.00.10 <- interpolacao
  interpolacao <- as.data.frame(approx(data$Filhos.Tidos, method = "linear", n = 11)[[2]])
  Interpolando.00.10 <- cbind(Interpolando.00.10, interpolacao)
  interpolacao <- as.data.frame(approx(data$Nascimentos, method = "linear", n = 11)[[2]])
  Interpolando.00.10 <- cbind(Interpolando.00.10, interpolacao)
  interpolacao <- as.data.frame(approx(data$TFT, method = "linear", n = 11)[[2]])
  Interpolando.00.10 <- cbind(Interpolando.00.10, interpolacao)
  interpolacao <- as.data.frame(approx(data$Brass, method = "linear", n = 11)[[2]])
  Interpolando.00.10 <- cbind(Interpolando.00.10, interpolacao)
  interpolacao <- as.data.frame(approx(data$Coale, method = "linear", n = 11)[[2]])
  Interpolando.00.10 <- cbind(Interpolando.00.10, interpolacao)
  interpolacao <- as.data.frame(approx(data$Gompertz, method = "linear", n = 11)[[2]])
  Interpolando.00.10 <- cbind(Interpolando.00.10, interpolacao)
  interpolacao <- as.data.frame(approx(data$Gompertz.sem.P, method = "linear", n = 11)[[2]])
  Interpolando.00.10 <- cbind(Interpolando.00.10, interpolacao)
  
  novo <- cbind(Estado = i, Interpolando.00.10)
  Interp.Total.2000.2010 <-  rbind(Interp.Total.2000.2010, novo)
  
}

Interp.Total.2000.2010 <- Interp.Total.2000.2010 %>%
  rename (Mulheres = `approx(data$Women, method = \"linear\", n = 11)[[2]]`,
          Filhos.Tidos = `approx(data$Filhos.Tidos, method = \"linear\", n = 11)[[2]]`,
          Nascimentos = `approx(data$Nascimentos, method = \"linear\", n = 11)[[2]]`,
          TFT = `approx(data$TFT, method = \"linear\", n = 11)[[2]]`,
          Brass = `approx(data$Brass, method = \"linear\", n = 11)[[2]]`,
          Coale.Trussel = `approx(data$Coale, method = \"linear\", n = 11)[[2]]`,
          Gompertz = `approx(data$Gompertz, method = \"linear\", n = 11)[[2]]`,
          Gompertz.sem.P = `approx(data$Gompertz.sem.P, method = \"linear\", n = 11)[[2]]`) %>%
  arrange(Estado) %>%
  group_by(Estado) %>%
  mutate(Ano = seq(2000, 2010),
         Parity = Filhos.Tidos/Mulheres, 
         UF = case_when(Estado == 11 ~ "Rondônia",
                        Estado == 12 ~ "Acre",
                        Estado == 13 ~ "Amazonas",
                        Estado == 14 ~ "Roraima",
                        Estado == 15 ~ "Pará",
                        Estado == 16 ~ "Amapá",
                        Estado == 17 ~ "Tocantins",
                        Estado == 21 ~ "Maranhão",
                        Estado == 22 ~ "Piauí",
                        Estado == 23 ~ "Ceará",
                        Estado == 24 ~ "Rio Grande do Norte",
                        Estado == 25 ~ "Paraíba",
                        Estado == 26 ~ "Pernambuco",
                        Estado == 27 ~ "Alagoas",
                        Estado == 28 ~ "Sergipe",
                        Estado == 29 ~ "Bahia",
                        Estado == 31 ~ "Minas Gerais",
                        Estado == 32 ~ "Espírito Santo",
                        Estado == 33 ~ "Rio de Janeiro",
                        Estado == 35 ~ "São Paulo",
                        Estado == 41 ~ "Paraná",
                        Estado == 42 ~ "Santa Catarina",
                        Estado == 43 ~ "Rio Grande do Sul",
                        Estado == 50 ~ "Mato Grosso do Sul",
                        Estado == 51 ~ "Mato Grosso",
                        Estado == 52 ~ "Goiás",
                        Estado == 53 ~ "Distrito Federal",
                        Estado == 99 ~ "Brasil"),
         ratio.Obs.Brass = Brass/TFT,
         ratio.Obs.CT = Coale.Trussel/TFT,
         ratio.Obs.Gompertz = Gompertz/TFT,
         ratio.Obs.Gompertz.sem.P = Gompertz.sem.P/TFT) %>%
  ungroup() %>%
  group_by(Ano) %>%
  mutate(index = rep(seq(1, 28), times = length(unique(Ano)))) %>%
  ungroup() %>%
  arrange(Ano, Estado) %>%
  select(UF, Estado, Ano, Mulheres, Filhos.Tidos, Nascimentos, Parity, 
         TFT, Brass, Coale.Trussel, Gompertz, Gompertz.sem.P, 
         ratio.Obs.Brass, ratio.Obs.CT, ratio.Obs.Gompertz, ratio.Obs.Gompertz.sem.P, index) %>%
  ungroup() 

rm(data, interpolacao, novo, Interpolando.00.10)

################################
## 2) De 2010-2018 - SINASC ##

Interp.Total.2011.2018 <- NULL

for (i in unique(TFT.2011.2018$Estado)) {
  cat(paste("\nProcessing", i, sep = " "))
  data <- TFT.2011.2018 %>%
    filter(Estado == i)  
  
  interpolacao <- as.data.frame(approx(data$Women, method = "linear", n = 8)[[2]])
  Interpolando.11.18 <- interpolacao
  interpolacao <- as.data.frame(approx(data$Nascimentos, method = "linear", n = 8)[[2]])
  Interpolando.11.18 <- cbind(Interpolando.11.18, interpolacao)
  interpolacao <- as.data.frame(approx(data$TFT, method = "linear", n = 8)[[2]])
  Interpolando.11.18 <- cbind(Interpolando.11.18, interpolacao)
  interpolacao <- as.data.frame(approx(data$Gompertz.sem.P, method = "linear", n = 8)[[2]])
  Interpolando.11.18 <- cbind(Interpolando.11.18, interpolacao)
  novo <- cbind(Estado = i, Interpolando.11.18)
  Interp.Total.2011.2018 <-  rbind(Interp.Total.2011.2018, novo)
  
}

Interp.Total.2011.2018 <- Interp.Total.2011.2018 %>%
  rename (Mulheres = `approx(data$Women, method = \"linear\", n = 8)[[2]]`,
          Nascimentos = `approx(data$Nascimentos, method = \"linear\", n = 8)[[2]]`,
          TFT = `approx(data$TFT, method = \"linear\", n = 8)[[2]]`,
          Gompertz.sem.P = `approx(data$Gompertz.sem.P, method = \"linear\", n = 8)[[2]]`) %>%
  arrange(Estado) %>%
  group_by(Estado) %>%
  mutate(Ano = seq(2011, 2018),
         UF = case_when(Estado == 11 ~ "Rondônia",
                        Estado == 12 ~ "Acre",
                        Estado == 13 ~ "Amazonas",
                        Estado == 14 ~ "Roraima",
                        Estado == 15 ~ "Pará",
                        Estado == 16 ~ "Amapá",
                        Estado == 17 ~ "Tocantins",
                        Estado == 21 ~ "Maranhão",
                        Estado == 22 ~ "Piauí",
                        Estado == 23 ~ "Ceará",
                        Estado == 24 ~ "Rio Grande do Norte",
                        Estado == 25 ~ "Paraíba",
                        Estado == 26 ~ "Pernambuco",
                        Estado == 27 ~ "Alagoas",
                        Estado == 28 ~ "Sergipe",
                        Estado == 29 ~ "Bahia",
                        Estado == 31 ~ "Minas Gerais",
                        Estado == 32 ~ "Espírito Santo",
                        Estado == 33 ~ "Rio de Janeiro",
                        Estado == 35 ~ "São Paulo",
                        Estado == 41 ~ "Paraná",
                        Estado == 42 ~ "Santa Catarina",
                        Estado == 43 ~ "Rio Grande do Sul",
                        Estado == 50 ~ "Mato Grosso do Sul",
                        Estado == 51 ~ "Mato Grosso",
                        Estado == 52 ~ "Goiás",
                        Estado == 53 ~ "Distrito Federal",
                        Estado == 99 ~ "Brasil")) %>%
  select(UF, Estado, Ano, Mulheres, Nascimentos, TFT, Gompertz.sem.P) %>%
  arrange(Estado, Ano) %>%
  ungroup() %>%
  mutate (Brass = NA,
          Coale.Trussel = NA, 
          Gompertz = NA) %>%
  select(Ano, UF, Estado, Mulheres, Nascimentos, TFT, Brass, Coale.Trussel, Gompertz, Gompertz.sem.P) %>%
  ungroup() %>%
  mutate(ratio.Obs.Brass = NA, 
         ratio.Obs.CT = NA,
         ratio.Obs.Gompertz = NA,
         ratio.Obs.Gompertz.sem.P = Gompertz.sem.P/TFT) %>%
  arrange(Ano, Estado) %>%
  group_by(Ano) %>%
  mutate(index = rep(seq(1, 28), times = length(unique(Ano)))) %>%
  ungroup()

rm(data, interpolacao, novo, Interpolando.11.18)

################################
## 3) Ajustando os anos 2011-2018 a partir das razoes de 2010
# Razoes de correcao de 2010 por UF
Brass.2010 <- TFT.2000.2010 %>%
  filter(Ano == 2010) %>%
  select(Ano, UF, Estado, ratio.Obs.Brass)

Coale.Trussel.2010 <- TFT.2000.2010 %>%
  filter(Ano == 2010) %>%
  select(Ano, UF, Estado, ratio.Obs.CT)

Gompertz.2010 <- TFT.2000.2010 %>%
  filter(Ano == 2010) %>%
  select(Ano, UF, Estado, ratio.Obs.Gompertz)

# Ajuste
Interp.Total.2011.2018 <- Interp.Total.2011.2018 %>% 
  group_by(UF, Ano) %>%
  mutate(ratio.Obs.Brass = Brass.2010$ratio.Obs.Brass[Brass.2010$UF == UF],
         ratio.Obs.CT = Coale.Trussel.2010$ratio.Obs.CT[Coale.Trussel.2010$UF == UF],
         ratio.Obs.Gompertz = Gompertz.2010$ratio.Obs.Gompertz[Gompertz.2010$UF == UF]) %>%
  ungroup() %>%
  mutate(Brass = TFT * ratio.Obs.Brass,
         Coale.Trussel = TFT * ratio.Obs.CT,
         Gompertz = TFT * ratio.Obs.Gompertz)

rm(Gompertz.2010, Brass.2010, Coale.Trussel.2010)

################################
## 4) Juntando os bancos para 2000-2018
Interp.Total.2000.2018 <- Interp.Total.2000.2010 %>%
  bind_rows(Interp.Total.2011.2018) %>%
  select(Ano, UF, Estado, Mulheres, Nascimentos, 
         TFT, Brass, Coale.Trussel, Gompertz, Gompertz.sem.P, 
         ratio.Obs.Brass, ratio.Obs.CT, ratio.Obs.Gompertz, ratio.Obs.Gompertz.sem.P) %>%
  arrange(Estado, Ano)

##----------------------------------------------------------------------------------------------------------

# Adicionando o Banco de Obitos Maternos

## 1) Juntando o banco de TFT com o banco de obitos
Interp.Total.2000.2018 <- Interp.Total.2000.2018 %>%
  left_join(Obitos.maternos2000.2018, by = c("Estado", "UF", "Ano")) %>%
  as.data.frame() %>%
  group_by(Ano, Estado) %>%
  mutate(NV.Ajus.Brass = Nascimentos * ratio.Obs.Brass,
         NV.Ajus.CT = Nascimentos * ratio.Obs.CT,
         NV.Ajus.Gompertz = Nascimentos * ratio.Obs.Gompertz,
         NV.Ajus.Gompertz.sem.P = Nascimentos * ratio.Obs.Gompertz.sem.P,
         RMM = (Obitos.Obs / Nascimentos) * 100000,
         RMM.Obt.Corr = (Obitos.Ajust / Nascimentos) * 100000,
         RMM.Brass = (Obitos.Ajust / NV.Ajus.Brass) * 100000,
         RMM.CT = (Obitos.Ajust / NV.Ajus.CT) * 100000,
         RMM.Gompertz = (Obitos.Ajust / NV.Ajus.Gompertz) * 100000,
         RMM.Gompertz.sem.P = (Obitos.Ajust / NV.Ajus.Gompertz.sem.P) * 100000,
         TMM = (Obitos.Obs / Mulheres) * 100000,
         TMM.Ajust = (Obitos.Ajust / Mulheres) * 100000) %>%
  ungroup()

##----------------------------------------------------------------------------------------------------------
# Fonte: Jain, A. K. (2011). Measuring the Effect of Fertility Decline on the Maternal Mortality Ratio. 
#        Studies in Family Planning, 42(4), 247–260. doi:10.1111/j.1728-4465.2011.00288.x 

### Estimation of the effect of declines in maternal mortality ratio (MMR) and fertility on estimated number of maternal ###
 ### deaths averted in 2018 ###

# Usando os dados do BR
BR.Decomp <- Interp.Total.2000.2018 %>%
  filter(sigla == "BR",
         Ano == 2000 | Ano == 2010 | Ano == 2018)  %>% 
  inner_join(BR.Pop, c("Ano" = "Ano")) %>%
  select(Ano, Estado, UF, sigla, region, Pop, Mulheres, Nascimentos, NV.Ajus.CT, NV.Ajus.Gompertz, TFT, RMM, RMM.Obt.Corr, RMM.Brass, 
         RMM.CT, RMM.Gompertz, RMM.Gompertz.sem.P) %>%
  mutate(TBN = (Nascimentos/Pop) * 1000,
         r = NA,
         P_hat = NA, 
         B_hat = NA,
         B = NA,
         D1_hat = NA,
         D2_hat = NA,
         D3_hat = NA,
         D = NA,
         X = NA,
         Y = NA,
         Z = NA)

# r = Annual population growth rate: 2000-2010 and 2010-2018
BR.Decomp$r[BR.Decomp$Ano == 2010] <- ((log(BR.Decomp$Pop[BR.Decomp$Ano == 2010] / BR.Decomp$Pop[BR.Decomp$Ano == 2000])) / 10)
BR.Decomp$r[BR.Decomp$Ano == 2018] <- ((log(BR.Decomp$Pop[BR.Decomp$Ano == 2018] / BR.Decomp$Pop[BR.Decomp$Ano == 2010])) / 8)

# P_hat = 2018 estimated population assuming constant annual growth rate from 2000-2010
BR.Decomp$P_hat[BR.Decomp$Ano == 2018] <- BR.Decomp$Pop[BR.Decomp$Ano == 2000] * (exp(18 * BR.Decomp$r[BR.Decomp$Ano == 2010]))

# B_hat = Projected births in 2018 assuming constant fertility
BR.Decomp$B_hat[BR.Decomp$Ano == 2018] <- BR.Decomp$P_hat[BR.Decomp$Ano == 2018] * BR.Decomp$TBN[BR.Decomp$Ano == 2000] / 1000

# B = Actual births in 2018 
BR.Decomp$B[BR.Decomp$Ano == 2018] <- BR.Decomp$Nascimentos[BR.Decomp$Ano == 2018]
#BR.Decomp$B2[BR.Decomp$Ano == 2018] <- BR.Decomp$P_hat[BR.Decomp$Ano == 2018] * BR.Decomp$TBN[BR.Decomp$Ano == 2018] / 1000


# D1_hat = No change in CBR and no change in MMR
BR.Decomp$D1_hat[BR.Decomp$Ano == 2018] <- BR.Decomp$B_hat[BR.Decomp$Ano == 2018] * BR.Decomp$RMM.CT[BR.Decomp$Ano == 2000] / 100000

# D3_hat = MMR declined but CBR did not
BR.Decomp$D3_hat[BR.Decomp$Ano == 2018] <- BR.Decomp$B_hat[BR.Decomp$Ano == 2018] * BR.Decomp$RMM.CT[BR.Decomp$Ano == 2018] / 100000

# D2_hat = CBR declined but MMR did not
BR.Decomp$D2_hat[BR.Decomp$Ano == 2018] <- BR.Decomp$B[BR.Decomp$Ano == 2018] * BR.Decomp$RMM.CT[BR.Decomp$Ano == 2000] / 100000

# D = Both CBR and MMR declined
BR.Decomp$D[BR.Decomp$Ano == 2018] <- BR.Decomp$B[BR.Decomp$Ano == 2018] * BR.Decomp$RMM.CT[BR.Decomp$Ano == 2018] / 100000


# Y = Total effect of decline in MMR 
BR.Decomp$Y <- BR.Decomp$D1_hat - BR.Decomp$D3_hat

# X = Total effect of fertility decline
BR.Decomp$X <- BR.Decomp$D1_hat - BR.Decomp$D2_hat

# Z = Total effect of declines in both fertility and MMR
BR.Decomp$Z <- BR.Decomp$D1_hat - BR.Decomp$D

BR.Decomp <- BR.Decomp %>%
  mutate(intersec.X.Y = X + Y - Z, # intersec.X.Y = Overlap between the effect of declines in fertility and in MMR
         Alpha = Y - intersec.X.Y, # Alpha = Net effect of decline in MMR
         Beta = X - intersec.X.Y, # Beta = Net effect of fertility decline
         Gama = (Alpha / Z) * 100, # Gama = Effect of safe motherhood on the % of the  potential number of maternal lives saved in 2018
         Delta = (Beta / Z) * 100, # Delta = Effect of decrease in live births on the % of the  potential number of maternal lives saved in 2018
         Omega = (intersec.X.Y / Z) * 100) # Omega = Effect of fertility reduction realized through its effect on MMR reduction

##----------------------------------------------------------------------------------------------------------

### Estimation of the decline in maternal mortality ratio and number of actual maternal deaths attributable to fertility decline ###

BR.Decomp <- BR.Decomp %>%
  mutate(RMM_hat = NA,
         D.00 = NA,
         D.18 = NA, 
         D_hat.18 = NA,
         Iota = NA,
         Kappa = NA,
         Lambda = NA,
         Iota.per = NA,
         Kappa.per = NA,
         Lambda.per = NA,
         Sigma = NA,
         Tau = NA,
         Eta = NA,
         Sigma.per = NA,
         Tau.per = NA,
         Eta.per = NA)

# RMM_hat = MMR in 2018 implied by fertility reduction observed during 2000-2018
BR.Decomp$RMM_hat[BR.Decomp$Ano == 2018] <- BR.Decomp$RMM.CT[BR.Decomp$Ano == 2018] + 
                                             ((BR.Decomp$B[BR.Decomp$Ano == 2018] / BR.Decomp$B_hat[BR.Decomp$Ano == 2018]) *
                                                (BR.Decomp$RMM.CT[BR.Decomp$Ano == 2000] - BR.Decomp$RMM.CT[BR.Decomp$Ano == 2018]))
                                                                      
# D.00 = Maternal deaths in 2000
BR.Decomp$D.00[BR.Decomp$Ano == 2018] <- (BR.Decomp$Nascimentos[BR.Decomp$Ano == 2000] * BR.Decomp$RMM.CT[BR.Decomp$Ano == 2000]) / 100000

# D.18 = Maternal deaths in 2018
BR.Decomp$D.18[BR.Decomp$Ano == 2018] <- (BR.Decomp$Nascimentos[BR.Decomp$Ano == 2018] * BR.Decomp$RMM.CT[BR.Decomp$Ano == 2018]) / 100000

# D_hat.18 = Maternal deaths in 2018 implied by fertility decline observed between 2000 and 2018
BR.Decomp$D_hat.18[BR.Decomp$Ano == 2018] <- (BR.Decomp$Nascimentos[BR.Decomp$Ano == 2018] *  BR.Decomp$RMM_hat[BR.Decomp$Ano == 2018]) / 100000


# Iota = Total decline in MMR between 1990 and 2008 
BR.Decomp$Iota[BR.Decomp$Ano == 2018] <- BR.Decomp$RMM.CT[BR.Decomp$Ano == 2000] - BR.Decomp$RMM.CT[BR.Decomp$Ano == 2018]
BR.Decomp$Iota.per[BR.Decomp$Ano == 2018] <- 100 #(in %)

# Kappa = Decline in MMR attributable to fertility reduction
BR.Decomp$Kappa[BR.Decomp$Ano == 2018] <- BR.Decomp$RMM.CT[BR.Decomp$Ano == 2000] - BR.Decomp$RMM_hat[BR.Decomp$Ano == 2018]
BR.Decomp$Kappa.per[BR.Decomp$Ano == 2018] <- (BR.Decomp$Kappa[BR.Decomp$Ano == 2018] * 100) / BR.Decomp$Iota[BR.Decomp$Ano == 2018] #(in %)

# Lambda = Decline in MMR attributable to safe motherhood initiatives
BR.Decomp$Lambda[BR.Decomp$Ano == 2018] <- BR.Decomp$RMM_hat[BR.Decomp$Ano == 2018] - BR.Decomp$RMM.CT[BR.Decomp$Ano == 2018]
BR.Decomp$Lambda.per[BR.Decomp$Ano == 2018] <- (BR.Decomp$Lambda[BR.Decomp$Ano == 2018] * 100) / BR.Decomp$Iota[BR.Decomp$Ano == 2018] #(in %)


# Sigma = Total decline in actual maternal deaths between 2000 and 2018
BR.Decomp$Sigma[BR.Decomp$Ano == 2018] <- BR.Decomp$D.00[BR.Decomp$Ano == 2018] - BR.Decomp$D.18[BR.Decomp$Ano == 2018] 
BR.Decomp$Sigma.per[BR.Decomp$Ano == 2018] <- 100 #(in %)
  
# Tau = Attributable to fertility decline
BR.Decomp$Tau[BR.Decomp$Ano == 2018] <-  BR.Decomp$D.00[BR.Decomp$Ano == 2018] - BR.Decomp$D_hat.18[BR.Decomp$Ano == 2018]
BR.Decomp$Tau.per[BR.Decomp$Ano == 2018] <- (BR.Decomp$Tau[BR.Decomp$Ano == 2018] * 100) / BR.Decomp$Sigma[BR.Decomp$Ano == 2018] #(in %)

# Eta = Attributable to safe motherhood
BR.Decomp$Eta[BR.Decomp$Ano == 2018] <- BR.Decomp$D_hat.18[BR.Decomp$Ano == 2018] - BR.Decomp$D.18[BR.Decomp$Ano == 2018]
BR.Decomp$Eta.per[BR.Decomp$Ano == 2018] <- (BR.Decomp$Eta[BR.Decomp$Ano == 2018] * 100) / BR.Decomp$Sigma[BR.Decomp$Ano == 2018] #(in %)


teste <- BR.Decomp %>%
  filter(Ano == 2018) %>%
  t()

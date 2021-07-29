### Decomposição da MMR ###
# Author: Rafaella Carnevali
# Build under R version 4.1.0

options(scipen = 9999)

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

# Para obter os dados de fecundidade usados neste script, primeiro rode o "Criando SINASC fecundidade 2000-2019.R"

##----------------------------------------------------------------------------------------------------------

## Importando os dados

Pop.00.30 <- read.csv('data/Pop 00-30.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
Obitos.maternos2000.2019 <- read.csv('data/Prop Obitos Maternos 2000-2019.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
#Fec.proj.20.40 <- read.csv('data/Projecao Fec 2020-2040.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
RMM.ibge.09.18 <- read.csv('data/RMM IBGE 2009-2018.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
#description <- read.csv('data/description decomp.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
description.new <- read.csv('data/description decomp - func.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')

source("R/Funcoes.r")
##----------------------------------------------------------------------------------------------------------

## Organizando o Banco de Dados

## 1) Obitos Maternos ##
Obitos.maternos2000.2019 <- Obitos.maternos2000.2019 %>%
  group_by(UF) %>%
  mutate( 
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

################################

## 2) Populacao Ambos os sexos 2000-2010 ##
Pop.00.30 <- Pop.00.30 %>%
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
                           UF == "Brasil" ~ "BR"))

################################
## 3) RMM extraidas do sistema de vigilância (2009-2018) ##

RMM.ibge.09.18 <- RMM.ibge.09.18 %>%
  rename(UF = Estados) %>%
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
                            UF == "Brasil" ~ 99)) %>%
  pivot_longer(!c(Estado, UF), names_to = "var", values_to = "RMM.Ibge") %>%
  mutate(Ano = rep(seq(2009, 2018, by = 1), 28)) %>%
  select(!var)

##----------------------------------------------------------------------------------------------------------

## Calculando a TEF por diferentes métodos para UFs

# Para obter os dados de fecundidade usados neste script, primeiro rode o "Criando SINASC fecundidade 2000-2019.R"

## 1) Gompertz ##
novo <- NULL
TEF.2000.2019 <- NULL

for (j in unique(fecundidade.2000.2019$Ano)) {
  for (i in  unique(fecundidade.2000.2019$Estado)) {
    cat(paste("\nProcessing UF", i, "ano", j, sep = " "))
    
    data <- fecundidade.2000.2019 %>%
      filter(Estado == i,
             Ano == j)  
    
    Gompertz.sem.P <- fertGompPF(ages = data$idade, asfr = data$ASFR, level = F, plot.diagnostic = F)[[1]]
    
    Gompertz.sem.P <- Gompertz.sem.P %>%
      mutate(Estado = i,
             Ano = j,
             idade = as.numeric(substr(age.group, start = 1, stop = 2))) %>%
      filter(idade >= 15) %>%
      select(Estado, Ano, idade, asfr.adj) %>%
      rename(Gompertz.sem.P = asfr.adj) %>%
      left_join(fecundidade.2000.2019, by = c("Estado", "Ano", "idade"))  %>%
      select(Estado, UF, region, Ano, idade, Gompertz.sem.P)
    
    novo <- rbind(Gompertz.sem.P, novo)
  }
}

fecundidade.2000.2019 <- fecundidade.2000.2019 %>%
  left_join(novo, by = c("Estado", "Ano", "idade", "region", "UF"))

rm(novo, data, Gompertz.sem.P)

################################

## 2) Calculando os ajustes de cada método por idade ##
fecundidade.2000.2019 <- fecundidade.2000.2019 %>%
  mutate(ratio.Obs.Gompertz.sem.P = Gompertz.sem.P/ASFR)

##----------------------------------------------------------------------------------------------------------

## Calculando a TFT 
## 1) De 2000-2010 - Sinasc ## 

Total.2000.2019 <- fecundidade.2000.2019 %>%
  group_by(Estado, UF, Ano) %>%
  summarise(Women = sum(Mulheres),
            Nascimentos = sum(Filhos.tidos.no.ultimo.ano),
            TFT = (sum(ASFR)*5)) %>%
  ungroup() %>%
  arrange(Estado, Ano)


##----------------------------------------------------------------------------------------------------------

## Calculando TFT para Gompertz, Brass e Coale & Trussel
## 1) De 2000-2010 - Censos ## 

TFT.2000.2019 <- NULL

for (j in unique(fecundidade.2000.2019$Ano)) {
  for (i in unique(fecundidade.2000.2019$Estado)) {
    cat(paste("\nProcessing", i, "ano", j, sep = " "))
    
    data <- fecundidade.2000.2019 %>%
      filter(Estado == i,
             Ano == j) 
    
    Gompertz.sem.P <- fertGompPF(ages = data$idade, asfr = data$ASFR, level = F, plot.diagnostic = F)[[2]]
    Gompertz.sem.P <- Gompertz.sem.P$TFR.adj
    novo <- cbind(Ano = j, Estado = i, Gompertz.sem.P)
    TFT.2000.2019 <-  rbind(TFT.2000.2019, novo)
    
  }
}

TFT.2000.2019 <- TFT.2000.2019 %>%
  as.data.frame() %>%
  right_join(Total.2000.2019, by = c("Ano", "Estado")) %>%
  select(Ano, UF, Estado, Women, Nascimentos, TFT, Gompertz.sem.P) %>%
  mutate(ratio.Obs.Gompertz.sem.P = Gompertz.sem.P/TFT) %>%
  ungroup()

rm(Gompertz.sem.P, novo, data, Total.2000.2019)

##----------------------------------------------------------------------------------------------------------

# Adicionando o Banco de Obitos Maternos
## 1) Juntando o banco de TFT com o banco de obitos

TFT.2000.2019 <- TFT.2000.2019 %>%
  left_join(Obitos.maternos2000.2019, by = c("Estado", "UF", "Ano")) %>%
  as.data.frame() %>%
  group_by(Ano, Estado) %>%
  mutate(NV.Ajus.Gompertz.sem.P = Nascimentos * ratio.Obs.Gompertz.sem.P,
         RMM = (Obitos.Obs / Nascimentos) * 100000,
         RMM.Obt.Corr = (Obitos.Ajust / Nascimentos) * 100000,
         RMM.Gompertz.sem.P = (Obitos.Ajust / NV.Ajus.Gompertz.sem.P) * 100000,
         TMM = (Obitos.Obs / Women) * 100000,
         TMM.Ajust = (Obitos.Ajust / Women) * 100000) %>%
  ungroup()

################################
## 2) Juntando as RMMs Geradas com as calculadas pelo IBGE e usando a razões para criar RMM.Ibge(2019) ##

TFT.2000.2019 <- TFT.2000.2019 %>%
  left_join(RMM.ibge.09.18, by = c("Estado", "UF", "Ano")) %>%
  mutate(ratio = RMM.Ibge/RMM) %>%
  group_by(UF) %>%
  mutate( 
    across("RMM.Ibge", ~(
      function(x) {
        for(i in 1:length(x))
        {
          if(is.na(x[i]) & Ano[i] > 2018)
            x[i] <- ratio[Ano == 2018] * RMM[Ano == 2018]
          
          else if(is.na(x[i]) & Ano[i] < 2018 & Ano[i] > 2008)
            x[i] <- x[i]
          
          else if(is.na(x[i]) & Ano[i] <= 2008)
            x[i] <- RMM[i]
        }
        return(x)
      } ) # end of function spec
      (.) ) # end of across spec
  ) %>% # end of mutate
  ungroup() %>%
  select(-ratio)

##----------------------------------------------------------------------------------------------------------
# Fonte: Jain, A. K. (2011). Measuring the Effect of Fertility Decline on the Maternal Mortality Ratio. 
#        Studies in Family Planning, 42(4), 247–260. doi:10.1111/j.1728-4465.2011.00288.x 

### Estimation of the effect of declines in maternal mortality ratio (MMR) and fertility on estimated number of maternal ###
 ### deaths averted in 2019 ###

## 1) Juntando as bases de populacao total e TFT ##
TFT.2000.2019 <- TFT.2000.2019 %>%
  left_join(Pop.00.30, by = c("Ano", "sigla", "UF", "Estado", "region")) 


################################
## 2) Usando os dados do BR ##
Decomp <- TFT.2000.2019 %>%
  filter(sigla == "BR",
         Ano == 2009 | Ano == 2014 | Ano == 2019)  %>% 
  select(Ano, Estado, UF, sigla, region, Pop, Women, Nascimentos, NV.Ajus.Gompertz.sem.P, 
         TFT, Gompertz.sem.P, RMM, RMM.Obt.Corr, RMM.Ibge, RMM.Gompertz.sem.P) %>%
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

# r = Annual population growth rate: 2009-2014 and 2014-2019
Decomp$r[Decomp$Ano == 2014] <- ((log(Decomp$Pop[Decomp$Ano == 2014] / Decomp$Pop[Decomp$Ano == 2009])) / (2014 - 2009))
Decomp$r[Decomp$Ano == 2019] <- ((log(Decomp$Pop[Decomp$Ano == 2019] / Decomp$Pop[Decomp$Ano == 2014])) / (2019 - 2014))

# P_hat = 2019 estimated population assuming constant annual growth rate from 2009-2014
Decomp$P_hat[Decomp$Ano == 2019] <- Decomp$Pop[Decomp$Ano == 2009] * (exp(19 * Decomp$r[Decomp$Ano == 2014]))

# B_hat = Projected births in 2019 assuming constant fertility
Decomp$B_hat[Decomp$Ano == 2019] <- Decomp$P_hat[Decomp$Ano == 2019] * Decomp$TBN[Decomp$Ano == 2009] / 1000

# B = Actual births in 2019 
Decomp$B[Decomp$Ano == 2019] <- Decomp$Nascimentos[Decomp$Ano == 2019]
#Decomp$B2[Decomp$Ano == 2019] <- Decomp$P_hat[Decomp$Ano == 2019] * Decomp$TBN[Decomp$Ano == 2019] / 1000


# D1_hat = No change in CBR and no change in MMR
Decomp$D1_hat[Decomp$Ano == 2019] <- Decomp$B_hat[Decomp$Ano == 2019] * Decomp$RMM.Ibge[Decomp$Ano == 2009] / 100000

# D3_hat = MMR declined but CBR did not
Decomp$D3_hat[Decomp$Ano == 2019] <- Decomp$B_hat[Decomp$Ano == 2019] * Decomp$RMM.Ibge[Decomp$Ano == 2019] / 100000

# D2_hat = CBR declined but MMR did not
Decomp$D2_hat[Decomp$Ano == 2019] <- Decomp$B[Decomp$Ano == 2019] * Decomp$RMM.Ibge[Decomp$Ano == 2009] / 100000

# D = Both CBR and MMR declined
Decomp$D[Decomp$Ano == 2019] <- Decomp$B[Decomp$Ano == 2019] * Decomp$RMM.Ibge[Decomp$Ano == 2019] / 100000


# Y = Total effect of decline in MMR 
Decomp$Y <- Decomp$D1_hat - Decomp$D3_hat

# X = Total effect of fertility decline
Decomp$X <- Decomp$D1_hat - Decomp$D2_hat

# Z = Total effect of declines in both fertility and MMR
Decomp$Z <- Decomp$D1_hat - Decomp$D

Decomp <- Decomp %>%
  mutate(intersec.X.Y = X + Y - Z, # intersec.X.Y = Overlap between the effect of declines in fertility and in MMR
         Alpha = Y - intersec.X.Y, # Alpha = Net effect of decline in MMR
         Beta = X - intersec.X.Y, # Beta = Net effect of fertility decline
         Gama = (Alpha / Z) * 100, # Gama = Effect of safe motherhood on the % of the  potential number of maternal lives saved in 2019
         Delta = (Beta / Z) * 100, # Delta = Effect of decrease in live births on the % of the  potential number of maternal lives saved in 2019
         Omega = (intersec.X.Y / Z) * 100) # Omega = Effect of fertility reduction realized through its effect on MMR reduction

##----------------------------------------------------------------------------------------------------------

### Estimation of the decline in maternal mortality ratio and number of actual maternal deaths attributable to fertility decline ###

Decomp <- Decomp %>%
  mutate(RMM_hat = NA,
         D.09 = NA,
         D.19 = NA, 
         D_hat.19 = NA,
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

# RMM_hat = MMR in 2019 implied by fertility reduction observed during 2009-2019
Decomp$RMM_hat[Decomp$Ano == 2019] <- Decomp$RMM.Ibge[Decomp$Ano == 2019] + 
                                             ((Decomp$B[Decomp$Ano == 2019] / Decomp$B_hat[Decomp$Ano == 2019]) *
                                                (Decomp$RMM.Ibge[Decomp$Ano == 2009] - Decomp$RMM.Ibge[Decomp$Ano == 2019]))
                                                                      
# D.09 = Maternal deaths in 2009
Decomp$D.09[Decomp$Ano == 2019] <- (Decomp$Nascimentos[Decomp$Ano == 2009] * Decomp$RMM.Ibge[Decomp$Ano == 2009]) / 100000

# D.19 = Maternal deaths in 2019
Decomp$D.19[Decomp$Ano == 2019] <- (Decomp$Nascimentos[Decomp$Ano == 2019] * Decomp$RMM.Ibge[Decomp$Ano == 2019]) / 100000

# D_hat.19 = Maternal deaths in 2019 implied by fertility decline observed between 2009 and 2019
Decomp$D_hat.19[Decomp$Ano == 2019] <- (Decomp$Nascimentos[Decomp$Ano == 2019] *  Decomp$RMM_hat[Decomp$Ano == 2019]) / 100000


# Iota = Total decline in MMR between 2009 and 2019
Decomp$Iota[Decomp$Ano == 2019] <- Decomp$RMM.Ibge[Decomp$Ano == 2009] - Decomp$RMM.Ibge[Decomp$Ano == 2019]
Decomp$Iota.per[Decomp$Ano == 2019] <- 100 #(in %)

# Kappa = Decline in MMR attributable to fertility reduction
Decomp$Kappa[Decomp$Ano == 2019] <- Decomp$RMM.Ibge[Decomp$Ano == 2009] - Decomp$RMM_hat[Decomp$Ano == 2019]
Decomp$Kappa.per[Decomp$Ano == 2019] <- (Decomp$Kappa[Decomp$Ano == 2019] * 100) / Decomp$Iota[Decomp$Ano == 2019] #(in %)

# Lambda = Decline in MMR attributable to safe motherhood initiatives
Decomp$Lambda[Decomp$Ano == 2019] <- Decomp$RMM_hat[Decomp$Ano == 2019] - Decomp$RMM.Ibge[Decomp$Ano == 2019]
Decomp$Lambda.per[Decomp$Ano == 2019] <- (Decomp$Lambda[Decomp$Ano == 2019] * 100) / Decomp$Iota[Decomp$Ano == 2019] #(in %)


# Sigma = Total decline in actual maternal deaths between 2009 and 2019
Decomp$Sigma[Decomp$Ano == 2019] <- Decomp$D.09[Decomp$Ano == 2019] - Decomp$D.19[Decomp$Ano == 2019] 
Decomp$Sigma.per[Decomp$Ano == 2019] <- 100 #(in %)
  
# Tau = Attributable to fertility decline
Decomp$Tau[Decomp$Ano == 2019] <-  Decomp$D.09[Decomp$Ano == 2019] - Decomp$D_hat.19[Decomp$Ano == 2019]
Decomp$Tau.per[Decomp$Ano == 2019] <- (Decomp$Tau[Decomp$Ano == 2019] * 100) / Decomp$Sigma[Decomp$Ano == 2019] #(in %)

# Eta = Attributable to safe motherhood
Decomp$Eta[Decomp$Ano == 2019] <- Decomp$D_hat.19[Decomp$Ano == 2019] - Decomp$D.19[Decomp$Ano == 2019]
Decomp$Eta.per[Decomp$Ano == 2019] <- (Decomp$Eta[Decomp$Ano == 2019] * 100) / Decomp$Sigma[Decomp$Ano == 2019] #(in %)

Decomp <- Decomp %>%
  mutate(across(22:48, round, 3))


################################
## 2) Fazendo a mesma decomposição para todas as UFs em loop (2009-2019) - COM AJUSTES ##

UnidFed <- unique(TFT.2000.2019$sigla)

datalist <- list()

for (i in UnidFed) {
  
  print(paste("Processing", i, sep = " "))
  
  dat <- Decomp.MMR(TFT.2000.2019, 2009, 2014, 2019, i, ajuste = T)
  datalist[[i]] <- dat # add it to your list
  
}

Decomp.com <- do.call(rbind, datalist)


rm(datalist, dat)

################################
## 3) Normalizando os resultados(2009-2019) - COM AJUSTES ##

Decomp.com <- Decomp.com %>% 
  mutate(
    # norm.Iota = rescale(Iota), 
    #      norm.Kappa = rescale(Kappa), 
    #      norm.Lambda = rescale(Lambda), 
         norm.Iota.per = 1,
         norm.Kappa.per = scales::rescale(Kappa.per, to = c(0, 1)), 
         norm.Lambda.per = scales::rescale(Lambda.per, to = c(0, 1)), 
         # norm.Sigma = scales::rescale(Sigma), 
         # norm.Tau = scales::rescale(Tau), 
         # norm.Eta = scales::rescale(Eta),
         norm.Sigma.per = 1,
         norm.Tau.per = scales::rescale(Tau.per), 
         norm.Eta.per = scales::rescale(Eta.per)
         ) 

# Testando uma escala de -1 a 1, mas nao deu certo
# Decomp.com %>% 
# mutate(norm.Iota = rescale(Iota, to = c(-1, 1)),
#        norm.Kappa = rescale(Kappa, to = c(-1, 1)),
#        norm.Lambda = rescale(Lambda, to = c(-1, 1)),
#        norm.Iota.per = 1,
#        norm.Kappa.per = scales::rescale(Kappa.per, to = c(-1, 1)),
#        norm.Lambda.per = scales::rescale(Lambda.per, to = c(-1, 1)),
#        norm.Sigma = scales::rescale(Sigma, to = c(-1, 1)),
#        norm.Tau = scales::rescale(Tau, to = c(-1, 1)),
#        norm.Eta = scales::rescale(Eta, to = c(-1, 1)),
#        norm.Sigma.per = 1,
#        norm.Tau.per = scales::rescale(Tau.per, to = c(-1, 1)), 
#        norm.Eta.per = scales::rescale(Eta.per, to = c(-1, 1))) %>%
#   View()

################################

## 4) Fazendo a mesma decomposição para todas as UFs em loop (2009-2019) - SEM AJUSTES ##

UnidFed <- unique(TFT.2000.2019$sigla)

datalist <- list()

for (i in UnidFed) {
  
  print(paste("Processing", i, sep = " "))
  
  dat <- Decomp.MMR(TFT.2000.2019, 2009, 2014, 2019, i, ajuste = F)
  datalist[[i]] <- dat # add it to your list
  
}

Decomp.sem <- do.call(rbind, datalist)


rm(datalist, dat)

################################
## 5) Normalizando os resultados(2009-2019) - SEM AJUSTES ##

Decomp.sem <- Decomp.sem %>% 
  mutate(norm.Iota = rescale(Iota), 
         norm.Kappa = rescale(Kappa), 
         norm.Lambda = rescale(Lambda), 
         norm.Iota.per = 1,
         norm.Kappa.per = scales::rescale(Kappa.per, to = c(0, 1)), 
         norm.Lambda.per = scales::rescale(Lambda.per, to = c(0, 1)), 
         norm.Sigma = scales::rescale(Sigma), 
         norm.Tau = scales::rescale(Tau), 
         norm.Eta = scales::rescale(Eta),
         norm.Sigma.per = 1,
         norm.Tau.per = scales::rescale(Tau.per), 
         norm.Eta.per = scales::rescale(Eta.per)) 


##----------------------------------------------------------------------------------------------------------

### Criando uma tabela resumo ###

estado <- c("BR","RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE",
           "BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF")

################################

# 1) TABELA DE DECOMPOSIÇÃO COM AJUSTE

tab.com <- Decomp.com %>%
  select(sigla, RMM.Ibge, TBN, r, P_hat, B_hat, B, D1_hat, D2_hat, 
         D3_hat, D, X, Y, Z, intersec.X.Y, Alpha, Beta, Gama, Delta, Omega, RMM_hat, 
         D.ano1, D.ano2, D_hat.ano2, Iota, Kappa, Lambda, Iota.per, Kappa.per, Lambda.per, 
         Sigma, Tau, Eta, Sigma.per, Tau.per, Eta.per) %>%
  rename(RMM = RMM.Ibge) %>%
  pivot_longer(-sigla) %>% 
  pivot_wider(names_from = sigla, values_from = value) %>%
  left_join(description.new, by = "name") %>% 
  ungroup() %>%
  select(name, description, "BR","RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE",
         "BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF") %>%
  gt() %>%
  tab_header(title = md("**Decomposição Jain (2011) por UF - Com Ajustes - 2009-2019**")) %>%
  cols_label(name = "Variable",
             description = "Description") %>% 
  tab_source_note(source_note = "Jain, A. K. (2011). Measuring the Effect of Fertility Decline on the Maternal Mortality Ratio. Studies in Family Planning, 42(4), 247–260") %>%
  fmt_number(columns = all_of(estado),
              decimals = 2) %>%
  fmt_number(columns = all_of(estado),
             rows = 4:6,
             scale_by = 1/1000,
             decimals = 2) %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      cell_borders(sides = "bottom", weight = px(3)), #Give a thick border below
      cell_text(weight = "bold", size = px(13))))  %>%
  tab_footnote(
    footnote = "Em milhares",
    locations = cells_body(columns = c(name, description), rows = 4:6)) %>%
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(columns = everything())) %>%
  tab_style(locations =  cells_body(columns ="name"),
            style = cell_text(weight = "bold")) %>%
  cols_width(starts_with("Description") ~ px(1300))  %>% 
  cols_align(align = "center", columns = all_of(estado))

# SALVANDO TABELA
tab.com %>%
  gtsave("tabela decomposicao UF (2009-2019).html", inline_css = TRUE)

################################

# 2) TABELA DE DECOMPOSIÇÃO SEM AJUSTE

tab.sem <- Decomp.sem %>%
  select(sigla, RMM, TBN, r, P_hat, B_hat, B, D1_hat, D2_hat, 
         D3_hat, D, X, Y, Z, intersec.X.Y, Alpha, Beta, Gama, Delta, Omega, RMM_hat, 
         D.ano1, D.ano2, D_hat.ano2, Iota, Kappa, Lambda, Iota.per, Kappa.per, Lambda.per, 
         Sigma, Tau, Eta, Sigma.per, Tau.per, Eta.per) %>%
  pivot_longer(-sigla) %>% 
  pivot_wider(names_from = sigla, values_from = value) %>%
  left_join(description.new, by = "name") %>% 
  ungroup() %>%
  select(name, description, "BR","RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE",
         "BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF") %>%
  gt() %>%
  tab_header(title = md("**Decomposição Jain (2011) por UF - Sem Ajustes - 2009-2019**")) %>%
  cols_label(name = "Variable",
             description = "Description") %>% 
  tab_source_note(source_note = "Jain, A. K. (2011). Measuring the Effect of Fertility Decline on the Maternal Mortality Ratio. Studies in Family Planning, 42(4), 247–260") %>%
  fmt_number(columns = all_of(estado),
             decimals = 2) %>%
  fmt_number(columns = all_of(estado),
             rows = 4:6,
             scale_by = 1/1000,
             decimals = 2) %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      cell_borders(sides = "bottom", weight = px(3)), #Give a thick border below
      cell_text(weight = "bold", size = px(13))))  %>%
  tab_footnote(
    footnote = "Em milhares",
    locations = cells_body(columns = c(name, description), rows = 4:6)) %>%
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(columns = everything())) %>%
  tab_style(locations =  cells_body(columns ="name"),
            style = cell_text(weight = "bold")) %>%
  cols_width(starts_with("Description") ~ px(1300))  %>% 
  cols_align(align = "center", columns = all_of(estado))

# SALVANDO TABELA
tab.sem %>%
  gtsave("tabela decomposicao UF sem ajuste (2009-2019).html", inline_css = TRUE)

################################

# 3) TABELA DE DECOMPOSIÇÃO COM AJUSTE - NORMALIZADA

tab.com.norm <- Decomp.com %>%
  select(sigla, RMM.Ibge, TBN, r, P_hat, B_hat, B, D1_hat, D2_hat, 
         D3_hat, D, X, Y, Z, intersec.X.Y, Alpha, Beta, Gama, Delta, Omega, RMM_hat, 
         D.ano1, D.ano2, D_hat.ano2, norm.Iota, norm.Kappa, norm.Lambda, norm.Iota.per, norm.Kappa.per, 
         norm.Lambda.per, norm.Sigma, norm.Tau, norm.Eta, norm.Sigma.per, norm.Tau.per, norm.Eta.per) %>%
  rename(RMM = RMM.Ibge,
         Iota = norm.Iota, 
         Kappa = norm.Kappa, 
         Lambda = norm.Lambda, 
         Iota.per = norm.Iota.per, 
         Kappa.per = norm.Kappa.per, 
         Lambda.per = norm.Lambda.per, 
         Sigma = norm.Sigma, 
         Tau = norm.Tau, 
         Eta = norm.Eta, 
         Sigma.per = norm.Sigma.per, 
         Tau.per = norm.Tau.per, 
         Eta.per = norm.Eta.per) %>%
  pivot_longer(-sigla) %>% 
  pivot_wider(names_from = sigla, values_from = value) %>%
  left_join(description.new, by = "name") %>% 
  ungroup() %>%
  select(name, description, "BR","RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE",
         "BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF") %>%
  gt() %>%
  tab_header(title = md("**Decomposição Jain (2011) por UF - Com Ajustes e Normalizado - 2009-2019**")) %>%
  cols_label(name = "Variable",
             description = "Description") %>% 
  tab_source_note(source_note = "Jain, A. K. (2011). Measuring the Effect of Fertility Decline on the Maternal Mortality Ratio. Studies in Family Planning, 42(4), 247–260") %>%
  fmt_number(columns = all_of(estado),
             decimals = 2) %>%
  fmt_number(columns = all_of(estado),
             rows = 4:6,
             scale_by = 1/1000,
             decimals = 2) %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      cell_borders(sides = "bottom", weight = px(3)), #Give a thick border below
      cell_text(weight = "bold", size = px(13))))  %>%
  tab_footnote(
    footnote = "Em milhares",
    locations = cells_body(columns = c(name, description), rows = 4:6)) %>%
  tab_footnote(
    footnote = "Normalizado",
    locations = cells_body(columns = c(name, description), rows = 24:35)) %>%
  tab_style(
    style = cell_text(size = px(11)),
    locations = cells_body(columns = everything())) %>%
  tab_style(locations =  cells_body(columns ="name"),
            style = cell_text(weight = "bold")) %>%
  cols_width(starts_with("Description") ~ pct(70))  %>% 
  cols_align(align = "center", columns = all_of(estado))

# SALVANDO TABELA
tab.com.norm %>%
  gtsave("tabela decomposicao UF norm (2009-2019).html", inline_css = TRUE)

################################

# 5) TABELA DE DECOMPOSIÇÃO SEM AJUSTE - NORMALIZADA

tab.sem.norm <- Decomp.sem %>%
  select(sigla, RMM, TBN, r, P_hat, B_hat, B, D1_hat, D2_hat, 
         D3_hat, D, X, Y, Z, intersec.X.Y, Alpha, Beta, Gama, Delta, Omega, RMM_hat, 
         D.ano1, D.ano2, D_hat.ano2, norm.Iota, norm.Kappa, norm.Lambda, norm.Iota.per, norm.Kappa.per, 
         norm.Lambda.per, norm.Sigma, norm.Tau, norm.Eta, norm.Sigma.per, norm.Tau.per, norm.Eta.per) %>%
  rename(Iota = norm.Iota, 
         Kappa = norm.Kappa, 
         Lambda = norm.Lambda, 
         Iota.per = norm.Iota.per, 
         Kappa.per = norm.Kappa.per, 
         Lambda.per = norm.Lambda.per, 
         Sigma = norm.Sigma, 
         Tau = norm.Tau, 
         Eta = norm.Eta, 
         Sigma.per = norm.Sigma.per, 
         Tau.per = norm.Tau.per, 
         Eta.per = norm.Eta.per) %>%
  pivot_longer(-sigla) %>% 
  pivot_wider(names_from = sigla, values_from = value) %>%
  left_join(description.new, by = "name") %>% 
  ungroup() %>%
  select(name, description, "BR","RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE",
         "BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF") %>%
  gt() %>%
  tab_header(title = md("**Decomposição Jain (2011) por UF - Sem Ajustes e Normalizado - 2009-2019**")) %>%
  cols_label(name = "Variable",
             description = "Description") %>% 
  tab_source_note(source_note = "Jain, A. K. (2011). Measuring the Effect of Fertility Decline on the Maternal Mortality Ratio. Studies in Family Planning, 42(4), 247–260") %>%
  fmt_number(columns = all_of(estado),
             decimals = 2) %>%
  fmt_number(columns = all_of(estado),
             rows = 4:6,
             scale_by = 1/1000,
             decimals = 2) %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      cell_borders(sides = "bottom", weight = px(3)), #Give a thick border below
      cell_text(weight = "bold", size = px(13))))  %>%
  tab_footnote(
    footnote = "Em milhares",
    locations = cells_body(columns = c(name, description), rows = 4:6))  %>%
  tab_footnote(
    footnote = "Normalizado",
    locations = cells_body(columns = c(name, description), rows = 24:35)) %>%
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(columns = everything())) %>%
  tab_style(locations =  cells_body(columns ="name"),
            style = cell_text(weight = "bold")) %>%
  cols_width(starts_with("Description") ~ px(1300))  %>% 
  cols_align(align = "center", columns = all_of(estado))

# SALVANDO TABELA
tab.sem.norm %>%
  gtsave("tabela decomposicao UF sem ajuste norm (2009-2019).html", inline_css = TRUE)

##----------------------------------------------------------------------------------------------------------



##----------------------------------------------------------------------------------------------------------

## TESTE ##
Decomp.com %>%
  select(sigla, Estado, UF, region, norm.Kappa.per, 
         norm.Lambda.per, norm.Eta.per, norm.Tau.per) %>%
  arrange(Estado) %>%
  mutate(norm.Eta.g.per = norm.Eta.per * (-1), # Attributable to safe motherhood (in %)
         norm.Lambda.g.per = norm.Lambda.per * (-1), 
         index = rep(seq(1, 28))
         ) %>%
  #View()
  ggplot(aes(x = norm.Kappa.per, y = index, group = Estado, color = region)) +
  geom_point(size = 6.5, alpha = 0.70) +
  geom_point(aes(x = norm.Lambda.g.per, y = index), shape = 16, size = 6.5, alpha = 0.80) +
  geom_segment(aes(x = norm.Lambda.g.per, xend = norm.Kappa.per, y = index, yend = index), color = '#6d6875') +
  geom_hline(yintercept = c(7, 16, 20, 23, 27) + 0.50, lwd = 0.95, linetype = "dashed", col = 'grey') +
  geom_vline(xintercept = 0, lwd = 1, col = 'darkgrey') +
  geom_text(aes(x = norm.Lambda.g.per, y = index + 0.05, label = sigla), color = 'black', size = 3.5, fontface = 'bold') +
  labs(title = "Proporção do total da mudança na RMM por UF - 2009-2019",
       x = '',
       y = '',
       caption = "Fonte: IBGE - SINASC 2009-2019, RMM 2009-2018 e Projeções de População \n Jain, A. K. (2011). Measuring the Effect of Fertility Decline on the Maternal Mortality Ratio. Studies in Family Planning, 42(4), 247–260") +
  guides(color = "none", size = "none") +
  theme_bw() +
  scale_y_continuous(breaks = NULL, limits = c(0.5, 32.0)) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1), labels = seq(-1, 1, by = 0.1)  %>% abs) + 
  geom_text(label = 'Atribuível a mudanças na \nMaternidade Segura', aes(x = -0.6, y = 31.5, vjust = 'center',
                                 size = 3), color = 'black') +
  geom_text(label = 'Atribuível a mudanças \nna Fecundidade', aes(x = 0.6, y = 31.5, vjust = 'center',
                                    size = 3), color = 'black') +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(color = "grey20", size = 15, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11)) +
  geom_text(label = 'Norte', aes(x = -1.2, y = 3.0, angle = 90, hjust = 'center', 
                                 size = 3), color = 'black') +
  geom_text(label = 'Nordeste', aes(x = -1.2, y = 12.5, angle = 90, hjust = 'center', 
                                    size = 3), color = 'black') +
  geom_text(label = 'Sudeste', aes(x = -1.2, y = 18.5, angle = 90, hjust = 'center', 
                                   size = 3), color = 'black') +
  geom_text(label = 'Sul', aes(x = -1.2, y = 22.0, angle = 90, hjust = 'center', 
                               size = 3), color = 'black') +
  geom_text(label = 'Centro-\nOeste', aes(x = -1.2, y = 25.3, angle = 90, hjust = 'center', 
                                          size = 3), color = 'black')+
  geom_text(label = 'Brasil', aes(x = -1.2, y = 29.0, angle = 90, hjust = 'center', 
                                  size = 3), color = 'black')

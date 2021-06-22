# Estimacao da RMM e TFTs para Brasil e UFs 2010-2018

## ATENÇÃO!!! 
  ## Este script foi desenvolvido ANTES da decisão de se usar somente as estimativas do SINASC. 
  ## Ele será ajustado pra usar apenas o Sinasc e não precisará interpolar o censo 2000-2010
  ## Este script também usa os dados APENAS até 2018. Será atualizado para 2019

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

## Importando os dados
fecundidade2000.2010 <- read.csv('data/Fecundidade 2000-2010 Censo.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
fecundidade2011.2018 <- read.csv('data/Fecundidade 2011-2018 Sinasc.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
Obitos.maternos2000.2018 <- read.csv('data/Prop Obitos Maternos 2000-2018.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')

##----------------------------------------------------------------------------------------------------------

## Organizando o Banco de Dados

################################
## 1) Censos 2000 e 2010 - Fecundidade idade 15-45 ##
fecundidade2000.2010 <- fecundidade2000.2010 %>%
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
         Parity = Filhos.Tidos.Nascidos.Vivos/Mulheres,
         ASFR = Filhos.tidos.no.ultimo.ano/Mulheres) %>%
  filter(idade >=  15,
         idade <= 45)

################################

## 2) SINASC - Fecundidade idade 15-45 ##
fecundidade2011.2018 <- fecundidade2011.2018 %>%
  rename (Grupos.de.idade = Idade) %>%
  mutate(Filhos.tidos.no.ultimo.ano = round(Filhos.tidos.no.ultimo.ano),
         idade = as.numeric(substr(Grupos.de.idade, start = 1, stop = 2)),
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

################################
## 4) GRAFICO das TEFs por metodo para Brasil ##
TEF.2000.2010 %>%
  filter(Estado == 99) %>%
  ggplot(aes(x = idade)) + 
  geom_path(aes(y = ASFR,  color = "Observada"), linetype = "solid", size = 1.2) + 
  geom_path(aes(y = Gompertz, color = "Gompertz"), linetype = "twodash", size = 1.2) + 
  geom_path(aes(y = Brass, color = "Brass"), linetype = "solid", size = 1.2) +
  geom_path(aes(y = Coale.Trussel, color = "Coale & Trussel"), linetype = "dashed", size = 1.2) +
  labs(title = "Taxa Específica de Fecundidade - Brasil 2000 e 2010",
       caption = "Fonte: IBGE - Censo Demográfico 2000 e 2010",
       y = "TEF",
       color = "Metodos") +
  theme_bw() +
  scale_color_manual(name = "Métodos:",
                     values = c("#00916e", "#00509d", "#fe9920", "#a61c3c")) +
  scale_x_continuous(name ="Idade", breaks = seq(15, 45, 5)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing.x = grid::unit(0.15, "cm"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "grey20", size = 15, hjust = 0.5, face = "bold"), 
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside") + 
  facet_grid(~Ano)

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
## 2) GRAFICO comparativo de pontos das razoes de diferencas (FATOR DE CORRECAO) entre a TFT e os metodos
TFT.2000.2010 %>%
  filter(Ano == 2010) %>%
  mutate(region = case_when(11 <= Estado & Estado <= 17 ~ "Norte",
                            21 <= Estado & Estado <= 29 ~ "Nordeste",
                            (31 <= Estado & Estado <= 33) | Estado == 35 ~ "Sudeste",
                            41 <= Estado & Estado <= 43 ~ "Sul",
                            50 <= Estado & Estado <= 53 ~ "Centro-Oeste"),
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
         Direta = 1,
         index = rep(seq(1, 28), times = length(unique(Ano)))) %>%
  ggplot(aes(x = Direta, y = index, color = region)) + 
  geom_point(shape = 16, size = 6, color = 'darkgrey', alpha = 0.80) + 
  #geom_segment(aes(x = Direta, xend = ratio.Obs.Gompertz, y = index, yend = index, lwd = 3.5, alpha = 0.20)) +
  geom_point(aes(x = ratio.Obs.Brass, y = index), shape = 16, size = 5, col = "#f4442e", alpha = 0.90) +
  geom_hline(yintercept = c(7, 16, 20, 23, 27) + 0.50, lwd = 1.1, col = 'darkgrey') +
  geom_point(aes(x = ratio.Obs.CT, y = index), shape = 16, size = 5, col =  "#1282a2", alpha = 1.2) +
  geom_point(aes(x = ratio.Obs.Gompertz, y = index), shape = 16, size = 5, col = "#FE9920", alpha = 0.90) +
  geom_point(aes(x = ratio.Obs.Gompertz.sem.P, y = index), shape = 16, size = 5, col = "#CC009C", alpha = 0.90) +
  scale_y_continuous(breaks = NULL, limits = c(1, 36.5)) +
  scale_x_continuous(breaks= seq(0.95, 1.6, by = 0.05)) +
  theme_bw() +
  labs(x = 'Razão entre Método/Estimacao Direta da TFT', y = '') +
  geom_segment(aes(x = ratio.Obs.Gompertz.sem.P, xend = ratio.Obs.Gompertz, y = index, yend = index), color = 'darkgrey') +
  geom_text(aes(x = 0.925, y = index, label = sigla), color = 'black', size = 3.5, fontface = 'bold') +
  theme(axis.text = element_text(size = 12, face = 'bold', color = 'black'),
        axis.title = element_text(size = 12, face = 'bold', color = 'black')) + 
  guides(color = FALSE, size = FALSE, alpha = FALSE) +
  geom_text(label = 'Norte', aes(x = 0.89, y = 3.0, angle = 90, hjust = 'center', 
                                 size = 3.5), color = 'black') +
  geom_text(label = 'Nordeste', aes(x = 0.89, y = 12.5, angle = 90, hjust = 'center', 
                                    size = 3.5), color = 'black') +
  geom_text(label = 'Sudeste', aes(x = 0.89, y = 18.5, angle = 90, hjust = 'center', 
                                   size = 3.5), color = 'black') +
  geom_text(label = 'Sul', aes(x = 0.89, y = 22.0, angle = 90, hjust = 'center', 
                               size = 3.5), color = 'black') +
  geom_text(label = 'Centro-\nOeste', aes(x = 0.89, y = 25.3, angle = 90, hjust = 'center', 
                                          size = 3.5), color = 'black')+
  geom_text(label = 'Brasil', aes(x = 0.89, y = 29.0, angle = 90, hjust = 'center', 
                                  size = 3.5), color = 'black') +
  geom_point(aes(x = 1.4, y = 35.5), shape = 16, size = 4.5, col = "#f4442e", alpha = 0.90) +
  geom_text(x = 1.42, y = 35.5, label = 'Brass', size = 3.8,
            hjust = 'left', color = 'black') +
  geom_point(aes(x = 1.4, y = 34), shape = 16, size = 4.5, color = "#FE9920", alpha = 0.90) +
  geom_text(x = 1.42, y = 34, label = "Gompertz", size = 3.8,
            hjust = 'left', color = 'black') +
  geom_point(aes(x = 1.4, y = 32.5), shape = 16, size = 5.5, color = 'darkgrey', alpha = 0.80) +
  geom_text(x = 1.42, y = 32.5, label = 'Estimação Direta', size = 3.8,
            hjust = 'left', color = 'black') +
  geom_point(aes(x = 1.4, y = 31), shape = 16, size = 4.5, col =  "#1282a2", alpha = 1.2) +
  geom_text(x = 1.42, y = 31, label = 'Coale & Trussel', size = 3.8,
            hjust = 'left', color = 'black') +
  geom_point(aes(x = 1.4, y = 29.5), shape = 16, size = 4.5, col =  "#CC009C", alpha = 1.2) +
  geom_text(x = 1.42, y = 29.5, label = 'Gompertz sem Parturição', size = 3.8,
            hjust = 'left', color = 'black')

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

################################
## 5)GRAFICO com os valores interpolados  de 2000 e 2010 & 2010 e 2018
# Os valores de 2018 foram obtidos com o fator de correcao de 2010 aplicados sobre a TFT de 2018
Interp.Total.2000.2018 %>%
  filter(Estado == 99) %>%
  ggplot(aes(x = Ano)) + 
  geom_point(aes(y = TFT,  color = "TFT"), size = 2) + 
  geom_point(aes(y = Brass, color = "Brass"), size = 2) + 
  geom_point(aes(y = Coale.Trussel, color = "Coale & Trussell"), size = 2) + 
  geom_point(aes(y = Gompertz, color = "Gompertz"), size = 2) + 
  geom_point(aes(y = Gompertz.sem.P, color = "Gompertz sem \nParturição"), size = 2) + 
  labs(title = "Taxa Fecundidade Total - Brasil 2000-2018 (Interpolados)",
       caption = "Fonte: IBGE - Censo Demográfico 2000 e 2010 e SINASC 2011-2018",
       color = "Metodos") +
  theme_bw() +
  scale_y_continuous(name ="TFT", breaks = c(1.5, 1.75, 2.0, 2.25, 2.50, 2.75, 3.0), limits = c(1.45, 3)) +
  scale_color_manual(name = "Métodos:",
                     values = c("#52AA5E", "#00509d", "#ADAABF", "#fe9920","#a61c3c")) +
  scale_x_continuous(name ="Ano", breaks = seq(2000, 2018, by = 1)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing.x = grid::unit(0.15, "cm"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "grey20", size = 12, hjust = 0.5, face = "bold"), 
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        #axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(angle = 50, hjust = 1, size = 11))
##----------------------------------------------------------------------------------------------------------

# Adicionando o Banco de Obitos Maternos

################################
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

################################
## 2) GRAFICO
Interp.Total.2000.2018 %>%
  filter(Estado == 99) %>%
  ggplot(aes(x = Ano, y = RMM) ) + 
  geom_point(shape = 'O', size = 6, color = 'black', alpha = 0.80) +
  geom_point(aes(x = Ano, y = RMM.Brass), shape = 16, size = 5, col = "#3A3B3C", alpha = 2.3) +
  geom_point(aes(x = Ano, y = RMM.CT),  shape = 16, size = 5, col = "#f4442e", alpha = 0.75) +
  geom_point(aes(x = Ano, y = RMM.Gompertz), shape = 16, size = 5, col = "#1282a2", alpha = 1.2) +
  geom_point(aes(x = Ano, y = RMM.Gompertz.sem.P), shape = 16, size = 5, col = "#fc9e4f", alpha = 0.90) +
  scale_x_continuous(name = "Ano", breaks = seq(2000, 2018, by = 1)) +
  scale_y_continuous(name ="TFT", breaks = seq(35, 75, by = 5), limits = c(26, 75)) +
  theme_bw() +
  labs(title = "RMM Observada e Ajustada - Brasil 2000-2018",
       x = 'Ano', 
       y = 'RMM',
       caption = "OBS: Os valores de Nascimentos e População foram interpolados linearmente entre 2000, 2010 e 2018 \n
       Fonte: IBGE - Censo Demográfico 2000 e 2010; SINASC 2011-2018; SIM 2000, 2010 e 2018; Proje??es IBGE 2018") +
 theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing.x = grid::unit(0.15, "cm"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "grey20", size = 12, hjust = 0.5, face = "bold"), 
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        #axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(angle = 50, hjust = 1, size = 11)) + 
  geom_point(aes(x = 2000, y = 30), shape = 'O', size = 5, color = 'black', alpha = 0.80) +
  geom_text(x = 2001, y = 30, label = 'RMM Obs', size = 3.8, hjust = 'left', color = 'black') +
  geom_point(aes(x = 2000, y = 27), shape = 16, size = 4.5, col = "#3A3B3C", alpha = 1.5) +
  geom_text(x = 2001, y = 27, label = "Brass", size = 3.8, hjust = 'left',col = "black") +
  geom_point(aes(x = 2005, y = 30), shape = 16, size = 4.5, col = "#f4442e", alpha = 0.8) +
  geom_text(x = 2006, y = 30, label = 'Coale & Trussel', size = 3.8, hjust = 'left', col = "black") +
  geom_point(aes(x = 2005, y = 27), shape = 16, size = 4.5, col = "#1282a2", alpha = 1.2) +
  geom_text(x = 2006, y = 27, label = 'Gompertz', size = 3.8, hjust = 'left', col = "black") +
  geom_point(aes(x = 2011, y = 30), shape = 16, size = 4.5, col = "#fc9e4f", alpha = 0.90) +
  geom_text(x = 2012, y = 30, label = 'Gompertz sem Parturição', size = 3.8, hjust = 'left', color = 'black')


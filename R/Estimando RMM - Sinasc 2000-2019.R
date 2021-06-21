# Estimacao da RMM e TFTs para Brasil e UFs 2010-2018
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

#devtools::install_github("josehcms/fertestr")
library(fertestr)

##----------------------------------------------------------------------------------------------------------
## ATENCAO

# Para obter os dados de fecundidade usados neste script, primeiro rode o "Criando SINASC fecundidade 2000-2019.R"

##----------------------------------------------------------------------------------------------------------

## Importando os dados
Obitos.maternos2000.2019 <- read.csv('data/Prop Obitos Maternos 2000-2019.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')

##----------------------------------------------------------------------------------------------------------

## Organizando o Banco de Dados

## 1) Obitos Maternos ##
Obitos.maternos2000.2019 <- Obitos.maternos2000.2019 %>%
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

################################
## 3) GRAFICO das TEFs por metodo para Brasil ##
fecundidade.2000.2019 %>%
  filter(Estado == 99,
         Ano == 2000|Ano == 2003|Ano == 2006|Ano == 2009|Ano == 2012|Ano == 2015|Ano == 2018) %>%
  ggplot(aes(x = idade)) + 
  geom_path(aes(y = ASFR,  color = "Observada"), linetype = "solid", size = 1.2) + 
  geom_path(aes(y = Gompertz.sem.P, color = "Gompertz sem Parturicao"), linetype = "twodash", size = 1.2) + 
  labs(title = "Taxa Específica de Fecundidade - Brasil 2000 - 2018",
       caption = "Fonte: IBGE - Censo Demográfico 2000 e 2010",
       y = "TEF",
       color = "Metodos") +
  theme_bw() +
  scale_color_manual(name = "Métodos:",
                     values = c("#00916e", "#00509d")) +
  scale_x_continuous(name ="Idade", breaks = seq(15, 45, 5)) +
  scale_y_continuous(name ="TEF", breaks = seq(0, 0.13, 0.025)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing.x = grid::unit(0.15, "cm"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "grey20", size = 15, hjust = 0.5, face = "bold"), 
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside") + 
  facet_wrap(~Ano, scales = "free")

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

################################
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

################################
## 2) GRAFICO comparativo de pontos das razoes de diferencas (FATOR DE CORRECAO) entre a TFT e os metodos
TFT.2000.2019 %>%
  filter(Ano == 2010) %>%
  arrange(Estado) %>%
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
  geom_point(shape = 16, size = 6, color = '#5A4956', alpha = 0.80) + 
  #geom_segment(aes(x = Direta, xend = ratio.Obs.Gompertz.sem.P, y = index, yend = index, lwd = 3.5, alpha = 0.20)) +
  geom_hline(yintercept = c(7, 16, 20, 23, 27) + 0.50, lwd = 1.1, col = 'darkgrey') +
  geom_point(aes(x = ratio.Obs.Gompertz.sem.P, y = index), shape = 16, size = 5, col = "#d45e79", alpha = 0.80) +
  scale_y_continuous(breaks = NULL, limits = c(1, 32.5)) +
  scale_x_continuous(breaks= seq(0.98, 1.14, by = 0.01)) +
  theme_bw() +
  labs(x = 'Razão entre Método/Estimacao Direta da TFT', y = '') +
  geom_segment(aes(x = ratio.Obs.Gompertz.sem.P, xend = Direta, y = index, yend = index), color = 'darkgrey') +
  geom_text(aes(x = ratio.Obs.Gompertz.sem.P, y = index, label = sigla), color = 'black', size = 3.5, fontface = 'bold') +
  theme(axis.text = element_text(size = 12, face = 'bold', color = 'black'),
        axis.title = element_text(size = 12, face = 'bold', color = 'black')) + 
  guides(color = FALSE, size = FALSE, alpha = FALSE) +
  geom_text(label = 'Norte', aes(x = 0.985, y = 3.0, angle = 90, hjust = 'center', 
                                 size = 3.5), color = 'black') +
  geom_text(label = 'Nordeste', aes(x = 0.985, y = 12.5, angle = 90, hjust = 'center', 
                                    size = 3.5), color = 'black') +
  geom_text(label = 'Sudeste', aes(x = 0.985, y = 18.5, angle = 90, hjust = 'center', 
                                   size = 3.5), color = 'black') +
  geom_text(label = 'Sul', aes(x = 0.985, y = 22.0, angle = 90, hjust = 'center', 
                               size = 3.5), color = 'black') +
  geom_text(label = 'Centro-\nOeste', aes(x = 0.985, y = 25.3, angle = 90, hjust = 'center', 
                                          size = 3.5), color = 'black')+
  geom_text(label = 'Brasil', aes(x = 0.985, y = 29.0, angle = 90, hjust = 'center', 
                                  size = 3.5), color = 'black') +
  geom_point(aes(x = 1.02, y = 32.5), shape = 16, size = 4.5, color = '#5A4956', alpha = 0.80) +
  geom_text(x = 1.022, y = 32.5, label = 'Estimação Direta', size = 3.8,
            hjust = 'left', color = 'black') +
  geom_point(aes(x = 1.02, y = 30.5), shape = 16, size = 4.5, col =  "#d45e79", alpha = 0.80) +
  geom_text(x = 1.022, y = 30.5, label = 'Gompertz sem Parturição', size = 3.8,
            hjust = 'left', color = 'black')

##----------------------------------------------------------------------------------------------------------

## 3)GRAFICO TFT Brasil 2000-2019

TFT.2000.2019 %>%
  filter(Estado == 99) %>%
  ggplot(aes(x = Ano)) + 
  geom_point(aes(y = TFT,  color = "TFT"), size = 2) + 
  geom_point(aes(y = Gompertz.sem.P, color = "Gompertz sem Parturição"), size = 2) + 
  labs(title = "Taxa Fecundidade Total - Brasil 2000-2019",
       caption = "Fonte: IBGE - SINASC 2011-2019 e Projeções de População IBGE",
       color = "Metodos") +
  theme_bw() +
  scale_y_continuous(name ="TFT", breaks = c(1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2), limits = c(1.6, 2.2)) +
  scale_color_manual(name = "Métodos:",
                     values = c("#d45e79", "#5A4956")) +
  scale_x_continuous(name ="Ano", breaks = seq(2000, 2019, by = 1)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing.x = grid::unit(0.15, "cm"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "grey20", size = 12, hjust = 0.5, face = "bold"), 
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(angle = 50, hjust = 1, size = 11))

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
## 2) GRAFICO - RMM Brasil 2000-2019 - Comparação OBS e Gompertz sem Parturicao

TFT.2000.2019 %>%
  filter(Estado == 99) %>%
  ggplot(aes(x = Ano, y = RMM) ) + 
  geom_point(shape = 'O', size = 6, color = '#5A4956', alpha = 1.50) +
  geom_point(aes(x = Ano, y = RMM.Obt.Corr), shape = 16, size = 5, col = "#9799ca", alpha = 0.90) +
  geom_point(aes(x = Ano, y = RMM.Gompertz.sem.P), shape = 16, size = 5, col = "#d45e79", alpha = 0.90) +
  scale_x_continuous(name = "Ano", breaks = seq(2000, 2019, by = 1)) +
  scale_y_continuous(name ="RMM", breaks = seq(43, 69, by = 5), limits = c(43, 69)) +
  theme_bw() +
  labs(title = "RMM Observada e Ajustada - Brasil 2000-2019",
       x = 'Ano', 
       y = 'RMM',
       caption = "Fonte: SINASC 2011-2019; SIM 2000-2019; Projecoes IBGE 2018") +
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
  geom_point(aes(x = 2000, y = 46), shape = 16, size = 5, color = '#9799ca', alpha = 0.80) +
  geom_text(x = 2001, y = 46, label = 'RMM Obitos Ajustados', size = 3.8, hjust = 'left', color = 'black') +
  geom_point(aes(x = 2000, y = 44.5), shape = 'O', size = 5, color = '#5A4956', alpha = 0.80) +
  geom_text(x = 2001, y = 44.5, label = 'RMM Observada', size = 3.8, hjust = 'left', color = 'black') +
  geom_point(aes(x = 2000, y = 43), shape = 16, size = 4.5, col = "#d45e79", alpha = 0.90) +
  geom_text(x = 2001, y = 43, label = 'Gompertz sem Parturição', size = 3.8, hjust = 'left', color = 'black')


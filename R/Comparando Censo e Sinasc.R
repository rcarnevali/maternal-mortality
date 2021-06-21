# Censo e Sinasc
# Author: Rafaella Carnevali
# Build under R version 4.1.0
options(scipen = 9999)

## Pacotes
.packages = c("devtools", "stringr", "foreign", "Hmisc",
              "scales", "zoo", "janitor", "ggplot2", "tidyr",
              "dplyr", "data.table", "gt", "paletteer")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0)
  install.packages(.packages[!.inst],dependencies = T)

# Load packages into session
lapply(.packages, require, character.only = T)

#devtools::install_github("josehcms/fertestr")
library(fertestr)

##----------------------------------------------------------------------------------------------------------

## Importando os dados
fecundidade2000.2010 <- read.csv('data/Fecundidade 2000-2010 Censo.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
fecundidade.SN.2000.2010 <- read.csv('data/Fecundidade 2000-2010 Sinasc.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')
fecundidade2011.2019 <- read.csv('data/Fecundidade 2011-2019 Sinasc.csv', dec = ",", header = TRUE, stringsAsFactors = FALSE, sep = ';')

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

## 2) SINASC 2000 e 2010 - DATASUS ##

fecundidade.SN.2000.2010 <- fecundidade.SN.2000.2010 %>%
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
         ASFR.Sinasc = SINASC/Mulheres) %>%
  filter(idade >=  15,
         idade <= 45) 


################################
## 3) SINASC 2011-2019 - DATASUS ##
fecundidade2011.2019 <- fecundidade2011.2019 %>%
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


##----------------------------------------------------------------------------------------------------------
## GRAFICO Filhos Tidos no ultimo ano 2000 e 2010 - BR e UFs

Filhos.tidos <- fecundidade2000.2010 %>%
  left_join(fecundidade.SN.2000.2010 %>% select(Ano, Estado, idade, SINASC), 
            by = c("idade", "Ano", "Estado")) %>%
  as.data.frame()

scale_factor <- 1

Estados <- as.factor(unique(Filhos.tidos$UF))

plot_list = list() 

for (i in Estados) {
  print(paste ("Processing", i, sep = " "))
  
  # png(paste0("TEF 2000-2010 Censo x Sinasc_", i,".png"), height = 9, width = 9, unit = 'in', res = 500)
  
  temp_plot <- Filhos.tidos %>%
    filter(UF == i) %>%
    ggplot(aes(x = idade)) + 
    geom_path(aes(y = Filhos.tidos.no.ultimo.ano,  color = "Censo"), linetype = "solid", size = 1.2) + 
    geom_path(aes(y =  SINASC, color = "Sinasc"), linetype = "twodash", size = 1.2) +
    labs(title = paste("Filhos Tidos nos ?ltimos 12 Meses", i, "- 2000 e 2010 - Censo e SINASC", sep = " "),
         caption = "Fonte: IBGE - Censo Demografico 2000 e 2010 e SINASC 2000 e 2010",
         y = "",
         color = "Fonte") + 
    theme_bw() +
    scale_x_continuous(name = "Idade", breaks = seq(15, 45, 5)) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.spacing.x = grid::unit(0.15, "cm"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(color = "grey20", size = 15, hjust = 0.5, face = "bold"), 
          legend.title = element_text(size = scale_factor * 11, face = "bold"),
          legend.text = element_text(size = scale_factor * 11),
          axis.title = element_text(size = scale_factor * 12),
          axis.text.y = element_text(size = scale_factor * 11),
          axis.text.x = element_text(size = scale_factor * 12),
          strip.text = element_text(size = scale_factor * 12),
          strip.background = element_blank(),
          strip.placement = "outside") +
    facet_wrap("Ano") 
  
  plot_list[[i]] <- temp_plot
  
  # print(plot_list[[i]])
  # 
  # print(temp_plot)
  
  ggsave(plot_list[[i]], file = paste0("Filhos Tidos Ultimo Ano 2000-2010 Censo x Sinasc_", i,".png"),
         dpi = 500, height = 9, width = 9, unit = 'in', scale = scale_factor)
  
  # print(temp_plot)
  # 
  # dev.off()
}

## TABELA Raz?o Total Filhos Tidos no ?ltimo ano

# webshot::install_phantomjs()

tab_1 <- Filhos.tidos %>%
  ungroup() %>%
  group_by(Ano, UF) %>%
  mutate(Censo = sum(Filhos.tidos.no.ultimo.ano),
         sist.Nasc = sum(SINASC),
         Ratio = Censo/sist.Nasc) %>%
  distinct(UF, Ratio) %>%
  filter(Ano == 2000) %>%
  ungroup() %>%
  select(UF, Ratio) %>%
  gt() %>%
  fmt_number(columns = c(Ratio), decimals = 3) %>%
  cols_label(Ratio = "Razao Censo/Sinasc",
             UF = "Unidade da Federacao") %>%
  tab_header(
    title = md("**Razao Censo/Sinasc Filhos Tidos no último Ano - 2000**"),
    subtitle = "Brasil e UFs") %>%
  tab_source_note(md("Fonte: IBGE - Censo Demografico 2000 e 2010 e SINASC 2000 e 2010"))  %>%
  tab_style(
    style = cell_fill(color = "#d8f3dc"),
    locations = cells_body(rows = Ratio > 1)) %>%
  tab_style(
    style = cell_fill(color = "#fbc4ab"),
    locations = cells_body(rows =  Ratio < 1))

tab_1 %>% gt::gtsave("Tabela Razão Censo/Sinasc Filhos Tidos 12 meses - 2000.png", #path = "~", 
                     zoom = 10.5, expand = 60)


Filhos.tidos.wide <- Filhos.tidos %>%
  group_by(Ano, UF, Estado, sigla, region) %>%
  mutate(Censo = sum(Filhos.tidos.no.ultimo.ano),
         sist.Nasc = sum(SINASC),
         Ratio = Censo/sist.Nasc) %>%
  ungroup() %>%
  distinct(UF, Ratio, Ano, Estado, sigla, region) %>%
  mutate(name = ifelse(Ano == 2000,
                       name <- "ratio_2000",
                       name <- "ratio_2010")) %>%
  select(-Ano) %>%
  pivot_wider(names_from = name, values_from = Ratio) %>%
  arrange(Estado)

Filhos.tidos.wide %>%
  arrange (Estado) %>%
  mutate(index = rep(seq(1, 28))) %>%
  ggplot(aes(x = ratio_2000, y = index, color = region)) + 
  geom_point(shape = 16, size = 5, color = "#9799ca", alpha = 1.0) + 
  geom_point(aes(x = ratio_2010, y = index),  shape = 16, size = 5, col = "#E18EA2", alpha = 0.85) +
  geom_hline(yintercept = c(7, 16, 20, 23, 27) + 0.50, lwd = 1.1, col = 'darkgrey') +
  scale_y_continuous(breaks = NULL, limits = c(1, 33)) +
  scale_x_continuous(breaks = seq(0.7, 1.5, by = 0.15), limits = c(0.57, 1.46)) +
  theme_bw() +
  labs(x = 'Raz?o Censo/Sinasc Filhos Tidos no último Ano - 2000 e 2010', 
       y = '',
       caption = "Fonte: IBGE - Censo Demografico 2000 e 2010 e SINASC 2000 e 2010")  +
  geom_segment(aes(x = ratio_2000, xend = ratio_2010, y = index, yend = index), color = 'darkgrey') +
  geom_text(aes(x = ratio_2010, y = index, label = sigla), color = 'black', size = 4, fontface = 'bold') +
  theme(axis.text = element_text(size = 12, face = 'bold', color = 'black'),
        axis.title = element_text(size = 12, face = 'bold', color = 'black')) + 
  guides(color = FALSE, size = FALSE, alpha = FALSE) +
  geom_text(label = 'Norte', aes(x = 0.58, y = 3.0, angle = 90, hjust = 'center', 
                                 size = 0.6), color = 'black') +
  geom_text(label = 'Nordeste', aes(x = 0.58, y = 12.5, angle = 90, hjust = 'center', 
                                    size = 0.6), color = 'black') +
  geom_text(label = 'Sudeste', aes(x = 0.58, y = 18.5, angle = 90, hjust = 'center', 
                                   size = 0.6), color = 'black') +
  geom_text(label = 'Sul', aes(x = 0.58, y = 22.0, angle = 90, hjust = 'center', 
                               size = 0.6), color = 'black') +
  geom_text(label = 'Centro-\nOeste', aes(x = 0.58, y = 25.3, angle = 90, hjust = 'center', 
                                          size = 0.6), color = 'black')+
  geom_text(label = 'Brasil', aes(x = 0.58, y = 29.0, angle = 90, hjust = 'center', 
                                  size = 0.6), color = 'black') +
  geom_point(aes(x = 1.33, y = 31), shape = 16, size = 5, color = "#9799ca", alpha = 1.0) +
  geom_text(x = 1.36, y = 31, label = '2000', size = 4.8,
            hjust = 'left', color = 'black') +
  geom_point(aes(x = 1.33, y = 32.5), shape = 16, size = 5, col = "#E18EA2", alpha = 0.85) +
  geom_text(x = 1.36, y = 32.5, label = '2010', size = 4.8,
            hjust = 'left', color = 'black')

##----------------------------------------------------------------------------------------------------------

## GRAFICO TEFs 2000 e 2010 - BR e UFs
scale_factor <- 1

TEF.Junta <- fecundidade2000.2010 %>%
  left_join(fecundidade.SN.2000.2010 %>% select(Ano, Estado, idade, ASFR.Sinasc), 
            by = c("idade", "Ano", "Estado")) %>%
  as.data.frame()

temp_plot <- TEF.Junta %>%
  filter(Estado == 99) %>%
  ggplot(aes(x = idade)) + 
  geom_path(aes(y = ASFR,  color = "Censo"), linetype = "solid", size = 1.2) + 
  geom_path(aes(y =  ASFR.Sinasc, color = "Sinasc"), linetype = "twodash", size = 1.2) +
  labs(title = "Taxa Espec?fica de Fecundidade - Brasil 2000 e 2010 - Censo e SINASC",
       caption = "Fonte: IBGE - Censo Demogr?fico 2000 e 2010 e SINASC 2000 e 2010",
       y = "TEF") +
  theme_bw() +
  scale_color_manual(name = "Fonte:",  values = c("#D56062", "#0F5257")) +
  scale_x_continuous(name ="Idade", breaks = seq(15, 45, 5)) +
  scale_y_continuous(name ="TEF", breaks = seq(0, 0.2, 0.025), limits = c(0, 0.2)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing.x = grid::unit(0.15, "cm"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "grey20", size = 15, hjust = 0.5, face = "bold"), 
        legend.title = element_text(size = scale_factor * 11, face = "bold"),
        legend.text = element_text(size = scale_factor * 11),
        axis.title = element_text(size = scale_factor * 12),
        axis.text.y = element_text(size = scale_factor * 11),
        axis.text.x = element_text(size = scale_factor * 12),
        strip.text = element_text(size = scale_factor * 12),
        strip.background = element_blank(),
        strip.placement = "outside") + 
  facet_grid(~Ano)

ggsave(temp_plot, file = "TEF 2000-2010 Censo x Sinasc_BR.png", 
       dpi = 500, height = 9, width = 9, unit = 'in', scale = scale_factor)


Estados <- as.factor(unique(TEF.Junta$UF))

plot_list = list() 

for (i in Estados) {
  print(paste ("Processing", i, sep = " "))
  
  # png(paste0("TEF 2000-2010 Censo x Sinasc_", i,".png"), height = 9, width = 9, unit = 'in', res = 500)
  
  temp_plot <- TEF.Junta %>%
    filter(UF == i) %>%
    ggplot(aes(x = idade)) + 
    geom_path(aes(y = ASFR,  color = "Censo"), linetype = "solid", size = 1.2) + 
    geom_path(aes(y =  ASFR.Sinasc, color = "Sinasc"), linetype = "twodash", size = 1.2) +
    labs(title = paste ("Taxa Espec?fica de Fecundidade", i, "- 2000 e 2010 - Censo e SINASC", sep = " "),
         caption = "Fonte: IBGE - Censo Demografico 2000 e 2010 e SINASC 2000 e 2010",
         y = "TEF") + 
    theme_bw() +
    scale_color_manual(name = "Fonte:",  values = c("#D56062", "#0F5257")) +
    scale_x_continuous(name = "Idade", breaks = seq(15, 45, 5)) +
    scale_y_continuous(name = "TEF", breaks = seq(0, 0.2, 0.05), limits = c(0, 0.2)) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.spacing.x = grid::unit(0.15, "cm"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(color = "grey20", size = 15, hjust = 0.5, face = "bold"), 
          legend.title = element_text(size = scale_factor * 11, face = "bold"),
          legend.text = element_text(size = scale_factor * 11),
          axis.title = element_text(size = scale_factor * 12),
          axis.text.y = element_text(size = scale_factor * 11),
          axis.text.x = element_text(size = scale_factor * 12),
          strip.text = element_text(size = scale_factor * 12),
          strip.background = element_blank(),
          strip.placement = "outside") +
    facet_wrap("Ano") 
  
  plot_list[[i]] <- temp_plot
  
  print(plot_list[[i]])
  
  # print(temp_plot)
  
  ggsave(plot_list[[i]], file = paste0("TEF 2000-2010 Censo x Sinasc_", i,".png"),
         dpi = 500, height = 9, width = 9, unit = 'in', scale = scale_factor)
  
  # print(temp_plot)
  # 
  # dev.off()
}

################################
## GRAFICO TEFs 2011-2019 - BR e UFs
scale_factor <- 1

fec.11.19.graf <- fecundidade2011.2019 %>%
  filter(Ano == 2011 | Ano == 2015 | Ano == 2019) %>%
  mutate(Ano = as.factor(Ano))

Estados <- as.factor(unique(fec.11.19.graf$UF))

plot_list = list() 


for (i in Estados) {
  
  print(paste ("Processing", i, sep = " "))
  
  temp_plot <- fec.11.19.graf %>%
    filter(UF == i) %>%
    ggplot(aes(x = idade, y = ASFR)) + 
    geom_line(aes(color = Ano), size = 1.4, alpha = 1.2) +
    labs(title = paste ("Taxa Espec?fica de Fecundidade", i, "- 2011 e 2019 - SINASC", sep = " "),
         caption = "Fonte: IBGE - SINASC 2011-2019",
         y = "TEF",
         color = "Ano") +
    theme_bw() +
    scale_color_manual(name = "Fonte:",  values = c("#355070", "#e27396", "#9db9e0")) +
    #scale_color_paletteer_d("cartography::blue.pal", 8, direction = -1) +
    scale_x_continuous(name ="Idade", breaks = seq(15, 45, 5)) +
    scale_y_continuous(name ="TEF", breaks = seq(0, 0.15, 0.025), limits = c(0, 0.15)) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.spacing.x = grid::unit(0.15, "cm"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(color = "grey20", size = 15, hjust = 0.5, face = "bold"), 
          legend.title = element_text(size = scale_factor * 11, face = "bold"),
          legend.text = element_text(size = scale_factor * 11),
          axis.title = element_text(size = scale_factor * 12),
          axis.text.y = element_text(size = scale_factor * 11),
          axis.text.x = element_text(size = scale_factor * 12),
          strip.text = element_text(size = scale_factor * 12),
          strip.background = element_blank(),
          strip.placement = "outside") 
  
  plot_list[[i]] <- temp_plot

  ggsave(plot_list[[i]], file = paste0("TEF 2011-2019 Sinasc_", i,".png"),
         dpi = 500, height = 9, width = 9, unit = 'in', scale = scale_factor)
}  

##----------------------------------------------------------------------------------------------------------
## Calculando a TEF por diferentes m?todos para UFs

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
## 3) Calculando os ajustes de cada m?todo por idade ##
  TEF.2000.2010 <- TEF.2000.2010 %>%
    mutate(ratio.Obs.Brass = Brass/ASFR,
           ratio.Obs.CT = Coale.Trussel/ASFR,
           ratio.Obs.Gompertz = Gompertz/ASFR)
  
##----------------------------------------------------------------------------------------------------------
################################
## GRAFICO das TEFs por metodo para Brasil ##
  TEF.2000.2010 %>%
    filter(Estado == 99) %>%
    left_join(fecundidade.BR.SN.00.10 %>% select(Ano, idade, Parity.Sinasc, ASFR.Sinasc), by = c("idade", "Ano")) %>%
    ggplot(aes(x = idade)) + 
    geom_path(aes(y = ASFR,  color = "Censo"), linetype = "solid", size = 1.2) + 
    geom_path(aes(y = Gompertz, color = "Censo (Gompertz)"), linetype = "twodash", size = 1.2) + 
    geom_path(aes(y = Brass, color = "Censo (Brass)"), linetype = "dashed", size = 1.2) +
    #geom_path(aes(y = Coale.Trussel, color = "Coale & Trussel"), linetype = "dashed", size = 1.2) +
    geom_path(aes(y = ASFR.Sinasc, color = "SINASC"), linetype = "solid", size = 1.2) +
    labs(title = "Taxa Espec?fica de Fecundidade - Brasil 2000 e 2010",
         caption = "Fonte: IBGE - Censo Demografico 2000 e 2010",
         y = "TEF",
         color = "Metodos") +
    theme_bw() +
    scale_color_manual(name = "M?todos:",
                       values = c("#00916e", "#00509d", "#fe9920", "black")) +
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
  
##---------------------------------------------------------------------------------------------------------
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
  
## 2) De 2000-2010 - SINASC ## 
TFT.Sinasc.2000.2010 <- fecundidade.SN.2000.2010 %>%
  group_by(Estado, UF, Ano) %>%
  summarise(Women = sum(Mulheres),
            Nascimentos.S = sum(SINASC),
            TFT.SINASC = (sum(ASFR.Sinasc)*5)) %>%
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

##----------------------------------------------------------------------------------------------------------  

## GRAFICO TFT CENSO + SINASC + METODOS

TFT.2000.2010 %>%
  left_join(TFT.Sinasc.2000.2010, by = c("Estado", "Ano", "Women", "UF"))  %>% 
  filter(Ano == 2010) %>%
  arrange(Estado) %>%
  mutate(Censo = TFT,
         index = rep(seq(1, 28), times = length(unique(Ano))),
         region = case_when(11 <= Estado & Estado <= 17 ~ "Norte",
                            21 <= Estado & Estado <= 29 ~ "Nordeste",
                            (31 <= Estado & Estado <= 33) | Estado == 35 ~ "Sudeste",
                            41 <= Estado & Estado <= 43 ~ "Sul",
                            50 <= Estado & Estado <= 53 ~ "Centro-Oeste"),
         sigla = case_when(UF == "Rond?nia" ~ "RO",
                           UF == "Acre" ~ "AC",
                           UF == "Amazonas" ~ "AM",
                           UF == "Roraima" ~ "RR",
                           UF == "Par?" ~ "PA",
                           UF == "Amap?" ~ "AP",
                           UF == "Tocantins" ~ "TO",
                           UF == "Maranh?o" ~ "MA",
                           UF == "Piau?" ~ "PI",
                           UF == "Cear?" ~ "CE",
                           UF == "Rio Grande do Norte" ~ "RN",
                           UF == "Para?ba" ~ "PB",
                           UF == "Pernambuco" ~ "PE",
                           UF == "Alagoas" ~ "AL",
                           UF == "Sergipe" ~ "SE",
                           UF == "Bahia" ~ "BA",
                           UF == "Minas Gerais" ~ "MG",
                           UF == "Esp?rito Santo" ~ "ES",
                           UF == "Rio de Janeiro" ~ "RJ",
                           UF == "S?o Paulo" ~ "SP",
                           UF == "Paran?" ~ "PR",
                           UF == "Santa Catarina" ~ "SC",
                           UF == "Rio Grande do Sul" ~ "RS",
                           UF == "Mato Grosso do Sul" ~ "MS",
                           UF == "Mato Grosso" ~ "MT",
                           UF == "Goi?s" ~ "GO",
                           UF == "Distrito Federal" ~ "DF",
                           UF == "Brasil" ~ "BR")) %>%
  ggplot(aes(x = Censo, y = index, color = region)) + 
  geom_point(shape = 'O', size = 6, color = 'black', alpha = 0.80) + 
  geom_point(aes(x = TFT.SINASC, y = index), shape = 16, size = 5, col = "#D56062", alpha = 0.75) +
  geom_hline(yintercept = c(7, 16, 20, 23, 27) + 0.50, lwd = 1.1, col = 'darkgrey') +
  geom_point(aes(x = Brass, y = index), shape = 16, size = 5, col =  "#9395C8", alpha = 1.2) +
  #geom_point(aes(x = Coale.Trussel, y = index), shape = 16, size = 5, col = "#f6ae2d", alpha = 0.90) +
  geom_point(aes(x = Gompertz, y = index), shape = 16, size = 5, col = "#f6ae2d", alpha = 0.90) +
  scale_y_continuous(breaks = NULL, limits = c(1, 35.5)) +
  scale_x_continuous(breaks = seq(1.5, 4.5, by = 0.5)) +
  theme_bw() +
  labs(x = 'Compara??o Estima??o TFT Censo, Sinasc e M?todos(Censo)', 
       y = '',
       caption = "Fonte: IBGE - Censo Demografico 2000 e 2010 e SINASC 2000 e 2010") +
  geom_segment(aes(x = Censo, xend = Gompertz, y = index, yend = index), color = 'darkgrey') +
  geom_text(aes(x = Brass, y = index, label = sigla), color = 'black', size = 4, fontface = 'bold') +
  theme(axis.text = element_text(size = 12, face = 'bold', color = 'black'),
        axis.title = element_text(size = 12, face = 'bold', color = 'black')) + 
  guides(color = FALSE, size = FALSE, alpha = FALSE) +
  geom_text(label = 'Norte', aes(x = 1.2, y = 3.0, angle = 90, hjust = 'center', 
                                 size = 3.5), color = 'black') +
  geom_text(label = 'Nordeste', aes(x = 1.2, y = 12.5, angle = 90, hjust = 'center', 
                                    size = 3.5), color = 'black') +
  geom_text(label = 'Sudeste', aes(x = 1.2, y = 18.5, angle = 90, hjust = 'center', 
                                   size = 3.5), color = 'black') +
  geom_text(label = 'Sul', aes(x = 1.2, y = 22.0, angle = 90, hjust = 'center', 
                               size = 3.5), color = 'black') +
  geom_text(label = 'Centro-\nOeste', aes(x = 1.2, y = 25.3, angle = 90, hjust = 'center', 
                                          size = 3.5), color = 'black')+
  geom_text(label = 'Brasil', aes(x = 1.2, y = 29.0, angle = 90, hjust = 'center', 
                                  size = 3.5), color = 'black') +
  annotate(geom = "text", x = 1.35, y = 33.0, size = 11, color = "grey20", label = paste('2010'))  +
  geom_point(aes(x = 2.8, y = 32.5), shape = 'O', size = 6, color = 'black', alpha = 0.80) +
  geom_text(x = 2.86, y = 32.5, label = 'Censo', size = 3.8,
            hjust = 'left', color = 'black') +
  geom_point(aes(x = 2.8, y = 35.5), shape = 16, size = 4.5, col = "#D56062", alpha = 0.75) +
  geom_text(x = 2.86, y = 35.5, label = 'Sinasc', size = 3.8,
            hjust = 'left', color = 'black') +
  geom_point(aes(x = 2.8, y = 34), shape = 16, size = 4.5, color = "#51355A", alpha = 0.90) +
  geom_text(x = 2.86, y = 34, label = "Brass(Censo)", size = 3.8,
            hjust = 'left', color = 'black') +
  geom_point(aes(x = 2.8, y = 31), shape = 16, size = 4.5, col =  "#f6ae2d", alpha = 1.2) +
  geom_text(x = 2.86, y = 31, label = 'Gompertz(Censo)', size = 3.8,
            hjust = 'left', color = 'black') 



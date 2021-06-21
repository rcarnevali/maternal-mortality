# Obtendo dados do SINASC
# Author: Rafaella Carnevali
# Build under R version 4.0.3
options(scipen = 9999)

library(tidyverse)
library(read.dbc)
library(foreign)
library(naniar)

##----------------------------------------------------------------------------------------------------------

## Definindo os dados para importar
vars <- c("LOCNASC", "IDADEMAE", "CONSULTAS", "CODMUNRES", "DTNASC", "QTDFILVIVO", "QTDFILMORT")

#Para todas as variaveis:
  # variaveis <- c("contador", "ORIGEM", "CODCART", "NUMREGCART", "DTREGCART", "CODESTAB", "CODMUNNASC", "LOCNASC", "IDADEMAE", 
  # "ESTCIVMAE", "ESCMAE", "CODOCUPMAE", "QTDFILVIVO", "QTDFILMORT", "CODMUNRES",  "CODPAISRES", "GESTACAO", "GRAVIDEZ", "PARTO",
  # "CONSULTAS", "DTNASC", "HORANASC", "SEXO", "APGAR1", "APGAR5", "RACACOR", "PESO", "IDANOMAL", "DTCADASTRO", "CODANOMAL",
  # "NUMEROLOTE", "VERSAOSIST", "DTRECEBIM", "DIFDATA", "DTRECORIG", "NATURALMAE", "CODMUNNATU", "SERIESCMAE", "DTNASCMAE", 
  # "RACACORMAE", "QTDGESTANT", "QTDPARTNOR", "QTDPARTCES", "IDADEPAI", "DTULTMENST", "SEMAGESTAC", "TPMETESTIM", "CONSPRENAT",
  # "MESPRENAT", "TPAPRESENT", "STTRABPART", "STCESPARTO", "TPROBSON", "STDNEPIDEM", "STDNNOVA")

anos <- c("2000", "2010", "2018")
ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DN"
files_list <- as.vector(sapply(url, paste, ufs, anos, ".DBC", sep = ""))

##----------------------------------------------------------------------------------------------------------

### Importando os dados
sinasc <- NULL

for(file in files_list){
  
  temp <- tempfile()
  
  # Fazendo Dowload
  part <- data.frame()
  download.file(file, temp, mode = "wb")
  part <- read.dbc::read.dbc(temp)
  file.remove(temp)
  
  # Juntando tudo
  if(nrow(part) > 0){
    if(!all(vars %in% names(part))) stop("Alguma variável não existe")
    if(is.null(vars)){
      sinasc <- plyr::rbind.fill(sinasc, part)
    } else {
      sinasc <- plyr::rbind.fill(sinasc, subset(part, select = vars))
    }
  }
}

sinasc <- sinasc %>%
  mutate(nascimento = ifelse(as.character(DTNASC) == "NULL", NA, as.character(DTNASC)),
         nac.char = nchar(nascimento),
         ano_nasc  = as.numeric(year(dmy(nascimento))),
         ano_nasc = ifelse(nac.char == 4 ,
                           as.numeric(nascimento),
                           ano_nasc),
         ano_nasc = ifelse(nac.char == 6 ,
                           as.numeric(str_sub(nascimento, 3, nchar(nascimento))),
                           ano_nasc),
         CODMUNRES = as.numeric(as.character(CODMUNRES)),
         UF = as.numeric(str_sub(as.character(CODMUNRES), start = 1, end = 2)),
         Estado = case_when(UF == 11 ~ "RO",
                            UF == 12 ~ "AC",
                            UF == 13 ~ "AM",
                            UF == 14 ~ "RR",
                            UF == 15 ~ "PA",
                            UF == 16 ~ "AP",
                            UF == 17 ~ "TO",
                            UF == 21 ~ "MA",
                            UF == 22 ~ "PI",
                            UF == 23 ~ "CE",
                            UF == 24 ~ "RN",
                            UF == 25 ~ "PB",
                            UF == 26 ~ "PE",
                            UF == 27 ~ "AL",
                            UF == 28 ~ "SE",
                            UF == 29 ~ "BA",
                            UF == 31 ~ "MG",
                            UF == 32 ~ "ES",
                            UF == 33 ~ "RJ",
                            UF == 35 ~ "SP",
                            UF == 41 ~ "PR",
                            UF == 42 ~ "SC",
                            UF == 43 ~ "RS",
                            UF == 50 ~ "MS",
                            UF == 51 ~ "MT",
                            UF == 52 ~ "GO",
                            UF == 53 ~ "DF"),
         region = case_when(11 <= UF & UF <= 17 ~ "North",
                            21 <= UF & UF <= 29 ~ "Northeast",
                            (31 <= UF & UF <= 33) | UF == 35 ~ "Southeast",
                            41 <= UF & UF <= 43 ~ "South",
                            50 <= UF & UF <= 53 ~ "Center-West"),
         IdadeReprod = ifelse(Idade_MS >= 10 & Idade_MS <= 49,
                              "Sim",
                              "N?o")) %>%
  select(!c(nascimento, nac.char))

##----------------------------------------------------------------------------------------------------------

### Se o codigo acima nao der certo
#devtools::install_github("rfsaldanha/microdatasus")
library(microdatasus)
ano <- 2018
sinasc <- microdatasus::fetch_datasus(ano, 01, ano, 12, information_system = "SINASC", vars = vars)
sinasc <- microdatasus::process_sinasc(sinasc, municipality_data = TRUE)

sinasc <- sinasc %>%
  mutate(CODMUNRES = as.numeric(as.character(CODMUNRES)),
         UF = as.numeric(str_sub(as.character(CODMUNRES), start = 1, end = 2)),
         sigla = case_when(UF == 11 ~ "RO",
                           UF == 12 ~ "AC",
                           UF == 13 ~ "AM",
                           UF == 14 ~ "RR",
                           UF == 15 ~ "PA",
                           UF == 16 ~ "AP",
                           UF == 17 ~ "TO",
                           UF == 21 ~ "MA",
                           UF == 22 ~ "PI",
                           UF == 23 ~ "CE",
                           UF == 24 ~ "RN",
                           UF == 25 ~ "PB",
                           UF == 26 ~ "PE",
                           UF == 27 ~ "AL",
                           UF == 28 ~ "SE",
                           UF == 29 ~ "BA",
                           UF == 31 ~ "MG",
                           UF == 32 ~ "ES",
                           UF == 33 ~ "RJ",
                           UF == 35 ~ "SP",
                           UF == 41 ~ "PR",
                           UF == 42 ~ "SC",
                           UF == 43 ~ "RS",
                           UF == 50 ~ "MS",
                           UF == 51 ~ "MT",
                           UF == 52 ~ "GO",
                           UF == 53 ~ "DF"),
         Estado = case_when(sigla == "RO" ~"Rondônia",
                            sigla == "AC" ~ "Acre",
                            sigla == "AM" ~ "Amazonas",
                            sigla == "RR" ~ "Roraima",
                            sigla == "PA" ~ "Pará",
                            sigla == "AP" ~ "Amapá",
                            sigla == "TO" ~ "Tocantins",
                            sigla == "MA" ~ "Maranhão",
                            sigla == "PI" ~ "Piauí",
                            sigla == "CE" ~ "Ceará",
                            sigla == "RN" ~ "Rio Grande do Norte",
                            sigla == "PB" ~ "Paraíba",
                            sigla == "PE" ~ "Pernambuco",
                            sigla == "AL" ~ "Alagoas",
                            sigla == "SE" ~ "Sergipe",
                            sigla == "BA" ~ "Bahia",
                            sigla == "MG" ~ "Minas Gerais",
                            sigla == "ES" ~ "Espírito Santo",
                            sigla == "RJ" ~ "Rio de Janeiro",
                            sigla == "SP" ~ "São Paulo",
                            sigla == "PR" ~ "Paraná",
                            sigla == "SC" ~ "Santa Catarina",
                            sigla == "RS" ~ "Rio Grande do Sul",
                            sigla == "MS" ~ "Mato Grosso do Sul",
                            sigla == "MT" ~ "Mato Grosso",
                            sigla == "GO" ~ "Goiás",
                            sigla == "DF" ~ "Distrito Federal"),
         region = case_when(11 <= UF & UF <= 17 ~ "North",
                            21 <= UF & UF <= 29 ~ "Northeast",
                            (31 <= UF & UF <= 33) | UF == 35 ~ "Southeast",
                            41 <= UF & UF <= 43 ~ "South",
                            50 <= UF & UF <= 53 ~ "Center-West"),
         IdadeReprod = ifelse(IDADEMAE >= 10 & IDADEMAE <= 49,
                              "Sim",
                              "Nao"))

#assign(paste("sinasc", ano, sep = "."), sinasc, envir = .GlobalEnv)

## Visualizando a distribuição de NAs
library(VIM)
library(Amelia)
library(mice)

# sinasc %>%
#   mutate(QTDFILTIDOS = QTDFILVIVO + QTDFILMORT,
#          # FILTIDOS_NA = ifelse(is.na(QTDFILMORT),
#          #                   NA,
#          #                   (QTDFILVIVO %>% replace_na(1)) + QTDFILMORT),
#          ANONASC = year(DTNASC)) %>%
#   select(!c(munResStatus, munResTipo, munResNome, munResUf, munResLat, munResLon, munResAlt, 
#             munResArea, CODMUNRES, DTNASC, IdadeReprod, LOCNASC, sigla)) %>%
#   group_by(Estado) %>%
#   md.pattern()

library(gt)

sinasc_2018 <- sinasc %>%
  mutate(QTDFILTIDOS = QTDFILVIVO + QTDFILMORT,
         # FILTIDOS_NA = ifelse(is.na(QTDFILMORT),
         #                   NA,
         #                   (QTDFILVIVO %>% replace_na(1)) + QTDFILMORT),
         ANONASC = year(DTNASC)) %>%
  select(!c(munResStatus, munResTipo, munResNome, munResUf, munResLat, munResLon, munResAlt, 
            munResArea, CODMUNRES, DTNASC, IdadeReprod, LOCNASC)) %>%
  group_by(Estado) %>%
  select(Estado, QTDFILTIDOS, QTDFILVIVO, QTDFILMORT) %>%
  miss_var_summary() %>%
  select(!n_miss) %>%
  rename(pct_miss_2018 = pct_miss)

library(purrr)
sinasc <- list(sinasc_2000, sinasc_2003, sinasc_2006, sinasc_2009, sinasc_2012, sinasc_2015, sinasc_2018) %>% 
  reduce(left_join, by = c("Estado", "variable"))

joined <- left_join(apples, 
                    left_join(elephants, 
                              left_join(bananas, cats, 
                                            by = c("Estado", "variable"), 
                                            by = c("Estado", "variable")), 
                    by = c("Estado", "variable")))

tabela <- sinasc %>%
  filter(variable == "QTDFILTIDOS") %>%
  select(!variable) %>%
  mutate(pct_miss_2000 = pct_miss_2000/100, 
         pct_miss_2003 = pct_miss_2003/100,
         pct_miss_2006 = pct_miss_2006/100,
         pct_miss_2009 = pct_miss_2009/100,
         pct_miss_2012 = pct_miss_2012/100,
         pct_miss_2015 = pct_miss_2015/100,
         pct_miss_2018 = pct_miss_2018/100) %>%
  ungroup() %>%
  gt() %>%
  tab_header(
    title = md("**Porcentagem de Missing na Informação de Filhos Tidos - SINASC**"),
    subtitle = md("Filhos Tidos = Missing em 'QTD Filhos Vivos' OU em 'QTD Filhos Mortos'")) %>%
  fmt_percent(columns = c(pct_miss_2000, pct_miss_2003, pct_miss_2006, pct_miss_2009, pct_miss_2012, pct_miss_2015, pct_miss_2018),
              decimals = 2) %>%
  tab_spanner(label = "Ano",
              columns = c(pct_miss_2000, pct_miss_2003, pct_miss_2006, pct_miss_2009, pct_miss_2012, pct_miss_2015, pct_miss_2018))  %>%
  cols_label(pct_miss_2000 = "2000",
             pct_miss_2003 = "2003",
             pct_miss_2006 = "2006",
             pct_miss_2009 = "2009",
             pct_miss_2012 = "2012",
             pct_miss_2015 = "2015",
             pct_miss_2018 = "2018")

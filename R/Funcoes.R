
Decomp.MMR <- function (data, ano1, meioperiodo, ano2, siglaUF, ajuste = T) {
  
  if (ajuste) { 
    
    Decomp <- data %>%
      filter(sigla == siglaUF,
             Ano == ano1 | Ano == meioperiodo | Ano == ano2)  %>% 
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
    
    # r = Annual population growth rate: meioperiodo-ano1 and ano1-ano2
    Decomp$r[Decomp$Ano == ano1] <- round(((log(Decomp$Pop[Decomp$Ano == ano1] / 
                                                  Decomp$Pop[Decomp$Ano == meioperiodo])) / (ano1 - meioperiodo)), 3)
    
    Decomp$r[Decomp$Ano == ano2] <- round(((log(Decomp$Pop[Decomp$Ano == ano2] / 
                                                  Decomp$Pop[Decomp$Ano == ano1])) / (ano2 - ano1)), 3)
    
    # P_hat = ano2 estimated population assuming constant annual growth rate from meioperiodo-ano1
    Decomp$P_hat[Decomp$Ano == ano2] <- round(Decomp$Pop[Decomp$Ano == meioperiodo] * 
                                                (exp(10 * Decomp$r[Decomp$Ano == ano1])), 3)
    
    # B_hat = Projected births in ano2 assuming constant fertility
    Decomp$B_hat[Decomp$Ano == ano2] <- round(Decomp$P_hat[Decomp$Ano == ano2] * 
                                                Decomp$TBN[Decomp$Ano == meioperiodo] / 1000, 3)
    
    # B = Actual births in ano2 
    Decomp$B[Decomp$Ano == ano2] <- round(Decomp$Nascimentos[Decomp$Ano == ano2], 3)
    
    # D1_hat = No change in CBR and no change in MMR
    Decomp$D1_hat[Decomp$Ano == ano2] <- round(Decomp$B_hat[Decomp$Ano == ano2] * 
                                                 Decomp$RMM.Ibge[Decomp$Ano == meioperiodo] / 100000, 3)
    
    # D3_hat = MMR declined but CBR did not
    Decomp$D3_hat[Decomp$Ano == ano2] <- round(Decomp$B_hat[Decomp$Ano == ano2] * 
                                                 Decomp$RMM.Ibge[Decomp$Ano == ano2] / 100000, 3)
    
    # D2_hat = CBR declined but MMR did not
    Decomp$D2_hat[Decomp$Ano == ano2] <- round(Decomp$B[Decomp$Ano == ano2] * 
                                                 Decomp$RMM.Ibge[Decomp$Ano == meioperiodo] / 100000, 3)
    
    # D = Both CBR and MMR declined
    Decomp$D[Decomp$Ano == ano2] <- round(Decomp$B[Decomp$Ano == ano2] * 
                                            Decomp$RMM.Ibge[Decomp$Ano == ano2] / 100000, 3)
    
    
    # Y = Total effect of decline in MMR 
    Decomp$Y <- round(Decomp$D1_hat - Decomp$D3_hat, 3)
    
    # X = Total effect of fertility decline
    Decomp$X <- round(Decomp$D1_hat - Decomp$D2_hat, 3)
    
    # Z = Total effect of declines in both fertility and MMR
    Decomp$Z <- round(Decomp$D1_hat - Decomp$D, 3)
    
    Decomp <- Decomp %>%
      mutate(intersec.X.Y = round(X + Y - Z, 3), # intersec.X.Y = Overlap between the effect of declines in fertility and in MMR
             Alpha = round(Y - intersec.X.Y, 3), # Alpha = Net effect of decline in MMR
             Beta = round(X - intersec.X.Y, 3), # Beta = Net effect of fertility decline
             Gama = round((Alpha / Z) * 100, 3), # Gama = Effect of safe motherhood on the % of the  potential number of maternal lives saved in ano2
             Delta = round((Beta / Z) * 100, 3), # Delta = Effect of decrease in live births on the % of the  potential number of maternal lives saved in ano2
             Omega = round((intersec.X.Y / Z) * 100), 3) # Omega = Effect of fertility reduction realized through its effect on MMR reduction
    
    ##-----------------------------------------------------------------------------------------##
    
    ### Estimation of the decline in maternal mortality ratio and number of actual maternal deaths attributable to fertility decline ###
    
    Decomp <- Decomp %>%
      mutate(RMM_hat = NA,
             D.ano1 = NA,
             D.ano2 = NA, 
             D_hat.ano2 = NA,
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
    
    # RMM_hat = MMR in ano2 implied by fertility reduction observed during meioperiodo-ano2
    Decomp$RMM_hat[Decomp$Ano == ano2] <-round(Decomp$RMM.Ibge[Decomp$Ano == ano2] + 
                                                 ((Decomp$B[Decomp$Ano == ano2] / Decomp$B_hat[Decomp$Ano == ano2]) *
                                                    (Decomp$RMM.Ibge[Decomp$Ano == meioperiodo] - Decomp$RMM.Ibge[Decomp$Ano == ano2])), 3)
    
    # D.ano1 = Maternal deaths in meioperiodo
    Decomp$D.ano1[Decomp$Ano == ano2] <- round((Decomp$Nascimentos[Decomp$Ano == meioperiodo] * 
                                                  Decomp$RMM.Ibge[Decomp$Ano == meioperiodo]) / 100000, 3)
    
    # D.ano2 = Maternal deaths in ano2
    Decomp$D.ano2[Decomp$Ano == ano2] <- round((Decomp$Nascimentos[Decomp$Ano == ano2] * 
                                                  Decomp$RMM.Ibge[Decomp$Ano == ano2]) / 100000, 3)
    
    # D_hat.ano2 = Maternal deaths in ano2 implied by fertility decline observed between meioperiodo and ano2
    Decomp$D_hat.ano2[Decomp$Ano == ano2] <- round((Decomp$Nascimentos[Decomp$Ano == ano2] *
                                                      Decomp$RMM_hat[Decomp$Ano == ano2]) / 100000, 3)
    
    
    # Iota = Total decline in MMR between meioperiodo and ano2
    Decomp$Iota[Decomp$Ano == ano2] <- round(Decomp$RMM.Ibge[Decomp$Ano == meioperiodo] - 
                                               Decomp$RMM.Ibge[Decomp$Ano == ano2], 3)
    
    Decomp$Iota.per[Decomp$Ano == ano2] <- 100 #(in %)
    
    # Kappa = Decline in MMR attributable to fertility reduction
    Decomp$Kappa[Decomp$Ano == ano2] <- round(Decomp$RMM.Ibge[Decomp$Ano == meioperiodo] - 
                                                Decomp$RMM_hat[Decomp$Ano == ano2], 3)
    
    Decomp$Kappa.per[Decomp$Ano == ano2] <- round((Decomp$Kappa[Decomp$Ano == ano2] * 100) / 
                                                    Decomp$Iota[Decomp$Ano == ano2], 3) #(in %)
    
    # Lambda = Decline in MMR attributable to safe motherhood initiatives
    Decomp$Lambda[Decomp$Ano == ano2] <- round(Decomp$RMM_hat[Decomp$Ano == ano2] - 
                                                 Decomp$RMM.Ibge[Decomp$Ano == ano2], 3)
    
    Decomp$Lambda.per[Decomp$Ano == ano2] <- round((Decomp$Lambda[Decomp$Ano == ano2] * 100) / 
                                                     Decomp$Iota[Decomp$Ano == ano2], 3) #(in %)
    
    
    # Sigma = Total decline in actual maternal deaths between meioperiodo and ano2
    Decomp$Sigma[Decomp$Ano == ano2] <- round(Decomp$D.ano1[Decomp$Ano == ano2] - 
                                                Decomp$D.ano2[Decomp$Ano == ano2], 3) 
    
    Decomp$Sigma.per[Decomp$Ano == ano2] <- 100 #(in %)
    
    # Tau = Attributable to fertility decline
    Decomp$Tau[Decomp$Ano == ano2] <-  round(Decomp$D.ano1[Decomp$Ano == ano2] - 
                                               Decomp$D_hat.ano2[Decomp$Ano == ano2], 3)
    
    Decomp$Tau.per[Decomp$Ano == ano2] <- round((Decomp$Tau[Decomp$Ano == ano2] * 100) / 
                                                  Decomp$Sigma[Decomp$Ano == ano2], 3) #(in %)
    
    # Eta = Attributable to safe motherhood
    Decomp$Eta[Decomp$Ano == ano2] <- round(Decomp$D_hat.ano2[Decomp$Ano == ano2] - 
                                              Decomp$D.ano2[Decomp$Ano == ano2], 3)
    
    Decomp$Eta.per[Decomp$Ano == ano2] <- round((Decomp$Eta[Decomp$Ano == ano2] * 100) / 
                                                  Decomp$Sigma[Decomp$Ano == ano2], 3) #(in %)
    
    Decomp <- Decomp %>%
      filter(Ano == ano2) %>%
      mutate(Ajuste = "VERDADEIRO",
             Periodo = paste(ano1, meioperiodo, ano2, sep = "-")) %>%
      select(Ano, UF, Estado, sigla, region, Pop, Women, Nascimentos, TFT, RMM, RMM.Ibge, 
             RMM.Obt.Corr, RMM.Gompertz.sem.P, TBN, r, P_hat, B_hat, B, D1_hat, D2_hat, 
             D3_hat, D, X, Y, Z, intersec.X.Y, Alpha, Beta, Gama, Delta, Omega, RMM_hat, 
             D.ano1, D.ano2, D_hat.ano2, Iota, Kappa, Lambda, Iota.per, Kappa.per, Lambda.per, 
             Sigma, Tau, Eta, Sigma.per, Tau.per, Eta.per, Ajuste, Periodo)
    
    
  } else if (!ajuste) {
    
    Decomp <- data %>%
      filter(sigla == siglaUF,
             Ano == ano1 | Ano == meioperiodo | Ano == ano2)  %>% 
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
    
    # r = Annual population growth rate: meioperiodo-ano1 and ano1-ano2
    Decomp$r[Decomp$Ano == ano1] <- round(((log(Decomp$Pop[Decomp$Ano == ano1] / 
                                                  Decomp$Pop[Decomp$Ano == meioperiodo])) / 
                                             (ano1 - meioperiodo)), 3)
    
    Decomp$r[Decomp$Ano == ano2] <- round(((log(Decomp$Pop[Decomp$Ano == ano2] / 
                                                  Decomp$Pop[Decomp$Ano == ano1])) / 
                                             (ano2 - ano1)), 3)
    
    # P_hat = ano2 estimated population assuming constant annual growth rate from meioperiodo-ano1
    Decomp$P_hat[Decomp$Ano == ano2] <- round(Decomp$Pop[Decomp$Ano == meioperiodo] * 
                                                (exp(10 * Decomp$r[Decomp$Ano == ano1])), 3)
    
    # B_hat = Projected births in ano2 assuming constant fertility
    Decomp$B_hat[Decomp$Ano == ano2] <- round(Decomp$P_hat[Decomp$Ano == ano2] * 
                                                Decomp$TBN[Decomp$Ano == meioperiodo] / 1000, 3)
    
    # B = Actual births in ano2 
    Decomp$B[Decomp$Ano == ano2] <- round(Decomp$Nascimentos[Decomp$Ano == ano2], 3)
    
    
    # D1_hat = No change in CBR and no change in MMR
    Decomp$D1_hat[Decomp$Ano == ano2] <- round(Decomp$B_hat[Decomp$Ano == ano2] * 
                                                 Decomp$RMM[Decomp$Ano == meioperiodo] / 100000, 3)
    
    # D3_hat = MMR declined but CBR did not
    Decomp$D3_hat[Decomp$Ano == ano2] <- round(Decomp$B_hat[Decomp$Ano == ano2] * 
                                                 Decomp$RMM[Decomp$Ano == ano2] / 100000, 3)
    
    # D2_hat = CBR declined but MMR did not
    Decomp$D2_hat[Decomp$Ano == ano2] <- round(Decomp$B[Decomp$Ano == ano2] * 
                                                 Decomp$RMM[Decomp$Ano == meioperiodo] / 100000, 3)
    
    # D = Both CBR and MMR declined
    Decomp$D[Decomp$Ano == ano2] <- round(Decomp$B[Decomp$Ano == ano2] * 
                                            Decomp$RMM[Decomp$Ano == ano2] / 100000, 3)
    
    
    # Y = Total effect of decline in MMR 
    Decomp$Y <- round(Decomp$D1_hat - Decomp$D3_hat, 3)
    
    # X = Total effect of fertility decline
    Decomp$X <- round(Decomp$D1_hat - Decomp$D2_hat, 3)
    
    # Z = Total effect of declines in both fertility and MMR
    Decomp$Z <- round(Decomp$D1_hat - Decomp$D, 3)
    
    Decomp <- Decomp %>%
      mutate(intersec.X.Y = round(X + Y - Z, 3), # intersec.X.Y = Overlap between the effect of declines in fertility and in MMR
             Alpha = round(Y - intersec.X.Y, 3), # Alpha = Net effect of decline in MMR
             Beta = round(X - intersec.X.Y, 3), # Beta = Net effect of fertility decline
             Gama = round((Alpha / Z) * 100, 3), # Gama = Effect of safe motherhood on the % of the  potential number of maternal lives saved in ano2
             Delta = round((Beta / Z) * 100, 3), # Delta = Effect of decrease in live births on the % of the  potential number of maternal lives saved in ano2
             Omega = round((intersec.X.Y / Z) * 100), 3) # Omega = Effect of fertility reduction realized through its effect on MMR reduction
    
    ##-----------------------------------------------------------------------------------------##
    
    ### Estimation of the decline in maternal mortality ratio and number of actual maternal deaths attributable to fertility decline ###
    
    Decomp <- Decomp %>%
      mutate(RMM_hat = NA,
             D.ano1 = NA,
             D.ano2 = NA, 
             D_hat.ano2 = NA,
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
    
    # RMM_hat = MMR in ano2 implied by fertility reduction observed during meioperiodo-ano2
    Decomp$RMM_hat[Decomp$Ano == ano2] <- round(Decomp$RMM[Decomp$Ano == ano2] + 
                                                  ((Decomp$B[Decomp$Ano == ano2] / Decomp$B_hat[Decomp$Ano == ano2]) *
                                                     (Decomp$RMM[Decomp$Ano == meioperiodo] - Decomp$RMM[Decomp$Ano == ano2])), 3)
    
    # D.ano1 = Maternal deaths in meioperiodo
    Decomp$D.ano1[Decomp$Ano == ano2] <- round((Decomp$Nascimentos[Decomp$Ano == meioperiodo] * 
                                                  Decomp$RMM[Decomp$Ano == meioperiodo]) / 100000, 3)
    
    # D.ano2 = Maternal deaths in ano2
    Decomp$D.ano2[Decomp$Ano == ano2] <- round((Decomp$Nascimentos[Decomp$Ano == ano2] * 
                                                  Decomp$RMM[Decomp$Ano == ano2]) / 100000, 3)
    
    # D_hat.ano2 = Maternal deaths in ano2 implied by fertility decline observed between meioperiodo and ano2
    Decomp$D_hat.ano2[Decomp$Ano == ano2] <- round((Decomp$Nascimentos[Decomp$Ano == ano2] *
                                                      Decomp$RMM_hat[Decomp$Ano == ano2]) / 100000, 3)
    
    
    # Iota = Total decline in MMR between meioperiodo and ano2
    Decomp$Iota[Decomp$Ano == ano2] <- round(Decomp$RMM[Decomp$Ano == meioperiodo] - Decomp$RMM[Decomp$Ano == ano2], 3)
    
    Decomp$Iota.per[Decomp$Ano == ano2] <- 100 #(in %)
    
    # Kappa = Decline in MMR attributable to fertility reduction
    Decomp$Kappa[Decomp$Ano == ano2] <- round(Decomp$RMM[Decomp$Ano == meioperiodo] - 
                                                Decomp$RMM_hat[Decomp$Ano == ano2], 3)
    
    Decomp$Kappa.per[Decomp$Ano == ano2] <- round((Decomp$Kappa[Decomp$Ano == ano2] * 100) / 
                                                    Decomp$Iota[Decomp$Ano == ano2], 3) #(in %)
    
    # Lambda = Decline in MMR attributable to safe motherhood initiatives
    Decomp$Lambda[Decomp$Ano == ano2] <- round(Decomp$RMM_hat[Decomp$Ano == ano2] - 
                                                 Decomp$RMM[Decomp$Ano == ano2], 3)
    
    Decomp$Lambda.per[Decomp$Ano == ano2] <- round((Decomp$Lambda[Decomp$Ano == ano2] * 100) / 
                                                     Decomp$Iota[Decomp$Ano == ano2], 3) #(in %)
    
    
    # Sigma = Total decline in actual maternal deaths between meioperiodo and ano2
    Decomp$Sigma[Decomp$Ano == ano2] <- round(Decomp$D.ano1[Decomp$Ano == ano2] - 
                                                Decomp$D.ano2[Decomp$Ano == ano2], 3)
    
    Decomp$Sigma.per[Decomp$Ano == ano2] <- 100 #(in %)
    
    # Tau = Attributable to fertility decline
    Decomp$Tau[Decomp$Ano == ano2] <-  round(Decomp$D.ano1[Decomp$Ano == ano2] - 
                                               Decomp$D_hat.ano2[Decomp$Ano == ano2], 3)
    
    Decomp$Tau.per[Decomp$Ano == ano2] <- round((Decomp$Tau[Decomp$Ano == ano2] * 100) / 
                                                  Decomp$Sigma[Decomp$Ano == ano2], 3) #(in %)
    
    # Eta = Attributable to safe motherhood
    Decomp$Eta[Decomp$Ano == ano2] <- round(Decomp$D_hat.ano2[Decomp$Ano == ano2] - 
                                              Decomp$D.ano2[Decomp$Ano == ano2], 3)
    
    Decomp$Eta.per[Decomp$Ano == ano2] <- round((Decomp$Eta[Decomp$Ano == ano2] * 100) / 
                                                  Decomp$Sigma[Decomp$Ano == ano2], 3) #(in %)
    
    Decomp <- Decomp %>%
      filter(Ano == ano2) %>%
      mutate(Ajuste = "FALSO",
             Periodo = paste(ano1, meioperiodo, ano2, sep = "-")) %>%
      select(Ano, UF, Estado, sigla, region, Pop, Women, Nascimentos, TFT, RMM, RMM.Ibge, 
             RMM.Obt.Corr, RMM.Gompertz.sem.P, TBN, r, P_hat, B_hat, B, D1_hat, D2_hat, 
             D3_hat, D, X, Y, Z, intersec.X.Y, Alpha, Beta, Gama, Delta, Omega, RMM_hat, 
             D.ano1, D.ano2, D_hat.ano2, Iota, Kappa, Lambda, Iota.per, Kappa.per, Lambda.per, 
             Sigma, Tau, Eta, Sigma.per, Tau.per, Eta.per, Ajuste, Periodo)
    
  }  
  
  return(Decomp)
}


##-------------------------------------------------------------------------------------------------------------------


correct.year <- function(x, year){ 
  
  m <- year(x) %% 100
  
  year(x) <- ifelse(m > year %% 100, 1900 + m, 2000 + m)
  
  return(x)
}

##----------------------------------------------------------------------------------------------------------

datalist <- list()

for (i in UnidFed) {
  
  print(paste("Processing", i, sep = " "))

  dat <- Decomp.MMR(TFT.2000.2019, 2009, 2014, 2019, i, ajuste = T)
  datalist[[i]] <- dat # add it to your list
  
}

Decomp <- do.call(rbind, datalist)


rm(datalist, dat)

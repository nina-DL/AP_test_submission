
# Load Data ---------------------------------------------------------------

Pi_typeI_nb3 = try_typeIer
Pi_power_nb3 = try_power


# nb1 ---------------------------------------------------------------

TypeI_nb1 = cbind(my_batches1, my_batches1*1, Pi_typeI_nb1$Pi_type_I_er,
                  #OLS_typeI_nb1$Zols_type_I_er_s2est_tcutoff, 
                  rep(NA, length(my_batches1)), 
                  AWAIPW_typeI_nb1$Z_awaipw_type_I_er,
                  Pi_typeI_nb1$Pi_type_I_er_adj, #OLS_typeI_nb1$Zols_type_I_er_adj, 
                  rep(NA, length(my_batches1)), 
                  AWAIPW_typeI_nb1$Z_awaipw_type_I_er_adj)

TypeI_nb1 = as.data.frame(TypeI_nb1)
names(TypeI_nb1) = c("T", "N", "Pi_test_typeI", 
                     #"OLS_typeI", 
                     "BOLS_typeI", 
                     "AWAIPW_typeI", "Pi_test_typeI (adj)", #"OLS_typeI (adj)", 
                     "BOLS_typeI (adj)",
                     "AWAIPW_typeI (adj)")

knitr::kable(TypeI_nb1)


Power_nb1 = cbind(my_batches1, my_batches1*1, Pi_power_nb1$Pi_power,
                  #OLS_power_nb1$Zols_power_s2est_tcutoff,
                  rep(NA, length(my_batches1)), 
                  AWAIPW_power_nb1$Z_awaipw_power,
                  Pi_power_nb1$Pi_power_adj, 
                  #OLS_power_nb1$Zols_power_adj,
                  rep(NA, length(my_batches1)), 
                  AWAIPW_power_nb1$Z_awaipw_power_adj)

Power_nb1 = as.data.frame(Power_nb1)
names(Power_nb1) = c("T", "N", "Pi_test_power", #"OLS_power", 
                     "BOLS_power", 
                     "AWAIPW_power", "Pi_test_power (adj)", #"OLS_power (adj)", 
                     "BOLS_power (adj)",
                     "AWAIPW_power (adj)")

knitr::kable(Power_nb1)


# nb3 ---------------------------------------------------------------

TypeI_nb3 = cbind(my_batches3, my_batches3*3, 
                  Pi_typeI_nb3$Pi_type_I_er,
                  #OLS_typeI_nb3$Zols_type_I_er_s2est_tcutoff, 
                  BOLS_typeI_nb3$Zbols_type_I_er_s2est_tcutoff, 
                  AWAIPW_typeI_nb3$Z_awaipw_type_I_er,
                  Pi_typeI_nb3$Pi_type_I_er_adj, 
                  #OLS_typeI_nb3$Zols_type_I_er_adj, 
                  BOLS_typeI_nb3$Zbols_type_I_er_adj,
                  AWAIPW_typeI_nb3$Z_awaipw_type_I_er_adj)

TypeI_nb3 = as.data.frame(TypeI_nb3)
names(TypeI_nb3) = c("T", "N", "Pi_test_typeI", 
                     #"OLS_typeI", 
                     "BOLS_typeI", 
                     "AWAIPW_typeI", "Pi_test_typeI (adj)", #"OLS_typeI (adj)", 
                     "BOLS_typeI (adj)","AWAIPW_typeI (adj)")

knitr::kable(TypeI_nb3)


Power_nb3 = cbind(my_batches3, my_batches3*3, 
                  Pi_power_nb3$Pi_power,
                  #OLS_power_nb3$Zols_power_s2est_tcutoff,
                  BOLS_power_nb3$Zbols_power_s2est_tcutoff,
                  AWAIPW_power_nb3$Z_awaipw_power,
                  Pi_power_nb3$Pi_power_adj, 
                  #OLS_power_nb3$Zols_power_adj,
                  BOLS_power_nb3$Zbols_power_adj,
                  AWAIPW_power_nb3$Z_awaipw_power_adj)

Power_nb3 = as.data.frame(Power_nb3)
names(Power_nb3) = c("T", "N", "Pi_test_power", #"OLS_power", 
                     "BOLS_power", "AWAIPW_power", 
                     "Pi_test_power (adj)", #"OLS_power (adj)", 
                     "BOLS_power (adj)","AWAIPW_power (adj)")

knitr::kable(Power_nb3)

# nb10 ---------------------------------------------------------------

TypeI_nb10 = cbind(my_batches10, my_batches10*10, Pi_typeI_nb10$Pi_type_I_er,
                  #OLS_typeI_nb10$Zols_type_I_er_s2est_tcutoff, 
                  BOLS_typeI_nb10$Zbols_type_I_er_s2est_tcutoff, 
                  AWAIPW_typeI_nb10$Z_awaipw_type_I_er,
                  Pi_typeI_nb10$Pi_type_I_er_adj, #OLS_typeI_nb10$Zols_type_I_er_adj, 
                  BOLS_typeI_nb10$Zbols_type_I_er_adj,
                  AWAIPW_typeI_nb10$Z_awaipw_type_I_er_adj)

TypeI_nb10 = as.data.frame(TypeI_nb10)
names(TypeI_nb10) = c("T", "N", "Pi_test_typeI", 
                     #"OLS_typeI", 
                     "BOLS_typeI", 
                     "AWAIPW_typeI", "Pi_test_typeI (adj)", #"OLS_typeI (adj)", 
                     "BOLS_typeI (adj)","AWAIPW_typeI (adj)")

knitr::kable(TypeI_nb10)


Power_nb10 = cbind(my_batches10, my_batches10*10, Pi_power_nb10$Pi_power,
                  #OLS_power_nb10$Zols_power_s2est_tcutoff,
                  BOLS_power_nb10$Zbols_power_s2est_tcutoff,
                  AWAIPW_power_nb10$Z_awaipw_power,
                  Pi_power_nb10$Pi_power_adj, 
                  #OLS_power_nb10$Zols_power_adj,
                  BOLS_power_nb10$Zbols_power_adj,
                  AWAIPW_power_nb10$Z_awaipw_power_adj)

Power_nb10 = as.data.frame(Power_nb10)
names(Power_nb10) = c("T", "N", "Pi_test_power", #"OLS_power", 
                      "BOLS_power", "AWAIPW_power", 
                     "Pi_test_power (adj)", #"OLS_power (adj)", 
                     "BOLS_power (adj)","AWAIPW_power (adj)")

knitr::kable(Power_nb10)

# nb20 ---------------------------------------------------------------

TypeI_nb20 = cbind(my_batches20, my_batches20*20, Pi_typeI_nb20$Pi_type_I_er,
                   OLS_typeI_nb20$Zols_type_I_er_s2est_tcutoff, 
                   BOLS_typeI_nb20$Zbols_type_I_er_s2est_tcutoff, 
                   AWAIPW_typeI_nb20$Z_awaipw_type_I_er,
                   Pi_typeI_nb20$Pi_type_I_er_adj, 
                   OLS_typeI_nb20$Zols_type_I_er_adj, 
                   BOLS_typeI_nb20$Zbols_type_I_er_adj,
                   AWAIPW_typeI_nb20$Z_awaipw_type_I_er_adj)

TypeI_nb20 = as.data.frame(TypeI_nb20)
names(TypeI_nb20) = c("T", "N", "Pi_test_typeI", 
                      "OLS_typeI", 
                      "BOLS_typeI", 
                      "AWAIPW_typeI", "Pi_test_typeI (adj)", "OLS_typeI (adj)", "BOLS_typeI (adj)","AWAIPW_typeI (adj)")

knitr::kable(TypeI_nb20)


Power_nb20 = cbind(my_batches20, my_batches20*20, 
                   Pi_power_nb20$Pi_power,
                   OLS_power_nb20$Zols_power_s2est_tcutoff,
                   BOLS_power_nb20$Zbols_power_s2est_tcutoff,
                   AWAIPW_power_nb20$Z_awaipw_power,
                   Pi_power_nb20$Pi_power_adj, 
                   OLS_power_nb20$Zols_power_adj,
                   BOLS_power_nb20$Zbols_power_adj,
                   AWAIPW_power_nb20$Z_awaipw_power_adj)

Power_nb20 = as.data.frame(Power_nb20)
names(Power_nb20) = c("T", "N", "Pi_test_power", "OLS_power", "BOLS_power", "AWAIPW_power", 
                      "Pi_test_power (adj)", "OLS_power (adj)", "BOLS_power (adj)","AWAIPW_power (adj)")

knitr::kable(Power_nb20)


# Make tables: Non-stationarity ---------------------------------------------------------------

AWAIPW_typeI_nb3 = typeIer_nt2
AWAIPW_power_nb3 = power_nt2

BOLS_typeI_nb3 = typeIer_nt2
BOLS_power_nb3 = power_nt2

Pi_typeI_nb3 = typeIer_nt2
Pi_power_nb3 = power_nt2

TypeI_nb3 = cbind(my_batches3, my_batches3*3, 
                  Pi_typeI_nb3$Pi_type_I_er,
                  #OLS_typeI_nb3$Zols_type_I_er_s2est_tcutoff, 
                  BOLS_typeI_nb3$Zbols_type_I_er_s2est_tcutoff, 
                  AWAIPW_typeI_nb3$Z_awaipw_type_I_er,
                  Pi_typeI_nb3$Pi_type_I_er_adj, 
                  #OLS_typeI_nb3$Zols_type_I_er_adj, 
                  BOLS_typeI_nb3$Zbols_type_I_er_adj,
                  AWAIPW_typeI_nb3$Z_awaipw_type_I_er_adj)

TypeI_nb3 = as.data.frame(TypeI_nb3)
names(TypeI_nb3) = c("T", "N", "Pi_test_typeI", 
                     #"OLS_typeI", 
                     "BOLS_typeI", 
                     "AWAIPW_typeI", "Pi_test_typeI (adj)", #"OLS_typeI (adj)", 
                     "BOLS_typeI (adj)","AWAIPW_typeI (adj)")

knitr::kable(TypeI_nb3)


Power_nb3 = cbind(my_batches3, my_batches3*3, 
                  Pi_power_nb3$Pi_power,
                  #OLS_power_nb3$Zols_power_s2est_tcutoff,
                  BOLS_power_nb3$Zbols_power_s2est_tcutoff,
                  AWAIPW_power_nb3$Z_awaipw_power,
                  Pi_power_nb3$Pi_power_adj, 
                  #OLS_power_nb3$Zols_power_adj,
                  BOLS_power_nb3$Zbols_power_adj,
                  AWAIPW_power_nb3$Z_awaipw_power_adj)

Power_nb3 = as.data.frame(Power_nb3)
names(Power_nb3) = c("T", "N", "Pi_test_power", #"OLS_power", 
                     "BOLS_power", "AWAIPW_power", 
                     "Pi_test_power (adj)", #"OLS_power (adj)", 
                     "BOLS_power (adj)","AWAIPW_power (adj)")

knitr::kable(Power_nb3)


# Clipping sensitivity ----------------------------------------------------
typeI_clip0 = try_typeIer
power_clip0 = try_power

typeI_clip005 = try_typeIer
power_clip005 = try_power

typeI_clip01 = try_typeIer
power_clip01 = try_power

typeI_clip02 = try_typeIer
power_clip02 = try_power

Pi_typeI_clipping = cbind("Clipping" = c(0, 0.05, 0.1, 0.2),
                          "Type-I" = c(typeI_clip0$Pi_type_I_er, typeI_clip005$Pi_type_I_er,
                            typeI_clip01$Pi_type_I_er, typeI_clip02$Pi_type_I_er),
                          "Adj_Type-I" = c(typeI_clip0$Pi_type_I_er_adj, typeI_clip005$Pi_type_I_er_adj,
                            typeI_clip01$Pi_type_I_er_adj, typeI_clip02$Pi_type_I_er_adj))

Pi_power_clipping = cbind("Clipping" = c(0, 0.05, 0.1, 0.2),
                          "Power" = c(power_clip0$Pi_power, power_clip005$Pi_power,
                                       power_clip01$Pi_power, power_clip02$Pi_power),
                          "Adj_Power" = c(power_clip0$Pi_power_adj, power_clip005$Pi_power_adj,
                                           power_clip01$Pi_power_adj, power_clip02$Pi_power_adj))



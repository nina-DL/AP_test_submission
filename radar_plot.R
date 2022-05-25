# install.packages("fmsb")
library(fmsb)

All_data_150 = data.frame("TypeII" = 1-c(Pi_power[[1]][3], BOLS_power[[1]][3], AWAIPW_power[[1]][3]),
                          "Adj_TypeII" = 1-c(Pi_power[[3]][3], BOLS_power[[3]][3], AWAIPW_power[[2]][3]),
                          "TypeI" = c(Pi_typeI[[1]][3], BOLS_typeI[[1]][3], AWAIPW_typeI[[1]][3]),
                          "Adj_TypeI" = c(Pi_typeI[[3]][3], BOLS_typeI[[3]][3], AWAIPW_typeI[[3]][3]),
                          "Regret" = Rew_deploy$Avg_Regret[3,2:4])
c(0.6,0.6,0.04,0.04,15)

All_150 = rbind("Max" = c(0.95,0.95,0.07,0.06,40), "Min" = c(0.6,0.6,0,0,0), All_data_150)
All_150 = cbind(All_150, "Time" = c(55, 0, 0.3, 2.3, 54))
All_150 = All_150[c(1,2,3,5,4),]
All_150_adj = All_150[,c(2,4,5,6)]
All_150_unadj = All_150[,c(1,3,5,6)]

par(xpd = TRUE, mfrow = c(1, 1), mar = c(2, 1, 2, 1))

radarchart(All_150)

areas <- c(rgb(1, 0, 0, 0.55),
           rgb(0, 1, 0, 0.15),
           rgb(0, 0, 1, 0.15))

radarchart(All_150,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = 2:4,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas,   # Color of the areas
           vlabels=c("Type-II error", "Type-II error \n (Adjusted)    ",
                     "Type-I error", "Type-I error \n (Adjusted)",
                     "Regret"), centerzero=TRUE, axistype=1,
           caxislabels=c("best", "", "", "", "worst"))      

  
radarchart(All_150_adj,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = 2:4,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas,   # Color of the areas
           vlabels=c(expression("Type-II error (1-Power)"),
                     expression("Type-I error          "),
                     "Regret", "     Computational\n            Time"), centerzero=TRUE, axistype=1,
           caxislabels=c("", "", "", "", "")
           )      

text(0, 0.97, "worst \n performance", cex = .7, col="gray30")
text(0, 0, "best \n performance", cex = .7, col="gray30")

text(-0.8, -0.1, "0.05", cex = .8)
text(0.95, -0.1, "54min", cex = .8)
text(0, -0.98, "35", cex = .8)
text(0, 0.8, "0.93", cex = .8)


radarchart(All_150_unadj,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = 2:4,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas,   # Color of the areas
           vlabels=c("Type-II error",
                     "Type-I error",
                     "Regret", "Time"), centerzero=TRUE, axistype=1,
           caxislabels=c("best", "", "", "", "worst"))      


legend("bottomright",
       legend = c(expression("AP-test"), expression(Z[AW-AIPW]), expression(Z[BOLS])),
       bty = "n", pch = 20, col = areas,
       text.col = "grey25", pt.cex = 2)


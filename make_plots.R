# Load necessary functions ------------------------------------------------
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)
library(gtable)
library(scales)
library(pbapply)

# Regret Plots by N -------------------------------------------------------

# 1nb
my_nb = 1
df = as.data.frame(Rew_nb1$Avg_Regret)
names(df) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df$N = df$Batch*my_nb
df = df[1:which(df$N==300),]
df <- reshape2::melt(df[,-1], id.var='N')

plot_1nb = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = .5)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c("solid","dashed","dotted"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  scale_colour_manual(" ",values=c("#F8766D","#529EFF", "#00BFC4"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  scale_shape_manual(" ",values=c(19, 19, 19),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  xlab('Sample Size (N)') + #xlim(0, 300) + ylim(0, 50)+
  ylab(expression("Total Regret: "*N*mu*"*"*"-"*sum(y[i], i==1, N))) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=8)) +
  ggtitle("Batch Size = 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(sec.axis=sec_axis(trans=~ . * 1, name="Batch (T)"))

df1 = as.data.frame(Rew_nb1$Avg_Reward_SE)
names(df1) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df1$N = df1$Batch*my_nb
df1 = df1[1:which(df1$N==300),]
df1 <- reshape2::melt(df1[,-1], id.var='N')
df1$upper = df$value + df1$value*1.96
df1$lower = df$value - df1$value*1.96

plot_1nb = plot_1nb+geom_ribbon(aes(ymin=df1$lower, ymax=df1$upper), linetype=1, alpha=0.1, size = .1)
plot_1nb

# 3nb
my_nb = 3
df = as.data.frame(Rew_nb3$Avg_Regret)
names(df) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df$N = df$Batch*my_nb
df = df[1:which(df$N==300),]
df <- reshape2::melt(df[,-1], id.var='N')

plot_3nb = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = .5)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c("solid","dashed","dotted"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  scale_colour_manual(" ",values=c("#F8766D","#529EFF", "#00BFC4"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  scale_shape_manual(" ",values=c(19, 19, 19),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  xlab('Sample Size (N)') + #xlim(0, 300) + ylim(0, 70) + 
  ylab(" ") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=8)) +
  ggtitle("Batch Size = 3") +
  theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(sec.axis=sec_axis(trans=~ . * 1, name="Batch (T)"))


df3 = as.data.frame(Rew_nb3$Avg_Reward_SE)
names(df3) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df3$N = df3$Batch*my_nb
df3 = df3[1:which(df3$N==300),]
df3 <- reshape2::melt(df3[,-1], id.var='N')
df3$upper = df$value + df3$value*1.96
df3$lower = df$value - df3$value*1.96

plot_3nb = plot_3nb+geom_ribbon(aes(ymin=df3$lower, ymax=df3$upper), linetype=1, alpha=0.1, size = .1)
plot_3nb

# 10nb
my_nb = 10
df = as.data.frame(Rew_nb10$Avg_Regret)
names(df) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df$N = df$Batch*my_nb
df = df[1:which(df$N==300),]
df <- reshape2::melt(df[,-1], id.var='N')

plot_10nb = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = .5)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c("solid","dashed","dotted"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  scale_colour_manual(" ",values=c("#F8766D","#529EFF", "#00BFC4"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  scale_shape_manual(" ",values=c(19, 19, 19),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  xlab('Sample Size (N)') + #xlim(0, 300) + ylim(0, 50) + 
  ylab(" ") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=8)) +
  ggtitle("Batch Size = 10") +
  theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(sec.axis=sec_axis(trans=~ . * 1/10, name="Batch (T)"))


df10 = as.data.frame(Rew_nb10$Avg_Reward_SE)
names(df10) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df10$N = df10$Batch*my_nb
df10 = df10[1:which(df10$N==300),]
df10 <- reshape2::melt(df10[,-1], id.var='N')
df10$upper = df$value + df10$value*1.96
df10$lower = df$value - df10$value*1.96

plot_10nb = plot_10nb+geom_ribbon(aes(ymin=df10$lower, ymax=df10$upper), linetype=1, alpha=0.1, size = .1)
plot_10nb 

# 20nb
my_nb = 20
df = as.data.frame(Rew_nb20$Avg_Regret)
names(df) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df$N = df$Batch*my_nb
df = df[1:which(df$N==300),]
df <- reshape2::melt(df[,-1], id.var='N')

plot_20nb = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = .5)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c("solid","dashed","dotted"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  scale_colour_manual(" ",values=c("#F8766D","#529EFF", "#00BFC4"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  scale_shape_manual(" ",values=c(19, 19, 19),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat"), labels=c(bquote("Standard TS ("*Pi*"-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)"))+
  xlab('Sample Size (N)') + #xlim(0, 300) + ylim(0, 50) + 
  ylab(" ") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=8)) +
  ggtitle("Batch Size = 20") +
  theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(sec.axis=sec_axis(trans=~ . * 1/20, name="Batch (T)"))

df20 = as.data.frame(Rew_nb20$Avg_Reward_SE)
names(df20) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df20$N = df20$Batch*my_nb
df20 = df20[1:which(df20$N==300),]
df20 <- reshape2::melt(df20[,-1], id.var='N')
df20$upper = df$value + df20$value*1.96
df20$lower = df$value - df20$value*1.96

plot_20nb = plot_20nb+geom_ribbon(aes(ymin=df20$lower, ymax=df20$upper), linetype=1, alpha=0.1, size = .1)
plot_20nb

ggarrange(plot_1nb, plot_3nb, plot_10nb, plot_20nb, ncol=4, nrow=1, common.legend = TRUE, legend="bottom")


# Regret & Prop Optimal Arm plot by N: OK ---------------------------------------

Rew_nb10$Avg_Regret = rbind(rep(0, 4), Rew_nb10$Avg_Regret)

# 10nb
my_nb = 10
df = as.data.frame(Rew_nb10$Avg_Regret)
names(df) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df$N = df$Batch*my_nb
df$Oracle = c(0)
df <- reshape2::melt(df[,-1], id.var='N')

plot_10nb = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = .6)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c("solid","solid","solid", "dashed"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat", "Oracle"), labels=c(bquote("Standard TS (AP-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)", "Oracle"))+
  scale_colour_manual(" ",values=c("#F8766D","#529EFF", "#00BFC4", "black"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat", "Oracle"), labels=c(bquote("Standard TS (AP-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)", "Oracle"))+
  scale_shape_manual(" ",values=c(19, 19, 19, 19),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat", "Oracle"), labels=c(bquote("Standard TS (AP-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)", "Oracle"))+
  xlab('Sample Size (N)') +  ylim(0,17) + xlim(0, 150) +
  ylab(expression("Total Regret: "*N*mu*"*"*"-"*sum(y[i], i==1, N))) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  #ggtitle("Batch Size = 10") +
  #theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold")) +
  theme(legend.position="none")

Rew_nb10$Avg_Reward_SE = rbind(rep(0, 4), Rew_nb10$Avg_Reward_SE)
df10 = as.data.frame(Rew_nb10$Avg_Reward_SE)
names(df10) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df10$N = df10$Batch*my_nb
df10$Oracle = c(0)
df10 <- reshape2::melt(df10[,-1], id.var='N')
df10$upper = df$value + df10$value*1.96
df10$lower = df$value - df10$value*1.96

plot_10nb = plot_10nb+geom_ribbon(aes(ymin=df10$lower, ymax=df10$upper), linetype=1, alpha=0.1, size = .1)
plot_10nb 

df = as.data.frame(Rew_nb10$Avg_Prop_opt_arm)
names(df) = c("Batch", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat")
df$N = df$Batch*my_nb
df$Oracle = c(1)
df <- reshape2::melt(df[,-1], id.var='N')


plot_10nb_arm = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = .6)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c("solid","solid","solid", "dashed"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat", "Oracle"), labels=c(bquote("Standard TS (AP-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)", "Oracle"))+
  scale_colour_manual(" ",values=c("#F8766D","#529EFF", "#00BFC4", "black"),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat", "Oracle"), labels=c(bquote("Standard TS (AP-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)", "Oracle"))+
  scale_shape_manual(" ",values=c(19, 19, 19, 19),breaks=c("Pi_stat", "BOLS_Stat", "AWAIPW_Stat", "Oracle"), labels=c(bquote("Standard TS (AP-test)"), "Restricted TS (BOLS)", "Restricted TS (AW-AIPW)", "Oracle"))+
  xlab('Sample Size (N)') + xlim(0, 150) + 
  #ylab(expression("% Optimal Arm Allocation: "*frac(sum(I*"("*A[i]*"="*1*")", i==1, N),N))) +
  ylab(expression("Proportion of Optimal Arm Allocation")) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  #ggtitle("Batch Size = 10") +
  #theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold")) +
  theme(legend.position="none") 

ggarrange(plot_10nb, plot_10nb_arm, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")


# Type-I Error Plots by N: OK -------------------------------------------------------

# 1nb
my_nb = 1
df = as.data.frame(TypeI_nb1[c(2,3,5,6,7,9,10)])
#df = as.data.frame(TypeI_nb1[,-1])
df = df[2:which(df$N==150),]
names(df) = c("N", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat",
              "Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
df <- reshape2::melt(df, id.var='N')
ablineN = 51

var_n = c("Pi_stat",  "BOLS_Stat", "AWAIPW_Stat",
        "Pi_stat_adj",  "BOLS_Stat_adj", "AWAIPW_Stat_adj")
labels_n = c(bquote("AP-test"),  bquote(Z[BOLS]), bquote(Z[AW-AIPW]),
             bquote("AP-test (with exact "*alpha*"-level control)"),  bquote(Z[BOLS]*" (with exact "*alpha*"-level control)"), bquote(Z[AW-AIPW]*" (with exact "*alpha*"-level control)"))
  
plot_1nb_typeI = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = 1)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c(rep("dotted",3), rep("solid",3)),breaks=var_n, labels=labels_n)+
  scale_colour_manual(" ",values=rep(c("#F8766D","#529EFF","#00C08D"),2),breaks=var_n, labels=labels_n)+
  scale_shape_manual(" ",values=c(19, 19, 19, 19, 19, 19),breaks=var_n, labels=labels_n)+
  xlab('Sample Size (N)') + ylim(0, 0.2) + #xlim(0, 300) + 
  ylab("Type-I Error Rate") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  ggtitle("Batch Size = 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(limits = c(0, 150), sec.axis=sec_axis(trans=~ . * 1, name="Step (T)")) +
  geom_vline(xintercept = ablineN, colour = "gray30", linetype=5)


# 3nb
my_nb = 3
df = as.data.frame(TypeI_nb3[c(2,3,5,6,7,9,10)])
#df = as.data.frame(TypeI_nb3[,-1])
df = df[1:which(df$N==150),]
names(df) = c("N", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat",
              "Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
df <- reshape2::melt(df, id.var='N')

plot_3nb_typeI = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = 1)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c(rep("dotted",3), rep("solid",3)),breaks=var_n, labels=labels_n)+
  scale_colour_manual(" ",values=rep(c("#F8766D","#529EFF","#00C08D"),2),breaks=var_n, labels=labels_n)+
  scale_shape_manual(" ",values=c(19, 19, 19, 19, 19, 19),breaks=var_n, labels=labels_n)+
  xlab('Sample Size (N)') + ylim(0, 0.2) + #xlim(0, 300) + 
  ylab(" ") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  ggtitle("Batch Size = 3") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(limits = c(0, 150), sec.axis=sec_axis(trans=~ . * 1/3, name="Step (T)")) +
  geom_vline(xintercept = ablineN, colour = "gray30", linetype=5)


# 10nb
my_nb = 10
df = as.data.frame(TypeI_nb10[c(2,3,5,6,7,9,10)])
#df = as.data.frame(TypeI_nb10[,-1])
df = df[1:which(df$N==150),]
names(df) = c("N", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat",
              "Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
df <- reshape2::melt(df, id.var='N')

plot_10nb_typeI = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = 1)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c(rep("dotted",3), rep("solid",3)),breaks=var_n, labels=labels_n)+
  scale_colour_manual(" ",values=rep(c("#F8766D","#529EFF","#00C08D"),2),breaks=var_n, labels=labels_n)+
  scale_shape_manual(" ",values=c(19, 19, 19, 19, 19, 19),breaks=var_n, labels=labels_n)+
  xlab('Sample Size (N)') + ylim(0, 0.2) + #xlim(0, 170) + 
  ylab(" ") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  ggtitle("Batch Size = 10") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(limits = c(0, 150), sec.axis=sec_axis(trans=~ . * 1/10, name="Step (T)")) +
  geom_vline(xintercept = ablineN, colour = "gray30", linetype=5)


# 10nb
my_nb = 20
df = as.data.frame(TypeI_nb20[c(2,3,5,6,7,9,10)])
#df = as.data.frame(TypeI_nb10[,-1])
df = df[1:which(df$N==160),]
names(df) = c("N", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat",
              "Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
df <- reshape2::melt(df, id.var='N')

plot_20nb_typeI = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = 1)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c(rep("dotted",3), rep("solid",3)),breaks=var_n, labels=labels_n)+
  scale_colour_manual(" ",values=rep(c("#F8766D","#529EFF","#00C08D"),2),breaks=var_n, labels=labels_n)+
  scale_shape_manual(" ",values=c(19, 19, 19, 19, 19, 19),breaks=var_n, labels=labels_n)+
  xlab('Sample Size (N)') + ylim(0, 0.3) + #xlim(0, 170) + 
  ylab(" ") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  ggtitle("Batch Size = 20") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(limits = c(0, 150), sec.axis=sec_axis(trans=~ . * 1/10, name="Step (T)")) +
  geom_vline(xintercept = ablineN, colour = "gray30", linetype=5)

ggarrange(plot_1nb_typeI, plot_3nb_typeI, plot_10nb_typeI, plot_20nb_typeI,  
          ncol=4, nrow=1, common.legend = TRUE, legend="bottom")

# Power Plots by N: OK -------------------------------------------------------

# 1nb
my_nb = 1
df = as.data.frame(Power_nb1[c(2,3,5,6,7,9,10)])
#df = as.data.frame(Power_nb1[,-1])
df = df[2:which(df$N==150),]
names(df) = c("N", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat",
              "Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
df <- reshape2::melt(df, id.var='N')

#var_n = c("Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
#labels_n = c(bquote("AP-test (Adj)"), bquote(Z[BOLS]*" (Adj)"), bquote(Z[AW-AIPW]*" (Adj)"))


plot_1nb_power = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = 1)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c(rep("dotted",3), rep("solid",3)),breaks=var_n, labels=labels_n)+
  scale_colour_manual(" ",values=rep(c("#F8766D","#529EFF","#00C08D"),2),breaks=var_n, labels=labels_n)+
  scale_shape_manual(" ",values=c(19, 19, 19, 19, 19, 19),breaks=var_n, labels=labels_n)+
  xlab('Sample Size (N)') + ylim(0, 0.5)  +  #xlim(0, 300) 
  ylab("Statistical Power") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  #ggtitle("Batch Size = 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(limits = c(0, 150), sec.axis=sec_axis(trans=~ . * 1, name="Step (T)")) +
  geom_vline(xintercept = ablineN, colour = "gray30", linetype=5)


# 3nb
my_nb = 3
df = as.data.frame(Power_nb3[c(2,3,5,6,7,9,10)])
#df = as.data.frame(Power_nb3[,-1])
df = df[2:which(df$N==150),]
names(df) = c("N", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat",
              "Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
df <- reshape2::melt(df, id.var='N')

plot_3nb_power = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = 1)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c(rep("dotted",3), rep("solid",3)),breaks=var_n, labels=labels_n)+
  scale_colour_manual(" ",values=rep(c("#F8766D","#529EFF","#00C08D"),2),breaks=var_n, labels=labels_n)+
  scale_shape_manual(" ",values=c(19, 19, 19, 19, 19, 19),breaks=var_n, labels=labels_n)+
  xlab('Sample Size (N)') + ylim(0, 0.5)  +  #xlim(0, 300) +
  ylab(" ") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  #ggtitle("Batch Size = 3") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(limits = c(0, 150), sec.axis=sec_axis(trans=~ . * 1/3, name="Step (T)")) +
  geom_vline(xintercept = ablineN, colour = "gray30", linetype=5)


# 10nb
my_nb = 10
#df = as.data.frame(Power_nb10[,-1])
df = as.data.frame(Power_nb10[c(2,3,5,6,7,9,10)])
df = df[1:which(df$N==150),]
names(df) = c("N", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat",
              "Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
df <- reshape2::melt(df, id.var='N')


plot_10nb_power = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = 1)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c(rep("dotted",3), rep("solid",3)),breaks=var_n, labels=labels_n)+
  scale_colour_manual(" ",values=rep(c("#F8766D","#529EFF","#00C08D"),2),breaks=var_n, labels=labels_n)+
  scale_shape_manual(" ",values=c(19, 19, 19, 19, 19, 19),breaks=var_n, labels=labels_n)+
  xlab('Sample Size (N)') + ylim(0, 0.5)  +  #xlim(0, 300) + 
  ylab(" ") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  #ggtitle("Batch Size = 10") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(limits = c(0, 150), sec.axis=sec_axis(trans=~ . * 1/10, name="Step (T)")) +
  geom_vline(xintercept = ablineN, colour = "gray30", linetype=5)

# 10nb
my_nb = 20
#df = as.data.frame(Power_nb10[,-1])
df = as.data.frame(Power_nb20[c(2,3,5,6,7,9,10)])
df = df[1:which(df$N==160),]
names(df) = c("N", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat",
              "Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
df <- reshape2::melt(df, id.var='N')


plot_20nb_power = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = 1)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c(rep("dotted",3), rep("solid",3)),breaks=var_n, labels=labels_n)+
  scale_colour_manual(" ",values=rep(c("#F8766D","#529EFF","#00C08D"),2),breaks=var_n, labels=labels_n)+
  scale_shape_manual(" ",values=c(19, 19, 19, 19, 19, 19),breaks=var_n, labels=labels_n)+
  xlab('Sample Size (N)') + ylim(0, 0.8)  +  #xlim(0, 300) + 
  ylab(" ") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  #ggtitle("Batch Size = 10") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(limits = c(0, 150), sec.axis=sec_axis(trans=~ . * 1/10, name="Step (T)")) +
  geom_vline(xintercept = ablineN, colour = "gray30", linetype=5)

ggarrange(plot_1nb_power, plot_3nb_power, plot_10nb_power,  plot_20nb_power,
          ncol=3, nrow=1, common.legend = TRUE, legend="bottom")


ggarrange(plot_1nb_typeI, plot_3nb_typeI, plot_10nb_typeI, plot_20nb_typeI,
          plot_1nb_power, plot_3nb_power, plot_10nb_power, plot_20nb_power,
          #plot_1nb, plot_3nb, plot_10nb, plot_20nb, 
          ncol=4, nrow=2, common.legend = TRUE, legend="bottom")



# Pi-test distribution ----------------------------------------------------


my_data = try_H0
nb = 10
k = 9
ss = nb*k

my_Q = pblapply(my_data, compute_Pi_stat, from_batch = 1, to_batch = k)
my_Q = data.frame(matrix(unlist(my_Q), nrow=length(my_Q), byrow=TRUE))
names(my_Q) = c("Batches", "N1", "N2", "Q_stat")
avg_Q = data.frame(table(my_Q$Q_stat)/M)


H0 = ggplot(avg_Q, aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c(rep("gray33", k), "indianred4")) +
  #ggtitle(bquote(Pi*"-test"~"distribution"), subtitle = bquote("N ="~.(ss)*" ("*n~"="~.(nb)*";"~"T ="~.(k)*")")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 13, face = "bold"))+
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=13)) +
  theme(legend.position="none") + ylim(0,0.7) +
  xlab(bquote("AP-test"~"value")) + ylab("Probability Mass") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey95'),
        panel.grid.minor = element_line(color = 'grey95')) +
  theme(axis.line = element_line(colour = "black")) +
  geom_label(aes(x=k+1/2, y=0.06, label="11%"), fill="white", colour="darkred", size=4.5) + 
  geom_label(aes(x=k, y=0.03, label=deparse(bquote("q"[alpha]^"*"))), parse = TRUE, fill = "white", colour="darkred", size=4.5) +
  geom_label(aes(x=k/2+1, y=0.6, label=deparse(bquote(H[0]*": "*mu[1]*"="*mu[0]))), fill = "white", parse = TRUE, size=5) +
  geom_label(aes(x=k/2+1, y=0.5, label=deparse(bquote("("*mu[1]*"=0; "*mu[0]*"=0)"))), fill = "white", parse = TRUE, size=4.5, label.size = NA)

H0  

my_data = try_H1

my_Q_H1 = pblapply(my_data, compute_Pi_stat, from_batch = 2, to_batch = (k+1))
my_Q_H1 = data.frame(matrix(unlist(my_Q_H1), nrow=length(my_Q_H1), byrow=TRUE))
names(my_Q_H1) = c("Batches", "N1", "N2", "Q_stat")
avg_Q_H1 = data.frame(table(my_Q_H1$Q_stat)/M)


H1 = ggplot(avg_Q_H1, aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c(rep("gray33", k), "indianred4")) +
  #ggtitle(bquote(Pi*"-test"~"distribution"),
  #        subtitle = bquote("N ="~.(ss)*" ("*n~"="~.(nb)*";"~"T ="~.(k)*")")) +
  #theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  #theme(plot.subtitle = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=13)) +
  theme(legend.position="none") + ylim(0, 0.7) +
  xlab(bquote("AP-test"~"value")) + ylab("Probability Mass") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey95'),
        panel.grid.minor = element_line(color = 'grey95')) +
  theme(axis.line = element_line(colour = "black")) +
  geom_label(aes(x=k+1/2, y=0.2, label="69%"), fill="white", colour="darkred", size=4.5) + 
  geom_label(aes(x=k, y=0.03, label=deparse(bquote("q"[alpha]^"*"))), parse = TRUE, fill = "white", colour="darkred", size=4.5) +
  geom_label(aes(x=k/2+1, y=0.6, label=deparse(bquote(H[1]*": "*mu[1]*">"*mu[0]))), fill = "white", parse = TRUE, size=5) +
  geom_label(aes(x=k/2+1, y=0.5, label=deparse(bquote("("*mu[1]*"=0.5; "*mu[0]*"=0)"))), fill = "white", parse = TRUE, size=4.5, label.size = NA)

H1 

myplot = ggarrange(H0, H1, ncol=2,  nrow = 1)

annotate_figure(myplot, top = text_grob(bquote("AP-test"~"distribution (N ="~.(ss)*": "*n~"="~.(nb)*";"~"T ="~.(k)*")"), face = "bold", size = 17))

# AWAIPW-test distr -----------------------------------------------------

my_data = try_H0
nb = 3
k = 17
ss = nb*k

my_Z = pblapply(my_data, compute_Z_AWAIPW, to_batch = k)
my_Z = data.frame(matrix(unlist(my_Z), nrow=length(my_Z), byrow=TRUE))
names(my_Z) = c("Batches", "N1", "N2", "Z_stat")
hist(my_Z$Z_stat)

my_data = try_H1

my_Z_H1 = pblapply(my_data, compute_Z_AWAIPW, to_batch = k)
my_Z_H1 = data.frame(matrix(unlist(my_Z_H1), nrow=length(my_Z_H1), byrow=TRUE))
names(my_Z_H1) = c("Batches", "N1", "N2", "Z_stat")
hist(my_Z_H1$Z_stat)


dat <- data.frame(xx = c(my_Z$Z_stat, my_Z_H1$Z_stat),
                  yy = rep(letters[1:2],each = M))

quant = qnorm(0.90)
typeI = round(sum(my_Z$Z_stat>quant)/M,2)*100
power = round(sum(my_Z_H1$Z_stat > quant)/M,2)*100

distr_plot_awaipw = ggplot(dat,aes(x=xx, fill=yy)) + xlim(-5,5) +
  geom_histogram(data=subset(dat,yy=='a'), aes(y = ..density..), alpha=0.3, binwidth = 0.25, position="identity") +
  geom_histogram(data=subset(dat,yy=='b'), aes(y = ..density..), alpha=0.3, binwidth = 0.25, position="identity") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey95'),
        panel.grid.minor = element_line(color = 'grey95')) +
  theme(axis.line = element_line(colour = "black")) +
  scale_fill_manual("", values = c("blue", "red"), labels = c(expression(H[0]*": "*mu[1]*"="*mu[0]*"=0"), expression(H[1]*": "*mu[1]*">"*mu[0]*" ("*mu[1]*"=0.5; "*mu[0]*"=0)"))) +
  scale_colour_manual("", values = "blue", labels = "Standard Normal Density") +
  theme(legend.position="bottom") +
  stat_function(fun = dnorm, aes(colour = "line1")) +
  xlab(expression(Z[AW-AIPW]*"-test Value")) +
  ylab("Probability Density") +
  geom_vline(xintercept = quant, linetype="dashed", color = "blue", size=.7) +
  annotate("text",label = bquote("t"[alpha]^"*"), x = 1, y = 0.01, size = 5, colour = "blue") +
  annotate("text",label = bquote("Type-I Error = "*.(typeI)*"%"), x = 3.5, y = 0.3, size = 4, colour = "black") +
  annotate("text",label = bquote("Power = "*.(power)*"%"), x = 3.5, y = 0.25, size = 4, colour = "black")

distr_plot_awaipw

# TS allo probs -----------------------------------------------------------
#Old sims: original NEURIPS submission
par(mfrow = c(2,3))

my_data3 = mysims_normal_H1_3nb
nb = 3
N = 30
t = N/nb
M = 10000

allo_prob_3nb_H0 = c()
for(i in 1:M){
  allo_prob_3nb_H0[i] = 1-my_data3[[i]][N,4]
}
hist(allo_prob_3nb_H0, ylim = c(0,28), freq = F, ylab = "", breaks = 50, main = paste0("N=",N," (n=",nb,";T=", t, ")"), xlab = expression(pi["t,1"]^TS))

nb = 3
N = 90
t = N/nb

allo_prob_3nb_H0 = c()
for(i in 1:M){
  allo_prob_3nb_H0[i] = 1-my_data3[[i]][N,4]
}
hist(allo_prob_3nb_H0, ylim = c(0,28), freq = F, ylab = "", breaks = 50, main = paste0("N=",N," (n=",nb,"; T=", t, ")"), xlab = expression(pi["t,1"]^TS))

nb = 3
N = 150
t = N/nb

allo_prob_3nb_H0 = c()
for(i in 1:M){
  allo_prob_3nb_H0[i] = 1-my_data3[[i]][N,4]
}
hist(allo_prob_3nb_H0, ylim = c(0,28), freq = F, ylab = "", breaks = 50, main = paste0("N=",N," (n=",nb,"; T=", t, ")"), xlab = expression(pi["t,1"]^TS))


my_data10 = mysims_normal_H1_10nb
nb = 10
N = 30
t = N/nb

allo_prob_10nb_H0 = c()
for(i in 1:M){
  allo_prob_10nb_H0[i] = 1-my_data10[[i]][N,4]
}
hist(allo_prob_10nb_H0, ylim = c(0,28), freq = F, ylab = "", breaks = 50, main = paste0("N=",N," (n=",nb,";T=", t, ")"), xlab = expression(pi["t,1"]^TS))

nb = 10
N = 90
t = N/nb

allo_prob_10nb_H0 = c()
for(i in 1:M){
  allo_prob_10nb_H0[i] = 1-my_data3[[i]][N,4]
}
hist(allo_prob_10nb_H0, ylim = c(0,28), freq = F, ylab = "", breaks = 50, main = paste0("N=",N," (n=",nb,"; T=", t, ")"), xlab = expression(pi["t,1"]^TS))

nb = 10
N = 150
t = N/nb

allo_prob_10nb_H0 = c()
for(i in 1:M){
  allo_prob_10nb_H0[i] = 1-my_data3[[i]][N,4]
}
hist(allo_prob_10nb_H0, ylim = c(0,28), freq = F, ylab = "", breaks = 50, main = paste0("N=",N," (n=",nb,"; T=", t, ")"), xlab = expression(pi["t,1"]^TS))




# Allocation Trajectory example plot --------------------------------------

# Standard TS, var 1, n = 1
plot(try_H0[[1]]$AlloProb1, type = "l", xlim = c(0, 150), col = "blue", xlab = "Sample Size (N)", 
     ylab = "Allocation Probability of the Experimental Arm", ylim = c(0, 1))
abline(h = 0.5, lty = 2)
points(1-try_H1[[2]]$AlloProb1, type = "l", col = "red")
legend("bottomright", legend = c("Equal Allocation", "Experimental arm identical to control arm", "Experimental arm superior to control"), 
       col = c("black", "blue", "red"), lty = c(2, 1, 1), bty = "n", cex = 0.9)

# legend("bottomright", 
#        legend = c("Equal Allocation", expression(H[0]*": experimental arm equivalent to control arm (equal means)"), bquote(H[1]*": experimental arm superior to control arm (higher mean)")), 
#        col = c("black", "blue", "red"), lty = c(2, 1, 1), bty = "n", cex = 0.9)


# Radar Chart -------------------------------------------------------------

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


# Type-I Error & Power - Non-stationarity: OK -------------------------------------------------------

# 3nb
my_nb = 3
df = as.data.frame(TypeI_nb3[c(2,3:8)])
#df = as.data.frame(TypeI_nb3[,-1])
df = df[1:which(df$N==150),]
names(df) = c("N", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat",
              "Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
df <- reshape2::melt(df, id.var='N')
ablineN = 51

var_n = c("Pi_stat",  "BOLS_Stat", "AWAIPW_Stat",
          "Pi_stat_adj",  "BOLS_Stat_adj", "AWAIPW_Stat_adj")
labels_n = c(bquote("AP-test"),  bquote(Z[BOLS]), bquote(Z[AW-AIPW]),
             bquote("AP-test (with exact "*alpha*"-level control)"),  bquote(Z[BOLS]*" (with exact "*alpha*"-level control)"), bquote(Z[AW-AIPW]*" (with exact "*alpha*"-level control)"))

plot_3nb_typeI = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = 1)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c(rep("dotted",3), rep("solid",3)),breaks=var_n, labels=labels_n)+
  scale_colour_manual(" ",values=rep(c("#F8766D","#529EFF","#00C08D"),2),breaks=var_n, labels=labels_n)+
  scale_shape_manual(" ",values=c(19, 19, 19, 19, 19, 19),breaks=var_n, labels=labels_n)+
  xlab('Sample Size (N)') + ylim(0, 0.2) + #xlim(0, 300) + 
  ylab("Type-I Error Rate") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  #ggtitle("Batch Size = 3") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(limits = c(0, 150), sec.axis=sec_axis(trans=~ . * 1/3, name="Step (T)")) +
  geom_vline(xintercept = ablineN, colour = "gray30", linetype=5)


df = as.data.frame(Power_nb3[c(2,3:8)])
#df = as.data.frame(Power_nb3[,-1])
df = df[2:which(df$N==150),]
names(df) = c("N", "Pi_stat", "BOLS_Stat", "AWAIPW_Stat",
              "Pi_stat_adj", "BOLS_Stat_adj", "AWAIPW_Stat_adj")
df <- reshape2::melt(df, id.var='N')

plot_3nb_power = ggplot(df, aes(x=N, y=value, col=variable)) + 
  geom_line(aes(linetype=variable, color=variable), size = 1)+
  geom_point(aes(color=variable), alpha = 1/100) +
  #scale_linetype_manual("",values=c("solid", "dashed")) +
  scale_linetype_manual(" ",values =c(rep("dotted",3), rep("solid",3)),breaks=var_n, labels=labels_n)+
  scale_colour_manual(" ",values=rep(c("#F8766D","#529EFF","#00C08D"),2),breaks=var_n, labels=labels_n)+
  scale_shape_manual(" ",values=c(19, 19, 19, 19, 19, 19),breaks=var_n, labels=labels_n)+
  xlab('Sample Size (N)') + ylim(0, 0.5)  +  #xlim(0, 300) +
  ylab("Power") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(color = 'grey85'),
        panel.grid.minor = element_line(color = 'grey91')) +
  theme(axis.line = element_line(colour = "black"), axis.title=element_text(size=12)) +
  #ggtitle("Batch Size = 3") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(legend.position="none") +
  scale_x_continuous(limits = c(0, 150), sec.axis=sec_axis(trans=~ . * 1/3, name="Step (T)")) +
  geom_vline(xintercept = ablineN, colour = "gray30", linetype=5)


ggarrange(plot_3nb_typeI, plot_3nb_power, 
          ncol=2, nrow=1, common.legend = TRUE, legend="bottom")


# Clipping sensitivity ----------------------------------------------------

Pi_power_clipping

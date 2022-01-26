d_lpsB <- d_TPM[98:103,] 
yRoof_lpsB <- round(max(d_lpsB[,2])*1.2,1) 
p_lpsB <- ggplot(d_lpsB, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_lpsB <- p_lpsB + scale_x_discrete(limit=c('WT', 'pro41'))
p_lpsB <- p_lpsB + ylim(0,yRoof_lpsB)
p_lpsB <- p_lpsB + theme_classic()
p_lpsB <- p_lpsB + ggtitle("lpsB")
p_lpsB <- p_lpsB + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_lpsB
library("ggsignif")
p2_lpsB <- p_lpsB + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_lpsB*0.9)
p2_lpsB



d_easE <- d_TPM[106:111,] 
yRoof_easE <- round(max(d_easE[,2])*1.2,1) 
p_easE <- ggplot(d_easE, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_easE <- p_easE + scale_x_discrete(limit=c('WT', 'pro41'))
p_easE <- p_easE + ylim(0,yRoof_easE)
p_easE <- p_easE + theme_classic()
p_easE <- p_easE + ggtitle("easE")
p_easE <- p_easE + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_easE
library("ggsignif")
p2_easE <- p_easE + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_easE*0.9)
p2_easE


d_easF <- d_TPM[114:119,] 
yRoof_easF <- round(max(d_easF[,2])*1.2,1) 
p_easF <- ggplot(d_easF, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_easF <- p_easF + scale_x_discrete(limit=c('WT', 'pro41'))
p_easF <- p_easF + ylim(0,yRoof_easF)
p_easF <- p_easF + theme_classic()
p_easF <- p_easF + ggtitle("easF")
p_easF <- p_easF + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_easF
library("ggsignif")
p2_easF <- p_easF + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_easF*0.9)
p2_easF


d_easG <- d_TPM[122:127,] 
yRoof_easG <- round(max(d_easG[,2])*1.2,1) 
p_easG <- ggplot(d_easG, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_easG <- p_easG + scale_x_discrete(limit=c('WT', 'pro41'))
p_easG <- p_easG + ylim(0,yRoof_easG)
p_easG <- p_easG + theme_classic()
p_easG <- p_easG + ggtitle("easG")
p_easG <- p_easG + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_easG
library("ggsignif")
p2_easG <- p_easG + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_easG*0.9)
p2_easG


d_easA <- d_TPM[130:135,] 
yRoof_easA <- round(max(d_easA[,2])*1.2,1) 
p_easA <- ggplot(d_easA, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_easA <- p_easA + scale_x_discrete(limit=c('WT', 'pro41'))
p_easA <- p_easA + ylim(0,yRoof_easA)
p_easA <- p_easA + theme_classic()
p_easA <- p_easA + ggtitle("easA")
p_easA <- p_easA + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_easA
library("ggsignif")
p2_easA <- p_easA + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_easA*0.9)
p2_easA

d_easH <- d_TPM[138:143,] 
yRoof_easH <- round(max(d_easH[,2])*1.2,1) 
p_easH <- ggplot(d_easH, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_easH <- p_easH + scale_x_discrete(limit=c('WT', 'pro41'))
p_easH <- p_easH + ylim(0,yRoof_easH)
p_easH <- p_easH + theme_classic()
p_easH <- p_easH + ggtitle("easH")
p_easH <- p_easH + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_easH
library("ggsignif")
p2_easH <- p_easH + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_easH*0.9)
p2_easH


d_dmaW <- d_TPM[146:151,] 
yRoof_dmaW <- round(max(d_dmaW[,2])*1.2,1) 
p_dmaW <- ggplot(d_dmaW, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_dmaW <- p_dmaW + scale_x_discrete(limit=c('WT', 'pro41'))
p_dmaW <- p_dmaW + ylim(0,yRoof_dmaW)
p_dmaW <- p_dmaW + theme_classic()
p_dmaW <- p_dmaW + ggtitle("dmaW")
p_dmaW <- p_dmaW + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_dmaW
library("ggsignif")
p2_dmaW <- p_dmaW + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_dmaW*0.9)
p2_dmaW


d_cloA <- d_TPM[154:159,] 
yRoof_cloA <- round(max(d_cloA[,2])*1.2,1) 
p_cloA <- ggplot(d_cloA, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_cloA <- p_cloA + scale_x_discrete(limit=c('WT', 'pro41'))
p_cloA <- p_cloA + ylim(0,yRoof_cloA)
p_cloA <- p_cloA + theme_classic()
p_cloA <- p_cloA + ggtitle("cloA")
p_cloA <- p_cloA + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_cloA
library("ggsignif")
p2_cloA <- p_cloA + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_cloA*0.9)
p2_cloA


d_easC <- d_TPM[162:167,] 
yRoof_easC <- round(max(d_easC[,2])*1.2,1) 
p_easC <- ggplot(d_easC, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_easC <- p_easC + scale_x_discrete(limit=c('WT', 'pro41'))
p_easC <- p_easC + ylim(0,yRoof_easC)
p_easC <- p_easC + theme_classic()
p_easC <- p_easC + ggtitle("easC")
p_easC <- p_easC + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_easC
library("ggsignif")
p2_easC <- p_easC + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_easC*0.9)
p2_easC


d_easD <- d_TPM[170:175,] 
yRoof_easD <- round(max(d_easD[,2])*1.2,1) 
p_easD <- ggplot(d_easD, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_easD <- p_easD + scale_x_discrete(limit=c('WT', 'pro41'))
p_easD <- p_easD + ylim(0,yRoof_easD)
p_easD <- p_easD + theme_classic()
p_easD <- p_easD + ggtitle("easD")
p_easD <- p_easD + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_easD
library("ggsignif")
p2_easD <- p_easD + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_easD*0.9)
p2_easD

d_lpsA <- d_TPM[242:247,] 
yRoof_lpsA <- round(max(d_lpsA[,2])*1.2,1) 
p_lpsA <- ggplot(d_lpsA, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_lpsA <- p_lpsA + scale_x_discrete(limit=c('WT', 'pro41'))
p_lpsA <- p_lpsA + ylim(0,yRoof_lpsA)
p_lpsA <- p_lpsA + theme_classic()
p_lpsA <- p_lpsA + ggtitle("lpsA")
p_lpsA <- p_lpsA + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_lpsA
library("ggsignif")
p2_lpsA <- p_lpsA + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_lpsA*0.9)
p2_lpsA




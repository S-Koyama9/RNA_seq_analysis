library(tidyverse)
library(scales)
library(ggplot2)
theme_set(theme_set(theme_classic() + theme(legend.position = "top")))
d_TPM <- read.csv("candidate genes.csv")
d_ltmM <- d_TPM[18:23,] 
yRoof_ltmM <- round(max(d_ltmM[,2])*1.2,1) 
p_ltmM <- ggplot(d_ltmM, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_ltmM <- p_ltmM + scale_x_discrete(limit=c('WT', 'pro41'))
p_ltmM <- p_ltmM + ylim(0,yRoof_ltmM)
p_ltmM <- p_ltmM + ggtitle("ltmM")
p_ltmM <- p_ltmM + theme_classic()
p_ltmM <- p_ltmM + labs(
  y = "TPM", x= ""
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_ltmM
library("ggsignif")
p2_ltmM <- p_ltmM + geom_signif(comparisons = list(c("WT", "pro41")),
                                  test = "t.test",
                                  na.rm = FALSE,
                                  map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                  y_position = yRoof_ltmM*0.9)
p2_ltmM




d_ltmG <- d_TPM[26:31,] 
yRoof_ltmG <- round(max(d_ltmG[,2])*1.2,1) 
p_ltmG <- ggplot(d_ltmG, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_ltmG <- p_ltmG + scale_x_discrete(limit=c('WT', 'pro41'))
p_ltmG <- p_ltmG + ylim(0,yRoof_ltmG)
p_ltmG <- p_ltmG + theme_classic()
p_ltmG <- p_ltmG + ggtitle("ltmG")
p_ltmG <- p_ltmG + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_ltmG
library("ggsignif")
p2_ltmG <- p_ltmG + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_ltmG*0.9)
p2_ltmG



d_ltmK <- d_TPM[34:39,] 
yRoof_ltmK <- round(max(d_ltmK[,2])*1.2,1) 
p_ltmK <- ggplot(d_ltmK, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_ltmK <- p_ltmK + scale_x_discrete(limit=c('WT', 'pro41'))
p_ltmK <- p_ltmK + ylim(0,yRoof_ltmK)
p_ltmK <- p_ltmK + theme_classic()
p_ltmK <- p_ltmK + ggtitle("ltmK")
p_ltmK <- p_ltmK + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_ltmK
library("ggsignif")
p2_ltmK <- p_ltmK + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_ltmK*0.9)
p2_ltmK






d_ltmB <- d_TPM[42:47,] 
yRoof_ltmB <- round(max(d_ltmB[,2])*1.2,1) 
p_ltmB <- ggplot(d_ltmB, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_ltmB <- p_ltmB + scale_x_discrete(limit=c('WT', 'pro41'))
p_ltmB <- p_ltmB + ylim(0,yRoof_ltmB)
p_ltmB <- p_ltmB + theme_classic()
p_ltmB <- p_ltmB + ggtitle("ltmB")
p_ltmB <- p_ltmB + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_ltmB
library("ggsignif")
p2_ltmB <- p_ltmB + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_ltmB*0.9)
p2_ltmB





d_ltmC <- d_TPM[50:55,] 
yRoof_ltmC <- round(max(d_ltmC[,2])*1.2,1) 
p_ltmC <- ggplot(d_ltmC, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_ltmC <- p_ltmC + scale_x_discrete(limit=c('WT', 'pro41'))
p_ltmC <- p_ltmC + ylim(0,yRoof_ltmC)
p_ltmC <- p_ltmC + theme_classic()
p_ltmC <- p_ltmC + ggtitle("ltmC")
p_ltmC <- p_ltmC + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_ltmC
library("ggsignif")
p2_ltmC <- p_ltmC + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_ltmC*0.9)
p2_ltmC







d_ltmF <- d_TPM[58:63,] 
yRoof_ltmF <- round(max(d_ltmF[,2])*1.2,1) 
p_ltmF <- ggplot(d_ltmF, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_ltmF <- p_ltmF + scale_x_discrete(limit=c('WT', 'pro41'))
p_ltmF <- p_ltmF + ylim(0,yRoof_ltmF)
p_ltmF <- p_ltmF + theme_classic()
p_ltmF <- p_ltmF + ggtitle("ltmF")
p_ltmF <- p_ltmF + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_ltmF
library("ggsignif")
p2_ltmF <- p_ltmF + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_ltmF*0.9)
p2_ltmF





d_ltmQ <- d_TPM[66:71,] 
yRoof_ltmQ <- round(max(d_ltmQ[,2])*1.2,1) 
p_ltmQ <- ggplot(d_ltmQ, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_ltmQ <- p_ltmQ + scale_x_discrete(limit=c('WT', 'pro41'))
p_ltmQ <- p_ltmQ + ylim(0,yRoof_ltmQ)
p_ltmQ <- p_ltmQ + theme_classic()
p_ltmQ <- p_ltmQ + ggtitle("ltmQ")
p_ltmQ <- p_ltmQ + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_ltmQ
library("ggsignif")
p2_ltmQ <- p_ltmQ + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_ltmQ*0.9)
p2_ltmQ




d_ltmP <- d_TPM[74:79,] 
yRoof_ltmP <- round(max(d_ltmP[,2])*1.2,1) 
p_ltmP <- ggplot(d_ltmP, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_ltmP <- p_ltmP + scale_x_discrete(limit=c('WT', 'pro41'))
p_ltmP <- p_ltmP + ylim(0,yRoof_ltmP)
p_ltmP <- p_ltmP + theme_classic()
p_ltmP <- p_ltmP + ggtitle("ltmP")
p_ltmP <- p_ltmP + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_ltmP
library("ggsignif")
p2_ltmP <- p_ltmP + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_ltmP*0.9)
p2_ltmP





d_ltmJ <- d_TPM[82:87,] 
yRoof_ltmJ <- round(max(d_ltmJ[,2])*1.2,1) 
p_ltmJ <- ggplot(d_ltmJ, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_ltmJ <- p_ltmJ + scale_x_discrete(limit=c('WT', 'pro41'))
p_ltmJ <- p_ltmJ + ylim(0,yRoof_ltmJ)
p_ltmJ <- p_ltmJ + theme_classic()
p_ltmJ <- p_ltmJ + ggtitle("ltmJ")
p_ltmJ <- p_ltmJ + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_ltmJ
library("ggsignif")
p2_ltmJ <- p_ltmJ + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_ltmJ*0.9)
p2_ltmJ

d_ltmE <- d_TPM[90:95,] 
yRoof_ltmE <- round(max(d_ltmE[,2])*1.2,1) 
p_ltmE <- ggplot(d_ltmE, aes(x=strain, y=TPM)) + geom_boxplot(width = 0.4)
p_ltmE <- p_ltmE + scale_x_discrete(limit=c('WT', 'pro41'))
p_ltmE <- p_ltmE + ylim(0,yRoof_ltmE)
p_ltmE <- p_ltmE + theme_classic()
p_ltmE <- p_ltmE + ggtitle("ltmE")
p_ltmE <- p_ltmE + labs(
  y = "TPM", x= "", 
)+
  theme(
    axis.title = element_text(size = 14)
  )
p_ltmE
library("ggsignif")
p2_ltmE <- p_ltmE + geom_signif(comparisons = list(c("WT", "pro41")),
                                test = "t.test",
                                na.rm = FALSE,
                                map_signif_level = c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05),
                                y_position = yRoof_ltmE*0.9)
p2_ltmE

## ---- load
library(tidyverse)
library(ggplot2)
library(reshape2)


## ---- data
YE1 <- load("data/yearly_exp1.rda")
YE2 <- load("data/yearly_exp2.rda")
YE2_sub <- load("data/yearly_exp2_sub.rda")
QE1 <- load("data/quarterly_exp1.rda")
QE2 <- load("data/quarterly_exp2.rda")
QE2_sub <- load("data/quarterly_exp2_sub.rda")
ME1 <- load("data/monthly_exp1.rda")
ME2 <- load("data/monthly_exp2.rda")

yearly_exp1 <- rename(yearly_exp1, spikiness = spike)
yearly_exp2 <- rename(yearly_exp2, spikiness = spike)
quarterly_exp1 <- rename(quarterly_exp1, spikiness = spike)
quarterly_exp2 <- rename(quarterly_exp2, spikiness = spike)
monthly_exp1 <- rename(monthly_exp1, spikiness = spike)
monthly_exp2 <- rename(monthly_exp2, spikiness = spike)

yearly_exp1 <- rename(yearly_exp1,  "T" = N)
yearly_exp2 <- rename(yearly_exp2, "T" = N)
quarterly_exp1 <- rename(quarterly_exp1,  "T" = N)
quarterly_exp2 <- rename(quarterly_exp2,  "T" = N)
monthly_exp1 <- rename(monthly_exp1,  "T" = N)
monthly_exp2 <- rename(monthly_exp2,  "T" = N)


quarterly_exp1 <- rename(quarterly_exp1, seasonality = seasonal_strength)
quarterly_exp2 <- rename(quarterly_exp2, seasonality = seasonal_strength)
monthly_exp1 <- rename(monthly_exp1, seasonality = seasonal_strength)
monthly_exp2 <- rename(monthly_exp2, seasonality = seasonal_strength)


## ---- Experiment 1
pcaM1YDF <- filter(yearly_exp1, datasource=="M1") # Yealy - E1
pcaM1YDFvariables <- pcaM1YDF[,1:25]
pcaM1Y <- prcomp(pcaM1YDFvariables, center=TRUE, scale=TRUE)
#summary(pcaM1Y)
PC1m1y = pcaM1Y$x[,1]
PC2m1y = pcaM1Y$x[,2]
PC3m1y = pcaM1Y$x[,3]
m1yPCAresults = data.frame(PC1m1y, PC2m1y, PC3m1y,pcaM1YDFvariables)
M1Ysimulated <- filter(yearly_exp1, datasource!="M1")
projectM1Ysimulated <- M1Ysimulated[,1:25]
simulatedPCAM1Y <- scale(projectM1Ysimulated,pcaM1Y$center, pcaM1Y$scale) %*% pcaM1Y$rotation
M1Ypca <- data.frame(PC1=PC1m1y, PC2=PC2m1y, PC3=PC3m1y)
rownames(M1Ypca) <- NULL
SimulatedPCAM1Y <- data.frame(PC1=simulatedPCAM1Y[,1], PC2=simulatedPCAM1Y[,2], PC3=simulatedPCAM1Y[,3])
M3Y <- filter(yearly_exp2, datasource=="M3")
projectM3Y <- M3Y[,1:25]
simM3YPCA <- scale(projectM3Y, pcaM1Y$center,pcaM1Y$scale) %*% pcaM1Y$rotation
M3YPCA <- data.frame(PC1=simM3YPCA[,1], PC2=simM3YPCA[,2], PC3=simM3YPCA[,3])
pcaALLM1Y <- bind_rows(SimulatedPCAM1Y, M3YPCA, M1Ypca, .id="source")

# observed series only
pc1_m1y <- ggplot(pcaALLM1Y, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("NA", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("Yearly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc1_m1y.png",  width = 5, height = 4)
pc2_m1y <- ggplot(pcaALLM1Y, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("NA", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5,size = 20, face = "bold"))
ggsave("figures/pc2_m1y.png",  width = 5, height = 4)
pc3_m1y <- ggplot(pcaALLM1Y, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("NA", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc3_m1y.png",  width = 5, height = 4)

pc1_m1ys <- ggplot(pcaALLM1Y, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("Yearly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc1_m1ys.png",  width = 5, height = 4)
pc2_m1ys <- ggplot(pcaALLM1Y, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5,size = 20, face = "bold"))
ggsave("figures/pc2_m1ys.png",  width = 5, height = 4)
pc3_m1ys <- ggplot(pcaALLM1Y, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc3_m1ys.png",  width = 5, height = 4)



pca1M1Y <- ggplot(pcaALLM1Y, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("Yearly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca1M1Y.png",  width = 5, height = 4)
pca2M1Y <- ggplot(pcaALLM1Y, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5,size = 20, face = "bold"))
ggsave("figures/pca2M1Y.png",  width = 5, height = 4)
pca3M1Y <- ggplot(pcaALLM1Y, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca3M1Y.png",  width = 5, height = 4)


# Quarterly data
pcaM1QDF <- filter(quarterly_exp1, datasource=="M1") # Quarterly - E1
pcaM1QDFvariables <- pcaM1QDF[,1:30]
pcaM1Q <- prcomp(pcaM1QDFvariables, center=TRUE, scale=TRUE)
summary(pcaM1Q)
PC1m1q = pcaM1Q$x[,1]
PC2m1q = pcaM1Q$x[,2]
PC3m1q = pcaM1Q$x[,3]
m1qPCAresults = data.frame(PC1m1q, PC2m1q, PC3m1q,pcaM1QDFvariables)
M1Qsimulated <- filter(quarterly_exp1, datasource!="M1")
projectM1Qsimulated <- M1Qsimulated[, 1:30]
simulatedPCAM1Q <- scale(projectM1Qsimulated,pcaM1Q$center, pcaM1Q$scale) %*% pcaM1Q$rotation
M1Qpca <- data.frame(PC1=PC1m1q, PC2=PC2m1q, PC3=PC3m1q)
rownames(M1Qpca) <- NULL
SimulatedPCAM1Q <- data.frame(PC1=simulatedPCAM1Q[,1], PC2=simulatedPCAM1Q[,2], PC3=simulatedPCAM1Q[,3])
M3Q <- filter(quarterly_exp2, datasource=="M3")
projectM3Q <- M3Q[,1:30]
simM3QPCA <- scale(projectM3Q, pcaM1Q$center,pcaM1Q$scale) %*% pcaM1Q$rotation
M3QPCA <- data.frame(PC1=simM3QPCA[,1], PC2=simM3QPCA[,2], PC3=simM3QPCA[,3])
pcaALLM1Q <- bind_rows(SimulatedPCAM1Q, M3QPCA, M1Qpca, .id="source")



pc1_m1q <- ggplot(pcaALLM1Q, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("NA", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("Quarterly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc1_m1q.png", width = 5, height = 4)
pc2_m1q <- ggplot(pcaALLM1Q, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("NA", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc2_m1q.png",  width = 5, height = 4)
pc3_m1q <- ggplot(pcaALLM1Q, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("NA", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc3_m1q.png",  width = 5, height = 4)


pc1_m1qs <- ggplot(pcaALLM1Q, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("Quarterly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc1_m1qs.png", width = 5, height = 4)
pc2_m1qs <- ggplot(pcaALLM1Q, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc2_m1qs.png",  width = 5, height = 4)
pc3_m1qs <- ggplot(pcaALLM1Q, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc3_m1qs.png",  width = 5, height = 4)



pca1M1Q <- ggplot(pcaALLM1Q, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("Quarterly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca1M1Q.png", width = 5, height = 4)
pca2M1Q <- ggplot(pcaALLM1Q, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca2M1Q.png",  width = 5, height = 4)
pca3M1Q <- ggplot(pcaALLM1Q, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca3M1Q.png",  width = 5, height = 4)

# Monthly data

pcaM1MDF <- filter(monthly_exp1, datasource=="M1") # Monthly-E1
pcaM1MDFvariables <- pcaM1MDF[,1:30]
pcaM1M <- prcomp(pcaM1MDFvariables, center=TRUE, scale=TRUE)
summary(pcaM1M)
PC1m1m = pcaM1M$x[,1]
PC2m1m = pcaM1M$x[,2]
PC3m1m = pcaM1M$x[,3]
m1mPCAresults = data.frame(PC1m1m, PC2m1m, PC3m1m,pcaM1MDFvariables)
M1Msimulated <- filter(monthly_exp1, datasource!="M1")
projectM1Msimulated <- M1Msimulated[, 1:30]
simulatedPCAM1M <- scale(projectM1Msimulated,pcaM1M$center, pcaM1M$scale) %*% pcaM1M$rotation
M1Mpca <- data.frame(PC1=PC1m1m, PC2=PC2m1m, PC3=PC3m1m)
rownames(M1Mpca) <- NULL
SimulatedPCAM1M <- data.frame(PC1=simulatedPCAM1M[,1], PC2=simulatedPCAM1M[,2], PC3=simulatedPCAM1M[,3])
M3M <- filter(monthly_exp2, datasource=="M3")
projectM3M <- M3M[, 1:30]
simM3MPCA <- scale(projectM3M, pcaM1M$center,pcaM1M$scale) %*% pcaM1M$rotation
M3MPCA <- data.frame(PC1=simM3MPCA[,1], PC2=simM3MPCA[,2], PC3=simM3MPCA[,3])
pcaALLM1M <- bind_rows(SimulatedPCAM1M, M3MPCA, M1Mpca, .id="source")

pc1_m1m <- ggplot(pcaALLM1M, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("NA", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("Monthly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc1_m1m.png", width = 5, height = 4)
pc2_m1m <- ggplot(pcaALLM1M, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("NA", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc2_m1m.png", width = 5, height = 4)
pc3_m1m <- ggplot(pcaALLM1M, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("NA", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc3_m1m.png", width = 5, height = 4)


pc1_m1ms <- ggplot(pcaALLM1M, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("Monthly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc1_m1ms.png", width = 5, height = 4)
pc2_m1ms <- ggplot(pcaALLM1M, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc2_m1ms.png", width = 5, height = 4)
pc3_m1ms <- ggplot(pcaALLM1M, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "NA", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pc3_m1ms.png", width = 5, height = 4)



pca1M1M <- ggplot(pcaALLM1M, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("Monthly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca1M1M.png", width = 5, height = 4)
pca2M1M <- ggplot(pcaALLM1M, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca2M1M.png", width = 5, height = 4)
pca3M1M <- ggplot(pcaALLM1M, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca3M1M.png", width = 5, height = 4)


## ---- Experiment 2

pcaM3YDF <- filter(yearly_exp2, datasource=="M3") # Yearly - E2
pcaM3YDFvariables <- pcaM3YDF [,1:25]
pcaM3Y <- prcomp(pcaM3YDFvariables, center=TRUE, scale=TRUE)
summary(pcaM3Y)
PC1m3y = pcaM3Y$x[,1]
PC2m3y = pcaM3Y$x[,2]
PC3m3y = pcaM3Y$x[,3]
m3yPCAresults = data.frame(PC1m3y, PC2m3y, PC3m3y,pcaM3YDFvariables)
M3Ysimulated <- filter(yearly_exp2, datasource!="M3")
projectM3Ysimulated <- M3Ysimulated[,1:25]
simulatedPCAM3Y <- scale(projectM3Ysimulated,pcaM3Y$center, pcaM3Y$scale) %*% pcaM3Y$rotation
M3Ypca <- data.frame(PC1=PC1m3y, PC2=PC2m3y, PC3=PC3m3y)
rownames(M3Ypca) <- NULL
SimulatedPCAM3Y <- data.frame(PC1=simulatedPCAM3Y[,1], PC2=simulatedPCAM3Y[,2], PC3=simulatedPCAM3Y[,3])
M1Y <- filter(yearly_exp1, datasource=="M1")
projectM1Y <- M1Y[,1:25]
simM1YPCA <- scale(projectM1Y, pcaM3Y$center,pcaM3Y$scale) %*% pcaM3Y$rotation
M1YPCA <- data.frame(PC1=simM1YPCA[,1], PC2=simM1YPCA[,2], PC3=simM1YPCA[,3])
rfsubsample <- load("data/yearly_exp2_sub.rda")
projectM3RFdataSub <- yearly_exp2_sub[,1:25]
M3RFdataSubPCA <- scale(projectM3RFdataSub, pcaM3Y$center,pcaM3Y$scale) %*% pcaM3Y$rotation
subsamplePCA <- data.frame(PC1=M3RFdataSubPCA[,1], PC2=M3RFdataSubPCA[,2], PC3=M3RFdataSubPCA[,3])
pcaALLM3Y <- bind_rows(SimulatedPCAM3Y, subsamplePCA, M3Ypca,M1YPCA, .id="source")
pca1M3Y <- ggplot(pcaALLM3Y, aes(x=PC1, y=PC2, color=source)) + geom_point()+
  scale_color_manual(values=c("forestgreen", "yellow", "black", "firebrick1"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("Yearly")
ggsave("figures/pca1M3Y.png", width = 5, height = 4)
pca2M3Y <- ggplot(pcaALLM3Y, aes(x=PC1, y=PC3, color=source)) + geom_point()+ 
  scale_color_manual(values=c("forestgreen", "yellow" ,"black", "firebrick1"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca2M3Y.png", width = 5, height = 4)
pca3M3Y <- ggplot(pcaALLM3Y, aes(x=PC2, y=PC3, color=source)) + geom_point()+ 
  scale_color_manual(values=c("forestgreen", "yellow", "black", "firebrick1" ))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca3M3Y.png", width = 5, height = 4)

pcaM3QDF <- filter(quarterly_exp2, datasource=="M3") #Quaterly - E2
pcaM3QDFvariables <- pcaM3QDF[, 1:30]
pcaM3Q <- prcomp(pcaM3QDFvariables, center=TRUE, scale=TRUE)
summary(pcaM3Q)
PC1m3q = pcaM3Q$x[,1]
PC2m3q = pcaM3Q$x[,2]
PC3m3q = pcaM3Q$x[,3]
m3qPCAresults = data.frame(PC1m3q, PC2m3q, PC3m3q,pcaM3QDFvariables)
M3Qsimulated <- filter(quarterly_exp2, datasource!="M3")
projectM3Qsimulated <- M3Qsimulated[,1:30]
simulatedPCAM3Q <- scale(projectM3Qsimulated,pcaM3Q$center, pcaM3Q$scale) %*% pcaM3Q$rotation
M3Qpca <- data.frame(PC1=PC1m3q, PC2=PC2m3q, PC3=PC3m3q)
rownames(M3Qpca) <- NULL
SimulatedPCAM3Q <- data.frame(PC1=simulatedPCAM3Q[,1], PC2=simulatedPCAM3Q[,2], PC3=simulatedPCAM3Q[,3])
M1Q <- filter(quarterly_exp1, datasource=="M1")
projectM1Q <- M1Q[,1:30]
simM1QPCA <- scale(projectM1Q, pcaM3Q$center,pcaM3Q$scale) %*% pcaM3Q$rotation
M1QPCA <- data.frame(PC1=simM1QPCA[,1], PC2=simM1QPCA[,2], PC3=simM1QPCA[,3])
projectM3QRFdataSub <- quarterly_exp2_sub[,1:30]
M3QRFdataSubPCA <- scale(projectM3QRFdataSub, pcaM3Q$center,pcaM3Q$scale) %*% pcaM3Q$rotation
subsamplePCAQ <- data.frame(PC1=M3QRFdataSubPCA[,1], PC2=M3QRFdataSubPCA[,2], PC3=M3QRFdataSubPCA[,3])
pcaALLM3Q <- bind_rows(SimulatedPCAM3Q, subsamplePCAQ, M3Qpca, M1QPCA, .id="source")
pca1M3Q <- ggplot(pcaALLM3Q, aes(x=PC1, y=PC2, color=source)) + geom_point()+ 
  scale_color_manual(values=c("forestgreen", "yellow", "black", "firebrick1"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("Quartely")
ggsave("figures/pca1M3Q.png", width = 5, height = 4)
pca2M3Q <- ggplot(pcaALLM3Q, aes(x=PC1, y=PC3, color=source)) + geom_point()+ 
  scale_color_manual(values=c("forestgreen", "yellow", "black", "firebrick1"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca2M3Q.png", width = 5, height = 4)
pca3M3Q <- ggplot(pcaALLM3Q, aes(x=PC2, y=PC3, color=source)) + geom_point()+
  scale_color_manual(values=c("forestgreen", "yellow", "black", "firebrick1"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca3M3Q.png", width = 5, height = 4)



pcaM3MDF <- filter(monthly_exp2, datasource=="M3") #Monthly - E2
pcaM3MDFvariables <- pcaM3MDF[,1:30]
pcaM3M <- prcomp(pcaM3MDFvariables, center=TRUE, scale=TRUE)
summary(pcaM3M)
PC1m3m = pcaM3M$x[,1]
PC2m3m = pcaM3M$x[,2]
PC3m3m = pcaM3M$x[,3]
m3mPCAresults = data.frame(PC1m3m, PC2m3m, PC3m3m,pcaM3MDFvariables)
M3Msimulated <- filter(monthly_exp2, datasource!="M3")
projectM3Msimulated <- M3Msimulated[,1:30]
simulatedPCAM3M <- scale(projectM3Msimulated,pcaM3M$center, pcaM3M$scale) %*% pcaM3M$rotation
M3Mpca <- data.frame(PC1=PC1m3m, PC2=PC2m3m, PC3=PC3m3m)
rownames(M3Mpca) <- NULL
SimulatedPCAM3M <- data.frame(PC1=simulatedPCAM3M[,1], PC2=simulatedPCAM3M[,2], PC3=simulatedPCAM3M[,3])
M1M <- filter(monthly_exp1, datasource=="M1")
projectM1M <- M1M[, 1:30]
simM1MPCA <- scale(projectM1M, pcaM3M$center,pcaM3M$scale) %*% pcaM3M$rotation
M1MPCA <- data.frame(PC1=simM1MPCA[,1], PC2=simM1MPCA[,2], PC3=simM1MPCA[,3])
pcaALLM3M <- bind_rows(SimulatedPCAM3M, M1MPCA, M3Mpca, .id="source")
pca1M3M <- ggplot(pcaALLM3M, aes(x=PC1, y=PC2, color=source)) + geom_point()+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("Monthly")
ggsave("figures/pca1M3M.png", width = 5, height = 4)
pca2M3M <- ggplot(pcaALLM3M, aes(x=PC1, y=PC3, color=source)) + geom_point()+ 
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca2M3M.png", width = 5, height = 4)
pca3M3M <- ggplot(pcaALLM3M, aes(x=PC2, y=PC3, color=source)) + geom_point()+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca3M3M.png", width = 5, height = 4)


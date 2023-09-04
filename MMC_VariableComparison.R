setwd("/Users/ilayda/Downloads")
KKMMCs <- read.csv("KKMMC.csv", strip.white=TRUE)
head(KKMMCs)
KKMMCs2 <-na.omit(KKMMCs)

KKMMCs2$Sex<-factor(KKMMCs2$Sex)

library(NatParksPalettes)

#MMC size vs Sex
graph1 <- ggplot(KKMMCs2, aes(Sex, Avg_spot_size, fill=Sex)) + geom_boxplot(outlier.shape = NA)
graph1 + geom_jitter(color="black", size=0.9, alpha=.9, width = 0.1)+ labs(y="Average MMC Size (um2)", x="") +  theme_classic()
#MMC density vs Sex
graph2 <- ggplot(KKMMCs2, aes(Sex, Spot_density, fill=Sex)) + geom_boxplot(outlier.shape = NA)
graph2 + geom_jitter(color="black", size=0.9, alpha=.9, width = 0.1)+ labs(y="MMC density (um2)", x="") +  theme_classic()


#test for normality
shapiro.test(KKMMCs2$Avg_spot_size)
#statitistically different than normal distribution = not normal!  
res <- wilcox.test(Avg_spot_size ~ Sex, data = KKMMCs2,  na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
res

#Grubbs test for outliers
install.packages("outliers")
library(outliers)
test <- grubbs.test(KKMMCs2$Avg_spot_size)
test

#remove outlier
KKMMCs2outlierremoved <- KKMMCs2[-4,]
head(KKMMCs2outlierremoved)
sexnooutlier <- ggplot(KKMMCs2outlierremoved, aes(Sex, Avg_spot_size, fill=Sex)) + geom_boxplot()
sexnooutlier + geom_jitter(color="black", size=0.9, alpha=.9, width = 0.1)+ labs(y="Average MMC Size (um2)", x="") +  theme_classic()
res2 <- wilcox.test(Avg_spot_size ~ Sex, data = KKMMCs2outlierremoved,  na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
res2
#still not significant with outlier removed


## MMC size vs Family
MMCfamily <- ggplot(KKMMCs2, aes(Family, Avg_spot_size, fill=Family)) + geom_boxplot(outlier.shape = NA)
MMCfamily + geom_jitter(color="black", size=0.9, alpha=.9, width = 0.1)+ labs(y="Average MMC Size (um2)", x="") +  theme_classic()


kruskal.test(Avg_spot_size ~ Family, data = KKMMCs2)
pairwise.wilcox.test(KKMMCs2$Avg_spot_size, KKMMCs2$Family, p.adjust.method = "BH")

#MMCsize vs length
ggplot(KKMMCs2, aes(x=Length, y=Avg_spot_size)) + geom_point()+ geom_smooth(method=lm, se=FALSE) + labs(y="Average MMC Size (um2)", x="Fish Length")+  theme_classic()

#method 2
#Does the plot
plot(KKMMCs2$Length, KKMMCs2$Avg_spot_size)
#Adds the line
abline(lm(KKMMCs2$Avg_spot_size~KKMMCs2$Length), col="red")
#Shows stats on command line
summary(lm(KKMMCs2$Avg_spot_size~KKMMCs2$Length))

#Perc pigment vs length
ggplot(KKMMCs2, aes(x=Length, y=Perc_Pigmented)) + geom_point()+ geom_smooth(method=lm, se=FALSE) + labs(y="Percent pigmentation", x="Fish Length")+  theme_classic()

#Density vs length
ggplot(KKMMCs2, aes(x=Length, y=Spot_density)) + geom_point()+ geom_smooth(method=lm, se=FALSE) + labs(y="Spot density per um2", x="Fish Length")+  theme_classic()

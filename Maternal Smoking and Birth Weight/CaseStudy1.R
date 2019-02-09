#Case Study 1: Maternal Smoking and Birth Weight
#In this case study, we look at the relationship between maternal smoking and birth weight,
#as well as the effect smoking has on gestation period.


#-------------------Kevin Lin and James Yang R script------------------------------
babies.1 <- read.csv("~/Desktop/babies-1.txt", sep="")
babies<- split(babies.1, babies.1$smoke)  #group smoker, nonsmoker and value 99

nonsmoker<- babies$`0`
smoker<- babies$`1`

nonsmoker_weight<- nonsmoker[1]   #take the column "weight" of nonsmoker
smoker_weight<- smoker[1]        #take the column "weight" of smoker
clean_nonsmoker<- nonsmoker_weight[ which(nonsmoker_weight$bwt<999),]
#exclude the data which is '999'

clean_smoker<- smoker_weight[ which(smoker_weight$bwt<999),]
#exclude the data which is '999'

summary(clean_nonsmoker)
summary(clean_smoker)

#  histogram of nonsmoker
hist(clean_nonsmoker, freq=FALSE, col=rgb(0,0,1,0.5), xlab="Birth Weight (Ounces)",
     main="Nonsmoker Child Birth Weight", ylim = c(0, 0.03))
curve(dnorm(x, mean=mean(clean_nonsmoker), sd=sd(clean_nonsmoker)), add=TRUE, 
      col="green", lwd = 2)

#  histogram of nonsmoker
hist(clean_smoker, freq=FALSE, col=rgb(1,0,0,0.5), xlab="Birth Weight (Ounces)",
     main="Smoker Child Birth Weight", ylim = c(0, 0.025))
curve(dnorm(x, mean=mean(clean_smoker), sd=sd(clean_smoker)), add=TRUE, 
      col="green", lwd = 2)

#combine two histogram
hist(clean_nonsmoker, freq=FALSE, col=rgb(0,0,1,0.5), xlab="Birth Weight (Ounces)",
     main="Comparison of Child Birth Weight between Smokers and Nonsmokers")
hist(clean_smoker, freq=FALSE, col=rgb(1,0,0,0.5),add=T)
legend("topright", c("Smoker", "Nonsmoker"), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), lwd=8)
box()

#Boxplot
bab=babies.1[which(babies.1$smoke<9),]
boxplot(bab$bwt~bab$smoke,main="Boxplot of Nonsmoker (0) and Smoker (1) Child Birth Weight",
         ylab = "Birth Weight (Ounces)", xlab = "Smoking Status")

#--------------------------------Evan Velasco R Script--------------------------------
library("moments")

#read data into the table
data <- read.table("babies.txt", header=TRUE)

#seperate the data to only include those who smoke
data.smoker <- data[data['smoke'] == 1, ]

#and for those who don't smoke
data.nonsmoker <- data[data['smoke'] == 0, ]

#Clean gestation and smoking data (used for boxplot later)
data.clean = data[which(data$gestation!=999),]
data.clean = data.clean[which(data.clean$smoke!=9),]

#Clean nonsmoker data to get rid of rows where gestation = 999
data.nonsmoker.clean = data.nonsmoker[which(data.nonsmoker$gestation != 999),]

#Clean smoker data to get rid of rows where gestation = 999
data.smoker.clean = data.smoker[which(data.smoker$gestation != 999),]

#summarize
summary(data.nonsmoker.clean$gestation)
summary(data.smoker.clean$gestation)

#Find SD
sd(data.nonsmoker.clean$gestation, na.rm = FALSE)
sd(data.smoker.clean$gestation, na.rm = FALSE)

#95% Confidence interval using standard error
gestationmean.nonsmoker = 280.2
n.nonsmoker = nrow(data.nonsmoker.clean)
sdgestation.nonsmoker = sd(data.nonsmoker.clean$gestation, na.rm = FALSE)
error.nonsmoker = qnorm(0.975)*(sdgestation.nonsmoker/n.nonsmoker)
leftCI.nonsmoker = gestationmean.nonsmoker - error.nonsmoker
rightCI.nonsmoker = gestationmean.nonsmoker + error.nonsmoker
CI.nonsmoker <- list(leftCI.nonsmoker, rightCI.nonsmoker)
print(CI.nonsmoker)

gestationmean.smoker = 278
n.smoker = nrow(data.smoker.clean)
sdgestation.smoker = sd(data.smoker.clean$gestation, na.rm = FALSE)
error.smoker = qnorm(0.975)*(sdgestation.smoker/n.smoker)
leftCI.smoker = gestationmean.smoker - error.smoker
rightCI.smoker = gestationmean.smoker + error.smoker
CI.smoker <- list(leftCI.smoker, rightCI.smoker)
print(CI.smoker)

#Bootstrap
smoker.BS.means = list()
nonsmoker.BS.means = list()
B = 500
for(i in 1:B){
  smoker.boot = sample(data.smoker.clean$gestation, length(data.smoker.clean$gestation),
          replace = T)
  nonsmoker.boot = sample(data.nonsmoker.clean$gestation, length(data.nonsmoker.clean$gestation),
          replace = T)
  smoker.BS.means[i] = mean(smoker.boot)
  nonsmoker.BS.means[i] = mean(nonsmoker.boot)
}

#Assessing normality of bootstrap means 
smoker.BS.means <- unlist(smoker.BS.means)
nonsmoker.BS.means <- unlist(nonsmoker.BS.means)

skewness(smoker.BS.means)
skewness(nonsmoker.BS.means)

kurtosis(smoker.BS.means)
kurtosis(nonsmoker.BS.means)

qqnorm(smoker.BS.means, main = "Normal QQ Plot for Smoker Gestation Bootstrap Means",
       ylab = "Gestation Period Mean (Days)")
qqline(smoker.BS.means)
qqnorm(nonsmoker.BS.means, main = "Normal QQ Plot for Nonsmoker Gestation Bootstrap Means",
       ylab = "Gestation Period Mean (Days)")
qqline(nonsmoker.BS.means)

#Bootstrap CI
quantile(smoker.BS.means, c(0.025, .975))

quantile(nonsmoker.BS.means, c(0.025, .975))

#skewness using the moments
skewness(data.nonsmoker.clean$gestation)
skewness(data.smoker.clean$gestation)

#kurtosis using the moments package
kurtosis(data.nonsmoker.clean$gestation)
kurtosis(data.smoker.clean$gestation)

#histograms
hist(data.nonsmoker.clean$gestation, freq=FALSE, col=rgb(1,1,0,0.7), 
     xlab="Gestation Period (Days)", ylim = c(0,0.025), main="Nonsmoker Gestation Period")
curve(dnorm(x, mean=mean(data.nonsmoker.clean$gestation), sd=sd(data.nonsmoker.clean$gestation,
     na.rm = FALSE)), add=TRUE, col="red", lwd = 2)

hist(data.smoker.clean$gestation, freq=FALSE, col=rgb(0,1,1,0.4), 
     xlab="Gestation Period (Days)", main="Smoker Gestation Period")
curve(dnorm(x, mean=mean(data.smoker.clean$gestation), 
     sd=sd(data.smoker.clean$gestation, na.rm = FALSE)), add=TRUE, col="red", lwd = 2)

hist(data.nonsmoker.clean$gestation, freq=FALSE, col=rgb(1,1,0,0.7), xlab="Gestation Period (Days)",
     main="Comparison of Gestation Period between Smokers and Nonsmokers", ylim = c(0, .035))
hist(data.smoker.clean$gestation, freq=FALSE, col=rgb(0,1,1,0.4), add=T)
legend("topright", c("Smoker", "Nonsmoker"), col=c(rgb(0,1,1,0.4), rgb(1,1,0,0.7)), lwd=8)
box()



#boxplots
boxplot(data.clean$gestation~data.clean$smoke,main="Boxplot of Nonsmoker (0) and Smoker (1) Gestation Period",
        ylab = "Gestation Period (Days)", xlab = "Smoking Status")

##Q-Q plots with normal distribution
qqnorm(data.smoker.clean$gestation, main = "Normal Q-Q Plot for Smoker Gestation Period",
       ylab = "Gestation Period (Days)")
qqline(data.smoker.clean$gestation)

qqnorm(data.nonsmoker.clean$gestation, main = "Normal Q-Q Plot for Nonsmoker Gestation Period",
       ylab = "Gestation Period (Days)")
qqline(data.nonsmoker.clean$gestation)

#-----------------------------Navin Souda R Script-------------------------------
library(moments)


dat = read.table('babies.txt', header = TRUE)
attach(dat)
summary(dat)
"Compare the frequency, or incidence, of low-birth-weight babies for the two
groups. How reliable do you think your estimates are? That is, how would the
incidence of low birth weight change if a few more or fewer babies were
classified as low birth weight?"

# Low bwt => <2,500 g or 5 lbs, 8 oz i.e. 88 oz




#### CREATING CONFIDENCE INTERVALS FOR BIRTH WEIGHT ####

smokers = dat[smoke == 1, ]
nonsmokers = dat[smoke == 0, ]

# check skewness and kurtosis to check if close to normal
skewness(smokers$bwt)
kurtosis(smokers$bwt)
skewness(nonsmokers$bwt)
kurtosis(nonsmokers$bwt)

# set up bootstrap to create confidence intervals
B = 500
smoker.BS.means = 1:B
nonsmoker.BS.means = 1:B


for(i in 1:B){
  smoker.boot = sample(smokers$bwt, length(smokers$bwt), replace = T)
  nonsmoker.boot = sample(nonsmokers$bwt, length(nonsmokers$bwt), replace = T)
  smoker.BS.means[i] = mean(smoker.boot)
  nonsmoker.BS.means[i] = mean(nonsmoker.boot)
}
#check plots for nomality
hist(smoker.BS.means)
hist(nonsmoker.BS.means)
qqnorm(smoker.BS.means, main = "Normal Q-Q Plot for Smoker Bootstrap Means" )
qqline(smoker.BS.means)
qqnorm(nonsmoker.BS.means, main = "Normal Q-Q Plot for Non-Smoker Bootstrap Means" )
qqline(nonsmoker.BS.means)

# test bootstrap means for normal distribution
ks.test((smoker.BS.means - mean(smoker.BS.means))/sd(smoker.BS.means), pnorm)
ks.test((nonsmoker.BS.means - mean(nonsmoker.BS.means))/sd(nonsmoker.BS.means), pnorm)

# create percentile CI
quantile(smoker.BS.means, c(0.025, 0.975))
quantile(nonsmoker.BS.means, c(0.025, 0.975))

# create CI with t value
mean(smokers$bwt) + c(-1, 1)*qt(.975, length(smokers$bwt)-1)*(sd(smokers$bwt)/sqrt(length(smokers$bwt)))
mean(nonsmokers$bwt) + c(-1, 1)*qt(.975, length(nonsmokers$bwt)-1)*(sd(nonsmokers$bwt)/sqrt(length(nonsmokers$bwt)))








#### CONSIDERING DIFFERENCES IN INCIDENCE AND VARIOUS THRESHOLDS ####


#set threshold for low birth weight

threshold = 88

# find incidence of low birth weight for smokers and nono-smokers
smokerIncidence88 = sum(smokers$bwt < threshold)/dim(smokers)[1]
nonsmokerIncidence88 = sum(nonsmokers$bwt < threshold)/dim(nonsmokers)[1]

sprintf("Smoker Incidence: %f", smokerIncidence88)
sprintf("Nonsmoker Incidence: %f", nonsmokerIncidence88)


smokerIncidences = 1:9
nonsmokerIncidences = 1:9
thresholds = seq(from = 80, to = 96, by = 2)
i = 1

# calculate incidence for various thresholds
for(thresh in thresholds){
  
  smokerIncidences[i] = sum(smokers$bwt < thresh)/dim(smokers)[1]
  nonsmokerIncidences[i] = sum(nonsmokers$bwt < thresh)/dim(nonsmokers)[1]
  i = i + 1
}

# plot change of incidence wrt threshold  
library(tidyverse)

incidences = data.frame(thresholds, smokerIncidences, nonsmokerIncidences)
ggplot(incidences, aes(thresholds)) + 
  geom_line(aes(y = smokerIncidences, color = 'Smoker')) +
  geom_line(aes(y = nonsmokerIncidences, color = 'Non-smoker')) + 
  geom_point(aes(y = smokerIncidences, color = 'Smoker')) + 
  geom_point(aes(y = nonsmokerIncidences, color = 'Non-smoker')) + 
  labs(x = "Thresholds", y = "Low Birth Weight Incidence", title = "Change in Incidence by Threshold") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.title = element_blank(), legend.position = c(0.2,0.8), plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(88), linetype="dotted")


# NOTE: s = smokers, ns = non-smokers
# transform to bernoulli/binomial/binary for threshold 88
sBernoulli = smokers$bwt < 88
nsBernoulli = nonsmokers$bwt < 88

#bootstrap more sample proportions

B = 400
bootSmokerProp = 1:B
bootNSProp = 1:B
for(i in 1:B){
  sBoot = sample(sBernoulli,size = 1.5*length(sBernoulli), replace = TRUE)
  nsBoot = sample(nsBernoulli, size = 1.5*length(nsBernoulli), replace = TRUE)
  bootNSProp[i] = sum(nsBoot)/length(nsBoot)
  bootSmokerProp[i] = sum(sBoot)/length(sBoot)
}

#check bootstrapped sample proportions for normality
hist(bootNSProp)
hist(bootSmokerProp)
qqnorm(bootNSProp)
qqline(bootNSProp)
qqnorm(bootSmokerProp)
qqline(bootSmokerProp)

# test for normality
ks.test(bootNSProp, pnorm, mean(bootNSProp), sd(bootNSProp))
ks.test(bootSmokerProp, pnorm, mean(bootSmokerProp), sd(bootSmokerProp))

# perform test of proportions
prop.test(c(sum(sBernoulli), sum(nsBernoulli)), c(length(sBernoulli), length(nsBernoulli)), alternative = "greater", correct = FALSE)

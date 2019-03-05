#-----------Evan Velasco Code----------
library("moments")
library("tree")

#read data into table
vgd <- read.table("videodata.txt", header=TRUE)

#clean data for use in CART
vgd.cart.clean <- vgd[(vgd$educ!=99) & (vgd$math!=99) & (vgd$work!=99) & (vgd$cdrom!=99),] 

#clean "like" column of data
vgd.like.clean <- vgd[(vgd$like!=99) & (vgd$like!=1),]

#Do students enjoy playing videogames?
#Seperating the data by how much the students like Video Games
vgd.like1 <- vgd[which(vgd$like==1),] #Never played
vgd.like2 <- vgd[which(vgd$like==2),] #Very much
vgd.like3 <- vgd[which(vgd$like==3),] #Somewhat
vgd.like4 <- vgd[which(vgd$like==4),] #Not really
vgd.like5 <- vgd[which(vgd$like==5),] #Not at all

#For this analysis, we will be classifiying enjoying video games as like==2 | like==3
#Else dislike video games

#Create a vector which contains the # of students from each preference category
likecount <- c(nrow(vgd.like1), nrow(vgd.like2), nrow(vgd.like3), nrow(vgd.like4), nrow(vgd.like5))

#Barplot showing distribution of preferences
barplot(likecount, names.arg = c("Never Played", "Like", "Somewhat", "Not Really", "Not at all"),
        main = "Do Students Like Video Games?", ylim = c(0,50), ylab = "Number of Students", 
        col = rgb(0,0,1,0.5))

#Proportion of students who "Like" or "Somewhat Like" Video Games 
(nrow(vgd.like2) + nrow(vgd.like3)) / nrow(vgd.like.clean) #77.53% enjoy video games

#graph the "like" and "dislike" category data
#Sort the data into "like" vs "dislike" categories
vgd.like.clean$like.category<- rep(NA, dim(vgd.like.clean)[1])

for(i in 1:dim(vgd.like.clean)[1]){
  like <- vgd.like.clean[i, 'like']
  if(like==4 || like==5){
    vgd.like.clean[i, 'like.category'] = 0
  }else{
    vgd.like.clean[i, 'like.category'] = 1
  }
}

vgd.category.like <- vgd.like.clean[(vgd.like.clean$like.category==1),] #like
vgd.category.dislike <- vgd.like.clean[(vgd.like.clean$like.category==0),] #dislike
like.categorycount <- c(nrow(vgd.category.like), nrow(vgd.category.dislike))

barplot(like.categorycount, names.arg = c("Like", "Dislike"), 
        main = "Number of Students Who Like vs Dislike Video Games",
        ylab = "Number of Students", col = "gold", ylim = c(0, 70))

#mean for "like" (yes or no)
mean(vgd.like.clean$like.category)


#Bootstrapping a CI for proportion of students who "like" or "Somewhat Like" Video Games

#BS for proportion who "like"
vgd.BS.proplike = list()
B = 500
for(i in 1:B){
  vgd.boot = sample(vgd.like.clean$like, length(vgd.like.clean$like), replace = T)
  vgd.BS.proplike[i] = (sum(vgd.boot==2) + sum(vgd.boot==3))/length(vgd.boot)
}
vgd.BS.proplike <- unlist(vgd.BS.proplike)

quantile(vgd.BS.proplike, c(0.025, .975))

#for dislike
vgd.BS.propdislike = list()
B = 500
for(i in 1:B){
  vgd.boot = sample(vgd.like.clean$like, length(vgd.like.clean$like), replace = T)
  vgd.BS.propdislike[i] = (sum(vgd.boot==4) + sum(vgd.boot==5))/length(vgd.boot)
}
vgd.BS.propdislike <- unlist(vgd.BS.propdislike)

quantile(vgd.BS.propdislike, c(0.025, .975))




skewness(vgd.BS.proplike)
kurtosis(vgd.BS.proplike)

skewness(vgd.BS.propdislike)
kurtosis(vgd.BS.propdislike)

#CART analysis
vgd.cart.clean$dis_like<- rep(NA, dim(vgd.cart.clean)[1])

for(i in 1:dim(vgd.cart.clean)[1]){
  like <- vgd.cart.clean[i, 'like']
  if(like==0 || like==4 || like==5){
    vgd.cart.clean[i, 'dis_like'] = 0
  }else{
    vgd.cart.clean[i, 'dis_like'] = 1
  }
}

vgd.tree <- tree(as.factor(dis_like)~educ+sex+age+home+math+work+own+cdrom+grade, 
                 data=vgd.cart.clean)
plot(vgd.tree, type="uniform")
text(vgd.tree)
title("Classification Tree for Student's Video Game Preferences")

#Extra Q: Women Video Game Play Time
vgd.women <- vgd[which(vgd$sex==0),]

summary(vgd.women$time)

#histogram of time based on sex
hist(vgd.women$time, breaks = 25, probability = TRUE, density = 20, col = "light green", 
     border = "dark green", main = "Time Women Spent Playing Video Games", 
     xlab = "Hours Spent Playing Video Games")
lines(density(vgd.women$time, bw = 0.25), col = "red")
lines(density(vgd.women$time, bw = 0.5), col = "blue")
rug(vgd.women$time)
legend("topright", c("Bandwidth = 0.25", "Bandwidth = 0.50"), 
       col=c("red", "blue"), lwd=3)

#Data does not appear to be normal. Lets examine this further using ecdfs
plot(ecdf(vgd.women$time), do.points = F, verticals = T)
x <- seq(min(vgd.women$time)-4, max(vgd.women$time), length.out = 1000)
lines(x, pnorm(x, mean=mean(vgd.women$time), sd=sd(vgd.women$time)), lty=3, col = 2)

#Q-Q Plots
qqnorm(vgd.women$time)
qqline(vgd.women$time)

#Kolmogorov-Smirnov test
ks.test((vgd.women$time - mean(vgd.women$time))/sd(vgd.women$time), pnorm)



#Hypothesis test for women play time
women.BS = list()
B = 500
for(i in 1:B){
  vgd.boot2 = sample(vgd.women$time, length(vgd.women$time), replace = T)
  women.BS[i] = mean(vgd.boot2)
}
women.BS <- unlist(women.BS)

quantile(women.BS, c(0.025, .975))


percent.women = mean(vgd.women$time)

#bootstrapped t test 
t=(percent.women-2)/sqrt(2^2/38)

percent.women.new=vgd.women$time-percent.women+2

B = 400 # the number of bootstrap samples we want
boot.sample <- array(dim = c(B, 38))
for (i in 1:B) {
  boot.sample[i,] <- sample(percent.women.new, size = , replace = TRUE)
}
boot.mean <- apply(X = boot.sample, MARGIN = 1, FUN = mean)

qqnorm(boot.mean)
qqline(boot.mean)

ks.test((boot.mean-mean(boot.mean))/sd(boot.mean), pnorm)
kurtosis(boot.mean)
skewness(boot.mean)

boot.tstat<-(boot.mean-2)/sqrt(2^2/38)

p_value=sum(boot.tstat<=t)/400
p_value

#----------Navin Souda Code------------
library(MASS)
library(rpart)
library(moments)
library(tidyverse)


initial = read.table('videodata.txt', header = T)
followup = read.table('videoMultiple.txt', header = T)

# Remove those who did not respond to 'like' question, or those who
# never played
cleanlike = initial[(initial['like'] != 99 & initial['like'] != 1), ]
cleanlike['like10'] = cleanlike['like'] <= 3

# tabulate likes/dislikes based on sex
sexTable = table(cleanlike$like10, cleanlike$sex)
sex.df = as.data.frame(sexTable)
colnames(sex.df) = c("Likes", "Gender", "Count")

# plot likes/dislikes based on sex
ggplot(data = sex.df, aes(fill = Likes, y = Count, x = Gender)) + 
  geom_col(position ='dodge')+
  scale_fill_brewer(palette = "Paired", labels = c('Does Not Like VG', 'Likes VG'))+
  labs(x = "Gender")


#remove those who did not respond to work question
workDat = cleanlike[cleanlike['work'] != 99, ]
workDat['work10'] = workDat['work'] != 0

# tabulate likes/dislikes based on work
workTable = table(workDat$like10, workDat$work10)
work.df = as.data.frame(workTable)
colnames(work.df) = c("Likes", "Works", "Count")

# plot likes/dislikes based on work
ggplot(data = work.df, aes(fill = Likes, y = Count, x = Works)) + 
  geom_col(position ='dodge')+
  labs(x = "Works")+
  scale_fill_brewer(palette = "Set1", labels = c('Does Not Like VG', 'Likes VG'))


#remove those who did not respond to the owning a computer question
ownDat = cleanlike[cleanlike['own'] != 99, ]
ownDat['own10'] = ownDat['own'] == 1

# tabulate likes/dislikes based on work
ownTable = table(ownDat$like10, ownDat$own10)
own.df = as.data.frame(ownTable)
colnames(own.df) = c("Likes", "Owns", "Count")

# plot likes/dislikes based on work
ggplot(data = own.df, aes(fill = Likes, y = Count, x = Owns)) + 
  geom_col(position ='dodge')+
  labs(x = "Owns Computer")+
  scale_fill_brewer(palette = "Dark2", labels = c('Does Not Like VG', 'Likes VG'))



# Box plot for freq vs time
ggplot(initial, aes(x = as.factor(freq), y = time, color = as.factor(freq)))+
  geom_boxplot() +
  guides(fill=guide_legend(title="Frequency"))+
  labs(x = "Frequency of Playing Video Games", y = "Time Played Week Prior to Exam", color = "Frequency")

#table for freq counts
table(initial$freq)


## bootstrap stuff
boot.population <- rep(initial$sex, length.out = 314)
sample1 <- sample(boot.population, size = 91, replace = FALSE)
B = 400 # the number of bootstrap samples we want
boot.sample <- array(dim = c(B, 91))
for (i in 1:B) {
  boot.sample[i, ] <- sample(boot.population, size = 91, replace = FALSE)
}



## bootstrap proportion

timeTable = table(initial$time)
boot.population = rep(as.numeric(names(timeTable)), times = counts)
boot.means = 1:400
for(b in 1:400){
  boot.sample = sample(boot.population, size = 91, replace = F)
  played = as.numeric(boot.sample != 0)
  boot.means[b] = mean(played)
}
hist(boot.means, breaks = 25, col= 8, main = "Histogram of bootstrapped proportions", xlab = 'Bootstrapped proportions')
quantile(boot.means, c(0.025, 0.975))
ks.test((boot.means - mean(boot.means))/sd(boot.means),pnorm)
qqnorm(boot.means)
qqline(boot.means)


##bo0tstrap time played
timeTable = table(initial$time)
sample.mean = mean(initial$time)
boot.population = rep(as.numeric(names(timeTable)), times = counts)
boot.means = 1:500
for(b in 1:500){
  boot.sample = sample(boot.population, size = 91, replace = F)
  boot.means[b] = mean(boot.sample)
}
hist(boot.means, breaks = 20, col = 3, main = "Histogram of bootstrapped mean for time played", xlab = "Time played in week prior")
quantile(boot.means, c(0.025, 0.975))
ks.test((boot.means - mean(boot.means))/sd(boot.means),pnorm)
qqnorm(boot.means)
qqline(boot.means)
se.boot = (sd(boot.means))*sqrt((N-n)/N)
normalCI = sample.mean + qt(df = 90, c(0.025, 0.975))*se.boot
normalCI


#grade distribution test
gradeTable = table(initial$grade)
gradeCount = c(0, 8, 52, 31)
dist = c(0.1, 0.4, 0.3,0.2  )
chisq.val = sum((gradeCount - expected)^2/expected)
q95.chisq = qchisq(0.95, 3)
chisq.val > q95.chisq

#-------Kevin Lin and James Yang Code--------
#--------R Code---------
videodata <- read.csv("~/Desktop/videodata.txt", sep="")
time<-videodata$time
student_number<-length(time)
who_dont_play= length(videodata[ which(videodata$time==0),]$time)
who_play=student_number- who_dont_play
fraction=who_play/student_number



#Question 2
#exclude the unknown value(99)
clean_data= videodata [which(videodata$freq<99),]
new_time= clean_data$time
freq= clean_data$freq
#change the frequency to quantity(1=7,2=1,3=0.23,4=0.08)
freq[c(5,6,7,8,10,11,12,16,22,23,25,26,27,34,35,44,49,58,61,66,70,72,76)]=0.08
freq[c(2,3,4,17,20,24,29,30,33,42,47,48,53,54,62,65,67,78)]=0.23
freq[c(9,13,21,31,46,50,52,56,69)]=7
freq[c(1,14,15,18,19,28,32,36,37,38,39,40,41,43,45,51,55,57,59,60,63,64,68,71,73,74,75)]=1
summary(new_time)
summary(freq)

#  histogram of time
hist(new_time, breaks=20,freq=T, col="violet", xlab="Time spent on playing weekly(hours)", ylab="Number of students", main="Number of hours played in the week prior to survey")
curve(dnorm(x, mean=mean(new_time), sd=sd(new_time)), add=TRUE, col="green")
#  histogram of freq
hist(freq, breaks=20, freq=T, col="blue", xlab="Time spent on playing weekly(hours)", ylab="Number of students",main="weekly frequency(hours)")
curve(dnorm(x, mean=mean(freq), sd=sd(freq)), add=TRUE, col="green")
#combine two histogram
hist(new_time, breaks=20,freq=T, col="violet", xlab="Time spent on playing weekly", ylab="Number of students",main="Compare of time spent weekly")
hist(freq, freq=T, col="blue",add=T)
box()

#Question 6
grade= videodata$grade
A=length(videodata[ which(videodata$grade==4),]$grade)
B=length(videodata[ which(videodata$grade==3),]$grade)
C=length(videodata[ which(videodata$grade==2),]$grade)
D=length(videodata[ which(videodata$grade==1),]$grade)
percent_of_A=A/length(grade)
percent_of_B=B/length(grade)
percent_of_C=C/length(grade)
percent_of_D=D/length(grade)

grade_defference<-matrix(c(20, 30, 40, 10, 34.1, 57.1, 8.8, 0, 70, 90, 78, 100),ncol=4,byrow=TRUE)
rownames(grade_defference)<-c("Target","Grade Expected","error percentage")
colnames(grade_defference)<-c("A","B","C","D or F")
grade_defference <- as.table(grade_defference)


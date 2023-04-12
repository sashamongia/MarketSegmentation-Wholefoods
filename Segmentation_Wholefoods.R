workdir <- "C:/Users/mwedel/Dropbox/Courses/BMSO758E/Projects/Project5"
setwd(workdir)
#install.packages("flexmix")
library("flexmix")
project <- read.csv("coffeemakers_long.csv")
attach(project)
str(project)
summary(project)
#Mixture MNL regression for 1-4 segments, with 3 runs for each
out1 = stepFlexmix( Choice ~ Philips + Krups + Capacity5 + Capacity10 + Price59 + Price79 + Filter + Grinder | Respondent,
model = FLXMRcondlogit(strata = ~ experiment),
data= project, control=list(verbose=0), k=1:4, nrep=3)
out1
# get model with smallest BIC
out2.1<-getModel(out1,"BIC")
summary(out2.1)
#Get the coefficients for each Segment
out2.2<-refit(out2.1)
summary(out2.2)
#Save the posterior probabilities
pr<-posterior(out2.1)
post1<-tapply(pr[,1],Respondent,mean)
post2<-tapply(pr[,2],Respondent,mean)
post=cbind(post1,post2)
#Calculate segment sizes and Entropy (Es)
pi=colMeans(post)
pi
#Calculate Entropy (Es)
Es=1-mean(-post*log(post))
Es
#Plots of posterior probabilities and coefficients
plot(out2.1)
plot(out2.2)
#Regression of posterior probabilities of Segment 2 on Urbanisation factor
urbaniz<-as.factor(tapply(urb,Respondent,mean))
levels(urbaniz)[1]="Rural"
levels(urbaniz)[2]="Urban"
summary(urbaniz)
out=lm(log(post2/post1)~urbaniz)
summary(out)
# plot of segment memberships against urbanization
plot(post2,urbaniz,type='h',
main='Segment 2 membership against Urbanization', xlab="Segment 2 Membership",col=urbaniz)
aggregate(cbind(post1,post2), list(as.factor(urbaniz)), FUN=mean)
data<-read.csv("/Users/A5coupe/Desktop/STA303/Assignment 2/bbw.csv")
detach(data)

#1
maturity=array(0,length(gestation))
MatSmoke=array(0,length(smoke))
for (i in 1:length(gestation))
{
  if (gestation[i]<259)
  {maturity[i]=1}
  else if (gestation[i]>293)
  {maturity[i]=3}
  else {maturity[i]=2}
}
for (i in 1:length(smoke))
{
  if (maturity[i]==1 & smoke[i]==1)
  {MatSmoke[i]="PreSmoke"}
  else if (maturity[i]==1 & smoke[i]==0)
  {MatSmoke[i]="PreNoSmoke"}
  else if (maturity[i]==2 & smoke[i]==1)
  {MatSmoke[i]="NorSmoke"}
  else if (maturity[i]==2 & smoke[i]==0)
  {MatSmoke[i]="NorNoSmoke"}
  else if (maturity[i]==3 & smoke[i]==1)
  {MatSmoke[i]="PostSmoke"}
  else if (maturity[i]==3 & smoke[i]==0)
  {MatSmoke[i]="PostNoSmoke"}
}

smoke<-data$bwt[data$smoke=="1"]
no_smoke<-data$bwt[data$smoke=="0"]
boxplot(smoke,no_smoke,main="8637 birth weight between smoked and not smoke",names=c("smoked","not smoke"),ylab="birth weight")


pre<-bwt[maturity==1]
normal<-bwt[maturity==2]
post<-bwt[maturity==3]
boxplot(pre,normal,post, main="8637 birth weight among 3 maturity levels", names=c("Pre","Normal","Post"),ylab="birth weight")

pre_smoke<-bwt[MatSmoke=="PreSmoke"]
pre_no_smoke<-bwt[MatSmoke=="PreNoSmoke"]
nor_smoke<-bwt[MatSmoke=="NorSmoke"]
nor_no_smoke<-bwt[MatSmoke=="NorNoSmoke"]
post_smoke<-bwt[MatSmoke=="PostSmoke"]
post_no_smoke<-bwt[MatSmoke=="PostNoSmoke"]
boxplot(pre_smoke,pre_no_smoke,nor_smoke,nor_no_smoke,post_smoke,post_no_smoke,names=c("pre_smoke","pre_no_smoke","nor_smoke","nor_no_smoke","post_smoke","post_no_smoke"),main="8637 birth weight among 6 groups",ylab="birth weight")
names=c("pre_smoke","pre_no_smoke","nor_smoke","nor_no_smoke","post_smoke","post_no_smoke")
#2
t.test(smoke,no_smoke,var.equal=TRUE)
#p-value = 3.672e-06 < 0.05, reject H0, different birth weight between

#3
summary(aov(bwt~factor(maturity)))
TukeyHSD(aov(bwt~factor(maturity)))
plot(TukeyHSD(aov(bwt~factor(maturity))))
#p-value < 2.2e-16 < 0.05, reject H0, the mean birth weight is different between 6 categories.
#4
summary(aov(bwt~MatSmoke))
TukeyHSD(aov(bwt~MatSmoke))
plot(TukeyHSD(aov(bwt~MatSmoke)))
#p-value < 2.2e-16 < 0.05, reject H0, so the mean birth weight is different between 6 categories 

#5 check assumption
lm_MatSmoke<-lm(bwt~MatSmoke)
plot(lm_MatSmoke,which=1,main="8637")
#From residuals vs fitted plot, residuals scattered randomly around zero, with a mean approximately 0.
bartlett.test(bwt~MatSmoke)
#p-value = 0.09627 > 0.05, accept H0, equal variance

plot(lm_MatSmoke,which=2,main="8637")
#normality satisfied
#independent: the babies and their mothers are chosen independently, and they don't have influence on others

#6
#a
#In Q4, we have six catogories, so there are 6 predictors.
#In two-way anova, there are 2 factors, smoke and maturity, so there are 2 predictors. Smoke 2 levels, maturity 3 levels

#b
no_na_data<-data[complete.cases(data),]
full<-lm(formula=bwt~factor(smoke)*factor(maturity),data=no_na_data)
reduced<-lm(formula=bwt~factor(smoke)+factor(maturity),data=no_na_data)
summary(anova(full,reduced))
anova(full)
#p-value=0.09317>0.05, do not reject H0, interaction is not significant.
#From Q4, the p-value of most combined groups is less rhan 0.05, and most confidence intervals do not contain 0
#So the interaction is statistically significant.

#7
#yes. Since three maturity levels has different number of babies, so the model is unbalanced design.
#It will affect constant variance assumption and variance estimate.
#We can consider the weighted lease squares regression and consider any evidence as exploratory.

#8
factor<-lm(formula=bwt~factor(smoke)+factor(maturity),data=no_na_data)
quantitative<-lm(formula=bwt~factor(smoke)+gestation,data=no_na_data)
#categorical variable to quantitative variable. 
#Quantitative variables are numeric. Categorical variables are descriptions of groups or things.
summary(anova(factor,quantitative))
#bwt=b0 + b1 Ismk + b2 gestation + e
#bwt=b0 + b1 Ismk + b2 Imtr1 + b3 Imtr2 + e

#9
#Factor 1: age - converting the childbearing age of mother to a factor with 4 levels:
# 1 if the childbearing age of mother is less than 25; 2 if 25-29, 3 if 30-39, 4 if 40 and more
#factor 2: drink - converting the drinking status of mother
# 1 if drink during pregenancy, 0 if not drink 


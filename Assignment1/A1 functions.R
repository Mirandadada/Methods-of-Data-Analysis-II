#Question1
data <-read.csv("/Users/A5coupe/Desktop/303/juries(1).csv")
dataS <-data$PERCENT[data$JUDGE=="SPOCKS"]
dataO <-data$PERCENT[data$JUDGE!="SPOCKS"]
boxplot(dataS, dataO, xlab="JUDGE", outline=FALSE, names=c("SPOCKS","OTHERS"), main="8637")


#Question2
data2 <-read.csv("/Users/A5coupe/Desktop/303/assign1data.csv")
dataM <-data2$height[data2$sex=="Male"]
dataF <-data2$height[data2$sex=="Female"]
boxplot(dataM, dataF, xlab="sex", ylab="height", names=c("Male","Female"), main="8637")

max(var(dataM),var(dataF))/min(var(dataM),var(dataF))

qqnorm(dataM, xlab="Theoretical Quantiles", ylab="Sample Quantiles", main="Male 8637")
qqline(dataM)
qqnorm(dataF, xlab="Theoretical Quantiles", ylab="Sample Quantiles", main="Female 8637")
qqline(dataF)

t.test(dataM,dataF)

subset<-data2[-37,]
subsetM <-subset$height[subset$sex=="Male"]
subsetF <-subset$height[subset$sex=="Female"]
boxplot(subsetM, subsetF, xlab="sex", ylab="height", names=c("Male","Female"), main="subset 8637")

max(var(subsetM),var(subsetF))/min(var(subsetM),var(subsetF))

qqnorm(subsetM, xlab="Theoretical Quantiles", ylab="Sample Quantiles", main="newMale 8637")
qqline(subsetM)
qqnorm(subsetF, xlab="Theoretical Quantiles", ylab="Sample Quantiles", main="newFemale 8637")
qqline(subsetF)

t.test(subsetM,subsetF)

model_old<-lm(height~sex, data=data2)
plot(model_old, which=c(4), main="8637")

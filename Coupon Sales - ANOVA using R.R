
install.packages("psych")
install.packages("car")


#Step 1.2(Mandatory) Invoke these packages using the Library function


####Coupon and Instore Promotion Data
df<-read.csv(file.choose())
show(df)
attach(df)
str(df)

#Step 2: Clearly identify the factors in the data
df$Instorepromotion<-factor(df$Instorepromotion, labels=c("1","2","3"))
show(Instorepromotion)
df$Couponlevel<-factor(df$Couponlevel, labels=c("1","2"))
show(Couponlevel)
#Checking if the data types are getting recognized appropriately after treatment of factors
str(df)

## Summary Statistics
###Calculate the means for promotion and coupon
##tapply = Table Apply

tapply(df$Sales,list(df$Instorepromotion, df$Couponlevel), mean)

##Note : High sales and high coupon gives relatively higher sales values
## As the intensity of promotion and coupons are reduced, sales reduces accordingly


###Create a plot to identify the interaction effects between promotion & coupon

interaction.plot(df$Instorepromotion,df$Couponlevel,df$Sales)

##Both lines are almost parallel hence we can say interaction effect is negligible

## Test for normality for all the 3 groups in promotion

cat("Normality p-values by Factor Place: ")
for (i in unique(factor(df$Instorepromotion))){
  cat(shapiro.test(df[df$Instorepromotion==i, ]$Sales)$p.value," ")
}

####P values are greater than 0.05, hence we do not reject the null hypothesis

####If for loop is confusing, use single test as follows for each promotion and coupon levels.
shapiro.test(df[df$Instorepromotion==1,]$Sales)$p.value

###Run the normality assumption test for both groups in coupon

cat("Normality p-values by Factor Place: ")
for (i in unique(factor(df$Couponlevel))){
  cat(shapiro.test(df[df$Couponlevel==i, ]$Sales)$p.value," ")
}
####P values are greater than 0.05, hence we do not reject the null hypothesis


###Create a qqplot to check the normality of entire dataset visually
### pch and cex are just numbers to decide on size and shape of the points
qqnorm(df$Sales, pch=19, cex=0.9)
qqline(df$Sales, col = 'red')
### Theoretical quantiles is the z scores and if it is <3 we accept
###From the image, we can see the entire dataset is normally distributed
### if the data are placed close to the line then the data is normaly distributed

######## Levene's test for variance

## Test for homogeneity of variance

####Testing variance visually using a boxplot
boxplot(Sales~Instorepromotion)
boxplot(Sales~Couponlevel)

###From the boxplot, we see that 2,3 have almost same variance .


####Check for variances in groups using Levene's test

leveneTest(df$Sales~df$Instorepromotion)
leveneTest(df$Sales~df$Couponlevel)


####As all the p values are greater than 0.05, we do not reject the null hypothesis

##################################################################################
####ANOVA based on promotion

aov1 <- aov(df$Sales~df$Instorepromotion)

summary(aov1)

##p value is less than 0.05, hence we reject the null hypothesis

TukeyHSD(aov1)
plot(TukeyHSD(aov1))

###We reject the null hypothesis as all the intervals do not contain zero within them

###Business Intuiton
###High, medium & low are boosting sales in some manner
###Jump from low to high gives a sales boost of 4.6
###Jump from medium to high gives a sales boost of 2.1
###If the costs of doing a high promotion are less than the above boosted sales values
### We can conclude saying that all stores can run the high promotion for better sales

###However, if the costs of doing high promotion are more than the boosted sales figures
### We can actually drop the high promotion stores to medium or even low




##################################################################################
##################################################################################
####ANOVA based on coupon

# Without Interaction
aov1 <- aov(df$Sales~df$Couponlevel+df$Instorepromotion)

summary(aov1)


# With Interaction
aov1 <- aov(df$Sales~df$Couponlevel*df$Instorepromotion)

summary(aov1)


####p value is less than 0.05, hence we reject the null hypothesis

TukeyHSD(aov1)
plot(TukeyHSD(aov1))

###Since the intervals do not contain zero, it means there is a significant difference
###In Business terms, I can offer higher coupons in all stores if my net profit increases
###We would need to calculate the net increase in profit using the sales boost of 2.666667



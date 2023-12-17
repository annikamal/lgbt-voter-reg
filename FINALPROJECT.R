## Final project
## Name: Annika Malhotra

## setting working directory
setwd("C:/Users/malho/OneDrive/Desktop/R")

## viewing the database
ces2020 <- read.csv("CES2020.csv")
View(ces2020)
nrow(ces2020)

## variable names:
gender
trans
sexuality
votereg

lgbtvotereg <- subset(ces2020, select=c(votereg, sexuality, gender, trans))

## isolating all my variables into a subset

table(lgbtvotereg$votereg)

## clarifying voter registration status

lgbtvotereg$reg <- NA

lgbtvotereg$reg[lgbtvotereg$votereg > 1] <- 0 #not registered
lgbtvotereg$reg[lgbtvotereg$votereg == 1] <- 1 #registered


transvoteregmean <- mean(lgbtvotereg$reg[lgbtvotereg$trans == 1], na.rm = T) 
## mean of people who are trans who also registered to vote. 84%.

cisvoteregmean <- mean(lgbtvotereg$reg[lgbtvotereg$trans != 1], na.rm = T)
## mean of people who are not trans who registered to vote. about 89%.

## isolating registration status for just transgender people
yestrans <- subset(lgbtvotereg, trans == 1)
table(lgbtvotereg$trans)
mean(yestrans$reg, na.rm=T) 

## one other way to find a mean of people who were registered to vote who are also trans, just to confirm

test <- t.test(lgbtvotereg$reg[lgbtvotereg$trans == 1], lgbtvotereg$reg[lgbtvotereg$trans != 1])

## t-test to determine the difference in means --
## btwn trans voter registration vs non-trans voter registration behavior

test

## p-value is less than 0.05. this determines that there is a statistically significant relationship!


lgbtvotereg$lgbt <- NA
lgbtvotereg$lgbt[lgbtvotereg$trans !=1 | lgbtvotereg$sexuality == 1] <- 0 #is not lgbt
lgbtvotereg$lgbt[lgbtvotereg$trans ==1 | lgbtvotereg$sexuality > 1] <- 1 # is lgb and/or trans


cishetvoteregmean <- mean(lgbtvotereg$reg[lgbtvotereg$lgbt != 1], na.rm = T)

## mean for non-lgbt+ people who registered is 89.5%

## isolating people who identify as lgbt to confirm mean numbers

yeslgbt <- subset(lgbtvotereg, lgbt == 1)

lgbtvoteregmean <- mean(yeslgbt$reg, na.rm=T)

lgbtvoteregmean

# the result is 84% mean.

table(lgbtvotereg$lgbt)

## heterosexual/cisgender people: 51,724
## lgbtq+ people: 9,266

testlgbt <- t.test(lgbtvotereg$reg[lgbtvotereg$lgbt >= 1], lgbtvotereg$reg[lgbtvotereg$lgbt == 0])

## people who are not lgbt who registered to vote vs people who identify as lgbt who registered to vote

testlgbt

## the p-value again is less than 0.05. this means there is a statistically significant relationship

## create barplot -> lgbtq+ vs straight/cis people behavior with voter registration.

valuesbar <- c(cishetvoteregmean, lgbtvoteregmean)

namesbar <- c("cis/hetero voter registrants", "lgbt+ voter registrants" )

barplot(ylim =  c(0,1),
        xlim = c(0, 3),
        valuesbar,
        names=namesbar,
        cex.names = .6,
        main="LGBT+ vs. non-LGBT+ voter registration behavior",
        ylab="Frequency percentage of voter registration",
        cex.lab = .7, 
        col=c("purple", "lightyellow"))


## other barplot with isolating just transgender people vs not trans (regardless of sexuality)

valuesbar2 <- c(cisvoteregmean, transvoteregmean)

namesbar2 <- c("non trans voter registrants", "trans registrants" )

barplot(ylim =  c(0,1),
        xlim = c(0, 3),
        valuesbar2,
        names=namesbar2,
        cex.names = .6,
        main="trans vs. non-trans voter registration behavior",
        ylab="Frequency percentage of voter registration",
        cex.lab = .7, 
        col=c("pink", "lightgreen"))

## the end!
## thank you :)



library(readstata13)
library(lmtest)
library(sandwich)

pubexp <- read.dta13("pubexp.dta")

head(pubexp)
pubexp$y <- pubexp$ee/pubexp$p
pubexp$x <- pubexp$gdp/pubexp$p

pubLM <- lm(y~x, data=pubexp)
plot(pubexp$x,pubLM$residuals)

bptest(pubLM, ~ I(x^2), data=pubexp)

summary(pubLM)
coeftest(pubLM, vcov.=vcovHC)

c(0.073173 - 1.96*0.005179,0.073173 + 1.96*0.005179)
c(0.0731732 - 1.96*0.0066033,0.0731732 + 1.96*0.0066033)

wei <- pubexp$x^(-1/2)
pubLM2 <- lm(y~x, data=pubexp, weights = wei)
summary(pubLM2)

c(0.071377 - 1.96*0.004651,0.071377 + 1.96*0.004651)
plot(pubexp$x,pubLM2$residuals)
bptest(pubLM2, ~ I(x^2), data=pubexp)

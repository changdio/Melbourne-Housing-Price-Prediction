library(tidyverse)
library(ggplot2)
library(psych)


df <- read.csv("melb_data.csv")

# Location vs. features influence
df <- df %>% filter(Suburb == "Brunswick"|Suburb == "Carlton"|
                      Suburb == "South Yarra" | Suburb == "Fitzroy")
# units only

df <- df %>% filter(Type == "u")

# Omit na
df <- df[!is.na(df$BuildingArea),]
df <- df[!is.na(df$Car),]

# any outliers?

ggplot(df, aes(Price)) + geom_histogram(bins=25,color="black")
describe(df$Price)

ggplot(df,aes(x=BuildingArea,y=Price)) + geom_point()

# Budget for home 1.2 mil.

df <- df %>% filter(Price <=1200000)
p1 <- ggplot(df, aes(Price)) + geom_histogram(bins = 20,color="black")
p1
describe(df$Price)

df <- df %>% filter(BuildingArea < 150 & BuildingArea > 31)
ggplot(df,aes(x=BuildingArea,y=Price)) + geom_point()


df$Suburb <- factor(df$Suburb, levels=c("Brunswick","Carlton","South Yarra","Fitzroy"))
levels(df$Suburb)

brunswick.fitzroy <- c(1,0,0,0)
carlton.fitzroy <- c(0,1,0,0)
southyarra.fitzroy <- c(0,0,1,0)
contrasts(df$Suburb) <- cbind(brunswick.fitzroy,carlton.fitzroy,southyarra.fitzroy)
contrasts(df$Suburb)

model <- lm(Price ~ Suburb + Car + BuildingArea, data=df)
summary(model)

plot(model)

df$standardized.residuals <- rstandard(model)
View(subset(df, standardized.residuals < -1.96 | standardized.residuals > 1.96))

library(car)
durbinWatsonTest(model)

library(rms)

vif(model)
1/vif(model)
mean(vif(model))

# Cook's Distance 
df$cooks <- cooks.distance(model)
plot(sort(df$cooks, decreasing=TRUE))
max(df$cooks)

# MODEL 2
model2 <- lm(Price ~ Suburb + BuildingArea + Rooms + Bedroom2 + Bathroom + Car, data=df)
summary(model2)

df$standardized.residuals2 <- rstandard(model2)
View(subset(df, standardized.residuals2 < -1.96 | standardized.residuals2 > 1.96))

durbinWatsonTest(model2)

vif(model2)
1/vif(model2)
mean(vif(model2))

# Cook's Distance 
df$cooks2 <- cooks.distance(model2)
plot(sort(df$cooks2, decreasing=TRUE))
max(df$cooks2)

# ANOVA
anova(model,model2)


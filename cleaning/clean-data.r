# store csv in "fight" variable
fight <- read.csv("FightData.csv")

# initial look at data
View(fight)
str(fight)
summary(fight)

#Create a factor for date of birth
fight$DOB.x <- as.Date(fight$DOB.x, format = "%m/%d/%Y")
head(fight$DOB.x)

#To get the age, subtract date of birth from current date
fight$age.x <- round(Sys.Date() - fight$DOB.x)
head(fight$age.x)

#Create a factor for the year of birth
fight$DOB.y <- as.Date(fight$DOB.y, format = "%m/%d/%Y")
head(fight$DOB.y)

#To get the age in terms of years for each fighter, subtract the year from current date
fight$age.y <- round(Sys.Date() - fight$DOB.y)
head(fight$age.y)

#Convert Height into Mean for NA values
fight$Height.x <- ifelse(is.na(fight$Height.x), mean(fight$Height.x, na.rm=TRUE), fight$Height.x)
fight$Height.y <- ifelse(is.na(fight$Height.y), mean(fight$Height.y, na.rm=TRUE), fight$Height.y)

#Convert Weight into Median for NA values
fight$Weight.x <- ifelse(is.na(fight$Weight.x), median(fight$Weight.x, na.rm=TRUE), fight$Weight.x)
fight$Weight.y <- ifelse(is.na(fight$Weight.y), median(fight$Weight.y, na.rm=TRUE), fight$Weight.y)

#Convert Reach into Mean for NA values
fight$Reach.x <- ifelse(is.na(fight$Reach.x), mean(fight$Reach.x, na.rm=TRUE), fight$Reach.x)
fight$Reach.y <- ifelse(is.na(fight$Reach.y), mean(fight$Reach.y, na.rm=TRUE), fight$Reach.y)

#Convert Age into Median for NA values
fight$age.x <- ifelse(is.na(fight$age.x), median(fight$age.x, na.rm=TRUE), fight$age.x)
fight$age.y <- ifelse(is.na(fight$age.y), median(fight$age.y, na.rm=TRUE), fight$age.y)

#Convert SLpM into Median for 0 values
fight$SLpM.x <- ifelse(fight$SLpM.x == 0, median(fight$SLpM.x), fight$SLpM.x)
fight$SLpM.y <- ifelse(fight$SLpM.y == 0, median(fight$SLpM.y), fight$SLpM.y)

#Convert Str..Acc into Median for 0 values
fight$Str..Acc..x <- ifelse(fight$Str..Acc..x == 0, median(fight$Str..Acc..x), fight$Str..Acc..x)
fight$Str..Acc..y <- ifelse(fight$Str..Acc..y == 0, median(fight$Str..Acc..y), fight$Str..Acc..y)

##Convert SApM into Median for 0 values
fight$SApM.x <- ifelse(fight$SApM.x == 0, median(fight$SApM.x), fight$SApM.x)
fight$SApM.y <- ifelse(fight$SApM.y == 0, median(fight$SApM.y), fight$SApM.y)

##Convert Str..Def into Median for 0 values
fight$Str..Def..x <- ifelse(fight$Str..Def..x == 0, median(fight$Str..Def..x), fight$Str..Def..x)
fight$Str..Def..y <- ifelse(fight$Str..Def..y == 0, median(fight$Str..Def..y), fight$Str..Def..y)

#Convert TD.Avg.. into Median for 0 values 
fight$TD.Avg..x <- ifelse(fight$TD.Avg..x == 0, median(fight$TD.Avg..x), fight$TD.Avg..x)
fight$TD.Avg..y <- ifelse(fight$TD.Avg..y == 0, median(fight$TD.Avg..y), fight$TD.Avg..y)

#Covert TD.Acc.. into Median for 0 values 
fight$TD.Acc..x <- ifelse(fight$TD.Acc..x == 0, median(fight$TD.Acc..x), fight$TD.Acc..x)
fight$TD.Acc..y <- ifelse(fight$TD.Acc..y == 0, median(fight$TD.Acc..y), fight$TD.Acc..y)

#Covert TD.Def.. into Median for 0 values 
fight$TD.Def..x <- ifelse(fight$TD.Def..x == 0, median(fight$TD.Def..x), fight$TD.Def..x)
fight$TD.Def..y <- ifelse(fight$TD.Def..y == 0, median(fight$TD.Def..y), fight$TD.Def..y)

#Covert Sub..Avg into Median for 0 values 
fight$Sub..Avg..x <- ifelse(fight$Sub..Avg..x == 0, median(fight$Sub..Avg..x), fight$Sub..Avg..x)
fight$Sub..Avg..y <- ifelse(fight$Sub..Avg..y == 0, median(fight$Sub..Avg..y), fight$Sub..Avg..y)

#Make DOB.x & DOB.y values null
fight$DOB.x <- NULL
fight$DOB.y <- NULL

#Convert Stance.x and Stance.y into factor

fight$Stance.x2 <- as.character(fight$Stance.x)
fight$Stance.x2 <- ifelse(is.na(fight$Stance.x2), "Others", fight$Stance.x2)

fight$Stance.x2 <- ifelse(fight$Stance.x2 == "Orthodox", "Orthodox", ifelse(fight$Stance.x2 == "Southpaw", "Southpaw", "Other"))

fight$Stance.x2 <- as.factor(fight$Stance.x2)


fight$Stance.y2 <- as.character(fight$Stance.y)
fight$Stance.y2 <- ifelse(is.na(fight$Stance.y2), "Others", fight$Stance.y2)

fight$Stance.y2 <- ifelse(fight$Stance.y2 == "Orthodox", "Orthodox", ifelse(fight$Stance.y2 == "Southpaw", "Southpaw", "Other"))

fight$Stance.y2 <- as.factor(fight$Stance.y2)

#Make Stance.x & Stance.y values null
fight$Stance.x <- NULL
fight$Stance.y <- NULL

#Create difference variable for weight, height, and recah
fight$Height.difference <- fight$Height.x - fight$Height.y
fight$Weight.difference <- fight$Weight.x - fight$Weight.y
fight$Reach.difference <- fight$Reach.x - fight$Reach.y

#Create ratio variable for weight, height, and reach
fight$height_reach.ratio.x <- fight$Height.x/fight$Reach.x 
fight$height_reach.ratio.y <- fight$Height.y/fight$Reach.y 
fight$height_weight.ratio.x <- fight$Height.x/fight$Weight.x
fight$height_weight.ratio.y <- fight$Height.y/fight$Weight.y

#Make Height, Weight and Reach values null
fight$Height.x <- NULL
fight$Height.y <- NULL
fight$Weight.x <- NULL
fight$Weight.y <- NULL
fight$Reach.x <- NULL
fight$Reach.y <- NULL

#normalize data
# normalize function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
#normalizing non-factor variables (all variables except c(1,20,21))
fight_norm <- fight
fight_norm[c(2:19,22:28)] <- as.data.frame(lapply(fight_norm[c(2:19,22:28)], normalize))

#comparing original vs. normalized data
str(fight)
str(fight_norm)
```
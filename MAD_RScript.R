#MAD: Aggression model for quantifying territorial aggression in CD1 male mice
#April 2021

#Please cite: "Quantitative standardization of resident mouse behavior for studies of aggression and social defeat"

#Install the packages with the functions needed for the analysis
#This only needs to be done one time for your computer

#Delete the leading "#" symbol to run
#install.packages("lavaan") 
#Delete the leading "#" symbol to run
#install.packages("car")

#Define file path for R to import and export files
#Note that all of your files should be stored here
setwd("C:/Some/File/Path")

#Load libraries 
library(lavaan) #latent variable analysis package, performs CFA
library(car) #contains variable recoding function

#Read file with provided RI data from experimentally naive mice
control_vars <- read.csv("./MAD_ScreeningData_CFA_CONTROLSONLY.csv", header = TRUE)

#Read file with your own RI lab data
#Please note that the variable names have to be the same, codebook provided
myData <- read.csv("./name_of_file.csv", header = TRUE)

#Recode latency into a categorical variable with 6 levels
#This generates 3 new latency variables (L1_v2, L2_v2, L3_v2) for the control animals and your RI data

#Latency Day 1
control_vars$L1_v2 <- recode(control_vars$L1, "0:1.99=6; 2:20.99=5; 21:40.99=4; 41:90.99=3; 91:179.99=2; 180:205=1")
myData$L1_v2 <- recode(myData$L1, "0:1.99=6; 2:20.99=5; 21:40.99=4; 41:90.99=3; 91:179.99=2; 180:205=1")

#Latency Day 2
control_vars$L2_v2 <- recode(control_vars$L2, "0:1.99=6; 2:20.99=5; 21:40.99=4; 41:90.99=3; 91:179.99=2; 180:205=1")
myData$L2_v2 <- recode(myData$L2, "0:1.99=6; 2:20.99=5; 21:40.99=4; 41:90.99=3; 91:179.99=2; 180:205=1")

#Latency Day 3
control_vars$L3_v2 <- recode(control_vars$L3, "0:1.99=6; 2:20.99=5; 21:40.99=4; 41:90.99=3; 91:179.99=2; 180:205=1")
myData$L3_v2 <- recode(myData$L3, "0:1.99=6; 2:20.99=5; 21:40.99=4; 41:90.99=3; 91:179.99=2; 180:205=1")

###### CFA #####

#MAD Model specification 
agg.model <- 'day1 =~ B1 + L1_v2 + D1
day2 =~ B2 + L2_v2 + D2
day3 =~ B3 + L3_v2 + D3
agg =~ day1 + day2 + day3

B1 ~~ B2
B2 ~~ B3
B1 ~~ B3

D1 ~~ D2
D2 ~~ D3
D1 ~~ D3

L2_v2 ~~ L1_v2

L3_v2 ~~ 0*L3_v2

L2_v2 ~~ 0*L3_v2

'

#CFA Model Estimation
MAD1 <- cfa(agg.model, data=control_vars, 
                std.lv=TRUE, estimator = "mlr",
                missing="fiml")

#Apply MAD to your data
new.data_MAD <- lavPredict(MAD1, type = "lv", newdata = myData, 
                           method = "EBM", se = "none", acov = "none", 
                           label = TRUE, fsm = FALSE)

#Generate a column of aggression scores for your data
myData$aggScore <- new.data_MAD[,4]

#Save aggression scores
write.csv(myData, "./aggScores.csv")

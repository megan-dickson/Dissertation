Data <- read.csv("~/Desktop/Data.csv", header = TRUE)
Data


##Running Chi2 and t-tests
chisq.test(Data$interp, Data$Helminths) 

t.test(Data$TB1[Data$Helminths == 0], Data$TB1[Data$Helminths == 1])

t.test(Data$TB2[Data$Helminths == 0], Data$TB2[Data$Helminths == 1])

t.test(Data$TB1[Data$interp == 0], Data$TB1[Data$interp == 1])

t.test(Data$TB2[Data$interp == 0], Data$TB2[Data$interp == 1])


##making viusals of some aspects of the data

BMI_histo <- hist(Data$BMI, xlab = "Body Mass Index (BMI)",main = "Histogram of BMI's")

BMI_bar <- plot(Data$Sex, Data$BMI, ylab = "BMI", xlab = "Sex", main = "BMI Across Gender")

TB1_sex <- plot(Data$Sex, Data$TB1, ylab = "TB1", xlab = "Sex", main = "TB1 Results Across Gender")

TB2_sex <- plot(Data$Sex, Data$TB2, ylab = "TB2", xlab = "Sex", main = "TB2 Results Across Gender")

Interp_sex <- plot(Data$Sex, Data$interp, ylab = "Interpretation", xlab = "Sex", main = "Overall Results Across Gender")

MClan_TB1 <- plot(Data$Mother.s.Clan, Data$TB1, ylab = "TB1", xlab = "Mother's Clan", main = "Mother's Clan TB1 Results")

MClan_TB2 <- plot(Data$Mother.s.Clan, Data$TB2, ylab = "TB2", xlab = "Mother's Clan", main = "Mother's Clan TB2 Results")

FClan_TB1 <- plot(Data$Father.s.Clan, Data$TB1, ylab = "TB1", xlab = "Father's Clan", main = "Father's Clan TB1 Results")

FClan_TB2 <- plot(Data$Father.s.Clan, Data$TB1, ylab = "TB1", xlab = "Father's Clan", main = "Father's Clan TB1 Results")




library(ggplot2)

ggplot(Data, aes(x=Sex, y=BMI)) +
  geom_boxplot() +
  facet_grid(. ~ interp) 


ggplot(Data, aes(x=Mother.s.Clan, y=TB1)) +
  geom_boxplot() 
ggplot(Data, aes(x=Mother.s.Clan, y=TB2)) +
  geom_boxplot() 


ggplot(Data, aes(x=Father.s.Clan, y=TB1)) +
  geom_boxplot() 
ggplot(Data, aes(x=Father.s.Clan, y=TB2)) +
  geom_boxplot() 



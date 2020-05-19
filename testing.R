## trying to make tables in publication style
Data <- read.csv("~/Desktop/Data.csv", header = TRUE)


library(expss)
library(tidyverse)

##selecting the columns we want to work with
new_data <- data.frame(Data$Sex, Data$DOB, Data$Helminths, Data$BMI, 
                       Data$Current.Residence, Data$Father.s.Clan, Data$Mother.s.Clan,
                       Data$Outcome) %>% 
  rename(Sex = Data.Sex, Age = Data.DOB, Helminths = Data.Helminths, BMI = Data.BMI, 
         Location = Data.Current.Residence, "Father's Clan" = Data.Father.s.Clan,
         "Mother's Clan" = Data.Mother.s.Clan, Outcome = Data.Outcome) %>%
  na.omit(new_data)
new_data

##relabeling certain things
new_data = apply_labels(new_data,
                        Sex = "Sex",
                        Age = "Age",
                        Helminths = "Helminths",
                        Helminths = c("Detected" = 1, "Absent" = 0),
                        BMI = "BMI",
                        Location = "Current Residence",
                        "Father's Clan" = "Father's Clan",
                        "Mother's Clan" = "Mother's Clan",
                        Outcome = "Clinical Interpretation",
                        Outcome = c("Positive" = "POSITIVE", "Negative" = "NEGATIVE", 
                                    "Indeterminate"="INDETERMINATE"))

##table showing gender and outcome
cro(new_data$Sex,new_data$Outcome)                    
##helminth and outcome
cro(new_data$Helminths,new_data$Outcome)   

## helminth and outcome across gender
cro_cpct(new_data$Sex, list(total(),new_data$Helminths, new_data$Outcome))

##messing around

new_data %>% 
  tab_cells(BMI, Age) %>%
  tab_cols(total(), Outcome, Helminths) %>% 
  tab_stat_mean_sd_n() %>%
  tab_pivot() %>% 
  set_caption("Table with summary statistics and significance marks.")


new_data %>% 
  tab_cells(Age) %>%
  tab_cols(total(), Helminths) %>% 
  tab_stat_mean_sd_n() %>%
  tab_pivot() %>% 
  set_caption("Table with summary statistics and significance marks.")


data(new_data)
counts = table(new_data$Age, new_data$Helminths)
props = prop.table(counts)
compare_proportions(props[,1], props[,2],
                    colSums(counts)[1], colSums(counts)[1])


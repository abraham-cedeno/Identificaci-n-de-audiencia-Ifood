## ----setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE, results='hide'----------------------------
library(tidyverse)
library(stringr)
library(ggpubr)
library(knitr)



## ----------------------------------------------------------------------------
dir1 <- "~"
dir2 <- "Desktop"
dir3 <- "AC"
dir4 <- "Useful"
dir5 <- "Carrer"
dir6 <- "Skills "
dir7 <- "3. Skills para trabajo"
dir8 <- "10. R data science, statistics, machine learning"
dir9 <- "Portfolio analysis" 
dir10 <- "3. Marketing Analytics" 
file_name  <- "Data"
PSDS_PATH <- file.path(dir1, dir2, dir3, dir4, dir5, dir6, dir7, dir8, dir9, dir10, file_name)


## ---- results='hide', message=FALSE, warning=FALSE---------------------------
Data <- read_csv(file.path(PSDS_PATH, 'ml_project1_data.csv'))


## ---- results='hide', message=FALSE, warning=FALSE---------------------------
Data<- arrange(Data,ID)


## ----------------------------------------------------------------------------
Data <- Data %>%
  mutate(Age = 2014 - Year_Birth)


## ----------------------------------------------------------------------------
str(Data)


## ----------------------------------------------------------------------------
#Unique categories in each categorical column
unique(Data$Education)
unique(Data$Marital_Status)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
summary_features <- data.frame(
  Group = c("ID", "Age", "Income", "Kidhome", "Teenhome", "Dt_Customer", "Recency", "MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds","NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases","NumWebVisitsMonth","AcceptedCpm1", "AcceptedCpm2", "AcceptedCpm3", "AcceptedCpm4", "AcceptedCpm5", "Complain", "Z_CostContact", "Z_Revenue", "Response","Education", "Marital Status"),
    
  Range = c(paste(range(Data$ID), collapse = "-"),
            paste(range(Data$Age), collapse = "-"),
            paste(range(Data$Income,na.rm = TRUE), collapse = "-"),
            paste(range(Data$Kidhome), collapse = "-"),
            paste(range(Data$Teenhome), collapse = "-"),
            paste(range(Data$Dt_Customer), collapse = " to "),   
            paste(range(Data$Recency), collapse = "-"),
            paste(range(Data$MntWines), collapse = "-"),
            paste(range(Data$MntFruits), collapse = "-"),

            paste(range(Data$MntMeatProducts), collapse = "-"),
            paste(range(Data$MntFishProducts), collapse = "-"),
            paste(range(Data$MntSweetProducts), collapse = "-"),
            paste(range(Data$MntGoldProds), collapse = "-"),
            paste(range(Data$NumDealsPurchases), collapse = "-"),
            paste(range(Data$NumWebPurchases), collapse = "-"),
            paste(range(Data$NumCatalogPurchases), collapse = "-"),

            paste(range(Data$NumStorePurchases), collapse = "-"),
            paste(range(Data$NumWebVisitsMonth), collapse = "-"),
            paste(range(Data$AcceptedCmp1), collapse = "-"),
            paste(range(Data$AcceptedCmp2), collapse = "-"),
            paste(range(Data$AcceptedCmp3), collapse = "-"),
            paste(range(Data$AcceptedCmp4), collapse = "-"),
            paste(range(Data$AcceptedCmp5), collapse = "-"),

            paste(range(Data$Complain), collapse = "-"),
            paste(range(Data$Z_CostContact), collapse = "-"),
            paste(range(Data$Z_Revenue), collapse = "-"),
            paste(range(Data$Response), collapse = "-"), "-", "-"),
  
     Data_Type = c("Categorical nominal","Numeric discrete","Numeric continuous", "Numeric discrete", "Numeric discrete","Numeric discrete", "Numeric discrete","Numeric continuous","Numeric continuous","Numeric continuous","Numeric continuous","Numeric continuous","Numeric continuous","Numeric discrete","Numeric discrete","Numeric discrete","Numeric discrete","Numeric discrete","Categorical nominal","Categorical nominal","Categorical nominal","Categorical nominal","Categorical nominal","Categorical nominal", "Not sure", "Not sure","Categorical nominal","Categorical nominal","Categorical nominal")

)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
kable(summary_features, caption = "Exploring the features characteristics")


## ----------------------------------------------------------------------------
duplicates <- duplicated(Data$ID)
num_true <- sum(duplicates)
print(num_true)
remove(duplicates,num_true)


## ---- results='hide', message=FALSE, warning=FALSE, echo=FALSE---------------
Unique_Data<- unique (Data[ , c("ID", "Year_Birth", "Income", "Kidhome", "Teenhome", "Dt_Customer", "Recency", "MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds","NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases","NumWebVisitsMonth","AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "Complain", "Z_CostContact", "Z_Revenue", "Response", "Education", "Marital_Status") ] )
remove(Unique_Data)


## ----------------------------------------------------------------------------
table(Data$Response)


## ---- results='hide',echo=FALSE, message=FALSE, warning=FALSE----------------
#ggplot(Data, aes(x = Education, fill = factor(Response))) +
 # geom_bar(position= "dodge", alpha =1, width = 0.75, color="white") +
  #labs(title = "Education level per Response", y= "Count per Response", x= "Categories")


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
Education <- function(True_False) {Data %>%
  filter(Response == True_False) %>%
  group_by(Education) %>% # Variable to be transformed
  count(Education)  %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) } 


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
Education0 <- Education(0)
Education1 <- Education(1)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
# I've created a plot to visualize day preference for casual riders in a pie chart
ggplot(Education0, aes(x ="", y = perc, fill = Education)) +
  geom_col(color="white") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar(theta = "y") +
  labs(title = "Education level for 0 response", y= "%", x= "%")

ggplot(Education1, aes(x ="", y = perc, fill = Education)) +
  geom_col(color="white") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar(theta = "y") +
  labs(title = "Education level for 1 response", y= "%", x= "%")


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
#ggplot(Data, aes(x = Marital_Status, fill = factor(Response))) +
 # geom_bar(position= "dodge", alpha =1, width = 0.75, color="white") +
  #labs(title = "Marital_Status per Response", y= "Count per Response", x= "Categories")


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
Marital_Status <- function(True_False) {Data %>%
  filter(Response == True_False) %>%
  group_by(Marital_Status) %>% # Variable to be transformed
  count(Marital_Status)  %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) } 


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
Marital_Status0 <- Marital_Status(0)
# Extract the first 5 characters
Marital_Status0$labels <- paste0(substr(Marital_Status0$labels, start = 1, stop = 4), substr(Marital_Status0$labels, start = 7, stop = 7))  
# Replace the 6th character with the 7th character

Marital_Status1 <- Marital_Status(1)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
# I've created a plot to visualize day preference for casual riders in a pie chart
ggplot(Marital_Status0, aes(x ="", y = perc, fill = Marital_Status)) +
  geom_col(color="white") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar(theta = "y") +
  labs(title = "Marital Status for 0 response", y= "%", x= "%")

ggplot(Marital_Status1, aes(x ="", y = perc, fill = Marital_Status)) +
  geom_col(color="white") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar(theta = "y") +
  labs(title = "Marital Status for 1 response", y= "%", x= "%")


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x = Complain, fill = factor(Response))) +
  geom_bar(position= "dodge", alpha =1, width = 0.75, color="white") +
  labs(title = "Response of people who complain vs people who didn't", y= "Count per Response", x= "Accepted or nor Campaign 1")


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x = AcceptedCmp1, fill = factor(Response))) +
  geom_bar(position= "dodge", alpha =1, width = 0.75, color="white") +
  labs(title = "People behavior to Campaign 1 vs Response", y= "Count per Response", x= "Accepted or nor Campaign 1")

## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x = AcceptedCmp2, fill = factor(Response))) +
  geom_bar(position= "dodge", alpha =1, width = 0.75, color="white") +
  labs(title = "People behavior to Campaign 2 vs Response", y= "Count per Response", x= "Accepted or nor Campaign 2")

ggplot(Data, aes(x = AcceptedCmp3, fill = factor(Response))) +
  geom_bar(position= "dodge", alpha =1, width = 0.75, color="white") +
  labs(title = "People behavior to Campaign 3 vs Response", y= "Count per Response", x= "Accepted or nor Campaign 3")

## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x = AcceptedCmp4, fill = factor(Response))) +
  geom_bar(position= "dodge", alpha =1, width = 0.75, color="white") +
  labs(title = "People behavior to Campaign 4 vs Response", y= "Count per Response", x= "Accepted or nor Campaign 4")

## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x = AcceptedCmp5, fill = factor(Response))) +
  geom_bar(position= "dodge", alpha =1, width = 0.75, color="white") +
  labs(title = "People behavior to Campaign 5 vs Response", y= "Count per Response", x= "Accepted or nor Campaign 5")


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
#ggplot(Data, aes(x = factor(Kidhome), fill = factor(Response))) +
 # geom_bar(position= "dodge", alpha =1, width = 0.75, color="white") +
 # labs(title = "Kidhome per Response", y= "Count per Response", x= "Categories")


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
Kidhome <- function(True_False) {Data %>%
  filter(Response == True_False) %>%
  group_by(Kidhome) %>% # Variable to be transformed
  count(Kidhome)  %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) } 


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
Kidhome0 <- Kidhome(0)
Kidhome1 <- Kidhome(1)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
# I've created a plot to visualize day preference for casual riders in a pie chart
ggplot(Kidhome0, aes(x ="", y = perc, fill = factor(Kidhome))) +
  geom_col(color="white") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar(theta = "y") +
  labs(title = "Kids home for 0 response", y= "%", x= "%")

ggplot(Kidhome1, aes(x ="", y = perc, fill = factor(Kidhome))) +
  geom_col(color="white") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar(theta = "y") +
  labs(title = "Kids home for 1 response", y= "%", x= "%")


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
TeensHome <- function(True_False) {Data %>%
  filter(Response == True_False) %>%
  group_by(Teenhome) %>% # Variable to be transformed
  count(Teenhome)  %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) } 


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
TeensHome0 <- TeensHome(0)
TeensHome1 <- TeensHome(1)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
# I've created a plot to visualize day preference for casual riders in a pie chart
ggplot(TeensHome0, aes(x ="", y = perc, fill = factor(Teenhome))) +
  geom_col(color="white") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar(theta = "y") +
  labs(title = "Teens home for 0 response", y= "%", x= "%")

ggplot(TeensHome1, aes(x ="", y = perc, fill = factor(Teenhome))) +
  geom_col(color="white") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size=4) +
  coord_polar(theta = "y") +
  labs(title = "Teens home for 1 response", y= "%", x= "%")


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=Age, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Age') + 
  theme_bw()


## ----------------------------------------------------------------------------
Data <- Data %>%
  filter(Age < 100)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=Age, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Age') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=Recency, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Recency (days since last purchase)') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=NumDealsPurchases, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='NumDealsPurchases') + 
  theme_bw()


## ----------------------------------------------------------------------------
Data <- Data %>%
  filter(NumDealsPurchases < 7.5)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=NumDealsPurchases, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='NumDealsPurchases') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=NumWebPurchases, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='NumWebPurchases') + 
  theme_bw()


## ----------------------------------------------------------------------------
Data <- Data %>%
  filter(NumWebPurchases < 15)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=NumWebPurchases, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='NumWebPurchases') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=NumCatalogPurchases, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='NumCatalogPurchases') + 
  theme_bw()


## ----------------------------------------------------------------------------
Data <- Data %>%
  filter(NumCatalogPurchases < 15)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=NumCatalogPurchases, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='NumCatalogPurchases') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=NumStorePurchases, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='NumStorePurchases') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=NumWebVisitsMonth, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='NumWebVisitsMonth') + 
  theme_bw()


## ----------------------------------------------------------------------------
Data <- Data %>%
  filter(NumWebVisitsMonth < 12)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=NumWebVisitsMonth, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='NumWebVisitsMonth') + 
  theme_bw()


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
Count_Responses <- Data %>%
  group_by(Dt_Customer, Response) %>%
  count(Dt_Customer, sort = TRUE) %>%
  return(Dt_Customer)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
ggplot(Count_Responses, aes(x=Dt_Customer,fill=factor(Response))) + geom_histogram(bins=20, width = 0.75, color="white") +
  geom_vline(xintercept = 5, linetype = "dashed", alpha = 0.5)+
  facet_wrap(~Response)+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="right")+
  labs(title = "Histogram for Date enrollment vs Response", y= "Number of responses", x= "Date of customer enrollment with the company")



## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------

# Load ggplot2 library
options(scipen=999)

ggplot(Data, aes(x=factor(Response), y=Income, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Income (in USD)') + 
  theme_bw()


## ----------------------------------------------------------------------------
Data <- Data %>%
  filter(Income  < 140000)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=Income, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Income (in USD)') + 
  theme_bw()


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
ggplot(Data, aes(x=Income,fill=factor(Response))) + geom_histogram(bins=20, width = 0.75, color="white") +
  geom_vline(xintercept = 5, linetype = "dashed", alpha = 0.5)+
  facet_wrap(~factor(Response))+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1,family="Times", face="bold", size=12, color="black"), 
        axis.title.x = element_text(family="Times", face="bold", size=16, color="black"),
        axis.text.y = element_text(family="Times", face="bold", size=12, color="black"),
        axis.title.y = element_text(family="Times", face="bold", size=16, color="black"),
        strip.text = element_text(size=10, face="bold"),
        plot.title = element_text(size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(family="Times", color = "black", size = 16,face="bold"),
        legend.position="right") +
  labs(y= "Number of clients", x= "Income (in usd)")



## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=MntWines, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Money spent on Wines (in USD)') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=MntFruits, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Money spent on fruits (in USD)') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=MntMeatProducts, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Money spent on meat (in USD)') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=MntFishProducts, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Money spent on fish (in USD)') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=MntSweetProducts, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Money spent on Sweet products (in USD)') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot(Data, aes(x=factor(Response), y=MntGoldProds, fill=factor(Response))) + 
  geom_boxplot() +
  labs(y='Money spent on Gold products (in USD)') + 
  theme_bw()


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
statistical_analysis_features <- data.frame(
  Group = c("ID", "Age", "Income", "Kidhome", "Teenhome", "Dt_Customer", "Recency", "MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds","NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases","NumWebVisitsMonth","AcceptedCpm1", "AcceptedCpm2", "AcceptedCpm3", "AcceptedCpm4", "AcceptedCpm5", "Complain", "Z_CostContact", "Z_Revenue", "Education", "Marital Status"),
    
Pursue_a_statistical_analysis= c("NO",
            "NO", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES,", "YES", "YES", "NO", "YES", "YES", "NO", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "NO", "NO", "YES", "YES")

)


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
kable(statistical_analysis_features, caption = "Which features are we going to keep analyzing?")


## ----------------------------------------------------------------------------
Data_0 <- Data %>%
  filter(Response == 0)
nrow(Data_0)

Data_1 <- Data %>%
  filter(Response == 1)
nrow(Data_1)


## ----------------------------------------------------------------------------
median_Income_0 <- median(Data_0$Income)
median_Income_0

median_Income_1 <- median(Data_1$Income)
median_Income_1


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
library(boot)
stat_fun_Income_1 <- function(x, idx) median(x[idx])
boot_obj_Income_1 <- boot(Data_1$Income, R=1000, statistic=stat_fun_Income_1)
boot_obj_Income_1


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
library(boot)
stat_fun_Income_0 <- function(x, idx) median(x[idx])
boot_obj_Income_0 <- boot(Data_0$Income, R=1000, statistic=stat_fun_Income_0)
boot_obj_Income_0


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
boot_ci_Income_1 <- boot.ci(boot_obj_Income_1, conf=0.95, type='basic')
X_Income_1 <- data.frame(median=boot_obj_Income_1$t)
ci90_Income_1 <- boot_ci_Income_1$basic[4:5]
ci_Income_1 <- data.frame(ci_Income_1=ci90_Income_1, y=c(8, 12))
ci_Income_1


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
boot_ci_Income_0 <- boot.ci(boot_obj_Income_0, conf=0.95, type='basic')
X_Income_0 <- data.frame(median=boot_obj_Income_0$t)
ci90_Income_0 <- boot_ci_Income_0$basic[4:5]
ci_Income_0 <- data.frame(ci_Income_0=ci90_Income_0, y=c(8, 12))
ci_Income_0


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
ggplot() +
  stat_density(data = X_Income_1, aes(x = median, group = 1, fill = "Response 1"), alpha = 0.5) +
  stat_density(data = X_Income_0, aes(x = median, group = 1, fill = "Response 0"), alpha = 0.5) +
  geom_vline(data = X_Income_0, aes(xintercept = median_Income_0), linetype = 2, color = "#F8766D") +
  geom_vline(data = X_Income_1, aes(xintercept = median_Income_1), linetype = 2, color = "#00BFC4") +
  theme_bw() +
  labs(title = "Density curve of response 1 vs 0", x = "Median samples", y = "Density", fill = "") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("Response 0", "Response 1"))


## ---- , echo=FALSE, message=FALSE, warning=FALSE, results='hide'-------------
perm_fun <- function(x, nA, nB)
{
  n <- nA + nB
  idx_b <- sample(1:n, nB)
  idx_a <- setdiff(1:n, idx_b)
  median_diff <- median(x[idx_b]) - median(x[idx_a])
  return(median_diff)
}


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
  perm_diffs[i] = perm_fun(Data$Income, 1830, 323)
}
par(mar=c(4,4,1,0)+.1)
hist(perm_diffs, xlab='Difference of the median for Response 1 and 0', main='', xlim=c(-10000, 25000))
abline(v=median_Income_1 - median_Income_0, lty=2, lwd=1.5)
text('  Observed\n  difference', x=median_Income_1 - median_Income_0,  y=par()$usr[4]-20, adj=0)

median(perm_diffs > (median_Income_1 - median_Income_0))


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
median_Wines_0 <- median(Data_0$MntWines)
median_Wines_0

median_Wines_1 <- median(Data_1$MntWines)
median_Wines_1


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
  perm_diffs[i] = perm_fun(Data$MntWines, 1830, 323)
}
par(mar=c(4,4,1,0)+.1)
hist(perm_diffs, xlab='Difference of the median for Response 1 and 0', main='', xlim=c(-100, 400))
abline(v=median_Wines_1 - median_Wines_0, lty=2, lwd=1.5)
text('  Observed\n  difference', x=median_Wines_1 - median_Wines_0,  y=par()$usr[4]-20, adj=0)

median(perm_diffs > (median_Wines_1 - median_Wines_0))


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
median_Fruits_0 <- median(Data_0$MntFruits)
median_Fruits_0

median_Fruits_1 <- median(Data_1$MntFruits)
median_Fruits_1


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
  perm_diffs[i] = perm_fun(Data$MntFruits, 1830, 323)
}
par(mar=c(4,4,1,0)+.1)
hist(perm_diffs, xlab='Difference of the median for Response 1 and 0', main='', xlim=c(-5, 20))
abline(v=median_Fruits_1 - median_Fruits_0, lty=2, lwd=1.5)
text('  Observed\n  difference', x=median_Fruits_1 - median_Fruits_0,  y=par()$usr[4]-20, adj=0)

median(perm_diffs > (median_Fruits_1 - median_Fruits_0))


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
median_Meat_0 <- median(Data_0$MntMeatProducts)
median_Meat_0

median_Meat_1 <- median(Data_1$MntMeatProducts)
median_Meat_1


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
  perm_diffs[i] = perm_fun(Data$MntMeatProducts, 1830, 323)
}
par(mar=c(4,4,1,0)+.1)
hist(perm_diffs, xlab='Difference of the median for Response 1 and 0', main='', xlim=c(-50, 170))
abline(v=median_Meat_1 - median_Meat_0, lty=2, lwd=1.5)
text('  Observed\n  difference', x=median_Meat_1 - median_Meat_0,  y=par()$usr[4]-20, adj=0)

median(perm_diffs > (median_Meat_1 - median_Meat_0))


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
median_Fish_0 <- median(Data_0$MntFishProducts)
median_Fish_0

median_Fish_1 <- median(Data_1$MntFishProducts)
median_Fish_1


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
  perm_diffs[i] = perm_fun(Data$MntFishProducts, 1830, 323)
}
par(mar=c(4,4,1,0)+.1)
hist(perm_diffs, xlab='Difference of the median for Response 1 and 0', main='', xlim=c(-5, 25))
abline(v=median_Fish_1 - median_Fish_0, lty=2, lwd=1.5)
text('  Observed\n  difference', x=median_Fish_1 - median_Fish_0,  y=par()$usr[4]-20, adj=0)

median(perm_diffs > (median_Fish_1 - median_Fish_0))


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
median_Sweet_0 <- median(Data_0$MntSweetProducts	)
median_Sweet_0

median_Sweet_1 <- median(Data_1$MntSweetProducts	)
median_Sweet_1


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
  perm_diffs[i] = perm_fun(Data$MntSweetProducts, 1830, 323)
}
par(mar=c(4,4,1,0)+.1)
hist(perm_diffs, xlab='Difference of the median for Response 1 and 0', main='', xlim=c(-5, 25))
abline(v=median_Sweet_1 - median_Sweet_0, lty=2, lwd=1.5)
text('  Observed\n  difference', x=median_Sweet_1 - median_Sweet_0,  y=par()$usr[4]-20, adj=0)

median(perm_diffs > (median_Sweet_1 - median_Sweet_0))


## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------
median_Gold_0 <- median(Data_0$MntGoldProds	)
median_Gold_0

median_Gold_1 <- median(Data_1$MntGoldProds	)
median_Gold_1


## ---- echo=FALSE, message=FALSE, warning=FALSE-------------------------------
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
  perm_diffs[i] = perm_fun(Data$MntGoldProds, 1830, 323)
}
par(mar=c(4,4,1,0)+.1)
hist(perm_diffs, xlab='Difference of the median for Response 1 and 0', main='', xlim=c(-5, 25))
abline(v=median_Gold_1 - median_Gold_0, lty=2, lwd=1.5)
text('  Observed\n  difference', x=median_Gold_1 - median_Gold_0,  y=par()$usr[4]-20, adj=0)

median(perm_diffs > (median_Gold_1 - median_Gold_0))


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
Count_Cmp1 <- Data %>%
  group_by(AcceptedCmp1, Response) %>%
  count(AcceptedCmp1, sort = TRUE) %>%
  return(AcceptedCmp1)

Count_Cmp1


## ----------------------------------------------------------------------------
obs_pct_diff_Cmp1 <- 100 * (79/141 -244/2012) #%conv1 - %conv2
obs_pct_diff_Cmp1


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
perm_fun_contar <- function(x, nA, nB)
{
  n <- nA + nB
  idx_b <- sample(1:n, nB)
  idx_a <- setdiff(1:n, idx_b)
  
  ones_b <- sum(x[idx_b] == 1)
  zeros_b <- sum(x[idx_b] == 0)
  
  ones_a <- sum(x[idx_a] == 1)
  zeros_a <- sum(x[idx_a] == 0)
  
  pct_ones_b <- ones_b / length(idx_b)
  pct_ones_a <- ones_a / length(idx_a)
  
  diff_pct_ones <- pct_ones_a - pct_ones_b
  
  return(diff_pct_ones)
}


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
## Statistical Significance and P-Values
conversion <- c(rep(0, 1830), rep(1, 323)) # definir cuantos 0 y 1 hay en response
set.seed(1)
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
perm_diffs[i] = 100 * perm_fun_contar(conversion, 141, 2012) #denominador de conv1-conv2
}

hist(perm_diffs, xlab='Difference between conversion rates (percent)', main='', xlim=c(-10, 55))
abline(v=obs_pct_diff_Cmp1, lty=2, lwd=1.5)
text('   Observed\n   difference', x=obs_pct_diff_Cmp1,  y=par()$usr[4]-20, adj=0)

# Obbserved difference
obs_pct_diff_Cmp1

### P-Value

mean(perm_diffs > obs_pct_diff_Cmp1)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
Count_Cmp2 <- Data %>%
  group_by(AcceptedCmp2, Response) %>%
  count(AcceptedCmp2, sort = TRUE) %>%
  return(AcceptedCmp2)

Count_Cmp2


## ----------------------------------------------------------------------------
obs_pct_diff_Cmp2 <- 100 * (20/30 -303/2123) #%conv1 - %conv2 of response by accepted cmp
obs_pct_diff_Cmp2


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
## Statistical Significance and P-Values
conversion <- c(rep(0, 1830), rep(1, 323)) # definir cuantos 0 y 1 hay en response
set.seed(1)
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
perm_diffs[i] = 100 * perm_fun_contar(conversion, 30, 2123) #denominador de conv1-conv2
}

hist(perm_diffs, xlab='Difference between conversion rates (percent)', main='', xlim=c(-25, 65))
abline(v=obs_pct_diff_Cmp2, lty=2, lwd=1.5)
text('   Observed\n   difference', x=obs_pct_diff_Cmp2,  y=par()$usr[4]-20, adj=0)

# Obbserved difference
obs_pct_diff_Cmp2

### P-Value

mean(perm_diffs > obs_pct_diff_Cmp2)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
Count_Cmp3 <- Data %>%
  group_by(AcceptedCmp3, Response) %>%
  count(AcceptedCmp3, sort = TRUE) %>%
  return(AcceptedCmp3)

Count_Cmp3


## ----------------------------------------------------------------------------
obs_pct_diff_Cmp3 <- 100 * (77/162 -246/1991) #%conv1 - %conv2 of response by accepted cmp
obs_pct_diff_Cmp3


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
## Statistical Significance and P-Values
conversion <- c(rep(0, 1830), rep(1, 323)) # definir cuantos 0 y 1 hay en response
set.seed(1)
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
perm_diffs[i] = 100 * perm_fun_contar(conversion, 162, 1991) #denominador de conv1-conv2
}

hist(perm_diffs, xlab='Difference between conversion rates (percent)', main='', xlim=c(-10, 50))
abline(v=obs_pct_diff_Cmp3, lty=2, lwd=1.5)
text('   Observed\n   difference', x=obs_pct_diff_Cmp3,  y=par()$usr[4]-20, adj=0)

# Obbserved difference
obs_pct_diff_Cmp3

### P-Value

mean(perm_diffs > obs_pct_diff_Cmp3)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
Count_Cmp4 <- Data %>%
  group_by(AcceptedCmp4, Response) %>%
  count(AcceptedCmp4, sort = TRUE) %>%
  return(AcceptedCmp4)

Count_Cmp4


## ----------------------------------------------------------------------------
obs_pct_diff_Cmp4 <- 100 * (59/159 -264/1994) #%conv1 - %conv2 of response by accepted cmp
obs_pct_diff_Cmp4


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
## Statistical Significance and P-Values
conversion <- c(rep(0, 1830), rep(1, 323)) # definir cuantos 0 y 1 hay en response
set.seed(1)
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
perm_diffs[i] = 100 * perm_fun_contar(conversion, 159, 1994) #denominador de conv1-conv2
}

hist(perm_diffs, xlab='Difference between conversion rates (percent)', main='', xlim=c(-10, 35))
abline(v=obs_pct_diff_Cmp4, lty=2, lwd=1.5)
text('   Observed\n   difference', x=obs_pct_diff_Cmp4,  y=par()$usr[4]-20, adj=0)

# Obbserved difference
obs_pct_diff_Cmp4

### P-Value

mean(perm_diffs > obs_pct_diff_Cmp4)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
Count_Cmp5 <- Data %>%
  group_by(AcceptedCmp5, Response) %>%
  count(AcceptedCmp5, sort = TRUE) %>%
  return(AcceptedCmp5)

Count_Cmp5


## ----------------------------------------------------------------------------
obs_pct_diff_Cmp5 <- 100 * (91/161 -232/1992) #%conv1 - %conv2 of response by accepted cmp
obs_pct_diff_Cmp5


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
## Statistical Significance and P-Values
conversion <- c(rep(0, 1830), rep(1, 323)) # definir cuantos 0 y 1 hay en response
set.seed(1)
perm_diffs <- rep(0, 1000)
for (i in 1:1000) {
perm_diffs[i] = 100 * perm_fun_contar(conversion, 161, 1992) #denominador de conv1-conv2
}

hist(perm_diffs, xlab='Difference between conversion rates (percent)', main='', xlim=c(-10, 55))
abline(v=obs_pct_diff_Cmp5, lty=2, lwd=1.5)
text('   Observed\n   difference', x=obs_pct_diff_Cmp5,  y=par()$usr[4]-20, adj=0)

# Obbserved difference
obs_pct_diff_Cmp5

### P-Value

mean(perm_diffs > obs_pct_diff_Cmp5)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
Count_Complain <- Data %>%
  group_by(Complain, Response) %>%
  count(Complain, sort = TRUE) %>%
  return(Complain)

Count_Complain


## ----------------------------------------------------------------------------
obs_pct_diff_Complain <- 100 * (320/2133-3/20) #%conv1 - %conv2 of response by accepted cmp
obs_pct_diff_Complain
#result is 0.002344116


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
## Statistical Significance and P-Values
conversion <- c(rep(0, 1830), rep(1, 323)) # definir cuantos 0 y 1 hay en response
set.seed(123)
perm_diffs <- rep(0, 1300)
for (i in 1:1000) {
perm_diffs[i] = 100 * perm_fun_contar(conversion, 2133, 20) #denominador de conv1-conv2
}

hist(perm_diffs, xlab='Difference between conversion rates (percent)', main='', xlim=c(-25, 35))
abline(v=obs_pct_diff_Complain, lty=2, lwd=1.5)
text('   Observed\n   difference', x=obs_pct_diff_Complain,  y=par()$usr[4]-30, adj=0)

# Obbserved difference
obs_pct_diff_Complain

### P-Value

mean(perm_diffs > obs_pct_diff_Complain)


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------
Count_Kidhome <- Data %>%
  group_by(Kidhome, Response) %>%
  count(Kidhome, sort = TRUE) %>%
  arrange(Kidhome) %>%
  return(Kidhome)

Count_Kidhome

## ----------------------------------------------------------------------------
reaction_Kidhome <- matrix(Count_Kidhome$n, nrow=3, ncol=2, byrow=TRUE)
reaction_Kidhome
dimnames(reaction_Kidhome) <- list(unique(Data$Kidhome), unique(Data$Response))
reaction_Kidhome

## ----------------------------------------------------------------------------
chisq.test(reaction_Kidhome, simulate.p.value=TRUE)


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
Count_Teenhome <- Data %>%
  group_by(Teenhome, Response) %>%
  count(Teenhome, sort = TRUE) %>%
  arrange(Teenhome) %>%
  return(Teenhome)

Count_Teenhome


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
reaction_Teenhome <- matrix(Count_Teenhome$n, nrow=3, ncol=2, byrow=TRUE)
reaction_Teenhome
dimnames(reaction_Teenhome) <- list(unique(Data$Teenhome), unique(Data$Response))
reaction_Teenhome


## ----------------------------------------------------------------------------
chisq.test(reaction_Teenhome, simulate.p.value=TRUE)


## ----------------------------------------------------------------------------
# Create a new column with the 6-month period
Data$Period <- cut(Data$Dt_Customer, breaks = "6 months")

# Count the number of dates in each period
table(Data$Period)


## ----------------------------------------------------------------------------
Count_Period <- Data %>%
  group_by(Period, Response) %>%
  count(Period, sort = TRUE) %>%
  arrange(Period) %>%
  return(Period)

Count_Period


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
# Extract unique values of Period column and convert to list
Lista_periodos <- Count_Period %>% 
                 pull(Period) %>% 
                 unique() 


## ----------------------------------------------------------------------------
reaction_Period <- matrix(Count_Period$n, nrow=4, ncol=2, byrow=TRUE)
reaction_Period
dimnames(reaction_Period) <- list(Lista_periodos, unique(Data$Response))
reaction_Period


## ----------------------------------------------------------------------------
chisq.test(reaction_Period, simulate.p.value=TRUE)


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
# Create a new column with the group of 10s
Data$Group_Recency <- cut(Data$Recency, breaks = seq(0, 100, by = 10), right = FALSE)

# Count the number of values in each group
table(Data$Group_Recency)


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
Count_Recency <- Data %>%
  group_by(Group_Recency, Response) %>%
  count(Group_Recency, sort = TRUE) %>%
  arrange(Group_Recency) %>%
  return(Group_Recency)

Count_Recency


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
# Extract unique values of Period column and convert to list
Lista_Recency <- Count_Recency %>% 
                 pull(Group_Recency) %>% 
                 unique() 
Lista_Recency


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
reaction_Recency <- matrix(Count_Recency$n, nrow=10, ncol=2, byrow=TRUE)
reaction_Recency
dimnames(reaction_Recency) <- list(Lista_Recency, unique(Data$Response))
reaction_Recency


## ----------------------------------------------------------------------------
chisq.test(reaction_Recency, simulate.p.value=TRUE)


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
# Create a new column with the group of 10s
Data$Group_NumWebPurchases <- cut(Data$NumWebPurchases, breaks = seq(0, 12, by = 6), right = FALSE)

# Count the number of values in each group
table(Data$Group_NumWebPurchases)


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
Count_NumWebPurchases <- Data %>%
  group_by(Group_NumWebPurchases, Response) %>%
  count(Group_NumWebPurchases, sort = TRUE) %>%
  arrange(Group_NumWebPurchases) %>%
  return(Group_NumWebPurchases)

Count_NumWebPurchases


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
# Extract unique values of Period column and convert to list
Lista_NumWebPurchases <- Count_NumWebPurchases %>% 
                 pull(Group_NumWebPurchases) %>% 
                 unique() 
Lista_NumWebPurchases


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
reaction_NumWebPurchases <- matrix(Count_NumWebPurchases$n, nrow=2, ncol=2, byrow=TRUE)
reaction_NumWebPurchases
dimnames(reaction_NumWebPurchases) <- list(Lista_NumWebPurchases, unique(Data$Response))
reaction_NumWebPurchases


## ----------------------------------------------------------------------------
chisq.test(reaction_NumWebPurchases, simulate.p.value=TRUE)


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
# Create a new column with the group of 10s
Data$Group_NumCatalogPurchases <- cut(Data$NumCatalogPurchases, breaks = seq(0, 12, by = 4), right = FALSE)

# Count the number of values in each group
table(Data$Group_NumCatalogPurchases)


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
Count_NumCatalogPurchases <- Data %>%
  group_by(Group_NumCatalogPurchases, Response) %>%
  count(Group_NumCatalogPurchases, sort = TRUE) %>%
  arrange(Group_NumCatalogPurchases) %>%
  return(Group_NumCatalogPurchases)

Count_NumCatalogPurchases


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
# Extract unique values of Period column and convert to list
Lista_NumCatalogPurchases <- Count_NumCatalogPurchases %>% 
                 pull(Group_NumCatalogPurchases) %>% 
                 unique() 
Lista_NumCatalogPurchases


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
reaction_NumCatalogPurchases <- matrix(Count_NumCatalogPurchases$n, nrow=3, ncol=2, byrow=TRUE)
reaction_NumCatalogPurchases
dimnames(reaction_NumCatalogPurchases) <- list(Lista_NumCatalogPurchases, unique(Data$Response))
reaction_NumCatalogPurchases


## ----------------------------------------------------------------------------
chisq.test(reaction_NumCatalogPurchases, simulate.p.value=TRUE)


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
# Create a new column with the group of 10s
Data$Group_NumWebVisitsMonth <- cut(Data$NumWebVisitsMonth, breaks = seq(0, 12, by = 3), right = FALSE)

# Count the number of values in each group
table(Data$Group_NumWebVisitsMonth)


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
Count_NumWebVisitsMonth <- Data %>%
  group_by(Group_NumWebVisitsMonth, Response) %>%
  count(Group_NumWebVisitsMonth, sort = TRUE) %>%
  arrange(Group_NumWebVisitsMonth) %>%
  return(Group_NumWebVisitsMonth)

Count_NumWebVisitsMonth


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
# Extract unique values of Period column and convert to list
Lista_NumWebVisitsMonth <- Count_NumWebVisitsMonth %>% 
                 pull(Group_NumWebVisitsMonth) %>% 
                 unique() 
Lista_NumWebVisitsMonth


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
reaction_NumWebVisitsMonth <- matrix(Count_NumWebVisitsMonth$n, nrow=4, ncol=2, byrow=TRUE)
reaction_NumWebVisitsMonth
dimnames(reaction_NumWebVisitsMonth) <- list(Lista_NumWebVisitsMonth, unique(Data$Response))
reaction_NumWebVisitsMonth


## ----------------------------------------------------------------------------
chisq.test(reaction_NumWebVisitsMonth, simulate.p.value=TRUE)


#Model 1 Code-----------------------------------------------------------------------------------------------------------

set.seed(9675)
library(rsample)
library('sjPlot')

all_contracts <- read.csv("../finalProject/linRegFinal.csv")

str(all_contracts)

# Change financials from characters to strings

all_contracts$QB <- as.numeric(gsub("M", "", all_contracts$QB))
all_contracts$RB <- as.numeric(gsub("M", "", all_contracts$RB))
all_contracts$WR <- as.numeric(gsub("M", "", all_contracts$WR))
all_contracts$TE <- as.numeric(gsub("M", "", all_contracts$TE))
all_contracts$OL <- as.numeric(gsub("M", "", all_contracts$OL))
all_contracts$DL <- as.numeric(gsub("M", "", all_contracts$DL))
all_contracts$LB <- as.numeric(gsub("M", "", all_contracts$LB))
all_contracts$DB <- as.numeric(gsub("M", "", all_contracts$DB))
all_contracts$Total <- as.numeric(gsub("M", "", all_contracts$Total))

# Adding a variable to represent a percentage of a team's spending

all_contracts$QB_percent <- (all_contracts$QB / all_contracts$Total) * 100
all_contracts$RB_percent <- (all_contracts$RB / all_contracts$Total) * 100
all_contracts$WR_percent <- (all_contracts$WR / all_contracts$Total) * 100
all_contracts$TE_percent <- (all_contracts$TE / all_contracts$Total) * 100
all_contracts$OL_percent <- (all_contracts$OL / all_contracts$Total) * 100
all_contracts$DL_percent <- (all_contracts$DL / all_contracts$Total) * 100
all_contracts$LB_percent <- (all_contracts$LB / all_contracts$Total) * 100
all_contracts$DB_percent <- (all_contracts$DB / all_contracts$Total) * 100

# Splitting into testing and training

contracts_split <- initial_split(all_contracts, prop = 0.85)
contracts_train <- training(contracts_split)
contracts_test <- testing(contracts_split)

# Creating a linear regression model for percentages

lm1 <- lm(Win.Percentage ~ QB_percent + RB_percent + WR_percent + OL_percent,
          data = contracts_train)

tab_model(lm1)

# Model Evaluation ----------------------------------------------

preds_train <- predict(lm1, contracts_train)
preds_test <- predict(lm1, contracts_test)

results_train <- tibble(
  'preds' = preds_train,
  'true' = contracts_train$Win.Percentage,
  'type' = "Train"
)

results_test <- tibble(
  'preds' = preds_test,
  'true' = contracts_test$Win.Percentage,
  'type' = "Test"
)

results_df <- bind_rows(results_train, results_test)

get_mae <- function(true, predictions) {
  mean(abs(true - predictions))
}

get_mae(results_train$true, results_train$preds)
get_mae(results_test$true, results_test$preds)

ggplot(results_df, aes(x = true, y = preds)) +
  geom_point(aes(color = type), size = 3) +  # Adjust point size
  geom_abline(color = "red") +
  facet_wrap(~ type) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 20),  # Adjust label size
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_text(size = 14),  # Adjust legend title size
    axis.text = element_text(size = 12)  # Adjust axis text size
  )

library("ggplot2")

linRegRYOE <- ggplot(data = totalStats, aes(x = Contract_Size_M, y = Yards_Over_Expected, label = Player)) +
  geom_smooth(method = "lm", se = FALSE, col = "red") +
  geom_text(nudge_y = 0.1, check_overlap = TRUE) +  # Update this line
  xlab("2023 Salary (Millions)") + 
  ylab("Rushing Yards Over Expected per Attempt") + 
  theme(axis.title = element_text(size = 24)) 

print(linRegRYOE)

#------------------------------------------

scatter_rb <- ggplot(data = all_contracts,
                     aes(x = RB_percent, y = Win.Percentage)) +
  geom_point() + 
  xlab("% of Total Salary Devoted to RB's") +
  ylab("Win Percentage") +
  geom_smooth(se = FALSE, col = "red") +
  theme(axis.title = element_text(size = 24)) 

plot(scatter_rb)



#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------


#Model 2 Code--------------------------------------------------------------------------------------------------------

#LIBRARY
install.packages("nflfastR")
library(dplyr)
library("nflfastR")
library("ggplot2")

options(scipen = 10)
set.seed(1818)
library('rpart')
library(partykit)
library(tidyverse)
library(titanic)
library(PerformanceAnalytics)
library(rpart)       
library(rpart.plot)  


# Loading the contracts and Next Gen Stats Files
all_contracts <- read.csv("../final_project/linRegFinal.csv")
nextGenStats <- read.csv("../final_project/nextGenStats.csv")
pffStats <- read.csv("../final_project/rushing_summary.csv")
CombinedData<- read.csv("../final_project/combined_data_2000-2023.csv")
rbcontracts <- read.csv("../final_project/rbContracts.csv")

#-----------------------------------------------------------------------------------------------------------------------
#CLEANING--
colnames(nextGenStats)[colnames(nextGenStats) == "PLAYER.NAME"] <- "Player"
colnames(CombinedData)[colnames(CombinedData) == "player"] <- "Player"
colnames(pffStats)[colnames(pffStats) == "player"] <- "Player"

pff_clean <- pffStats %>% filter(attempts >= 100)
pff_clean <- pff_clean[pff_clean$Player != "Jalen Hurts", ]
colnames(pff_clean)[colnames(pff_clean) == "player"] <- "Player"

##cleaning rbcontracts names so they could match nextGenStats, & pff-clean
rbcontracts[55, "Player"] <- "AJ Dillon"
rbcontracts[59, "Player"] <- "Brian Robinson"
rbcontracts[51, "Player"] <- "De'Von Achane"
rbcontracts[35, "Player"] <- "Kenneth Walker"

#Merge 3 data sets: nextGenStats, pff-clean, & rbcontracts
merged_data <- merge(nextGenStats, pff_clean, by = "Player", all = TRUE)
merged_data <- merge(merged_data, rbcontracts, by = "Player", all = TRUE)

View(merged_data)

#clean the merged data set:
merged_data <- merged_data[-c(4,6,7,8,10,11,13,14,17,18,19,
                              20,21,22,23,24,26,28,29,30,32,
                              34,36,37,38,41,42,43,44,46,48,
                              49,50,51,52,53,54,55,56,57,59,
                              61,62,63,64,66,67,68,69,71,75,
                              76,77,78,81,82,83,84,87,89,90,
                              91,94,95,97,98,99,100,101,102,
                              104,105,108,109,110,111,112,113,
                              114,115,116,117,119,120,122,123,
                              124,125,126,127,129,131,133,134,
                              135,136,137,138,140,141,142,143,
                              144,145,146,149,150,151,153,156,
                              157,158,159,161,164), ]

#converts non-numeric columns to numeric ones
merged_data$Total.Value <- as.numeric(gsub("[^0-9.-]", "", merged_data$Total.Value))
merged_data$APY <- as.numeric(gsub("[^0-9.-]", "", merged_data$APY))
merged_data$Total.Guaranteed <- as.numeric(gsub("[^0-9.-]", "", merged_data$Total.Guaranteed))
merged_data$Avg..Guarantee.Year <- as.numeric(gsub("[^0-9.-]", "", merged_data$Avg..Guarantee.Year))

View(merged_data)

#Turn Total.Value, APY, Total.Guaranteed,Avg..Guaranteed into decimals, 
merged_data$Total.Value <- merged_data$Total.Value / 1000000
merged_data$APY <- merged_data$APY / 1000000
merged_data$Total.Guaranteed <- merged_data$Total.Guaranteed / 1000000
merged_data$Avg..Guarantee.Year <- merged_data$Avg..Guarantee.Year / 1000000
merged_data$grades_offense <- merged_data$grades_offense / 100


View(merged_data)


#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
  #MODELS:
# cool variables to run: Win.Percentage, APY, grades_offense, RYOE, RYOE.ATT, TD


                                              ##Regression Trees 
library(visNetwork)
mod_tree <- rpart(APY ~ Win.Percentage + grades_offense + EFF + RYOE + grades_run,
                  data = merged_data,
                  method = "anova")


print(mod_tree)
plot(mod_tree)


visNetwork::visTree(
  mod_tree,
  main = "Contract Size per Year & RB Grades",
  nodesPopSize = TRUE,
  edgesFontSize = 10,
  nodesFontSize = 15,
  width = "100%",
  height = "1200px",
  shapeVar = "ellipse",
  colorVar = "tan",
  shapeY = "circle",
  colorY = "azure3",
  colorEdges = "cornflowerblue",)


              #PREDICTION MODEL
predictions <- predict(mod_tree)
rmse_value <- sqrt(mean((merged_data$APY - predictions)^2))
cat("RMSE:", rmse_value, "\n")

# RMSE = 3.63395

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
                         ##Scatter plots showing APY~ Win % as a factor of offensive grades
install.packages("plotly")
library(plotly)

mod_plot1<- ggplot(data = merged_data, aes(x = APY, y = Win.Percentage)) + 
  geom_point(aes(color = grades_offense), size = 3) +
  scale_color_gradient(low = "blue", high = "red") +  
  geom_smooth() +
  theme_replace() + 
  theme(legend.position = "bottom", legend.justification = "right")

plot(mod_plot1)

#Makes plot interactive 
ggplotly(mod_plot1)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
system("git clone https://github.com/danilopez7775/RB-analysis.git")
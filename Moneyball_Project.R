library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
batting <- fread("Batting.csv", header=TRUE)

batting$BA <- batting$H/batting$AB
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)
batting$`1B` <- batting$H - batting$`2B` - batting$`3B` - batting$HR
batting$SLG <- (batting$`1B` + 2*batting$`2B` + 3*batting$`3B` + 4*batting$HR) / batting$AB

sal <- fread("Salaries.csv")
batting <- subset(batting, yearID >= 1985)
combo <- merge(batting, sal, by = c("yearID", "playerID"))

lost_players <- c("giambja01", "damonjo01", "saenzol01")

lost_df <- subset(combo, playerID %in% lost_players)

lost_df <- subset(lost_df, yearID == 2001)

# constraints (2001):
# 1. sum(salaries_newplayers) <= 15 mil dollars
# 2. sum(AB_newplayers) >= sum(AB_oldplayers)
# 3. mean(OBP_newplayers) >= mean(OBP_lostplayers)

combo_2001 <- subset(combo, yearID == 2001)
combo_2001 <- combo_2001[, .(playerID, salary, AB, OBP, teamID.x, teamID.y)]
combo_2001 <- arrange(combo_2001, salary, desc(AB), desc(OBP))

lost_df <- lost_df[, .(playerID, salary, AB, OBP, teamID.x, teamID.y)]
sum(lost_df$salary)
sum(lost_df$AB)
sum(lost_df$OBP)/3

combo_2001 <- combo_2001[!(teamID.x == "OAK" | teamID.y == "OAK"),]

# METHOD 1

attempts <- 0
repeat {
  attempts <- attempts + 1
  picked <- sample(combo_2001$playerID, 3)
  new_df <- combo_2001[playerID %in% picked, .SD[1], by = playerID]
  if(sum(new_df$salary, na.rm = TRUE) <= 15000000 & 
     sum(new_df$AB, na.rm = TRUE) >= sum(lost_df$AB, na.rm = TRUE) & 
     mean(new_df$OBP, na.rm = TRUE) >= mean(lost_df$OBP, na.rm = TRUE)) {
    break
  }
  if (attempts > 2000) stop("Rerun code!")
}
picked
new_df
sum(new_df$salary)
sum(new_df$AB)
mean(new_df$OBP)
mean(lost_df$OBP)

# METHOD 2

combo_2001 <- subset(combo, salary <  5000000 & OBP > 0.4 & AB >= 500)

pl <- ggplot(combo_2001, aes(x = OBP, y = salary)) + 
  geom_point(aes(color = factor(AB))) +
  labs(color = "At Bats", x = "On Base Percentage", y = "Salary (USD)")
pl
ggplotly(pl)
summary(combo_2001)

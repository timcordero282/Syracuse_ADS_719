#########################################
# Author: Tim Cordero
# Purpose: IST 719 Project
# Date: 11/20/2022
#########################################
#install.packages("ggplot2")
library(ggplot2)
#########################################

file <- file.choose()
players <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

str(players)

dim(players)
#################################################################################
# single dist
boxplot(players$PTS, col = "#E37600", main = "NHL Points Distribution"
     , xlab = "Points", ylab = "Frequency", border = "#E3C600")

boxplot(players$G, col = "#E37600", main = "NHL Goals Distribution"
        , xlab = "Goals", ylab = "Frequency", border = "#E3C600")

boxplot(players$A, col = "#E37600", main = "NHL Assists Distribution"
        , xlab = "Assists", ylab = "Frequency", border = "#E3C600")
#################################################################################
# stats by player
num.colors <- 5
FUN <- colorRampPalette(c("#E37600", "#E3C600"))
my.cols <- FUN(num.colors)

# points
pointsByPLR <- aggregate(players$PTS, list(players$Player), sum)
colnames(pointsByPLR) <- c("Name", "Points")
pointsByPLR <- pointsByPLR[order(-pointsByPLR$Points),] 
pointsByPLR$Name <- factor(pointsByPLR$Name, levels = pointsByPLR$Name[order(-pointsByPLR$Points)])
ggplot(pointsByPLR[0:5,]) + aes(x = Name, y = Points, fill = Name) + geom_bar(stat = "identity") +
  scale_fill_manual(values = my.cols,aesthetics = "fill") + ggtitle("Top 5 Players By Points")

# goals
goalsByPLR <- aggregate(players$G, list(players$Player), sum)
colnames(goalsByPLR) <- c("Name", "Goals")
goalsByPLR <- goalsByPLR[order(-goalsByPLR$Goals),] 
goalsByPLR$Name <- factor(goalsByPLR$Name, levels = goalsByPLR$Name[order(-goalsByPLR$Goals)])
ggplot(goalsByPLR[0:5,]) + aes(x = Name, y = Goals, fill = Name) + geom_bar(stat = "identity") + 
  scale_fill_manual(values = my.cols,aesthetics = "fill") + ggtitle("Top 5 Players By Goals")

# assists
assistsByPLR <- aggregate(players$A, list(players$Player), sum)
colnames(assistsByPLR) <- c("Name", "Assists")
assistsByPLR <- assistsByPLR[order(-assistsByPLR$Assists),] 
assistsByPLR$Name <- factor(assistsByPLR$Name, levels = assistsByPLR$Name[order(-assistsByPLR$Assists)])
ggplot(assistsByPLR[0:5,]) + aes(x = Name, y = Assists, fill = Name) + geom_bar(stat = "identity") + 
  scale_fill_manual(values = my.cols,aesthetics = "fill") + ggtitle("Top 5 Players By Assists")

#################################################################################
#points by position
num.colors <- 6
FUN <- colorRampPalette(c("#E37600", "#E3C600"))
my.cols <- FUN(num.colors)
pointsByPOS <- tapply(players$PTS, list(players$Pos), sum)

pie(pointsByPOS, main="Points by Position",
        col=my.cols)

#################################################################################
# teams by player points

num.colors <- 10
FUN <- colorRampPalette(c("#E37600", "#E3C600"))
my.cols <- FUN(num.colors)


pointsByTM <- aggregate(players$PTS, list(players$Tm), sum)
colnames(pointsByTM) <- c("Team", "Points")
pointsByTM <- pointsByTM[order(-pointsByTM$Points),] 
pointsByTM$Team <- factor(pointsByTM$Team, levels = pointsByTM$Team[order(-pointsByTM$Points)])


ggplot(pointsByTM[0:10,]) + aes(Team, Points) + 
  geom_point(aes(fill = factor(Team)), shape=21, size = 5) + 
  scale_fill_manual(values=my.cols) + ggtitle("Top teams by player points")

# Importing the player data
batting <- read.csv('D:/Courses/Udemy R Tutorial/R-Course-HTML-Notes/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Capstone Project/Batting.csv')
head(batting)

head(batting$AB)
head(batting$X2B)

# Batting average
batting$BA <- batting$H/batting$AB

# On Base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

# Creating X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Creating Slugging Average (SLG)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

str(batting)

# Loading salaries
sal <- read.csv('D:/Courses/Udemy R Tutorial/R-Course-HTML-Notes/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Capstone Project/Salaries.csv')

head(sal)

str(sal)

# Removing batting data before 1985
batting <- subset(batting,yearID >= 1985)

summary(batting)

# Merging batting and sal
combo <- merge(batting,sal, by=c('playerID', 'yearID'))

summary(combo)

# Getting the lost players
lost_players <- subset(combo, playerID %in% c('giambja01','damonjo01','saenzol01'))

lost_players

# Considering players from 2001 only
lost_players <- subset(lost_players, yearID == 2001)

lost_players

lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
lost_players

library(deployer)
# Getting list of available players for selection
avail.players <- filter(combo,yearID==2001)

library(ggplot2)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()

# Removing players with salaries larger or equal to 8000000, having OBP <= 0 and AB>= 500
avail.players <- filter(avail.players,salary<8000000,OBP>0)

# The total AB of the lost players is 1469. This is about 1500, meaning I should probably cut off my avail.players at 1500/3= 500 AB.
avail.players <- filter(avail.players,AB >= 500)

possible <- head(arrange(avail.players,desc(OBP)),10)
possible <- possible[,c('playerID','OBP','AB','salary')]

possible

# Can't choose giambja again, but the other ones look good (2-4)
# List of possible players
possible[2:4,]

#loading libraries
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(writexl)
library(lubridate)
library(skimr)
library(forcats)

#Exploratory Data Analysis 20K games taken in 2018 from users from the top 100 teams on Lichess.
#GLicko chess rating system used of players between 784 and 2723 rating

games %>% skim(white_rating)
games %>% skim(black_rating)


#Ways people win on Chess

ggplot(games, aes(x=victory_status, fill=as.factor(victory_status) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")

#It seems most of the time people resign more often than loosing via checkmate.
#Also its surprising to see such a low percentage of draws as in proffessional chess around 50% of games end in draw

#Do White wins more than black?

ggplot(games, aes(x=winner, fill=as.factor(winner) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")

sum(games$winner == "white", na.rm=TRUE)
sum(games$winner == "black", na.rm=TRUE)
sum(games$winner == "draw", na.rm=TRUE)

#Percentage f wins white, black or draw

winer<-games %>% select(7)

percentageofwins<-sapply(winer, table)/nrow(winer) * 100

#black 45.40
#draw   4.74
#white 49.86


#It seems White has a little bit of advantage against black having won 4.46% more games.


#Scatter plot number of turns

games %>%
  filter(turns<200) %>% 
  ggplot( aes(x=turns)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Number of Turns") +
  theme_ipsum()

skim(games$turns)

#mean=60.5
#standard deviation= 33.6: 33.6
#median= 55

#the average number of turns came to be 60.5, but in this case we should use the median 55
#as we have a large standard deviation of 33.6.



#Calculating most used openings 



mostused<-games %>% select(15)

mostusedo<-sapply(mostused, table)/nrow(mostused) * 100

nrow(mostusedo)

#There seems there is a great variation between the openings as there is no opening 
#used more than 2% and there are 1477 different openings used

#lets see if in those top 3 openings there is diference in winning percentage between white and black

target<-c("Van't Kruijs Opening","Sicilian Defense","Sicilian Defense: Bowdler Attack")

top3o<-filter(games, opening_name %in% target)

top3o1<-top3o %>% select(7)

percentageofwinsinopening<-sapply(top3o1, table)/nrow(top3o1) * 100

#black 57.14
#draw   4.31
#white 38.55

#there was a big difference in the percentage of wins in does top 3 openings vs all openings
#black shifting the balance and winning 57.14% compared to 45.40% from all openings,
#and white winning 11.31% during the top three openings

#Now as the people who play Chess knows openings such as the sicilian defense and  others 
#have many variations wich is the cause that we see ni opening used more than 2% lets try 
#to get rid of the variations to see wich opening theories are used more, and who wins more in the top 3

test<-games %>% select(7,15)

openings<-test %>% separate(opening_name, c("main_opening", "value"), ": ", extra = "merge")

openings1<-openings %>% select(2)

openings2 <-sapply(openings1, table)/nrow(openings1) * 100

#Now we see that in 25% of the games they use the top 3 main openings and in 50% of the games they use 
#the top 10 openings

target2<-c("Sicilian Defense","French Defense","Queen's Pawn Game")

openings0<-openings %>% select(1,2)

openings00<-filter(openings0, main_opening %in% target2)

openings000<-openings00 %>% select(1)

percentageofwinsinopeningm<-sapply(openings000, table)/nrow(openings000) * 100

#black 48.32
#draw   4.88
#white 46.80

#In the top 3 main openings used black still have a little bit of advantage over white,
#but not as much as we see in the top 3 general openings.






#How Often do Players with higher rating win

trial1<-games %>% select(7,10,12)

trial1$higherratedwinner=ifelse(trial1$white_rating>trial1$black_rating & trial1$winner=="white" |
                           trial1$black_rating>trial1$white_rating & trial1$winner=="black","True","False" )


ggplot(trial1, aes(x=higherratedwinner, fill=as.factor(higherratedwinner) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")


stat<-trial1 %>% select(4)

sapply(stat, table)/nrow(stat) * 100

#61.58% of the time higher rated players won wich I consider it a very low number 


#Top 3 openings for white and black on win percentage

top3W<-games %>%
  select(7,15) 
 
top3W2<-top3W %>% 
  group_by(winner,opening_name) %>% 
  summarize(conteo=n())

top3W3<-top3W %>% 
  group_by(opening_name) %>% 
  summarize(conteo=n())

top3W2$conteot <- top3W3$conteo[match(top3W2$opening_name, top3W3$opening_name)]

top3W2 <- transform(top3W2, win_percentage = conteo / conteot)

#I will filter and use only the openings that each has won at least 30 times

top3W4<-top3W2 %>% 
  filter(conteo>29)

#The top 3 openings for White Where 

#Russian Game: Damiano Variation= 80% Win rate
#Pirc Defense #5= 74% Win rate
#Zukertort Opening: Queen's Gambit Invitation= 70% Win Rate

#Top 3 for black where

#King's Pawn Game= 71.43%
#Van't Kruijs Opening= 61.14%
#Sicilian Defense: Old Sicilian= 58.49%



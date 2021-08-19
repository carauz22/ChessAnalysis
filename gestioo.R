
#loading libraries
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(writexl)
library(lubridate)
library(skimr)
library(forcats)



#changing date to correct format

database <-automatizado %>% 
  mutate(fecha = dmy(FECHA))  


#separating day, week, year

database1<- database %>%
  mutate(year = year (fecha),
         month = month(fecha),
         day = day (fecha),
         week = week(fecha),
         wday= wday(fecha))

#repaired watches by unit and revenue

repaired<-database1 %>% filter(ESTADO == "Reparado")

#repaired by week

repairedbyweek<-repaired %>% filter(year==2021,week>24,week<27)

#watches pending by week

target<-c("Aprobado en espera de repuesto","Por cotizar","Cotizado")

pending<- filter(database1, ESTADO %in% target)

#watches pending by week

pendingbyweek<-pending %>% filter(year==2021,week>24,week<27)

#repiared watches unit total by week

nrow(repairedbyweek)

#revenue of repaired watches


sum(repairedbyweek[, 'TOTAL'])

#tpending watches total by week

nrow(pendingbyweek)

#revenue of pending watches

sum(pendingbyweek[, 'TOTAL'])

#total of units repaired vs pending

nrow(repairedbyweek)
nrow(pendingbyweek)

#total of revenue repaired vs pending

sum(repairedbyweek[, 'TOTAL'])
sum(pendingbyweek[, 'TOTAL'])

#contribution of sales by brand

brands<-database1 %>% 
  group_by(MARCA) %>% 
  summarise(total= sum(TOTAL))




#barplot top 5 brands


brands2<-brands %>% slice_max(total, n = 5)

brands2 %>%
  arrange(total) %>% 
  mutate(MARCA=factor(MARCA, levels=MARCA)) %>% 
  ggplot( aes(x=MARCA, y=total)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("") +
  theme_bw()

#barplot top 5 brands by week

brandsS<-database1 %>% filter(year==2021,week>24,week<27)

brandsS2<-marcasS %>% 
  group_by(MARCA) %>% 
  summarise(marcaS=sum(TOTAL))


brandsS3<-brandsS2 %>% slice_max(marcaS, n = 5)



brandsS3 %>%
  arrange(marcaS) %>% 
  mutate(MARCA=factor(MARCA, levels=MARCA)) %>% 
  ggplot( aes(x=MARCA, y=marcaS)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("") +
  theme_bw()



#barplot sales by weekday

weekd<-database1 %>% 
  group_by(wday) %>% 
  summarise(total= sum(TOTAL))

weekdays<-weekd %>% slice_max(total, n=5)

weekdays %>%
  ggplot( aes(x=wday, y=total)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("") +
  theme_bw()

out<-filter(automatizado,ÁREA =="Salida")

skim(out$TOTAL)

#Wacthes that are ready to give it back to clients

out2<-out %>% 
  group_by(MARCA) %>% 
  summarise(total= sum(TOTAL),
            conteo= n())

out1<-out2 %>% slice_max(total, n=5)

out1 %>% ggplot( aes(x=MARCA, y=total)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("") +
  theme_bw()

out1 <- transform(out1, gan_percentage = conteo / conteot)

out3<-out2 %>% slice_max(conteo, n=5)


out3 %>% ggplot( aes(x=MARCA, y=conteo)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("") +
  theme_bw()



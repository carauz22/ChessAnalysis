
#Loading Libraries

library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(writexl)
library(lubridate)
library(skimr)
library(forcats)
library(corrplot)
library(data.table)
library(quantmod)
library(hash)
library(gapminder)
library(GGally)


#Loading first database

global.food.prices.database.wfp.1 <- read.csv("~/Desktop/Project/Food Prices/Databases/global-food-prices-database-wfp-1.csv")
   
   
   #This dataset contains Global Food Prices data from the World Food Programme covering foods 
   #such as maize, rice, beans, fish, and sugar for 76 countries and some 1,500 markets.
   #It is updated weekly but contains to a large extent monthly data. 
   #The data goes back as far as 1992 for a few countries, 
   #although many countries started reporting from 2003 or thereafter.
   
   #Source: https://data.humdata.org/dataset/wfp-food-prices
   
   
   
   
   
   
   #Loading second database
   
   #This data set containts agriculture percent of GDP by year by country
   
   agriculture_percent_of_gdp <- read.csv("~/Desktop/Project/Food Prices/Databases/agriculture_percent_of_gdp.csv")
      View(agriculture_percent_of_gdp)
   
     #Source: https://data.worldbank.org/indicator/NV.AGR.TOTL.ZS
     
     
     
   
   #Loading third Data Set 
   
      population <- read_excel("Desktop/Project/Food Prices/Databases/population.xls")
      population2 <- read_excel("Desktop/Project/Food Prices/Databases/population2.xls")                                                                                            


#containts Population by year by country 

#Source: https://data.worldbank.org/indicator/SP.POP.TOTL
 
 
 
 

#Loading 4th database

      gdp_per_cap <- read.csv("~/Desktop/Project/Food Prices/Databases/gdp_per_cap.csv")
 View(gdp.per.cap)

#This database containts Gdp Per Capita by year by country
 
 #Source:https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
 
 
 
 
 
 
 
 
  #Cleaning data to join databases
 
 
 #database1: 
 
 #getting rid of rows that will not be used and renaming rows name
  
 gf<- global.food.prices.database.wfp.1 %>% rename("country" = "adm0_name", "year"= "mp_year","city" = "adm1_name","product"="cm_name")
 
 skim(gf)
 
 
 gf2<-gf %>% select(2,8,10,12,14,15,16,17) %>% filter(mp_price<10000,pt_name=="Retail")
 
 
 #summarizing data by products with same country and year.
 
 gf3<-gf2 %>% 
    group_by(product,country,year,cur_name,um_name) %>% 
    summarize(price=sum(mp_price),
              conteo=n()
              )
 
gf3 <- transform(gf3, price_average = price / conteo)

gf4<-gf3 %>% select(1:5,8)


#Only using Kilograms as it is the measurement that appears the most. 

gf5<-gf4 %>% filter(um_name=="KG",)

skim(gf5)

#changing all the results from diffrent types of currency to usd

unique.currency = unique(gf5$cur_name)
from <- unique.currency
to <- rep("USD",length(from))
conversion <- getQuote(paste0(from, to, "=X"))
summary(is.na(conversion$Last))
 
nans <- conversion %>%
   filter(is.na(Last))
nans

conversion$Last[is.na(conversion$Last)] = c(NIS,SSP)
current.price <-  conversion$Last
h= hash()
h[from]=current.price

conversion1<-conversion %>% select(2)


conversion2<-tibble::rownames_to_column(conversion1, "currency")


conversion3<-conversion2 %>% separate(currency, c("cur_name", "value"), "USD=X")

conversion4<-conversion3 %>% select(1,3)

gf05<-inner_join(gf5,conversion4,by="cur_name")

gf05<-transform(gf05,USD_price = price_average*Last)

gf7<-gf05 %>% select(1:3,8)

 
 #database2: 
 
#Cleaning database editing, so thtat years appear as a column
 

 colnames(agriculture_percent_of_gdp)[1] <- "country"
 
 agri_per <- gather(agriculture_percent_of_gdp,year,agr_gdp_per,-country)
 
agri_per$year <- strtoi(substring(agri_per$year,2))
View(agri_per)

agri_per[is.na(agri_per)] <- 0
  
agri_per2<-agri_per %>% filter(agr_gdp_per>0)

#Database3:

#Joining all years together and editing, so that years apper as a column

population01<-population %>% select(1,5:50)

colnames(population01)[1] <- "country"

population1 <- gather(population01,year,population,-country)


colnames(population2)[1]<-"country"

population02<- gather(population2,year,population,-country)

population03<-full_join(population1,population02)

population03 <- population03 %>%  
   mutate(year = as.numeric(year))

#Database4:

#Renaming columns to join tables

gdp_per_cap01<-gdp_per_cap %>% select(1,5:66)

colnames(gdp_per_cap01)[1] <- "country"

gdp_per_cap1 <- gather(gdp_per_cap01,year,gdp_per_cap,-country)

gdp_per_cap1$year <- strtoi(substring(gdp_per_cap1$year,2))
View(gdp_per_cap1)

gdp_per_cap1[is.na(gdp_per_cap1)] <- 0

gdp_per_cap_02<-gdp_per_cap1 %>% filter(gdp_per_cap>1)

#Joining tables
 
data1<-inner_join(gf7,agri_per2,by=c("country","year"))

data2<-inner_join(population03,gdp_per_cap_02,by=c("country","year"))

database<-inner_join(data1,data2,by=c("country","year"))

gapminder1<-gapminder %>% select(1,2)

gapminder2 <- unique( gapminder1 )

database<-inner_join(database,gapminder2,by=("country"))
   

database00<-database %>% separate(product, c("main_product", "description"), " ", extra = "merge")



 #Lets start by calculating statistics of our final database

skim(database)

#At the end we got data ranging from 1992-2018, but mostly from 2011-2018.
#The mean price of our items was 0.89 USD with a high sd of 1.32











#Lets Start an Analysis to see if an increase in years means and increase in product prices in general




database01<-database00 %>% 
  group_by(main_product,year) %>% 
  summarize(count=n(),
            total_price=sum(USD_price))

#Lets use the 5  objects with most data points for the linear regression to
#see how much time explains price of objects

database000<-database00 %>% 
   group_by(main_product) %>% 
   summarize(count=n(),)

targetd<-c("Rice","Maize","Wheat",
"Sorghum","Beans")

database02<-filter(database01, main_product %in% targetd)

database03<-transform(database02,ave_price = total_price/count)

database03<-database03 %>% select(1,2,5)

ggplot(database03, aes(x=year, y=ave_price, color=main_product)) + 
   geom_point(size=3) +
   theme_ipsum()
   
#There seems to be an increase in price in all the products as the years go by.

cor(database03$year, database03$ave_price)

test<-database03 %>% select(2,3)

regression<- lm(year ~ ave_price, data=test)

#Coefficients:
#   (Intercept)    ave_price  
#1997.48        23.28  

modelsummary<-summary(regression)

#R-squared: 0.49
#F-Stat: 105.7
#p-value: 0.0000000000000002

#I think time explaining 49% of the variables is a great result as we know many other 
#factors can affect food prices such as weather, disease outbreaks, war, and natural disasters.

#Now lets analyze products by Country and Continent to see in witch continent 
#the cost of top 5 products is higher


database04<-database00 %>% 
   group_by(main_product,year,country,continent) %>% 
   summarize( total_price=sum(USD_price))


database05<-database04 %>% filter(main_product=="Rice")

ddply(database05, .(continent), summarise, Sumx1 = sum(total_price))



database05<-database04 %>% 
   group_by(main_product,continent,year) %>% 
   summarise(conteo=n(),total=sum(total_price))


database05<-database05 %>% filter(main_product=="Rice")

database05<-transform(database05,ave_price = total/conteo)

database05<-database05 %>% select(1,2,3,6)

#I will filter the data so that all continents have the same data points wich is until 2013

database05<-database05 %>% filter(year>2012)

database06 <-  database05 %>%
   group_by(main_product,continent) %>%
   arrange(year,.by_group = T) %>%
   mutate(percent_change=round((ave_price-lag(ave_price))*100/
                               lag(ave_price),2),
          percent_change =replace_na(percent_change,0))


ggplot(database06, aes(x=year, y=ave_price,group=continent,color=continent)) +
   geom_line() 

ggplot(database06, aes(x=year, y=percent_change,group=continent,color=continent)) +
   geom_line() 

#Both Line charts gives us a lot of information of price by continent where in the first one we can see
#Rice is a lot more expensive on America than the other three continents, but price is dropping quick 
#having the biggest percent change. Also surpsingly we see continents acting very differently 
#as in Europe and Africa there has been a price increase where as in Asia and America there has been a price 
#drop




#now lets see of the other products have similiar behaviour


database07<-database04 %>% filter(main_product=="Beans")

ddply(database07, .(continent), summarise, Sumx1 = sum(total_price))



database07<-database04 %>% 
   group_by(main_product,continent,year) %>% 
   summarise(conteo=n(),total=sum(total_price))


database07<-database07 %>% filter(main_product=="Beans")

database07<-transform(database07,ave_price = total/conteo)

database07<-database07 %>% select(1,2,3,6)


database07<-database07 %>% filter(year>2012)

database07 <-  database07 %>%
   group_by(main_product,continent) %>%
   arrange(year,.by_group = T) %>%
   mutate(percent_change=round((ave_price-lag(ave_price))*100/
                                  lag(ave_price),2),
          percent_change =replace_na(percent_change,0))


ggplot(database07, aes(x=year, y=ave_price,group=continent,color=continent)) +
   geom_line() 

ggplot(database07, aes(x=year, y=percent_change,group=continent,color=continent)) +
   geom_line() 



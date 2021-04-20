#Biostats
#Emma Buckley
#Day 1
#Viewing data and Basic Stats
#19 April 2021

?BOD #to see info about the data
BOD #view the data
str(BOD) #to see the types of data
summary(BOD)

str(InsectSprays)
View(InsectSprays)
unique(InsectSprays$spray)# to see the different levels of spray

?Loblolly
str(Loblolly)

?HairEyeColor
HairEyeColor
str(HairEyeColor)

Seatbelts
str(Seatbelts)
View(Seatbelts)
?Seatbelts

cars
str(cars)

?esoph
str(esoph)

?JohnsonJohnson
str(JohnsonJohnson)
View(JohnsonJohnson)

?volcano
str(volcano)
View(volcano)


#Load built-in data
pines <- Loblolly
str(pines) #structure of the data
class(pines$height)#what type of class a particular column is

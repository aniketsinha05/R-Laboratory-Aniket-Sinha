library(dplyr)
data("iris")
head(iris)
str(iris)
summary(iris)

data("mtcars")
head(mtcars)
str(mtcars)

data("Titanic")
Titanic

sum(is.na(iris))
colSums(is.na(mtcars))

iris %>%
  group_by(Species)%>%
  summarise(
    Avg_Sepal_Length=mean(Sepal.Length),
    Avg_Speal_Length=mean(Petal.Length)
  )


#Exercise Questions:-
#Importing Dataset and checking its structure
data("airquality")
str(airquality)

#number of missing values in airquality?
sum(is.na(airquality))

#summary of mtcars and interpretting the output
summary(mtcars)

#mean horsepower in mtcars
mean(mtcars$hp)
data(iris)
head(iris)
library(dplyr)
iris%>%
  group_by(Species)%>%
  summarise(
    Avg_Sepal_Width=mean(Sepal.Width)
  )


#Exercise Questions
data(airquality)
str(airquality)

sum(is.na(airquality))

summary(mtcars)

mean(mtcars$hp)
library(dplyr)
iris%>%
  group_by(Species)%>%
  summarise(
    Avg_Sepal_Width=mean(Sepal.Width)
  )

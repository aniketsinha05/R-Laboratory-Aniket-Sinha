print("Hello and Welcome to R Programming")
x<-25
y<-5

sumval<-x+y
diffval<-x-y
prodval<-x*y
divval<-x/y

sumval
diffval
prodval
divval

greaterCheck<- x>y
equalCheck<- x==y

print(greaterCheck)
print(equalCheck)

data("iris")
head(iris)

str(iris)

summary(iris)


#Exercise Questions of Practical 1:-

#1. Display the first 15 rows of mtcars dataset
data("mtcars")

head(mtcars,15)

#2. Find Maximum and Minimum Value of Sepal.Length from iris
max(iris$Sepal.Length)
min(iris$Sepal.length)

#3. Calculate the mean of the variable mpg in mtcars
mean(mtcars$mpg)

#4. Display the structure of the airquality dataset
data("airquality")
summary(airquality)

#5. Check whether the number 100 is greater than 50 in R.
checknum<-100>50
print(checknum)

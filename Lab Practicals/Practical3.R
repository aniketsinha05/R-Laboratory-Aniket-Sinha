library(dplyr)
data("airquality")
head(airquality)

sum(is.na(airquality))
colSums(is.na(airquality))

airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)

airquality$Solar.R[is.na(airquality$Solar.R)] <- mean(airquality$Solar.R, na.rm = TRUE)

data("iris")

iris_with_duplicates<-rbind(iris,iris[1:5,])

nrow(iris_with_duplicates)

iris_clean<-distinct(iris_with_duplicates)
nrow(iris_clean)

iris_clean$Species <- tolower(as.character(iris_clean$Species))
iris_clean$Species <- as.factor(iris_clean$Species)

head(iris_clean)

data(airquality)
sum(is.na(airquality))

#Exercise Questions:-

library(dplyr)

# Load dataset
data("airquality")
head(airquality)

# 1. Check for missing values
sum(is.na(airquality))
colSums(is.na(airquality))

# 2. Handle missing values (mean, median)
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)] <- median(airquality$Solar.R, na.rm = TRUE)

# 3. Remove duplicates (iris example)
data("iris")
iris_with_duplicates <- rbind(iris, iris[1:5, ])
nrow(iris_with_duplicates)

iris_clean <- distinct(iris_with_duplicates)
nrow(iris_clean)

# 4. Standardize formats
iris_clean$Species <- tolower(as.character(iris_clean$Species))
iris_clean$Species <- as.factor(iris_clean$Species)

# View cleaned dataset
head(iris_clean)


# 1. Variables & Data Types:-

# 1. Create Variables to store:

  #Your Name (character)
name <- "Aniket"
  #Your age (numeric)
age <- 20
  #Whether you are a student (logical)
student <- TRUE

# 2. Create a vector containing the numbers 1 to 10.

v1 <- c(1,2,3,4,5,6,7,8,9,10)

# 3. Create a sequence from 5 to 50 with a step size of 5.

seq1 <- seq(5,50,by=5)

# 4. Store the names of 5 fruits in a character vector and display the second and fourth fruit.

seqFruits <- c("Apple","Banana","Orange","Guava","Papaya")
seqFruits[2]
seqFruits[4]
seqFruits[c(2,4)]

# 5. Create a numeric vector of 10 random numbers between 1 and 100, then find:

seqRandom <- sample(1:100, 10)
print(seqRandom)

  # a. Maximum, Minimum & Mean Value
max(seqRandom)
min(seqRandom)
mean(seqRandom)

# 6. Create a data frame with columns: Name, Age, Marks. Enter at least 5 records.

df <- data.frame(
  Name = c("Aniket", "Sinha", "1", "2", "3"),
  Age = c(20, 21, 19, 22, 20),
  Marks = c(85, 92, 78, 88, 95)
)

# 7. Write code to sort the data frame by Marks in descending order.

sortedDF <- df[order(-df$Marks), ]

# 2. Operators in R:-

10+5
10 - 5
10 * 5
10 / 5
10 %% 3
10 %/% 3

15>10
7==7

a <- c(2, 4, 6, 8)
b <- c(1, 3, 5, 7)

a+b
a-b
a*b

a[a>5]
b[b<=4]

5 %in% a

x <- c(TRUE, FALSE, TRUE, FALSE)
y <- c(TRUE, TRUE, FALSE, FALSE)
x&y
x|y
!x
!y



# 3. Loops in R:-
seq100 <- seq(1,100)
for (i in 1:10) {
  print(seq100[i])
}
i=1
while (i<=100) {
  print(seq100[i])
  i=i+1
}
i=1
while(i<=50){
  print(seq100[i])
  i=i+1
}
i=7
while(i<=70){
  print(seq100[i])
  i=i+7
}

n=4
fact=1
i=n
while(i>=1){
  fact=fact*i
  i=i-1
}
print(fact)

for(i in 1:4){
  for(j in 1:i){
    cat("*")
  }
  cat("\n")
}

# 4. Conditionals in R:-
num <- -5  
if (num > 0) {
  print("The number is positive")
} else if (num < 0) {
  print("The number is negative")
} else {
  print("The number is zero")
}

num <- 7  
if (num %% 2 == 0) {
  print("The number is even")
} else {
  print("The number is odd")
}

year <- 2024  
if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
  print("The year is a leap year")
} else {
  print("The year is not a leap year")
}

marks <- 45  
if (marks >= 40) {
  print("Pass")
} else {
  print("Fail")
}

marks <- 85  
if (marks >= 90) {
  print("Grade: A")
} else if (marks >= 75) {
  print("Grade: B")
} else if (marks >= 60) {
  print("Grade: C")
} else {
  print("Grade: Fail")
}

# 5. Functions in R:-
# 1. Add two numbers
a <- 5
b <- 3
sum_result <- a + b
print(sum_result)

# 2. Square a number
n <- 4
square_result <- n * n
print(square_result)

# 3. Factorial of a number using recursion
num <- 5
factorial_result <- 1
for (i in 1:num) {
  factorial_result <- factorial_result * i
}
print(factorial_result)

# 4. Check if a number is prime
num_to_check <- 7
is_prime <- TRUE
if (num_to_check <= 1) {
  is_prime <- FALSE
} else {
  for (i in 2:(num_to_check - 1)) {
    if (num_to_check %% i == 0) {
      is_prime <- FALSE
      break
    }
  }
}
print(is_prime)

# 5. Calculate mean, median, and standard deviation of a vector
vector <- c(1, 2, 3, 4, 5)
mean_result <- mean(vector)
median_result <- median(vector)
sd_result <- sd(vector)

print(mean_result)
print(median_result)
print(sd_result)

# 6. Find the top 5 highest values in a column of a data frame
data <- data.frame(scores = c(90, 85, 95, 88, 92, 99, 73))
sorted_scores <- sort(data$scores, decreasing = TRUE)
top_5_scores <- head(sorted_scores, 5)
print(top_5_scores)

rm(list = ls())


#6th Question using adult-data.txt
cols <- c("age","workclass","fnlwgt","education","education_num","marital_status","occupation","relationship","race","sex","capital_gain","capital_loss","hours_per_week","native_country","income")
adult <- read.csv("C:/Users/User/Documents/ARS/datasets_lab1/adult-data.txt",header=FALSE,col.names=cols,strip.white=TRUE,na.strings="?",stringsAsFactors=FALSE)

print(head(adult,10))
View((head(adult,10)))

str(adult)
print(mean(adult$age, na.rm=TRUE))
print(mean(adult$age, na.rm=FALSE))

#print(mean(adult$education))

print(table(adult$income))
?table
?mean
?names
occ_counts<-sort(table(adult$occupation),decreasing = TRUE)
most_common_occupation<-names(occ_counts)[1]
print(most_common_occupation)

print(tapply(adult$hours_per_week,adult$income, mean, na.rm=TRUE))
?tapply


edu_counts<- sort(table(adult$education),decreasing = TRUE)
barplot(edu_counts,las=2,cex.names=0.7,main="Education Level Distribution",ylab="Count")

barplot(edu_counts,las=1,cex.names=0.7,main="Education Level Distribution",ylab="Count")

?barplot


?with

tbl<- with(adult,table(native_country,income))
?tbl
prop_over50k <- tbl[2,">=50k"] / rowSums(tbl)
prop_over50k
?rowSums



# Load the IPL dataset
# Load the IPL dataset
ipl <- read.csv("batting_bowling_ipl_bat.csv")

# Display the first 10 rows
head(ipl, 10)

# Top 5 players with highest total runs
top5_runs <- ipl[order(-ipl$Runs), c("Name", "Runs")]
head(top5_runs, 5)

# Player with highest batting average
ipl[which.max(ipl$Ave), c("Name", "Ave")]

# Bar chart for Top 10 players by Strike Rate
top10_sr <- ipl[order(-ipl$SR), c("Name", "SR")]
top10_sr <- head(top10_sr, 10)

barplot(top10_sr$SR,
        names.arg = top10_sr$Name,
        las = 2,
        col = "skyblue",
        main = "Top 10 Players by Strike Rate",
        ylab = "Strike Rate")

# Correlation between Runs and Fours (as Matches column not available)
cor(ipl$Runs, ipl$Fours, use = "complete.obs")


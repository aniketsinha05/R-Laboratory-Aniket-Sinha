# ==========================================================
# 1. Accessing Various File Types
# ==========================================================

# 1(a) Load a CSV file into a dataframe
facebook_data <- read.csv("dataset_Facebook.csv", sep = ";")

# 1(b) Load an Excel file into a dataframe
install.packages("readxl")
library(readxl)
lungcap_data <- read_excel("LungCap_Dataset.xls")

# 1(c) Load a text file separated by commas
text_data <- read.table("train_titanic.csv", sep = ",", header = TRUE)

# 2. Display the first 10 rows of each dataset
head(facebook_data, 10)
head(lungcap_data, 10)
head(text_data, 10)

# 3. Check the data type of each column in Titanic dataset
titanic <- read.csv("train_titanic.csv")
str(titanic)

# 4. Save Titanic dataset into a new CSV file after filtering only survivors
titanic_survivors <- subset(titanic, Survived == 1)
write.csv(titanic_survivors, "titanic_survivors.csv", row.names = FALSE)


# ==========================================================
# 2. Data Selection & Manipulation
# ==========================================================

# --------------------------
# Titanic dataset
# --------------------------
titanic <- read.csv("titanic.csv")

# (a) Select only Name, Sex, Age, and Survived
titanic_selected <- titanic[, c("Name", "Sex", "Age", "Survived")]
head(titanic_selected, 10)

# (b) Show passengers older than 50
titanic_age50 <- subset(titanic, Age > 50)
head(titanic_age50, 10)

# (c) Count the number of survivors in each passenger class
table(titanic$Pclass, titanic$Survived)


# --------------------------
# Facebook dataset
# --------------------------
facebook <- read.csv("dataset_Facebook.csv")

# (a) Find the post with maximum likes
facebook[which.max(facebook$like), ]

# (b) Average number of shares per post
mean(facebook$share, na.rm = TRUE)

# (c) Create new column Engagement = Likes + Comments + Shares
facebook$Engagement <- facebook$like + facebook$comment + facebook$share
head(facebook, 10)


# --------------------------
# Lung Capacity dataset
# --------------------------
lungcap <- read_excel("LungCap_Dataset.xls")

# (a) Select children below age 12
lungcap_children <- subset(lungcap, Age < 12)
head(lungcap_children, 10)

# (b) Group by Gender and calculate average Lung Capacity
aggregate(LungCap ~ Gender, data = lungcap, FUN = mean)

# (c) Find the child with maximum Lung Capacity
lungcap[which.max(lungcap$LungCap), ]


# ==========================================================
# 3. Data Manipulation (Using dplyr/base R)
# ==========================================================
library(dplyr)

# 1. Rename the columns of Titanic dataset to lowercase
names(titanic) <- tolower(names(titanic))

# 2. Sort the Titanic dataset by Age in descending order
titanic_sorted <- titanic[order(-titanic$age), ]
head(titanic_sorted, 10)

# 3. Create AgeGroup column
titanic$AgeGroup <- cut(titanic$age,
                        breaks = c(-Inf, 12, 18, 59, Inf),
                        labels = c("Child", "Teen", "Adult", "Senior"))

# 4. Calculate mean Fare by Pclass and Survived
aggregate(fare ~ pclass + survived, data = titanic, FUN = mean)

# 5. Facebook: group posts by Type and compute average likes
aggregate(like ~ Type, data = facebook, FUN = mean)


# ==========================================================
# 4. Handling Missing Values
# ==========================================================

# 1. Identify columns with missing values in Titanic
colSums(is.na(titanic))

# 2. Replace missing Age values with median Age
titanic$age[is.na(titanic$age)] <- median(titanic$age, na.rm = TRUE)

# 3. Drop rows where Embarked is missing
titanic <- titanic[!is.na(titanic$embarked), ]

# 4. For LungCap, fill missing LungCap with mean
lungcap$LungCap[is.na(lungcap$LungCap)] <- mean(lungcap$LungCap, na.rm = TRUE)

# 5. Explanation
# - Listwise deletion: removes entire rows with any missing value
# - Pairwise deletion: uses all available data for each analysis (may use different N per variable pair)
# - Mean/median imputation: replaces missing values with mean/median of that column


# ==========================================================
# 5. Exploratory Data Analysis (EDA) with ggplot2
# ==========================================================
library(ggplot2)

# (1) Histogram of Age (Titanic)
ggplot(titanic, aes(x = age)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of Age", x = "Age", y = "Count")

# (2) Bar chart of Pclass vs passenger count
ggplot(titanic, aes(x = factor(pclass))) +
  geom_bar() +
  labs(title = "Passenger Count by Pclass", x = "Pclass", y = "Count")

# (3) Boxplot of LungCap by Gender
ggplot(lungcap, aes(x = Gender, y = LungCap)) +
  geom_boxplot() +
  labs(title = "Lung Capacity by Gender", x = "Gender", y = "Lung Capacity")

# (4a) Scatter plot of Likes vs Comments (Facebook)
ggplot(facebook, aes(x = like, y = comment)) +
  geom_point() +
  labs(title = "Likes vs Comments", x = "Likes", y = "Comments")

# (4b) Histogram of Shares (Facebook)
ggplot(facebook, aes(x = share)) +
  geom_histogram(binwidth = 50) +
  labs(title = "Histogram of Shares", x = "Shares", y = "Count")

# (5) Pie chart of survivors vs non-survivors (Titanic)
titanic_survival <- as.data.frame(table(titanic$survived))
colnames(titanic_survival) <- c("Survived", "Count")

ggplot(titanic_survival, aes(x = "", y = Count, fill = Survived)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Survivors vs Non-Survivors")


# ==========================================================
# 6. Detecting and Handling Outliers
# ==========================================================

# (1a) Boxplot of Fare (Titanic)
ggplot(titanic, aes(y = fare)) +
  geom_boxplot() +
  labs(title = "Boxplot of Fare", y = "Fare")

# (1b) Boxplot of Age by Survival (Titanic)
ggplot(titanic, aes(x = factor(survived), y = age)) +
  geom_boxplot() +
  labs(title = "Age by Survival", x = "Survived", y = "Age")

# Q: Which group has more spread?
# -> Compare IQR visually. Usually "Not Survived" has wider spread.

# (2a) Boxplot of Likes (Facebook)
ggplot(facebook, aes(y = like)) +
  geom_boxplot() +
  labs(title = "Boxplot of Likes", y = "Likes")

# (2b) Combined boxplot of Likes, Shares, and Comments
library(reshape2)
facebook_long <- melt(facebook[, c("like", "share", "comment")])

ggplot(facebook_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplots of Likes, Shares, and Comments", x = "Metric", y = "Value")


library(dplyr)
data("iris")

# 1. t-test (setosa vs versicolor)
t_test_result <- t.test(Sepal.Length ~ Species,
                        data = iris %>% filter(Species %in% c("setosa","versicolor")))
t_test_result

# 2. ANOVA
anova_model <- aov(Sepal.Length ~ Species, data = iris)
summary(anova_model)

# 3. Correlation
correlation <- cor(iris$Sepal.Length, iris$Petal.Length)
correlation

# 4. Correlation test
cor_test <- cor.test(iris$Sepal.Length, iris$Petal.Length)
cor_test


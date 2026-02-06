install.packages("readxl")   # only if not installed
install.packages("tidyr")    # for reshaping

library(readxl)
library(tidyr)

# session > Set Working Directory > Choose Directory…

data <- read_excel("Lec4_LambDiet_data.xlsx", sheet = 1)
head(data)

#ANOVA in R wants one response column + one factor column.
diet_long <- pivot_longer(
  data,
  cols = everything(),
  names_to = "diet",
  values_to = "value"
)

# remove missing values
diet_long <- na.omit(diet_long)
diet_long

# boxplots
boxplot(value ~ diet,
        data = diet_long,
        xlab = "diet",
        ylab = "weight gain",
        main = "Side-by-Side Boxplot")
means <- tapply(diet_long$value, diet_long$diet, mean)
points(seq_along(means), means,
       pch = 18, col = "red", cex = 1.5)

#ANOVA analysis
anova_model <- aov(value ~ diet, data = diet_long)
summary(anova_model)

#residual analysis
par(mfrow = c(2, 2))
plot(anova_model)
par(mfrow = c(1,1))

# bartlett's test for equal variance
bartlett.test(value ~ diet, data = diet_long)
# levene's test
library(car)
leveneTest(value ~ diet, data = diet_long)




#2415091115_I MADE GILANG ARYDHANA RAHJASA_IKI SI 24

# Install the necessary packages (if not already installed)
install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")

# Load the libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Read the data from the Excel file
data <- read_excel("C:/Users/LOQ/Downloads/UAS STATPROB/UAS_DATASET_GAME.xlsx", sheet = "DATA")

names(data)

# Assumption Test

# a. Normality test for the entire dataset
#1. Age normality test
shapiro_test <- shapiro.test(data$`Age`)
cat("Normality test for entire data(Age) : p-value =", shapiro_test$p.value, "\n")
#2. Play Time normality test
shapiro_test <- shapiro.test(data$`Play_time`)
cat("Normality test for entire data (Play_time): p-value =", shapiro_test$p.value, "\n")
#3. Income normality test
shapiro_test <- shapiro.test(data$`Income`)
cat("Normality test for entire data (income) : p-value =", shapiro_test$p.value, "\n")



# b. Normality test for each data group (based on Genre)
# Normality test per play time group
normality_per_group <- tapply(data$`Play_time`, data$Genre, function(x) shapiro.test(x)$p.value)
cat("Normality test per group (Play Time):\n")
print(normality_per_group)

# Normality test per age group
normality_per_group_age <- tapply(data$`Age`, data$Genre, function(x) shapiro.test(x)$p.value)
cat("Normality test per group (Age):\n")
print(normality_per_group_age)

# Normality test per income group
normality_per_group_income <- tapply(data$`Income`, data$Genre, function(x) shapiro.test(x)$p.value)
cat("Normality test per group (Income):\n")
print(normality_per_group_income)


# c. Variance Homogeneity Test using Bartlett's Test

# Variance Homogeneity Test for Play Time (hours)
bartlett_test_play_time <- bartlett.test(`Play_time` ~ Genre, data = data)
cat("Hasil Uji Homogenitas Varians untuk Play Time (jam) berdasarkan Genre: p-value =", bartlett_test_play_time$p.value, "\n")

# Variance Homogeneity Test for Age
bartlett_test_age <- bartlett.test(`Age` ~ Genre, data = data)
cat("Hasil Uji Homogenitas Varians untuk Age (tahun) berdasarkan Genre: p-value =", bartlett_test_age$p.value, "\n")

# Variance Homogeneity Test for Income
bartlett_test_income <- bartlett.test(`Income` ~ Genre, data = data)
cat("Hasil Uji Homogenitas Varians untuk Income (Rp) berdasarkan Genre: p-value =", bartlett_test_income$p.value, "\n")


# One-Way ANOVA


# 1. One-Way ANOVA for Play Time (hours) by Genre
anova_play_time <- aov(`Play_time` ~ Genre, data = data)
cat("Hasil One-Way ANOVA untuk Play Time (jam):\n")
summary(anova_play_time)

# 2. One-Way ANOVA for Age by Genre
anova_age <- aov(`Age` ~ Genre, data = data)
cat("Hasil One-Way ANOVA untuk Age (tahun):\n")
summary(anova_age)

# 3. One-Way ANOVA for Income by Genre
anova_income <- aov(`Income` ~ Genre, data = data)
cat("Hasil One-Way ANOVA untuk Income (Rp):\n")
summary(anova_income)


# Visualisation (Charts using ggplot2)


# 1. Boxplot for Play time by Genre     
ggplot(data, aes(x = Genre, y = Play_time, fill = Genre)) +
  geom_boxplot(color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Play time distribution by Genre",
       x = "Genre", y = "Play Time (jam)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")

# 2. Boxplot for Age by Genre
ggplot(data, aes(x = Genre, y = Age, fill = Genre)) +
  geom_boxplot(color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Age distribution by Genre",
       x = "Genre", y = "Age (tahun)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")

# 3. Boxplot for Income by Genre
ggplot(data, aes(x = Genre, y = Income, fill = Genre)) +
  geom_boxplot(color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Income distribution by Genre",
       x = "Genre", y = "Income (Rp)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")


# Post-Hoc Test (Tukey HSD)


# Taking the p-value of the ANOVA result
anova_play_time_p_value <- summary(anova_play_time)[[1]][1, "Pr(>F)"]
anova_age_p_value <- summary(anova_age)[[1]][1, "Pr(>F)"]
anova_income_p_value <- summary(anova_income)[[1]][1, "Pr(>F)"]

# Tukey HSD for Play time if the result is significant
if (anova_play_time_p_value < 0.05) {
  tukey_play_time <- TukeyHSD(anova_play_time)
  cat("Hasil Tukey HSD untuk Play Time:\n")
  print(tukey_play_time)
} else {
  cat("ANOVA untuk Play Time tidak signifikan (p-value =", anova_play_time_p_value, "), tidak dilakukan Tukey HSD.\n")
}

# Tukey HSD for Age if the result is significant
if (anova_age_p_value < 0.05) {
  tukey_age <- TukeyHSD(anova_age)
  cat("Hasil Tukey HSD untuk Age:\n")
  print(tukey_age)
} else {
  cat("ANOVA untuk Age tidak signifikan (p-value =", anova_age_p_value, "), tidak dilakukan Tukey HSD.\n")
}

# Tukey HSD for Income if the result is significant
if (anova_income_p_value < 0.05) {
  tukey_income <- TukeyHSD(anova_income)
  cat("Hasil Tukey HSD untuk Income:\n")
  print(tukey_income)
} else {
  cat("ANOVA untuk Income tidak signifikan (p-value =", anova_income_p_value, "), tidak dilakukan Tukey HSD.\n")
}

}


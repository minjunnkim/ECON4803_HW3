# Setup

data <- read.csv('india.csv')

female_villages <- data[data$female == 1, ]
male_villages <- data[data$female == 0, ]

mean_water_female <- mean(female_villages$water, na.rm = TRUE)
mean_water_male <- mean(male_villages$water, na.rm = TRUE)

diff_mean_water <- mean_water_female - mean_water_male

mean_irrigation_female <- mean(female_villages$irrigation, na.rm = TRUE)
mean_irrigation_male <- mean(male_villages$irrigation, na.rm = TRUE)

diff_mean_irrigation <- mean_irrigation_female - mean_irrigation_male

cat("Difference in means for water facilities:", round(diff_mean_water, 2), "\n")
cat("Difference in means for irrigation facilities:", round(diff_mean_irrigation, 2), "\n")

# 1
# (a)

var_water_female <- var(female_villages$water, na.rm = TRUE)
var_water_male <- var(male_villages$water, na.rm = TRUE)

n_female <- sum(!is.na(female_villages$water))
n_male <- sum(!is.na(male_villages$water))

SE_diff_mean_water <- sqrt((var_water_female / n_female) + (var_water_male / n_male))

cat("Standard Error of the difference-in-means estimator for water facilities:", round(SE_diff_mean_water, 2), "\n")

# (b)

z_score_water <- diff_mean_water / SE_diff_mean_water

cat("Z-score for the test on water facilities:", round(z_score_water, 2), "\n")

# (d)

p_value_water <- 2 * (1 - pnorm(abs(z_score_water)))

cat("P-value for the test on water facilities:", round(p_value_water, 4), "\n")

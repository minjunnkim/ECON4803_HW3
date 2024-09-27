# Setup

data <- read.csv('india.csv')

mean_water_female <- mean(data$water[data$female == 1], na.rm = TRUE)
mean_water_male <- mean(data$water[data$female == 0], na.rm = TRUE)

diff_means_water <- mean_water_female - mean_water_male

mean_irrigation_female <- mean(data$irrigation[data$female == 1], na.rm = TRUE)
mean_irrigation_male <- mean(data$irrigation[data$female == 0], na.rm = TRUE)

diff_means_irrigation <- mean_irrigation_female - mean_irrigation_male

cat("Difference in means for water facilities:", round(diff_means_water, 2), "\n")
cat("Difference in means for irrigation facilities:", round(diff_means_irrigation, 2), "\n")

# 1
# (a)

sd_water_female <- sd(data$water[data$female == 1], na.rm = TRUE)
sd_water_male <- sd(data$water[data$female == 0], na.rm = TRUE)

n_female <- sum(data$female == 1, na.rm = TRUE)
n_male <- sum(data$female == 0, na.rm = TRUE)

# Calculate the standard error of the difference-in-means
std_error_water <- sqrt((sd_water_female^2 / n_female) + (sd_water_male^2 / n_male))

cat("Standard Error of the difference-in-means estimator for water facilities:", round(std_error_water, 2), "\n")

# (b)

z_score_water <- diff_means_water / std_error_water

cat("Z-score for the test on water facilities:", round(z_score_water, 2), "\n")

# (d)

p_value_water <- 2 * (1 - pnorm(abs(z_score_water)))

cat("P-value for the test on water facilities:", round(p_value_water, 4), "\n")

# 2
# (a)

null_hypothesis_diff <- -1

z_score_water_null <- (diff_means_water - null_hypothesis_diff) / std_error_water

cat("Z-score for the test with hypothesized difference of -1:", round(z_score_water_null, 2), "\n")

# (b)
p_value_water_null <- 2 * (1 - pnorm(abs(z_score_water_null)))

cat("P-value for the test with hypothesized difference of -1:", round(p_value_water_null, 4), "\n")


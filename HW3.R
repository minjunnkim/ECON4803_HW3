# Part I

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

# 3
# (a)

z_critical <- qnorm(0.975)
margin_of_error <- z_critical * std_error_water

ci_lower <- diff_means_water - margin_of_error
ci_upper <- diff_means_water + margin_of_error

cat("95% Confidence Interval: [", round(ci_lower, 2), ", ", round(ci_upper, 2), "]\n")

# 4
# (a)

sd_irrigation_female <- sd(data$irrigation[data$female == 1], na.rm = TRUE)
sd_irrigation_male <- sd(data$irrigation[data$female == 0], na.rm = TRUE)

n_female_irrigation <- sum(data$female == 1, na.rm = TRUE)
n_male_irrigation <- sum(data$female == 0, na.rm = TRUE)

std_error_irrigation <- sqrt((sd_irrigation_female^2 / n_female_irrigation) + (sd_irrigation_male^2 / n_male_irrigation))

z_critical <- qnorm(0.975)
margin_of_error_irrigation <- z_critical * std_error_irrigation

ci_lower_irrigation <- diff_means_irrigation - margin_of_error_irrigation
ci_upper_irrigation <- diff_means_irrigation + margin_of_error_irrigation

cat("95% Confidence Interval for Irrigation Facilities: [", round(ci_lower_irrigation, 2), ", ", round(ci_upper_irrigation, 2), "]\n")

# 5
# (a)

data_male <- data[data$female == 0, ]

plot(data_male$water, data_male$irrigation,
     xlab = "New or Repaired Drinking Water Facilities",
     ylab = "New or Repaired Irrigation Facilities",
     main = "Scatter Plot of Drinking Water vs. Irrigation Facilities (Villages without Female Leaders)")

# (b)

covariance_male <- cov(data_male$water, data_male$irrigation, use = "complete.obs")

cat("Covariance (without female leaders):", round(covariance_male, 2), "\n")

# (c)

correlation_male <- cor(data_male$water, data_male$irrigation, use = "complete.obs")

cat("Correlation (without female leaders):", round(correlation_male, 2), "\n")

# (d)

z <- 0.5 * log((1 + correlation_male) / (1 - correlation_male))

se_z <- 1 / sqrt(nrow(data_male) - 3)

z_critical <- qnorm(0.975)

ci_lower_z <- z - z_critical * se_z
ci_upper_z <- z + z_critical * se_z

ci_lower_correlation <- (exp(2 * ci_lower_z) - 1) / (exp(2 * ci_lower_z) + 1)
ci_upper_correlation <- (exp(2 * ci_upper_z) - 1) / (exp(2 * ci_upper_z) + 1)

cat("95% Confidence Interval for Correlation: [", round(ci_lower_correlation, 2), ", ", round(ci_upper_correlation, 2), "]\n")

# Part II

# 6
# (a)

data_online_prices <- read.csv("online_prices.csv")

correlation_prices <- cor(data_online_prices$price, data_online_prices$price_online, use = "complete.obs")

cat("Correlation between price in-store and online:", round(correlation_prices, 5), "\n")

# (c)

n <- sum(!is.na(data_online_prices$price) & !is.na(data_online_prices$price_online))

se_correlation <- sqrt((1 - correlation_prices^2) / (n - 2))

cat("Standard error of the sample correlation coefficient:", round(se_correlation, 5), "\n")

# (d)

z_correlation <- 0.5 * log((1 + correlation_prices) / (1 - correlation_prices))

z_critical <- qnorm(0.975)

ci_lower_z <- z_correlation - z_critical * se_correlation
ci_upper_z <- z_correlation + z_critical * se_correlation

ci_lower_correlation <- (exp(2 * ci_lower_z) - 1) / (exp(2 * ci_lower_z) + 1)
ci_upper_correlation <- (exp(2 * ci_upper_z) - 1) / (exp(2 * ci_upper_z) + 1)

cat("95% Confidence Interval for the correlation: [", round(ci_lower_correlation, 5), ", ", round(ci_upper_correlation, 5), "]\n")

# 7
# (a)

data_online_prices$percent_difference <- ((data_online_prices$price - data_online_prices$price_online) / data_online_prices$price) * 100

mean_percent_difference <- mean(data_online_prices$percent_difference, na.rm = TRUE)

cat("Sample mean of percent_difference:", round(mean_percent_difference, 2), "%\n")

# (c)

n_percent_difference <- sum(!is.na(data_online_prices$percent_difference))

sd_percent_difference <- sd(data_online_prices$percent_difference, na.rm = TRUE)

se_percent_difference <- sd_percent_difference / sqrt(n_percent_difference)

cat("Standard error of the sample mean of percent_difference:", round(se_percent_difference, 5), "\n")

# (d)

margin_of_error_percent_difference <- qnorm(0.975) * se_percent_difference
ci_lower_percent_difference <- mean_percent_difference - margin_of_error_percent_difference
ci_upper_percent_difference <- mean_percent_difference + margin_of_error_percent_difference

cat("95% Confidence Interval for the mean of percent_difference: [", round(ci_lower_percent_difference, 5), ", ", round(ci_upper_percent_difference, 5), "]\n")

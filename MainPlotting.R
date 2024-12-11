# Load required library
library(ggplot2)

# Calculate the average price at each time step
average_price <- colMeans(matrix3)

# Plot the average price over the days
days <- 0:(ncol(matrix3) - 1)  # Days correspond to columns
plot(days, average_price, type = "l", col = "blue", lwd = 2,
     xlab = "Days", ylab = "Average Price",
     main = "Average Price Over Days")

# Add gridlines for better visualization
grid()

# Plot the distribution of prices on the last day
last_day_prices <- matrix3[, ncol(matrix3)]
hist(last_day_prices, breaks = 50, col = "lightblue",
     main = "Distribution of Prices on the Last Day",
     xlab = "Price", ylab = "Frequency", border = "white")

# Optional: Add a density curve to the histogram
lines(density(last_day_prices), col = "red", lwd = 2)

# Save plots to files (optional)
dev.copy(png, filename = "average_price_plot.png")
dev.off()

dev.copy(png, filename = "price_distribution_last_day.png")
dev.off()

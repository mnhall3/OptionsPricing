# Load required library
library(ggplot2)

# Calculate the average price at each time step
average_price <- colMeans(matrix6)

# Plot the average price over the days
days <- 0:(ncol(matrix6) - 1)  # Days correspond to columns
plot(days, average_price, type = "l", col = "blue", lwd = 2,
     xlab = "Trading Days (From Nov 18, 2022)", ylab = "Average Price ($)",
     main = "Tesla Average Price (p = -0.5) \n Nov 22, 2022 - Jan 22, 2023")

# Add gridlines for better visualization
grid()

# Plot the distribution of prices on the last day
last_day_prices <- matrix6[, ncol(matrix6)]
hist(last_day_prices, breaks = 300, col = "lightblue", 
     main = "Tesla Distribution of Prices on the Last Day \n (Jan 22, 2023)",
     xlab = "Price ($)", ylab = "Frequency", border = "white", xlim = range(0,1000))

# Optional: Add a density curve to the histogram
# lines(density(last_day_prices), col = "red", lwd = 2)

# Save plots to files (optional)
dev.copy(png, filename = "average_price_plot.png")
dev.off()

dev.copy(png, filename = "price_distribution_last_day.png")
dev.off()

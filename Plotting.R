# Calculate the average price at each time step
average_price <- colMeans(matrix6)

# Calculate the standard deviation at each time step
std_dev_per_day <- apply(matrix6, 2, sd)

# Define upper and lower bounds for the range
upper_bound <- average_price + std_dev_per_day
lower_bound <- average_price - std_dev_per_day

# Generate the x-axis (days)
days <- 0:(ncol(matrix6) - 1)

# Plot the
# Plot the average price line
plot(days, average_price, type = "l", col = "blue", lwd = 2,
     xlab = "Trading Days (From Nov 18, 2022)", ylab = "Stock Price ($)",
     main = "S&P \n (Nov 22, 2018 - Jan 22, 2019) ",
     ylim = range(lower_bound, upper_bound))

# Add the shaded area for ±1 standard deviation
polygon(c(days, rev(days)), 
        c(upper_bound, rev(lower_bound)), 
        col = rgb(0.1, 0.4, 0.8, 0.2), border = NA)

# Re-plot the average line to ensure visibility
lines(days, average_price, col = "blue", lwd = 2)

# Add a legend
legend("topleft", legend = c("Average Price", "±1 Standard Deviation"),
       col = c("blue", rgb(0.1, 0.4, 0.8, 0.2)),
       lwd = c(2, NA), fill = c(NA, rgb(0.1, 0.4, 0.8, 0.2)), border = NA)

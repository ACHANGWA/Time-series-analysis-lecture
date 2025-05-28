library(readr)
library(psych)

df <- read_csv("C:/Users/ACHANGWA CHIARA/Desktop/Dissertation_2023/CFR_Region/mockdata1.csv")
setwd("C:/Users/ACHANGWA CHIARA/Desktop")
# Run summary statistics
stats_df <- psych::describe(df$Number_of_new_cases)
print(stats_df)

# Set Date column to Date format
df$Date <- as.Date(df$Date)

par(mgp = c(3, 0.5, 0))

par(font.lab = 1)
# Plot time series using base R

plot(df$Date, df$Number_of_new_cases,
     type = "l",                         # line plot
     col = "steelblue",                 # line color
     lwd = 2,                           # line width
     main = "Time Series Plot",        # title
     xlab = "Date",                    # x-axis label
     ylab = "Number of cases",         # y-axis label
     ylim = c(0, 600000),              # y-axis limits
     xaxt = "n",                       # suppress x-axis for custom formatting
     yaxt = "n",                     # suppress y-axis for custom formatting
     cex.lab = 1.0)    # increase axis label font size
# Customize axes
axis(1, at = pretty(df$Date), labels = format(pretty(df$Date), "%Y-%m-%d"), las = 1, cex.axis = 0.8)
axis(2, at = seq(0, 600000, by = 100000), las = 1, cex.axis = 0.8)

# Set file path and image size
png("timeseries_plot.png", width = 1200, height = 800, res = 150)

dev.off()

#####

### subset Jan 2020 to Dec 2021

subset_data <- df[df$Date >= as.Date("2020-01-01") & df$Date <= as.Date("2021-12-31"), ]

stats_subset <- psych::describe(subset_data$Number_of_new_cases)
print(stats_subset)
#median(subset_data$Number_of_new_cases)
# View the first few rows of the subset
View(subset_data)
# Plot the subset
png("subset_timeseries_plot.png", width = 1200, height = 800, res = 150)

par(mgp = c(3, 0.5, 0))

par(font.lab = 1)
# Plot time series using base R
plot(subset_data$Date, subset_data$Number_of_new_cases,
     type = "l",                         # line plot
     col = "steelblue",                 # line color
     lwd = 2,                           # line width
     main = "Time Series Plot",        # title
     xlab = "Date",                    # x-axis label
     ylab = "Number of cases",         # y-axis label
     ylim = c(0, 10000),              # y-axis limits
     xaxt = "n",                       # suppress x-axis for custom formatting
     yaxt = "n",                     # suppress y-axis for custom formatting
     cex.lab = 1.0)    # increase axis label font size

# Customize axes
axis(1, at = pretty(subset_data$Date), labels = format(pretty(subset_data$Date), "%Y-%m-%d"), las = 1, cex.axis = 0.8)
axis(2, at = seq(0, 10000, by = 2000), las = 1, cex.axis = 0.8)

dev.off()

####

data <- read_csv("C:/Users/ACHANGWA CHIARA/Desktop/mockdata3.csv")
View(data)
data$Month <- match(data$Month, month.abb)

data$Time <- data$Year + data$Month / 12

data$Time <- as.Date(paste(data$Year, data$Month, "01", sep = "-"))

par(mgp = c(3, 0.5, 0))

par(font.lab = 1)

# Plot time series using base R
plot(data$Time, data$Total,
     type = "l",                         # line plot
     col = "steelblue",                 # line color
     lwd = 2,                           # line width
     main = "Time Series Plot",        # title
     xlab = "Date",                    # x-axis label
     ylab = "Monthly Total Chlamydia Cases",         # y-axis label
     ylim = c(0, 1500),              # y-axis limits
     xaxt = "n",                       # suppress x-axis for custom formatting
     yaxt = "n",                     # suppress y-axis for custom formatting
     cex.lab = 1.0)    # increase axis label font size

# Customize axes
axis(1, at = pretty(data$Time), labels = format(pretty(data$Time), "%Y-%m-%d"), las = 1, cex.axis = 0.8)
axis(2, at = seq(0, 1500, by = 500), las = 1, cex.axis = 0.8)
png("Chlamydia_timeseries_plot.png", width = 1200, height = 800, res = 150)
dev.off()


### ARIMA model


library(lubridate)
library(forecast)
data <- read_csv("C:/Users/ACHANGWA CHIARA/Desktop/Dissertation_2023/CFR_Region/mockdata1.csv")

dataR <- subset(data, Date >= as.Date("2021-01-01") & Date <= as.Date("2021-12-31"))

# Step 3: Create time series object with daily frequency
# Get numeric start date components
start_year <- year(min(dataR$Date))
start_day <- yday(min(dataR$Date))  # day of year, typically 1

# Ensure time series is correctly ordered
dataR <- dataR[order(dataR$Date), ]

# Create daily time series object
ts_data <- ts(dataR$Number_of_new_cases, start = c(start_year, start_day), frequency = 365)

# Step 4: Fit ARIMA model
fit <- auto.arima(ts_data)

# Step 5: Forecast next 365 days (1 year ahead)
forecastedValues <- forecast(fit, h = 90)
# Step 6: Plot the forecast (will show fractional years on x-axis)
plot(forecastedValues,
     main = "ARIMA Forecast of Daily New Cases (2022)",
     xlab = "Time (Year)", ylab = "Number of New Cases",
     col.main = "darkgreen")

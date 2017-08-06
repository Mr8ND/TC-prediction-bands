
# Explore the density of hurricane death times

# Run Simulate_Validation_Paths.R first to get dflist

death.times <- sapply(dflist, FUN = function(x) nrow(x))

# View histogram of death times

hist(death.times, breaks = 20, freq = F, xlim=c(0,130), ylim=c(0,0.04),
     xlab = "Death Time", ylab = "Density", main = "Density of TC Death Times (Exp)")

# Histogram of death times, with an overlying kernel density estimate 
# type bw.nrd to see code used to calculate bandwidth

time.density <- density(death.times, bw = bw.nrd(death.times), kernel = "gaussian")
hist(death.times, breaks = 20, freq = F, xlim = c(0,130), ylim = c(0,0.04),
     xlab = "Death Time", ylab = "Density", main = "Kernel Density of TC Death Times")
par(new = T)
plot(time.density, xlim = c(0,130), ylim = c(0,0.04), xlab = "", ylab = "", main = "")

# sampling from kernel density - example to show that this sampling method works

time.samp <- sample(time.density$x, size = 250000, replace=TRUE, prob=time.density$y)
hist(time.samp, breaks = 20, freq = F, xlim = c(0,130), ylim = c(0,0.04),
     xlab = "Sample Death Time", ylab = "Density", main = "Density of Sampled TC Death Times (Kernel)")
par(new = T)
plot(time.density, xlim = c(0,130), ylim = c(0,0.04), xlab = "", ylab = "", main = "")

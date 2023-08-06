# Read File
veri <- read.table("DatasetNA.txt", header = TRUE, sep = " ", dec = ",")

# Create my histogram()
custom_histogram <- function(data, bins = 10, main_title = "Histogram", x_label = "X", y_label = "Frequency", color = "blue") {
  # ignore <NA>
  data <- data[!is.na(data)]
  
  # Calculate histogram 
  hist_counts <- rep(0, bins)
  data_min <- min(data)
  data_max <- max(data)
  range_width <- data_max - data_min
  bin_width <- range_width / bins
  
  for (i in 1:length(data)) {
    bin_index <- floor((data[i] - data_min) / bin_width) + 1
    if (bin_index > 0 && bin_index <= bins) {
      hist_counts[bin_index] <- hist_counts[bin_index] + 1
    }
  }
  
  # Use plot()
  breaks <- seq(data_min, data_max, length.out = bins + 1)
  bar_centers <- breaks[-(length(breaks))] + bin_width / 2
  plot(bar_centers, hist_counts, type = "h", lwd = 10, col = color, xlab = x_label, ylab = y_label, main = main_title)
}

# Select Var1-Var8 colums 
veri_altkume <- veri[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")]

# String -> numeric
veri_numeric <- apply(veri_altkume, 2, function(x) as.numeric(as.character(gsub(",", ".", x))))


# a: The function should have options of the number of bins, main title, x and y labels, x and y limits, color options.
# b: Each histogram should be plotted in a separate page (output screen).
# histograms : seperate page
for (i in 1:ncol(veri_numeric)) {
  custom_histogram(veri_numeric[, i], main_title = paste("Histogram of", colnames(veri_numeric)[i]), x_label = colnames(veri_numeric)[i])
}

# c : More than one histogram could be plotted in the same page (output screen) together.
# Create a PDF file
pdf("all_histograms.pdf")

# Create histograms and save in the PDF file
for (i in 1:ncol(veri_numeric)) {
  custom_histogram(veri_numeric[, i], main_title = paste("Histogram of", colnames(veri_numeric)[i]), x_label = colnames(veri_numeric)[i])
  
}

# Close the PDF file
dev.off()




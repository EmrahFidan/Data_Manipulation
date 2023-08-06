# Create my Boxplot
custom_boxplot <- function(data, main_title = "Boxplot", x_label = "X", y_label = "Y", color = "lightblue") {
  # Calculate boxplot 
  stats <- fivenum(data)
  
  # Generate x values 
  x <- rep(1, length(data))
  
  # Plot boxplot components
  plot(x, data, type = "n", xlim = c(0.5, 1.5), ylim = range(data, na.rm = TRUE, finite = TRUE),
       xlab = x_label, ylab = y_label, main = main_title)

  rect(0.9, stats[2], 1.1, stats[4], col = color)
  segments(0.9, stats[2], 1.1, stats[2])
  segments(0.9, stats[4], 1.1, stats[4])
  segments(1, stats[2], 1, stats[1])
  segments(1, stats[4], 1, stats[5])
  segments(0.8, stats[3], 1.2, stats[3], lwd = 2)  # Horizontal line
}

# Read Data
veri <- read.table("DatasetNA.txt", header = TRUE, sep = " ", dec = ",")

# Select Var1-Var8
veri_altkume <- veri[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")]

# String -> numeric 
veri_numeric <- apply(veri_altkume, 2, function(x) as.numeric(as.character(gsub(",", ".", x))))

# Draw boxplot
for (i in 1:ncol(veri_numeric)) {
custom_boxplot(veri_numeric[, i], main_title = paste("Boxplot of", colnames(veri_numeric)[i]), x_label = colnames(veri_numeric)[i], color = "blue")
}

# A page 
# Create a PDF file
pdf("all_boxplots.pdf")

# Create boxplots and save in the PDF file
for (i in 1:ncol(veri_numeric)) {
  custom_boxplot(veri_numeric[, i], main_title = paste("Boxplot - ", colnames(veri_numeric)[i]), x_label = colnames(veri_numeric)[i])
}

# Close the PDF file
dev.off()




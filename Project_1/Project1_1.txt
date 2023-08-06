# Read Data
data <- read.table("DatasetNA.txt", header = TRUE)

# a. The function should have options of main title, x and y labels, x and y limits, color options.
# b. Group and Gender categorical variables should be barplotted in a separate page.

# Create My Barplot
my_barplot_group <- function() {
  # Group Person Count
  group_freq <- table(data$Group)
  
  heights <- as.numeric(group_freq)
  
  bar_names <- names(group_freq)
  
  plot_height <- max(heights) + 2
  plot_width <- length(heights) * 1.5
  bar_width <- 0.8
  
  # Graphic
  plot(0, 0, xlim = c(0, plot_width-1), ylim = c(0, plot_height), type = "h", xlab = "Group", ylab = "Person", main = "Groups")
  
  # Bars
  rect(1 - bar_width / 2, 0, 1 + bar_width / 2, heights[1], col = "skyblue")
  rect(2 - bar_width / 2, 0, 2 + bar_width / 2, heights[2], col = "yellow")
  rect(3 - bar_width / 2, 0, 3 + bar_width / 2, heights[3], col = "pink")
  rect(4 - bar_width / 2, 0, 4 + bar_width / 2, heights[4], col = "green")
  
  # Bars Name
  text(1:length(heights), par("usr")[2] - 0, labels = bar_names, srt = 0, xpd = TRUE)
}

my_barplot_gender <- function() {
  # Gender Person Count
  gender_freq <- table(data$Gender)
  
  heights <- as.numeric(gender_freq)
  
  bar_names <- names(gender_freq)
  
  plot_height <- max(heights) + 2
  plot_width <- length(heights) * 3
  bar_width <- 0.8
  
  # Graphic
  plot(0, 0, xlim = c(0, plot_width-1), ylim = c(0, plot_height), type = "h", xlab = "Genders", ylab = "Frekans", main = "Gender Dağılımı")
  
  # Bars
  
  rect(1 - bar_width / 2, 0, 1 + bar_width / 2, heights[1], col = "red")
  rect(2 - bar_width / 2, 0, 2 + bar_width / 2, heights[2], col = "blue")
  
  # Bars Name
  
  text(1:length(heights), par("usr")[2] - 0, labels = bar_names, srt = 0, xpd = TRUE)
}

my_barplot_group()
my_barplot_gender()


# c.Group and Gender categorical variables should be barplotted horizontally or vertically in the same page together :

# Create My Barplot Function
my_barplot_group_gender <- function() {
  # Group and Gender Person Count
  group_freq <- table(data$Group)
  gender_freq <- table(data$Gender)
  
  heights <- as.numeric(group_freq)
  heights2 <- as.numeric(gender_freq)
  
  bar_names <- names(group_freq)
  bar_names2 <- names(gender_freq)
  
  plot_height <- max(heights) + 2
  plot_width <- length(heights) * 1.5
  bar_width <- 0.8
  
  plot_height2 <- max(heights2) + 2
  plot_width2 <- length(heights2) * 3
  bar_width2 <- 0.8
  
  # Graphic
  par(mfrow = c(1, 2))  # same page
  plot(0, 0, xlim = c(0, plot_width-1), ylim = c(0, plot_height), type = "h", xlab = "Group", ylab = "Person", main = "Groups")
  
  rect(1 - bar_width / 2, 0, 1 + bar_width / 2, heights[1], col = "skyblue")
  rect(2 - bar_width / 2, 0, 2 + bar_width / 2, heights[2], col = "yellow")
  rect(3 - bar_width / 2, 0, 3 + bar_width / 2, heights[3], col = "pink")
  rect(4 - bar_width / 2, 0, 4 + bar_width / 2, heights[4], col = "green")
  
  
  text(1:length(heights), par("usr")[2] - 0, labels = bar_names, srt = 90, xpd = TRUE)
  
  plot(0, 0, xlim = c(0, plot_width2-3), ylim = c(0, plot_height2), type = "h", xlab = "Gender", ylab = "Person", main = "Genders")
  
  
  rect(1 - bar_width2 / 2, 0, 1 + bar_width2 / 2, heights2[1], col = "red")
  rect(2 - bar_width2 / 2, 0, 2 + bar_width2 / 2, heights2[2], col = "blue")
  
  text(1:length(heights2), par("usr")[3] + 9, labels = bar_names2, srt = 90, xpd = TRUE)
  
}
my_barplot_group_gender()








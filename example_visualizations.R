# Example R Visualizations
# Run each section by placing your cursor inside it and pressing Ctrl+Enter (or Cmd+Enter on Mac)
# Plots will appear in the VS Code R plot viewer panel on the right

# ── Install packages if needed ──────────────────────────────────────────────
# Uncomment and run these lines once if you don't have ggplot2:
# install.packages("ggplot2")
# install.packages("httpgd")   # for live plot panel in VS Code

# ── 1. Base R: Simple Scatter Plot ──────────────────────────────────────────
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(3, 5, 2, 8, 6, 9, 4, 7, 10, 1)

plot(x, y,
     main  = "Simple Scatter Plot",
     xlab  = "X Values",
     ylab  = "Y Values",
     col   = "steelblue",
     pch   = 16,
     cex   = 1.5)

# ── 2. Base R: Bar Chart ─────────────────────────────────────────────────────
categories <- c("Group A", "Group B", "Group C", "Group D")
values     <- c(23, 45, 12, 67)

barplot(values,
        names.arg = categories,
        main      = "Bar Chart Example",
        xlab      = "Category",
        ylab      = "Count",
        col       = c("tomato", "steelblue", "seagreen", "goldenrod"),
        border    = "white")

# ── 3. Base R: Histogram ─────────────────────────────────────────────────────
set.seed(42)
data <- rnorm(500, mean = 50, sd = 10)

hist(data,
     main   = "Histogram of Normally Distributed Data",
     xlab   = "Value",
     ylab   = "Frequency",
     col    = "steelblue",
     border = "white",
     breaks = 25)

# ── 4. Base R: Line Chart ────────────────────────────────────────────────────
months <- 1:12
sales  <- c(120, 135, 148, 162, 175, 190, 185, 178, 165, 155, 140, 130)

plot(months, sales,
     type  = "b",
     main  = "Monthly Sales",
     xlab  = "Month",
     ylab  = "Sales ($)",
     col   = "darkorange",
     lwd   = 2,
     pch   = 16,
     xaxt  = "n")
axis(1, at = 1:12,
     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec"))

# ── 5. ggplot2: Scatter Plot with Trend Line ─────────────────────────────────
library(ggplot2)

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 6, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", linewidth = 1.5) +
  scale_color_manual(values = c("4" = "steelblue",
                                "6" = "goldenrod",
                                "8" = "tomato"),
                     name = "Cylinders") +
  labs(title = "Car Weight vs. Fuel Efficiency",
       x     = "Weight (1000 lbs)",
       y     = "Miles per Gallon") +
  theme_minimal(base_size = 18)

# ── 6. ggplot2: Box Plot ─────────────────────────────────────────────────────
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 4, linewidth = 1) +
  scale_fill_manual(values = c("4" = "steelblue",
                               "6" = "goldenrod",
                               "8" = "tomato")) +
  labs(title = "MPG Distribution by Number of Cylinders",
       x     = "Cylinders",
       y     = "Miles per Gallon",
       fill  = "Cylinders") +
  theme_minimal(base_size = 18)

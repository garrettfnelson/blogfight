
   ## --------------------- ##
   ## --------------------- ##
   ## Quant Project Script  ##
   ##     Team Houston      ##
   ## --------------------- ##
   ## --------------------- ##

#dev.off()
#rm(list=ls())
##Full Data Set##
#setwd("/Users/benai/Library/Mobile Documents/com~apple~CloudDocs/BDocuments/Colorado Boulder/Quant Methods/Assignments Folder/Project/Blog Fight Real")


### Part 1 : Construct Data ###

##Full Data Set###
FullData <- read.csv("AsPercentDataFull.csv")
FullUnrate <- read.csv("UNRATEFullData.csv")

#pre taylor#
AsPercentPreTaylor <- read.csv("GovSpendingPre.csv")
UnemploymentRatePreTaylor <- read.csv("Unrate (1).csv")

plot(AsPercentPreTaylor$As.., UnemploymentRatePreTaylor$UNRATE, main = "Gov Spend vs. Unemployment Change Pre Taylor",
     xlab = "Gov Spending", ylab = "Unemployment Change")

#post taylor#
AsPercentPostTaylor <- read.csv("GovSpendingPost.csv")
UnemploymentRatePostTaylor <- read.csv("UnratePost.csv")

plot(AsPercentPostTaylor$As.., UnemploymentRatePostTaylor$UNRATE, main = "Gov Spend vs. Unemployment Change Post Taylor",
     xlab = "Gov Spending", ylab = "Unemployment Change")

### Part 2 : Taylor's Data ###

AsPercent <- read.csv("AsPercentData.csv")
UnemploymentRate <- read.csv("Unrate.csv")

names(AsPercent) <- c("Date", "GCE", "GDP", "As..")

plot(AsPercent$As.., UnemploymentRate$UNRATE, main = "Gov Spend vs. Unemployment Change",
     xlab = "Gov Spending", ylab = "Unemployment Change")

# Plot the Full Data
plot(FullData$As.., FullUnrate$UNRATE, main = "Full Gov Spend vs. Unemployment Change",
     xlab = "Gov Spending", ylab = "Unemployment Change")


### Part 3 : Plot Data distinguished ###
plot(AsPercent$As.., UnemploymentRate$UNRATE,  ##need to subset by date (taylor start and end date)
     xlab = "Government purchases as a percent of GDP",
     ylab = "Unemployment rate",
     main = "Unemployment Rate vs Government Purchases",
     pch = 20,  # Type of point
     col = "lightblue3")  # Color of points
points(AsPercentPreTaylor$As.., UnemploymentRatePreTaylor$UNRATE,
       pch = 20, col = "lightcoral")
points(AsPercentPostTaylor$As.., UnemploymentRatePostTaylor$UNRATE,
       pch = 20, col = "chartreuse3")

legend("topleft", legend = c("Taylor", "Pre Taylor", "Post Taylor"), fill = c("lightblue3", "lightcoral", "chartreuse3"), pch = 16)

# Contrasting correlations across the 3 time periods
# Correlations for Taylor's sample period
cor_taylor <- cor(AsPercent$As.., UnemploymentRate$UNRATE)

# Correlations for pre-Taylor sample period
cor_pretaylor <- cor(AsPercentPreTaylor$As.., UnemploymentRatePreTaylor$UNRATE)

# Correlations for post-Taylor sample period
cor_posttaylor <- cor(AsPercentPostTaylor$As.., UnemploymentRatePostTaylor$UNRATE)

# Print the correlations for each period
cat("Correlation for Taylor's sample period (1990 Q1 - 2010 Q3):", cor_taylor, "\n")
cat("Correlation for pre-Taylor sample period (1948 - 1989):", cor_pretaylor, "\n")
cat("Correlation for post-Taylor sample period (2010 Q4 - present):", cor_posttaylor, "\n")

# (i) Taylor's Sample Period: The strong positive correlation implies that increased government purchases as a percentage of GDP were linked to elevated unemployment rates throughout this time frame.
# (ii) Pre-Taylor Period: The subtle negative correlation may imply that, during this phase, the connection between government spending and unemployment was less distinct or possibly influenced more significantly by other factors than government spending.
# (iii) Post-Taylor Period: The extremely strong positive correlation underscores an even more pronounced association between heightened government spending as a percentage of GDP and elevated unemployment rates compared to the Taylor period.

# Evaluating Potential Cherry-Picking
# It can be assumed that Wolfers likely knew and had access to the data from the pre-Taylor period prior to writing his post. It could make sense that he emphasized the lack of correlation from that period to criticize Taylor's findings without acknowledging the strong correlation present in Taylor’s sample period. If this is the case it is most likely cherry-picking.
# Obviously Taylor and Wolfers would have no idea what the future would hold in creating their data sets


### Part 4 ###

# A

# "is the relationship that Taylor presents in his scatter plot stable over time?"

unrate_taylor    <- FullUnrate[169:251, 2] # taylor data: 1990Q1 - 2010Q3
unrate_2004_2023 <- FullUnrate[221:303, 2]
unrate_1994_2013 <- FullUnrate[182:264, 2]
unrate_1984_2003 <- FullUnrate[142:224, 2]
unrate_1974_1993 <- FullUnrate[102:184, 2]
unrate_1964_1983 <- FullUnrate[62:144,  2]
unrate_1954_1973 <- FullUnrate[22:104,  2]

# matching government spending data
spending_taylor    <- FullData[169:251, 4]
spending_2004_2023 <- FullData[221:303, 4]
spending_1994_2013 <- FullData[182:264, 4]
spending_1984_2003 <- FullData[142:224, 4]
spending_1974_1993 <- FullData[102:184, 4]
spending_1964_1983 <- FullData[62:144,  4]
spending_1954_1973 <- FullData[22:104,  4]

# correlation values for spending to unemployment rate
cor_taylor    <- cor(spending_taylor, unrate_taylor)
cor_2004_2023 <- cor(spending_2004_2023, unrate_2004_2023)
cor_1994_2013 <- cor(spending_1994_2013, unrate_1994_2013)
cor_1984_2003 <- cor(spending_1984_2003, unrate_1984_2003)
cor_1974_1993 <- cor(spending_1974_1993, unrate_1974_1993)
cor_1964_1983 <- cor(spending_1964_1983, unrate_1964_1983)
cor_1954_1973 <- cor(spending_1954_1973, unrate_1954_1973)

cat("Correlation for Taylor's sample period (1990 Q1 - 2010 Q3):", cor_taylor, "\n")
cat("Correlation for Taylor's sample period (2004 Q1 - 2023 Q3):", cor_2004_2023, "\n")
cat("Correlation for Taylor's sample period (1994 Q1 - 2013 Q3):", cor_1994_2013, "\n")
cat("Correlation for Taylor's sample period (1984 Q1 - 2003 Q3):", cor_1984_2003, "\n")
cat("Correlation for Taylor's sample period (1974 Q1 - 1993 Q3):", cor_1974_1993, "\n")
cat("Correlation for Taylor's sample period (1964 Q1 - 1983 Q3):", cor_1964_1983, "\n")
cat("Correlation for Taylor's sample period (1954 Q1 - 1973 Q3):", cor_1954_1973, "\n")

# Forward Looking Data for the most part holds high correlation values, but Taylor could not have known about these values. It appears suspicious that the backward looking data has correlations which are much lower than the sample period he chooses

# Plots for Each Decade
plot(spending_taylor, unrate_taylor,
     xlab = "Government purchases as a percent of GDP by Decade",
     ylab = "Unemployment rate",
     main = "Unemployment Rate vs Government Purchases",
     pch = 20,  # Type of point
     col = "black")  # Color of points
points(spending_1954_1973, unrate_1954_1973,
       pch = 20, col = "lightcoral")
points(spending_1964_1983, unrate_1964_1983,
       pch = 20, col = "pink3")
points(spending_1974_1993, unrate_1974_1993,
       pch = 20, col = "lightgreen")
points(spending_1984_2003, unrate_1984_2003,
       pch = 20, col = "deeppink")
points(spending_2004_2023, unrate_2004_2023,
       pch = 20, col = "lightslateblue")

lm_taylor <- lm(spending_taylor ~ unrate_taylor)
abline(lm_taylor, col = "black", lty = 2)

lm_taylor

# Existing code for plotting points
plot(FullData$As.., FullUnrate$UNRATE,
     # xaxt = "n",
     xlab = "Government purchases as a percent of GDP by Decade (%)",
     ylab = "Unemployment rate (%)",
     main = "Unemployment Rate vs Government Purchases",
     pch = 20,  # Type of point
     col = "grey",
     xlim = c(min(.17), max(.25)),  # Adjust x-axis limits
     ylim = c(min(2), max(11)))  # Adjust y-axis limits
# text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 45, pos = 1, xpd = TRUE)
     
# Color of points

# Add points for each sequence
points(spending_1954_1973, unrate_1954_1973, pch = 20, col = "red")
points(spending_1964_1983, unrate_1964_1983, pch = 20, col = "orange2")
points(spending_1974_1993, unrate_1974_1993, pch = 20, col = "lightgreen")
points(spending_1984_2003, unrate_1984_2003, pch = 20, col = "royalblue1")
points(spending_2004_2023, unrate_2004_2023, pch = 20, col = "lightslateblue")

# Add lines for the slope of each sequence
# abline(lm(unrate_1954_1973 ~ spending_1954_1973), col = "red")
# abline(lm(unrate_1964_1983 ~ spending_1964_1983), col = "orange2")
# abline(lm(unrate_1974_1993 ~ spending_1974_1993), col = "lightgreen")
# abline(lm(unrate_1984_2003 ~ spending_1984_2003), col = "lightblue")
# abline(lm(unrate_2004_2023 ~ spending_2004_2023), col = "lightslateblue")

lm10 <- lm(unrate_1954_1973 ~ spending_1954_1973)
summary(lm10)
segments(0.19, 7.885-12.922*0.19, 0.23, 7.885-12.922*0.23, col = 'red', lwd = 2)

lm11 <- lm(unrate_1964_1983 ~ spending_1964_1983)
summary(lm11)
segments(0.19, 24.154-82.280*0.19, 0.23, 24.154-82.280*0.23, col = 'orange2', lwd = 2)

lm12 <- lm(unrate_1974_1993 ~ spending_1974_1993)
summary(lm12)
segments(0.19, -5.184+57.938*0.19, 0.23, -5.184+57.938*0.23, col = 'lightgreen', lwd = 2)

lm13 <- lm(unrate_1984_2003 ~ spending_1984_2003)
summary(lm13)
segments(0.19, -8.744+74.758*0.19, 0.23, -8.744+74.758*0.23, col = 'royalblue1', lwd = 2)

lm14 <- lm(unrate_2004_2023 ~ spending_2004_2023)
summary(lm14)
segments(0.19, -18.07+128.19*0.19, 0.23, -18.07+128.19*0.23, col = 'lightslateblue', lwd = 2)

legend("topright", legend = c("1954-1973", "1964-1983", "1974-1993", "1984-2003", "2004-2023"),
       pch = 20, col = c("red", "orange2", "lightgreen", "royalblue1", "lightslateblue"), cex = 0.8)

# Black Data points from years outside the 2 decade scope chosen

# Add Commentary 

# B

set.seed(001)

# Create new plot
plot(FullData$As.., FullUnrate$UNRATE, 
     xlab = "Government Purchases as a Percent of GDP", 
     ylab = "Unemployment Rate (%)", 
     pch = 20, 
     col = "black",
     main = "Unemployment Rate vs Government Purchases by Decade")

UnrateP4 <- FullUnrate[22:251, 2]
FullDataP4 <- FullData[22:251, 4]

# Create a sequence of dates
dates <- seq(from = as.Date("1953-04-01"), to = as.Date("2010-07-01"), by = "3 months")

# Create x and y columns
x <- UnrateP4
y <- FullDataP4  # Replace with your actual y values

# Combine x, y, and dates into a data frame
DataPart4 <- data.frame(DATE = dates, x = x, y = y)

# Convert 'Date' from character to Date class
DataPart4$DATE <- as.Date(DataPart4$DATE, format = "%Y-%m-%d")  # Corrected to uppercase DATE

# Sort by Decade x2
DataPart4$Year <- as.numeric(format(DataPart4$DATE, "%Y"))
DataPart4$Decade <- cut(DataPart4$Year, breaks = seq(1940, 2020, by = 20), 
                        labels = paste0(seq(1940, 2010, by = 20), "s"), 
                        include.lowest = TRUE, right = FALSE)
DataPart4$Decade <- factor(DataPart4$Decade)

# Color by decade
unique_decades <- levels(DataPart4$Decade)
colors <- rainbow(length(unique_decades))
decade_colors <- setNames(colors, unique_decades)

# Loop over each decade to add regression lines
for (d in levels(DataPart4$Decade)) {
  # Subset data for the decade
  decade_data <- subset(DataPart4, Decade == d)
  
  # Fit a linear model for each decade
  lm_decade <- lm(x ~ y, data = decade_data)
  
  # Use x-axis values from the original plot
  gov_purch_seq <- seq(min(FullData$As.., na.rm = TRUE), 
                       max(FullData$As.., na.rm = TRUE), length.out = 230)
  
  # Predict unemployment rate for this sequence
  pred_unemp <- predict(lm_decade, newdata = data.frame(x = gov_purch_seq))
  
  # Add a line for the predictions using the mapped color
  lines(gov_purch_seq, pred_unemp, col = decade_colors[d], lwd = 2)
}

# Add a legend
legend("topright", legend = names(decade_colors), col = colors, lty = 1, lwd = 2, title = "Decade", cex = 0.8)

# Commentary on unemployment rate by decade impacts



# Part 5: Cherry Picking Simulation
# Set the number of replications and the number of quarters
num_replications <- 1000
num_quarters <- 256
min_subsample_length <- 60

# Initialize vectors to store correlations
full_sample_correlations <- numeric(num_replications)
cherry_picked_correlations <- numeric(num_replications)

# Simulate the cherry picking process
set.seed(001)  # For reproducibility
for (i in 1:num_replications) {
  # Generate two uncorrelated random variables x and y
  x <- rnorm(num_quarters)
  y <- rnorm(num_quarters)
  
  # Compute correlation for the full sample
  full_sample_correlations[i] <- cor(x, y)
  
  # Cherry picking: finding the maximum correlation in subsamples
  max_corr <- -1
  for (start in 1:(num_quarters - min_subsample_length + 1)) {
    end <- start + min_subsample_length - 1
    corr <- cor(x[start:end], y[start:end])
    max_corr <- max(max_corr, corr)
  }
  cherry_picked_correlations[i] <- max_corr
}

# Plotting the results
hist(full_sample_correlations, breaks = 30, col = rgb(1,0,0,0.5), 
     main = "Full Sample vs Cherry Picked Correlations", 
     xlab = "Correlation", xlim = c(-1, 1))
hist(cherry_picked_correlations, breaks = 30, col = rgb(0,0,1,0.5), add = TRUE)
legend("topright", legend = c("Full Sample", "Cherry Picked"), 
       fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

# Statistical test
t.test(cherry_picked_correlations, full_sample_correlations)

# is it accurate to describe Taylor’s plot as “cherry picking, plain and simple”? Conclusion
  

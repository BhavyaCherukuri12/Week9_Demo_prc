
# Load Convert the state.x77 dataset to a dataframe
state_df <- as.data.frame(state.x77)
str(state_df)

View(state_df)


# Assume 'data' is your dataframe

# Rename column names
colnames(state_df)[colnames(state_df) == "Life Exp"] <- "Life_exp"
colnames(state_df)[colnames(state_df) == "HS Grad"] <- "Hs_grad"

# Use subset
subset_data <- subset(state_df, select = c("Life_Exp", "Hs_Grad", other_columns_you_need))

# Now 'subset_data' contains only the columns 'life_exp' and 'hs_grad' and any other columns you specified








# Step 3a: Examine linearity using pairs() function
pairs(state_df)


windows(20,10)
pairs(state_df)
#install.packages("psych")
library(psych)


pairs.panels(state_df,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)  






# Plot scatterplots for all combinations of variables
pairs(state_df)


windows(20,12)
par(mfrow=c(4,2))

scatter.smooth(x=state_df$Population,
               y=state_df$Murder, 
               xlab = "Population (,00000)",
               ylab = "correlation of Murder",
               main="corelation of Population ~ Murder  ")

scatter.smooth(x=state_df$Income,
               y=state_df$Murder, 
               xlab = "Income",
               ylab = "correlation of Murder",
               main="corelation of Income ~ Murder  ")

scatter.smooth(x=state_df$Illiteracy,
               y=state_df$Murder, 
               xlab = "Illiteracy",
               ylab = "correlation of Murder",
               main="corelation of Illiteracy ~ Murder  ")

scatter.smooth(x=state_df$Life_exp,
               y=state_df$Murder, 
               xlab = "Life_exp",
               ylab = "correlation of Murder",
               main="corelation of Life_exp ~ Murder  ")

scatter.smooth(x=state_df$Hs_grad,
               y=state_df$Murder, 
               xlab = "Hs_grad",
               ylab = "correlation of Murder",
               main="corelation of Hs_grad ~ Murder  ")

scatter.smooth(x=state_df$Frost,
               y=state_df$Murder, 
               xlab = "Frost",
               ylab = "correlation of Murder",
               main="corelation of Frost ~ Murder  ")

scatter.smooth(x=state_df$Area,
               y=state_df$Murder, 
               xlab = "Area",
               ylab = "correlation of Murder",
               main="corelation of Area ~ Murder  ")
window(20,12)
par(mfrow=c(1,2))
boxplot(x=cars$dist, main="car distance")

boxplot(cars$speed,main="car of speed")




# Check correlation
correlation_matrix <- cor(state_df)
correlation_matrix

install.packages("corrplot")

# Load necessary libraries
library(corrplot)

# Calculate correlation matrix
correlation_matrix <- cor(state_df)
state_df

window(20,12)

# Plot correlation matrix
corrplot(correlation_matrix, method = "color")


correlation_matrix<-cor(state_df)
windows(20,16)
corPlot(correlation_matrix)


attach(state_df)
paste("correlation for murder and frost: ",
                      round(cor(Mur)))









# Step 3b: Check for outliers using suitable charts
# For example, you can create boxplots for each variable
boxplot(state_df)



# Step 3c: Check for normality
library(e1071)

# Shapiro-Wilk test for normality
shapiro_test <- lapply(state_df, shapiro.test)
shapiro_test

# QQ plot for normality
par(mfrow=c(2,3))
for (i in 1:ncol(state_df)) {
  qqnorm(state_df[,i], main=colnames(state_df)[i])
  qqline(state_df[,i], col="red")
}

# Step 3d: Transform variables if necessary using the box-cox transform library
# For example, if a variable is not normally distributed, you can transform it using box-cox
# You can use the MASS package for the box-cox transformation
library(MASS)





# Example: Transforming a variable named "illiteracy"
transformed_illiteracy <- boxcox(state_df$Illiteracy ~ 1, data = state_df)
lambda <- transformed_illiteracy$x[which.max(transformed_illiteracy$y)]
transformed_illiteracy_data <- (state_df$Illiteracy^lambda - 1) / lambda


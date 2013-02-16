# Disclaimer: loops were avoided so the results for each run could be commented inline

# Environment and data
library(Hmisc);
load("./data/loansDataWithDate.rda");
dateDownload;

# Help functions
# Parse interest rates to have more meaningful results
percToNumeric <- function(factor) {
  asChar <- as.character(factor);
  as.numeric(substr(asChar, 1, nchar(asChar) - 1));
}

plotSorted <- function(results, sorted, col="green") {
  plot(sorted$x);
  points(results[sorted$ix], col=col);  
}

findBetterModel <- function(model1, model2) {
  plot(abs(model1$residuals) - abs(model2$residuals));
  totalErrorDiff <- sum(abs(model1$residuals) - abs(model2$residuals));
  if (totalErrorDiff < 0) {
    finalModel <- model1;
  } else {
    finalModel <- model2;
  }
  finalModel;
}

# Look at data
loansData <- loansData[!is.na(loansData$Open.CREDIT.Lines),];  # Remove missing so you don't have to be careful anymore (all cases)
sum(is.na(loansData));  # No missing cases
loansData[which(loansData$Amount.Funded.By.Investors < 100),];  # 6 cases that no loan was given. Don't desconsider, might be important

# Plot some graphs  to see the data
par(mfrow=c(1, 1));
hist(percToNumeric(loansData$Interest.Rate), main="Interest Rates Histogram", xlab="Interest Rates (%)");
hist(loansData$Amount.Requested, main="Loan Amount Requested Histogram", xlab="Amount Requested (US$)");
hist(loansData$Amount.Funded.By.Investors, main="Loan Amount Funded By Investor Histogram", xlab="Amount Funded (US$)");

# Plot tables to factors
table(loansData$Loan.Length); # 2 levels
table(loansData$State);  # Some reasons of asking are too litle to have confidence on result. Might cause an overfitting
table(loansData$FICO.Range);  # Well distributes in the center
table(loansData$Inquiries.in.the.Last.6.Months);  # Same here about skeweness

# Group similar data
transformedData <- loansData;
transformedData$Interest.Rate <- percToNumeric(loansData$Interest.Rate);
transformedData$Amount.Requested <- cut2(loansData$Amount.Requested, g=5);
transformedData$Amount.Funded.By.Investors <- cut2(loansData$Amount.Funded.By.Investors, g=5);
transformedData$FICO.Range <- tapply(as.character(loansData$FICO.Range), 1:length(loansData$FICO.Range) - 1, function(value) {
  first <- as.numeric(substr(value, 1, 3));
  second <- as.numeric(substr(value, 5, 7));  
  (first + second) / 2
});
transformedData$Debt.To.Income.Ratio <- percToNumeric(loansData$Debt.To.Income.Ratio);

# Plot to see correlations
plot(transformedData$Interest.Rate, transformedData$FICO.Range, 
     main="Interest Rate x FICO range", 
     xlab="FICO range", 
     ylab="Interest Rate (%)");  # Clearly a liner correlation
# Definetly many light blues in the top, dark blue in the middle
plot(transformedData$Interest.Rate, 
     transformedData$FICO.Range, 
     col=transformedData$Amount.Requested,
     main="Interest Rate x FICO range (Colored by amount requested)",
     xlab="FICO range", 
     ylab="Interest Rate (%)");
# Again a linear correlation. Maybe it interacts with other Amount
plot(transformedData$Interest.Rate, 
     transformedData$FICO.Range, 
     col=transformedData$Amount.Funded.By.Investors,
     main="Interest Rate x FICO range (Colored by amount funded)",
     xlab="FICO range", 
     ylab="Interest Rate (%)");
# Defitely there's a correlation
plot(transformedData$Interest.Rate, 
     transformedData$FICO.Range, 
     col=transformedData$Loan.Length,
     main="Interest Rate x FICO range (Colored by loan length)",
     xlab="FICO range", 
     ylab="Interest Rate (%)");

set.seed(1234);
original <- loansData;
chosen <- as.logical(rbinom(transformedData$FICO.Range, 1, 0.1));
crossData <- transformedData[chosen,];
transformedData <- transformedData[!chosen,]
loansData <- loansData[!chosen,]

# Variables yet to consider: Amount.Requested + Amount.Funded.By.Investor + Amount.Requested * Amount.Funded.By.Investor + Loan.Length
# Modeling with no ajustments but continuous FICO as numeric
lm1 <- lm(transformedData$Interest.Rate ~ transformedData$FICO.Range);
# Plotting residulas
par(mfrow = c(3, 2));

plot(transformedData$FICO.Range, lm1$residuals, col=transformedData$Amount.Requested);
plot(lm1$fitted.values, lm1$residuals, col=transformedData$Amount.Requested);

plot(transformedData$FICO.Range, lm1$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(lm1$fitted.values, lm1$residuals, col=transformedData$Amount.Funded.By.Investors);

plot(transformedData$FICO.Range, lm1$residuals, col=transformedData$Loan.Length);
plot(lm1$fitted.values, lm1$residuals, col=transformedData$Loan.Length);

# Fit the model with the length
lm2 <- lm(transformedData$Interest.Rate ~ transformedData$FICO.Range + transformedData$Loan.Length);
# Fitting complete model
lm3 <- lm(transformedData$Interest.Rate ~ transformedData$FICO.Range + transformedData$Loan.Length + transformedData$Amount.Requested + 
          transformedData$Amount.Funded.By.Investors + transformedData$Amount.Requested * transformedData$Amount.Funded.By.Investors);
# Fitting mid term model. Too many variables...
lm4 <- lm(transformedData$Interest.Rate ~ transformedData$FICO.Range + transformedData$Loan.Length + transformedData$Amount.Requested);
# Fitting complete model, amount as a factor
lm5 <- lm(transformedData$Interest.Rate ~ loansData$FICO.Range + transformedData$Loan.Length + transformedData$Amount.Requested);

lm6 <- lm(transformedData$Interest.Rate ~ loansData$FICO.Range + loansData$Loan.Length + loansData$Amount.Requested);

lm7 <- lm(transformedData$Interest.Rate ~ loansData$Amount.Requested +
            loansData$Amount.Funded.By.Investors +
            loansData$Loan.Length +
            loansData$Loan.Purpose +
            loansData$State +
            transformedData$Debt.To.Income.Ratio +
            loansData$Home.Ownership +
            loansData$Monthly.Income +
            loansData$FICO.Range +
            loansData$Open.CREDIT.Lines +
            loansData$Revolving.CREDIT.Balance +
            loansData$Inquiries.in.the.Last.6.Months +
            loansData$Employment.Length);

notFullModels <- list(lm1, lm2, lm3, lm4, lm5, lm6);
allModels <- list(lm1, lm2, lm3, lm4, lm5, lm6, lm7);

# Plot final graph
par(mfrow=c(3, 3));
sorted <- sort(transformedData$Interest.Rate, index.return=TRUE);
for (model in allModels) {
  plotSorted(transformedData$Interest.Rate + model$residuals, sorted);
}

betterFit <- lm1;
for (model in notFullModels) {
  betterFit <- findBetterModel(betterFit, model);
}
summary(betterFit);  # Model 6 was the better desconsidering the full model

# Compare better with full
plot(abs(betterFit$residuals) - abs(lm7$residuals));
sum(abs(betterFit$residuals) - abs(lm7$residuals));  # Model 7 fitted sligtlhy better. .1% interest rate

# Cross validation
loansData <- original[chosen,];
transformedData <- crossData;
par(mfrow=c(3, 3));
sorted <- sort(transformedData$Interest.Rate, index.return=TRUE);
for (model in allModels) {
  plotSorted(predict(model, transformedData), sorted);  
}

# Comparing models predictions
p <- tapply(allModels, 1:7, function(model) { abs(predict(model[[1]], transformedData) - transformedData$Interest.Rate); })

index <- 1;
for (i in 1:6) {
  if (sum(p[[i]] - p[[index]]) < 0) {
    index <- i;
  }
}
# Again 6 was the better model. Compare to 7
plot(p[[index]] - p[[7]]);
sum(p[[index]] - p[[7]]);  # Positive. Model 6 performed a little better, predicting around .05% closer to the correct interest rate
# Mean error 0.004. Insignificant!

# Choose better model, in case, 6
summary(allModels[[index]]);
confint(allModels[[index]]);

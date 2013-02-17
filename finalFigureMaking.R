library(Hmisc);
load("./data/loansDataWithDate.rda");

# Help functions
# Parse interest rates to have more meaningful results
percToNumeric <- function(factor) {
  asChar <- as.character(factor);
  as.numeric(substr(asChar, 1, nchar(asChar) - 1));
}

plotSorted <- function(results, sorted, col, main) {
  plot(sorted$x, pch=19, xlab="Sample index in group", ylab="Interest rate value", main=main);
  points(results[sorted$ix], col=col);
  legend(0, 25, c("Observed", "Fitted"), fill=c("black", col));
}

# Clean dats
loansData <- loansData[!is.na(loansData$Open.CREDIT.Lines),];  # Remove missing so you don't have to be careful anymore (all cases)
sum(is.na(loansData));  # No missing cases

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

set.seed(1234);
original <- loansData;
chosen <- as.logical(rbinom(transformedData$FICO.Range, 1, 0.1));
crossData <- transformedData[chosen,];
transformedData <- transformedData[!chosen,]
loansData <- loansData[!chosen,]

# Variables yet to consider: Amount.Requested + Amount.Funded.By.Investor + Amount.Requested * Amount.Funded.By.Investor + Loan.Length
# Modeling with no ajustments but continuous FICO as numeric
lm_simple <- lm(transformedData$Interest.Rate ~ transformedData$FICO.Range);
lm_good <- lm(transformedData$Interest.Rate ~ loansData$FICO.Range + loansData$Loan.Length + loansData$Amount.Requested);
lm_full <- lm(transformedData$Interest.Rate ~ loansData$Amount.Requested +
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

allModels <- list(list(lm_simple, "blue", "Interest ~ FICO"), 
                  list(lm_good, "green", "Interest ~ FICO,Length,Amount,Requested"), 
                  list(lm_full, "red", "Interest rate ~ All available variables"));

# Plot final graph
pdf("finalImage.pdf", width=10);

?pdf

par(mfrow=c(2, 3));
title("Training group", outer=TRUE);
sorted <- sort(transformedData$Interest.Rate, index.return=TRUE);
for (model in allModels) {
  plotSorted(transformedData$Interest.Rate + model[[1]]$residuals, sorted, col=model[[2]], main=model[[3]]);
}

# Cross validation
loansData <- original[chosen,];
transformedData <- crossData;
sorted <- sort(transformedData$Interest.Rate, index.return=TRUE);
for (model in allModels) {
  plotSorted(predict(model[[1]], transformedData), sorted, col=model[[2]], main=model[[3]]);  
}

dev.off()
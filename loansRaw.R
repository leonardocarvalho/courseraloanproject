# Environment and data
library(Hmisc);
getwd();
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda", destfile="./data/loansData.rda", method="wget");
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf", destfile="./data/loansCodeBook.pdf", method="wget");
load("./data/loansData.rda");
dateDownload <- date();
dateDownload;
save(loansData, dateDownload, file="./data/loansDataWithDate.rda")

# Look at data
head(loansData);
summary(loansData);
colSums(is.na(loansData));  # Has missing values. Be careful with montly income, credit lines, credit balance, inquires in last 6 months
loansData[which(is.na(loansData$Open.CREDIT.Lines)),];  # Only 2 cases cover all missing values
loansData <- loansData[!is.na(loansData$Open.CREDIT.Lines),];  # Remove missing so you don't have to be careful anymore
sum(is.na(loansData));  # No missing cases
sapply(loansData[1,], class);
loansData[which(loansData$Amount.Funded.By.Investors < 100),];  # 6 cases that no loan was given. Should be desconsidered?

percToNumeric <- function(factor) {
  asChar <- as.character(factor);
  as.numeric(substr(asChar, 1, nchar(asChar) - 1));
}

# Check on loans reate
class(loansData$Interest.Rate);
hist(percToNumeric(loansData$Interest.Rate));

# Plot it all to see
hist(loansData$Amount.Requested);
hist(loansData$Amount.Requested^0.5);
hist(loansData$Amount.Funded.By.Investors^0.5);
hist(loansData$Monthly.Income);
hist(log10(loansData$Monthly.Income + 1));
hist(loansData$Revolving.CREDIT.Balance);
hist(loansData$Revolving.CREDIT.Balance^0.35);
hist(loansData$Inquiries.in.the.Last.6.Months);
hist(percToNumeric(loansData$Debt.To.Income.Ratio));

# Plot tables to factors
table(loansData$Loan.Length);
table(loansData$Loan.Purpose);
table(loansData$State);  # Some reasons of asking are too litle to have confidence on result. Might cause an overfitting
table(loansData$Home.Ownership);
table(loansData$FICO.Range);
table(loansData$Inquiries.in.the.Last.6.Months);  # Same here about skeweness
table(loansData$Employment.Length);

# Group similar data
transformedData <- loansData;
transformedData$Interest.Rate <- percToNumeric(loansData$Interest.Rate);
transformedData$Amount.Requested <- cut2(loansData$Amount.Requested, g=8);
transformedData$Amount.Funded.By.Investors <- cut2(loansData$Amount.Funded.By.Investors, g=8);
transformedData$Monthly.Income <- cut2(log10(loansData$Monthly.Income + 1), g=8);
transformedData$Revolving.CREDIT.Balance <- cut2(loansData$Revolving.CREDIT.Balance^0.35, g=8);

transformedData$FICO.Range <- tapply(as.character(loansData$FICO.Range), 1:length(loansData$FICO.Range) - 1, function(value) {
  first <- as.numeric(substr(value, 1, 3));
  second <- as.numeric(substr(value, 5, 7));  
  (first + second) / 2
});

hist(transformedData$FICO.Range);

# Plot to see correlations
plot(transformedData$Interest.Rate, loansData$FICO.Range);  # Clearly a liner correlation
plot(transformedData$Interest.Rate, transformedData$FICO.Range);  # Clearly a liner correlation

# Looks like there's a positive correlation
plot(transformedData$Interest.Rate, loansData$Amount.Requested);
# Definetly many light blues in the top, dark blue in the middle
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=transformedData$Amount.Requested);

# Again a linear correlation. Maybe it interacts with other Amount
plot(transformedData$Interest.Rate, loansData$Amount.Funded.By.Investors);  
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=transformedData$Amount.Funded.By.Investors);  # Same pattern

# Defitely there's a correlation. Is it important?
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=loansData$Loan.Length);

# Maybe theres something here -- will desconsider
plot(transformedData$Interest.Rate, loansData$Revolving.CREDIT.Balance^0.35);
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=transformedData$Revolving.CREDIT.Balance);

# Does not matter
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=loansData$Loan.Purpose);
boxplot(transformedData$Interest.Rate ~ loansData$Loan.Purpose);
boxplot(loansData$FICO.Range ~ loansData$Loan.Purpose);
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=loansData$Debt.To.Income.Ratio);
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=loansData$State);
boxplot(transformedData$Interest.Rate ~ loansData$State);
boxplot(transformedData$FICO.Range ~ loansData$State);
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=as.numeric(loansData$Home.Ownership));
plot(transformedData$Interest.Rate, log10(loansData$Monthly.Income));
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=transformedData$Monthly.Income);
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=loansData$Open.CREDIT.Lines);
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=loansData$Inquiries.in.the.Last.6.Months);
plot(transformedData$Interest.Rate, loansData$FICO.Range, col=loansData$Employment.Length);

# Variables yet to consider: Amount.Requested + Amount.Funded.By.Investor + Amount.Requested * Amount.Funded.By.Investor + Loan.Length
# Modeling

lm1 <- lm(transformedData$Interest.Rate ~ loansData$FICO.Range);
summary(lm1);
# Plotting fit
par(mfrow=c(1,1));
plot(loansData$FICO.Range, transformedData$Interest.Rate);
points(loansData$FICO.Range, lm1$fitted.values, pch=19, col="red");
# Plotting residulas
par(mfrow = c(1, 3));
plot(loansData$FICO.Range, lm1$residuals);
plot(loansData$Interest.Rate, lm1$residuals);
plot(lm1$fitted.values, lm1$residuals);

# Considering Amount Requested
plot(loansData$FICO.Range, lm1$residuals, col=transformedData$Amount.Requested);
plot(loansData$Interest.Rate, lm1$residuals, col=transformedData$Amount.Requested);
plot(lm1$fitted.values, lm1$residuals, col=transformedData$Amount.Requested);
plot(loansData$FICO.Range, lm1$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(loansData$Interest.Rate, lm1$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(lm1$fitted.values, lm1$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(loansData$FICO.Range, lm1$residuals, col=transformedData$Loan.Length);
plot(loansData$Interest.Rate, lm1$residuals, col=transformedData$Loan.Length);
plot(lm1$fitted.values, lm1$residuals, col=transformedData$Loan.Length);

lm1.1 <- lm(transformedData$Interest.Rate ~ transformedData$FICO.Range);
summary(lm1.1);
# Plotting fit
par(mfrow=c(1,1));
plot(transformedData$FICO.Range, transformedData$Interest.Rate);
points(transformedData$FICO.Range, lm1.1$fitted.values, pch=19, col="red");
# Plotting residulas
par(mfrow = c(1, 3));
plot(transformedData$FICO.Range, lm1.1$residuals);
plot(transformedData$Interest.Rate, lm1.1$residuals);
plot(lm1$fitted.values, lm1.1$residuals);

# Considering Amount Requested
plot(transformedData$FICO.Range, lm1.1$residuals, col=transformedData$Amount.Requested);
plot(transformedData$Interest.Rate, lm1.1$residuals, col=transformedData$Amount.Requested);
plot(lm1$fitted.values, lm1.1$residuals, col=transformedData$Amount.Requested);
plot(transformedData$FICO.Range, lm1.1$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(transformedData$Interest.Rate, lm1.1$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(lm1$fitted.values, lm1.1$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(transformedData$FICO.Range, lm1.1$residuals, col=transformedData$Loan.Length);
plot(transformedData$Interest.Rate, lm1.1$residuals, col=transformedData$Loan.Length);
plot(lm1$fitted.values, lm1.1$residuals, col=transformedData$Loan.Length);
  
# Fit the model with the length
lm2 <- lm(transformedData$Interest.Rate ~ transformedData$FICO.Range + loansData$Loan.Length);
summary(lm2);

# Plotting fit
par(mfrow = c(1, 1));
plot(transformedData$FICO.Range, transformedData$Interest.Rate);
points(transformedData$FICO.Range, lm2$fitted.values, pch=19, col="red");

# Checking improvement with Load length
par(mfrow = c(1, 2));
plot(transformedData$FICO.Range, lm2$residuals, col=transformedData$Loan.Length);
plot(lm2$fitted.values, lm2$residuals, col=transformedData$Loan.Length);

# Checking now the amount
par(mfrow = c(2, 3));
plot(transformedData$FICO.Range, lm2$residuals, col=transformedData$Amount.Requested);
plot(transformedData$Interest.Rate, lm2$residuals, col=transformedData$Amount.Requested);
plot(lm1$fitted.values, lm2$residuals, col=transformedData$Amount.Requested);
plot(transformedData$FICO.Range, lm2$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(transformedData$Interest.Rate, lm2$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(lm1$fitted.values, lm2$residuals, col=transformedData$Amount.Funded.By.Investors);

# Fitting complete model
lm3 <- lm(transformedData$Interest.Rate ~ transformedData$FICO.Range + loansData$Loan.Length + loansData$Amount.Requested + 
          loansData$Amount.Funded.By.Investors + loansData$Amount.Requested * loansData$Amount.Funded.By.Investors);
summary(lm3);

# Checking improvement with Load length
par(mfrow = c(2, 2));
plot(transformedData$FICO.Range, lm3$residuals, col=transformedData$Amount.Requested);
plot(lm2$fitted.values, lm3$residuals, col=transformedData$Amount.Requested);
plot(transformedData$FICO.Range, lm3$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(lm2$fitted.values, lm3$residuals, col=transformedData$Amount.Funded.By.Investors);

# Fitting complete model
lm4 <- lm(transformedData$Interest.Rate ~ transformedData$FICO.Range + loansData$Loan.Length + loansData$Amount.Requested);
summary(lm4);

# Checking improvement with Load length
par(mfrow = c(2, 2));
plot(transformedData$FICO.Range, lm4$residuals, col=transformedData$Amount.Requested);
plot(lm2$fitted.values, lm4$residuals, col=transformedData$Amount.Requested);
plot(transformedData$FICO.Range, lm4$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(lm2$fitted.values, lm4$residuals, col=transformedData$Amount.Funded.By.Investors);

# No relevant improvement from 4 to 3. Keep simpler model 4.

# Fitting complete model
lm5 <- lm(transformedData$Interest.Rate ~ transformedData$FICO.Range + loansData$Loan.Length + transformedData$Amount.Requested);
summary(lm5);

# Checking improvement with Load length
par(mfrow = c(2, 2));
plot(transformedData$FICO.Range, lm5$residuals, col=transformedData$Amount.Requested);
plot(lm2$fitted.values, lm5$residuals, col=transformedData$Amount.Requested);
plot(transformedData$FICO.Range, lm5$residuals, col=transformedData$Amount.Funded.By.Investors);
plot(lm2$fitted.values, lm5$residuals, col=transformedData$Amount.Funded.By.Investors);

# Selected model 4
# Plot final graph
par(mfrow=c(2, 3));
sorted <- sort(transformedData$Interest.Rate, index.return=TRUE);
plot(sorted$x);
points((transformedData$Interest.Rate + lm1$residuals)[sorted$ix], col="green");
plot(sorted$x);
points((transformedData$Interest.Rate + lm1.1$residuals)[sorted$ix], col="blue");
plot(sorted$x);
points((transformedData$Interest.Rate + lm2$residuals)[sorted$ix], col="pink");
plot(sorted$x);
points((transformedData$Interest.Rate + lm3$residuals)[sorted$ix], col="yellow");
plot(sorted$x);
points((transformedData$Interest.Rate + lm4$residuals)[sorted$ix], col="red");
plot(sorted$x);
points((transformedData$Interest.Rate + lm5$residuals)[sorted$ix], col="gray");

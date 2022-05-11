options(max.print=1000000)

data.class(astoriaprices)
astoria_df<-as.data.frame(astoriaprices)
data.class(astoria_df)
dim(astoria_df)

head(astoria_df)
astoria_df$commercial_units <-NULL


head(astoria_df)
astoria_df[astoria_df==0] <- NA

head(astoria_df)
dim(astoria_df)
ast_df <- na.omit(astoria_df)

#na.omit(ast_df)

head(ast_df)

dim(ast_df)
dim(ast_df)

#Histogram
par(mfrow=c(3,3))
hist(ast_df$residential_units, main = "Fig 1. Hist of Residential Units", xlab = "residential_units", ylab = "Freq") # display histograms
hist(ast_df$total_units, main = "Fig 2. Hist of Total Units", xlab = "total_units", ylab = "Freq") 
hist(ast_df$land_sq_ft, main = "Fig 3. Hist of Land Sq Ft", xlab = "Land Sq Ft", ylab = "Freq") 
hist(ast_df$gross_sq_ft, main = "Fig 4. Hist of Gross Sq Ft", xlab = "Gross Sq Ft", ylab = "Freq")
hist(ast_df$tax_class, main = "Fig 5. Hist of Gross Sq Ft", xlab = "Tax Class", ylab = "Freq")
hist(ast_df$age, main = "Fig 6. Hist of Gross Sq Ft", xlab = "Age", ylab = "Freq")
hist(ast_df$sale_price, main = "Fig 7. Hist of Sale Price", xlab = "Sale Price", ylab = "Freq") 

#Scatterplot
par(mfrow=c(3,3))
plot(ast_df$residential_units,ast_df$sale_price, main = "Fig 8. Residential Units v Sale Price",xlab = "Residential Units", ylab = "Sale Price")
plot(ast_df$total_units,ast_df$sale_price, main = "Fig 9. Total Units v Sale Price",xlab = "Total Units", ylab = "Sale Price")
plot(ast_df$land_sq_ft,ast_df$sale_price, main = "Fig 10. Land Sq Ft v Sale Price",xlab = "Land Sq Ft", ylab = "Sale Price")
plot(ast_df$gross_sq_ft,ast_df$sale_price, main = "Fig 11. Gross Sq Ft v Sale Price",xlab = "Gross Sq Ft", ylab = "Sale Price")
plot(ast_df$tax_class,ast_df$sale_price, main = "Fig 12. Tax Class v Sale Price",xlab = "Tax Class", ylab = "Sale Price")
plot(ast_df$age,ast_df$sale_price, main = "Fig 13. Age v Sale Price",xlab = "Age", ylab = "Sale Price")

#Descriptive Statistics
summary(ast_df)
install.packages("YRmisc")
library("YRmisc")
ds.summ(ast_df)

#Correlation Matrix
round(cor(ast_df),2)

#Regression
fit <-lm(sale_price~residential_units+total_units+land_sq_ft+gross_sq_ft+tax_class+age,data=ast_df,na.action=na.omit)
fit$coefficients
fit$coefficients[1]

summary(fit)
summary(fit)$r.squared

fit$fitted.values
fit$residuals

#Residuals
v<-data.frame(ast_df,p=fit$fitted.values,r=fit$residuals)
par(mfrow=c(1,1))
hist(v$r, main = "Hist of Residuals")

plot(v$p,v$sale_price) 


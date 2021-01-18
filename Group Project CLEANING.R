# CLEANING 

#Load Libraries
library(plyr)
library(ggplot2)

#Load Dataset
cdata <- read.csv("df_udemy_unique.csv")

str(cdata) #62040 rows, 21 cols

#Remove columns:
#discount_price__price_string
#price_detail__price_string
#is_wishlisted

cdata$discount_price__price_string <- NULL
cdata$price_detail__price_string <- NULL
cdata$is_wishlisted <- NULL
cdata$avg_rating_recent <- NULL #Represented by rating
cdata$avg_rating <- NULL

str(cdata) #Now down to 16 variables

#Change currency
#discount_price__amount (currency rate) 
#discount_price__currency (INR to MYR)
#price_detail__amount (currency rate)
#price_detail__currency (INR to MYR)

cdata$discount_price__amount <- round(cdata$discount_price__amount * 0.055,2)
head(cdata$discount_price__amount)

cdata$price_detail__amount <- round(cdata$price_detail__amount * 0.055,2)
head(cdata$price_detail__amount)

cdata$discount_price__currency <- "MYR"
cdata$price_detail__currency <- "MYR"
head(cdata)


#Check for NA
colSums(is.na(cdata)) # Note that most NA are in the prices, as some courses are free this value is expected to be NA. 

#Replacing NA to 0

cdata[is.na(cdata)] <- 0

colSums(is.na(cdata))
cdata$discount_price__amount

#Export Data to Working Directory
write.csv(cdata, "Cleaned Data.csv")

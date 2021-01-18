df1 <- read.csv('D:/Principle_dataset/udemy_output_All_Lifestyle_p1_p626.csv')
head(df1)
df2 <- read.csv('D:/Principle_dataset/udemy_output_All_IT__Software_p1_p626.csv')
df3 <- read.csv('D:/Principle_dataset/udemy_output_All_Finance__Accounting_p1_p626.csv')
df4 <- read.csv('D:/Principle_dataset/udemy_output_All_Business_p1_p626.csv')
df_full <- merge(df1,df2,df3,df4)

#use this to merge dataset
setwd('D:/Principle_dataset')
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){
  read.csv(i, header=TRUE)
 })
df <- do.call(rbind.data.frame, All)
write.csv(df,"df_udemy.csv", row.names=FALSE)
head(df)

#Find any duplicated rows

library(tidyverse)
length(unique(df$title)) 
#only 62042 rows are unique, so we need to remove the duplicated rows
length(duplicated(df$title))
df_unique <- df %>% distinct(title, .keep_all = TRUE)
write.csv(df_unique,"df_udemy_unique.csv", row.names=FALSE)

#9/1 start from here
library(tidyverse)
df_unique <- read.csv('D:/Principle_dataset/df_udemy_unique.csv')
table(df_unique$is_paid)
tail(df_unique)
table(df_unique$is_wishlisted)
library(dplyr)
glimpse(df_unique)
str(df_unique)
summary(df_unique)
install.packages('skimr')
library(skimr)
skim(df_unique)

#Currency is in INR
df_unique%>% distinct(discount_price__currency)
df_unique%>% distinct(price_detail__currency)

#1:Create new column for the dates and transfrom the price from INR to MYR
#2:Remove unwanted columns (duplicated column 'discount_price_price_string', price_detail__price_string and currencies)
df2_unique <- mutate(df_unique, created_date = substr(df_unique$created,1,10)) %>%
  mutate(df_unique, published_date = substr(df_unique$published_time,1,10)) %>%
  mutate(df_unique, discount_price__amount_myr = df_unique$discount_price__amount*0.055) %>%
  mutate(df_unique, price_detail__amount_myr = df_unique$price_detail__amount*0.055)%>%
  select(-discount_price__currency,-price_detail__currency, -discount_price__price_string, -price_detail__price_string)
  
install.packages("datetime")

head(df2_unique)
library(datetime)
df2_unique$created_date2 <- as.date(df2_unique$created, format = '%Y-%m-%d')
head(df2_unique)
colSums(is.na(df2_unique))
df2_unique <- select(df2_unique,-crteated_year)
head(df2_unique)

boxplot(df2_unique$price_detail__amount_myr)
df2_unique$price_detail__amount_myr[is.na(df2_unique$price_detail__amount_myr)]<-0
boxplot(df2_unique$price_detail__amount_myr)
hist(df2_unique$avg_rating)
write.csv(df2_unique,"df_udemy_unique2.csv", row.names=FALSE)
#save(df2_unique, file = "df_udemy_unique2.rda")
#a <-count(df2_unique[df2$Category == 'Business'])
#b<- df2_unique %>% group_by(Category)
#freq_course <- df2_unique %>%
  #count(Category == 'Business')
#freq_course$n[freq_course$Category == 'Business']
#df2 <- df2_unique %>%  summarise(
  #mean_rat = mean(avg_rating)
#)
#df2
freq_course <- df2_unique %>% group_by(Category) %>%
   summarise(Category_num = n(), .groups = 'drop') %>% filter(Category == 'Lifestyle')
a <- unique(freq_course$title)
a <- as.numeric(a)
a
df2_unique <- read.csv('D:/Principle_dataset/df_udemy_unique2.csv')
course_bestrat <- df2_unique %>% select(title, avg_rating, Category, is_paid) %>% filter(Category == 'Business') %>% filter(is_paid == 'TRUE')
course_bestrat_top <- course_bestrat[order(course_bestrat$avg_rating),]
course_bestrat_top <- tail(course_bestrat_top, 12)
course_bestrat_top <- head(course_bestrat_top, 10)
r <- c()
for(i in 10:1){r <- c(r, i)}
row.names(course_bestrat_top) <- r
course_bestrat_top
options(repr.plot.width=15, repr.plot.height=11)
plot3 <- ggplot(data = course_bestrat_top, mapping = aes(x = reorder(title, -avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", mapping = aes(fill = Category, color = Category), alpha = .8, size = 1.5) +
  coord_flip() +
  geom_label(mapping = aes(label = round(avg_rating, 1)), size = 4, fill = "#F5FFFA", fontface = "bold") + ggtitle("Top 10 best rated paid courses")
plot3
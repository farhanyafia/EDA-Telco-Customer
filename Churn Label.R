##import library
library(tidyverse)
library(tidyr)
library(ggplot2)
library(readxl)
library(dplyr)
library(readxl)
library(sqldf)
library(CatEncoders)
library(ggcorrplot)
library(caret)

## load data
df_demograph <- read_excel("D:/Kerja/Data Analyst/Project/si padil/Telco_customer_churn_demographics.xlsx")
df_location <- read_excel("D:/Kerja/Data Analyst/Project/si padil/Telco_customer_churn_location.xlsx")
df_population <- read_excel("D:/Kerja/Data Analyst/Project/si padil/Telco_customer_churn_population.xlsx")
df_services <- read_excel("D:/Kerja/Data Analyst/Project/si padil/Telco_customer_churn_services.xlsx")
df_status <- read_excel("D:/Kerja/Data Analyst/Project/si padil/Telco_customer_churn_status.xlsx")
head(df_demograph)
head(df_status)
head(df_population)
head(df_services)
head(df_location)


## 1. 10 kota dengan pelanggan churn terbanyak
# data processing
df1 <- list(df_location,df_status)
f1 <- df1 %>% reduce(left_join, by='Customer ID') %>% select(c(`City`, `Churn Label`)) %>% 
  filter(`Churn Label` == "Yes") %>%  count(`City`) %>% rename(Value=n) %>% arrange(-`Value`)%>% 
  top_n(10, `Value`)

# set graphic bar
Grap1 <-ggplot(f1, aes(x=City, y=Value)) +
  geom_bar(stat='identity',color='red',fill='red') +
  theme(axis.text.x=element_text(angle=45, hjust=0.9))+
  labs(title="Top 10 City with the Most Churn", subtitle="Sample of Churn Label",
       caption="Collected by Mr. abang yapi")
Grap1+annotate("text",x=10,y=250,label="San Diego are The largest",color="Black",
               fontface="bold", size=4.5)

## 2. berapa rata-rata umur pelanggan
#data processing
df2 <- list(df_status,df_demograph)
f2 <- df2 %>% reduce(left_join, by='Customer ID') %>% select(c(`Age`, `Churn Label`)) %>%
  group_by(`Churn Label`) %>% summarise(mean_age= mean(`Age`)) 

# set graphic bar
Grap2 <-ggplot(f2, aes(x=`Churn Label`, y=`mean_age`)) +
  geom_bar(stat="identity",color='Green',fill='Green')+
  theme(axis.text.x=element_text(angle=0, hjust=0.9))+
  labs(title="Average age in Churn Label Suscriber", subtitle="Sample of Churn Label",
       caption="Collected by Mr. abang yapi")
Grap2+annotate("text",x=2,y=75,label="Mean in every Churn Label",color="Black",
               fontface="bold", size=5.0)

## 3. berapa banyak pelanggan churn berdasarkan gender
#data processing
df3 <-list(df_status,df_demograph)
f3 <- df3 %>% reduce(left_join, by='Customer ID') %>% select(c(`Gender`, `Churn Label`)) %>% 
  filter(`Churn Label` == "Yes") %>% group_by(`Gender`) %>% count(`Gender`)

# set graphic bar
Grap3 <-ggplot(f3, aes(x=Gender, y=n)) +
  geom_bar(stat='identity',color='red',fill='red') +
  theme(axis.text.x=element_text(angle=0, hjust=0.9))+
  labs(title="Total Churn Label Subs by Gender", subtitle="Sample of Churn Label",
       caption="Collected by Mr. abang yapi")
Grap3+annotate("text",x=2,y=1000,label="Female more bigger",color="Black",
               fontface="bold", size=4.5)

## 4. berapa banyyak pelanggan churn berdasarkan jenis kewarganegaraanya
#data processing
df4 <-list(df_status,df_demograph)
f4 <- df3 %>% reduce(left_join, by='Customer ID') %>% select(c(`Senior Citizen`,`Married`,`Dependents`, `Churn Label`)) %>% 
  group_by(`Churn Label`)

# set graphic bar
Grap4 <-ggplot(data=f4)+
  geom_bar(mapping=aes(x=`Senior Citizen`, fill=`Married`, group=`Churn Label`)) +
  facet_wrap(~`Married`)

## 5. Kota manakah yang paling banyak populasinya
#data procesing 
df5 <- list(df_population,df_location) %>% reduce(left_join, by='Zip Code') %>% select(c(`City`, `Population`))
f5 <- df5 %>% group_by(`City`) %>% drop_na() %>% summarise(Total=sum(`Population`)) %>% top_n(10, `Total`) %>% arrange(-Total)

# set graphic bar
Grap5 <- ggplot(f5, aes(x=reorder(City,-Total), y=Total))+
  geom_bar(stat='identity',color='blue',fill='blue')
Grap5+annotate("text",x=2,y=15000000,label="Mean in every Churn Label",color="Black",
               fontface="bold", size=5.0)

## 6. alasan apa yang paling banyak customer churned?
df6 <-df_status %>% select(c(`Churn Reason`,`Churn Label`))
f6<- df6 %>% filter(`Churn Label` == "Yes")

# set graphic bar
Grap6 <-ggplot(data=df6)+
  geom_bar(mapping=aes(x=`Churn Reason`,group=`Churn Reason`))

## 7. rata-rata nilai kepuasan yang churn
df7 <- df_status %>% select(c(`Satisfaction Score`, `Churn Label`))
f7 <- df7 %>% group_by(`Churn Label`) %>% drop_na() %>% summarise(Mscore= mean(`Satisfaction Score`))

# set graphic bar
Grap7 <-ggplot(f7, aes(x=`Churn Label`, y=`Mscore`)) +
  geom_bar(stat="identity",color='Green',fill='Green')+
  theme(axis.text.x=element_text(angle=0, hjust=0.9))+
  labs(title="Average Satisfaction Score in Churn Label Suscriber", subtitle="Sample of Churn Label",
       caption="Collected by Mr. abang yapi")
Grap7+annotate("text",x=2,y=5,label="Mean in every Churn Label",color="Black",
               fontface="bold", size=5.0)

## 8. Jenis kontrak yang paling banyak pada pelanggan churn
df8 <- list(df_services,df_status) %>% reduce(right_join, by='Customer ID') %>% select((c(`Contract`, `Churn Label`)))
f8 <- df8 %>% group_by(`Churn Label`) %>% drop_na()

# set graphic bar
Grap8 <-ggplot(data=df8)+
  geom_bar(mapping=aes(x=`Contract`,))+
  theme(axis.text.x=element_text(angle=0, hjust=0.9))+
  labs(title="The Most Churn Lable According Contract ", subtitle="Sample of Churn Label",
       caption="Collected by Mr. abang yapi")
Grap8+annotate("text",x=2.7,y=3500,label="Month-to-month is more bigger ",color="Blue",
               fontface="bold", size=5.0)

## 9. Berapa nilai tengah dari biaya bulanan yang dikeluarkan pelanggan churn?
df9 <- list(df_services,df_status) %>% reduce(left_join, by='Customer ID') %>% select((c(`Monthly Charge`, `Churn Label`)))
f9 <- df9 %>% group_by(`Churn Label`) %>% drop_na()

#setting up partision 
y0 <-  f9 %>% summarise(min(`Monthly Charge`)) 
y50 <-  f9 %>% summarise(median(`Monthly Charge`)) 
y100 <-  f9 %>% summarise(max(`Monthly Charge`)) 

# Set boxplot
Grap9 <-ggplot(f9, aes(x=`Churn Label`, y=`Monthly Charge`))+
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4, fill="green", colour= "black")+
  theme(axis.text.x=element_text(angle=0, hjust=0.9))+
  labs(title="Median Value monthly Charge", subtitle="Sample of Churn Label",
       caption="Collected by Mr. abang yapi")
Grap9+annotate("text",x=2.1,y=130,label="Median in every Churn Label",color="Black",
               fontface="bold", size=3.0)

## 10. Pelanggan manakah yang mempunyai total revenue terbesar dengan status churn?
df10 <- list(df_services,df_status) %>% reduce(left_join, by='Customer ID') %>% filter(`Churn Label` == "Yes")

# Categorical features
df_Cf = df10 %>% select((c(`Referred a Friend`,`Offer`,`Phone Service`,`Multiple Lines`,`Internet Service`,
                       `Internet Type`,`Online Security`,`Online Backup`,`Device Protection Plan`,`Premium Tech Support`,
                       `Streaming TV`,`Streaming Movies`,`Streaming Music`,`Unlimited Data`,`Contract`,`Paperless Billing`,
                       `Payment Method`,`Total Revenue`)))
# rubah menjadi numerical
dfcf <- lapply(df_Cf, function(x) as.numeric(as.character(x))) 

grap10 = ggplot(df_Cf, aes(`Total Revenue`,`Contract`,`Monthly Charge`, fill = `Total Revenue`))+
  geom_tile()

model.matrix(~0+., data=df_Cf) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2, sig.level = 0.5)

#PREPROCESSING
#ambil categorical data
cat_data <- df10 %>% select((c(`Referred a Friend`,`Offer`,`Phone Service`,`Multiple Lines`,`Internet Service`,
                           `Internet Type`,`Online Security`,`Online Backup`,`Device Protection Plan`,`Premium Tech Support`,
                           `Streaming TV`,`Streaming Movies`,`Streaming Music`,`Unlimited Data`,`Contract`,`Paperless Billing`,
                           `Payment Method`)))
#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=cat_data)

#perform one-hot encoding on data frame
x <- data.frame(predict(dummy, newdata=cat_data))

#ambil kolom target
y <- df10 %>% select(`Total Revenue`)
cor <- cor(x, y, use = "pairwise.complete.obs") #ngitung korelasi feature x target
df_cor <- data.frame(cor) #ubah ke dataframe
df_cor <- df_cor  %>% filter(`Total.Revenue` > 0.4 | `Total.Revenue` < -0.5) #filter features yang punya nilai korelasi tinggi

#UBAH BENTUK DATA
df_cor['target'] <- 'Total Revenue' #bikin kolom baru buat nama target
df_cor <- cbind(features = rownames(df_cor), df_cor) #bikin kolom baru yang nilainya dari index
rownames(df_cor) <- 1:nrow(df_cor) #bikin index baru

#VISUALISASI
Grap_11 <- ggplot(df_cor, aes(`target`, `features`)) + # Create heatmap with ggplot2
  geom_tile(aes(fill = `Total.Revenue`))
Grap_11               


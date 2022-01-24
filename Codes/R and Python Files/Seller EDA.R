##Sellers EDA and EDA - Olivia Aneye

library(data.table)
library(ggplot2)
library(broom)
library(sandwich)
library(dplyr)
library(readxl)
library(readr)
library(rio)


Olist <- read_excel("Olistf.xlsx") # Merged dataset

# Data validation: 

nrow(Olist) # Total # of rows in merged dataset
# 118315

ncol(Olist) 
#23

summary(Olist)
names(Olist)


# EDA:

summary(Olist)


### I- Original Sellers dataset from Kaggle:

Sellers <- read_excel("Sellers.xlsx")

class(Sellers)

ncol(Sellers)
names(Sellers)
summary(Sellers)

length(unique(Sellers$seller_id))  # 3095 distinct sellers. Same as # from summary ==> no repeated value
length(unique(Sellers$seller_city)) # 611. Some sellers are in the same city
length(unique(Sellers$seller_zip_code_prefix)) # 2246 sellers zip codes, so some zip codes are repeated 
length(unique(Sellers$seller_state)) # 23 states

## Finding sellers with largest orders

summary(Olist)

Sellers_1 <- Olist %>% select(price, seller_id) %>% print(n=10)

Sellers_ordered <- Sellers_1 %>% arrange(desc(price)) %>% print(n = 10) #Top 10 sellers with largest price items value

# 1. e3b4998c7a498169dc7bce44e6bb6277
# 2. 80ceebb4ee9b31afb6c6a916a574a1e2
# 3. ee27a8f15b1dded4d213a468ba4eb391
# 4. 59417c56835dd8e2e72f91f809cd4092
# 5. 59417c56835dd8e2e72f91f809cd4092
# 6. c72de06d72748d1a0dfb2125be43ba63
# 7. 512d298ac2a96d1931b6bd30aa21f61d
# 8. ed4acab38528488b65a9a9c603ff024a
# 9: 821fb029fc6e495ca4f08a35d51e53a5
# 10: fa1c13f2614d7b5c4749cbc52fecda94


names(Sellers)

## Which sellers have the lowest priced items?

tail(Sellers_ordered,10)

# Ordered by price

# 1. 2709af9587499e95e803a6498a5a56e9
# 2  1.2  2709af9587499e95e803a6498a5a56e9
# 3  1.2  2709af9587499e95e803a6498a5a56e9
# 4  1.2  2709af9587499e95e803a6498a5a56e9
# 5  1.2  2709af9587499e95e803a6498a5a56e9
# 6  1.2  2709af9587499e95e803a6498a5a56e9
# 7  1.2  2709af9587499e95e803a6498a5a56e9
# 8  0.85 96804ea39d96eb908e7c3afdb671bb9e
# 9  0.85 96804ea39d96eb908e7c3afdb671bb9e
# 10  0.85 96804ea39d96eb908e7c3afdb671bb9e

## Sellers ordered by city

                                                              
Sellers_by_city_desc <- Sellers %>% count(seller_city) %>% arrange(desc(n)) %>% print(n=10)

Sellers_by_city_asc <- Sellers %>% count(seller_city) %>% arrange(n) %>% print(n=10)

filter(Sellers_by_city_desc, n == 1) # Cities with only 1 seller


## Sellers grouped by zipcodes:

Sellers_by_zip_desc <- Sellers %>% count(seller_zip_code_prefix) %>% arrange(desc(n)) %>% print(n=10)

Sellers_by_zip_asc <- Sellers %>% count(seller_zip_code_prefix) %>% arrange(n) %>% print(n=10)

filter(Sellers_by_zip_desc, n == 1) # zip with only 1 seller


## Sellers by states:

Sellers_by_state_desc <- Sellers %>% count(seller_state) %>% arrange(desc(n)) %>% print(n=10)

## Sellers by Cities:

filter(Sellers_by_city_desc, n == 1)


## Top Sellers by total number of orders:

Sellers_by_ordersn <- Olist %>% count(seller_id) %>% arrange(desc(n)) %>% print(n=10)

Olist %>% count(seller_id) %>% arrange(n) %>% print(n=20)

Top_Five_Sellers <- head(Sellers_by_ordersn)

## Top_Five_Sellers     

Top_Five_sellers_info <- Sellers %>% filter(seller_id %in% c("4a3ca9315b744ce9f8e9374361493884", "6560211a19b47992c3666cc44a7e94c0",
                                                   "1f50f920176fa81dab994f9023523100", "cc419e0650a3c5ba77189a1882b7556a",
                                                   "da8622b14eb17ae2831f4ac5b9dab84a")) %>% print
# All info about top 5 sellers:

Top_Five_sellers_info # From original sellers raw file, not merged dataset

# Top 5 sellers info from merged Dataset (Olist)

Top_Five_Sellers_Olist <- Olist %>% filter(seller_id %in% c("4a3ca9315b744ce9f8e9374361493884", "6560211a19b47992c3666cc44a7e94c0",
                                                              "1f50f920176fa81dab994f9023523100", "cc419e0650a3c5ba77189a1882b7556a",
                                                              "da8622b14eb17ae2831f4ac5b9dab84a")) 
Top_100_sellers

Top_Seller <- Olist %>% filter(seller_id == "4a3ca9315b744ce9f8e9374361493884") %>% print

names(Top_Seller)

Order_price <- Olist %>% group_by(seller_id) %>% summarize(Total_Sales = sum(price))%>% print(n=10)

## A look at some statistics 

sd(Olist$freight_value)
summary(Olist$freight_value)                                                                                                    
summary(Olist$payment_value)   


# Top Seller by number of orders:

Top_Seller_Olist <- Olist %>% filter(seller_id == "4a3ca9315b744ce9f8e9374361493884") %>% print

Sellers_Olist <- select(Olist, seller_id, freight_value, price, payment_value, order_purchase_date, order_estimated_delivery_date, order_delivered_date)
print(Sellers_Olist, 10)  # Extracting variables of interest only                                    

cor(Top_Seller$price, Top_Seller$freight_value) 

cor(Top_Seller$payment_value, Top_Seller$freight_value)

sd(Top_Seller$freight_value)
summary(Top_Seller$freight_value)

summary(Top_Seller$payment_value) # Top seller has on average $142.06 Braz. currency orders

Top_Seller %>% filter(freight_value >= 17.50) %>% count(seller_id)

Top_Ten_Sellers <- head(Sellers_by_ordersn,10)
Top_Ten_Sellers

# Max freight value is 224.34. Outlier? Why so big? 

nrow(Top_Seller)


## Freight ratio: How much of total order value does freight represent?

Olist$freight_ratio <- Olist$freight_value/Olist$payment_value

summary(Olist$freight_ratio)

mean(Olist$freight_ratio) ## Returns a weird number for mean: "inf". Take a closer look why

export(Sellers_by_ordersn,"Top_Sellers_Orders.csv")
export(Top_Five_Sellers,"Top_5_Sellers_Orders.csv")
export(Top_Ten_Sellers,"Top_10_Sellers_Orders.csv")
export(Sellers_by_state_desc, "Sellers_By_State.csv")
export(Sellers_by_city_desc, "Sellers_By_City.csv")


## Focus on sellers in remote areas: "Isolated sellers"

# Map in Tableau to find their states & cities

# From cities:

Isolated_Sellers <- Sellers %>% filter(seller_city %in% c("rio branco", "porto velho","manaus", "ji parana",
                                                           "sinop", "cuiaba", "jaciara", "coxim")) %>% print



# BAsed on state: better route

Isolated_Sellers_State <- Sellers %>% filter(seller_state %in% c("AM", "PA", "AC", "RO", "PI", "SE")) %>% print

names(Sellers)

Isolated_Sellers_State_Olist <- Olist %>% filter(seller_id %in% c("4be2e7f96b4fd749d52dff41f80e39dd", "3364a91ec4d56c98e44174de954b94f6","47efca563408aae19bb7206c2d969ea9",
                                                                  "4b39558c138930b9e5489c93b6df5fe3", "c53bcd3be457a342a97e39e5a9f0be22", "06a2c3af7b3aee5d69171b0e14f0ee87 ",
                                                                  " a5259c149128e82c9d6d46e0c1c812bb", "327b89b872c14d1c0be7235ef4871685  ",
                                                               " 67225bff54a172ff67579aaf583efd78")) %>% print

export(Isolated_Sellers_State_Olist, "Isolated Sellers with Products.csv")   

Isolated_Sellers_by_orders <- Isolated_Sellers_State_Olist %>% count(seller_id) %>% arrange(desc(n)) %>% print

Isolated_Sellers_State_Olist_Details <- Isolated_Sellers_State_Olist %>% select(seller_id, price, payment_value, freight_value, order_purchase_date, 
                                                                                                          order_estimated_delivery_date, order_delivered_date) %>% print
export(Isolated_Sellers_State_Olist_Details, "Isolated Sellers Details.csv")


Sellers_by_ordersn

Least_performing_Sellers <- Sellers_by_ordersn %>% filter(n <= 2) %>% select(seller_id) %>% print

Heighest_Freight_Sellers <- Olist %>% filter(freight_ratio >= 0.5) %>% print

Olist %>% filter(freight_ratio <=0.25) %>% select(seller_id, customer_city, customer_state) %>% print


# Top 5 sellers freight ratios:

Top_Five_Sellers_Olist

Top_5_sellers_Freight <- Top_Five_Sellers_Olist %>% select(seller_id, freight_ratio) %>% print

summary(Top_5_sellers_Freight)



# Looking at revenue and payments:

Revenue_by_Sellers <- Olist %>% group_by(seller_id) %>% summarize(Total_Revenue = sum(payment_value)) %>% select(seller_id, Total_Revenue) %>% 
  arrange(desc(Total_Revenue)) %>% print
names(Olist)

Isolated_Sellers_Revenue <- Isolated_Sellers_State_Olist %>% group_by(seller_id) %>% summarize(Total_Revenue = sum(payment_value)) %>% 
  select(seller_id, Total_Revenue) %>% arrange(desc(Total_Revenue)) %>% print

Isolated_Sellers_Avg_Payment <- Isolated_Sellers_State_Olist %>% group_by(seller_id) %>% summarize(Avg_sale = mean(payment_value)) %>% 
  select(seller_id, Avg_sale) %>% arrange(desc(Avg_sale)) %>% print


Isolated_Sellers_State_Olist <- Olist %>% filter(seller_id %in% c("4be2e7f96b4fd749d52dff41f80e39dd", "3364a91ec4d56c98e44174de954b94f6","47efca563408aae19bb7206c2d969ea9",
                                                                  "4b39558c138930b9e5489c93b6df5fe3", "c53bcd3be457a342a97e39e5a9f0be22", 
                                                                  "a5259c149128e82c9d6d46e0c1c812bb", "327b89b872c14d1c0be7235ef4871685",
                                                                  "67225bff54a172ff67579aaf583efd78")) %>% print

## Look at Isolated Sellers Products:

Isolated_Sellers_Products <- table(Isolated_Sellers_State_Olist$product_category_name_english)
Isolated_Sellers_Products


## 26 out of 48 of those are auto items.

Sellers_for_auto <- Isolated_Sellers_State_Olist %>% filter(product_category_name_english == "auto") %>% 
  distinct(seller_id,.keep_all = FALSE) %>% print

Sellers %>% filter(seller_id %in% c("47efca563408aae19bb7206c2d969ea9", "3364a91ec4d56c98e44174de954b94f6")) %>% print

## 2 sellers, in states RO and PI sold the auto items. 

Isolated_Sellers_ordersn <- Isolated_Sellers_State_Olist %>% count(seller_id) %>% arrange(desc(n)) %>% print




export(Isolated_Sellers_State_Olist, "Isolated Sellers Details.csv")

Isolated_Sellers_freight <- Isolated_Sellers_State_Olist %>% group_by(seller_id) %>% summarize(Avg_Freight_Ratio = mean(freight_ratio))%>%
  print

Top_100_sellers <- head(Sellers_by_ordersn, 100)
Top_100_sellers
export(Top_100_sellers, "Top 100 Sellers Orders.csv")

export(Isolated_Sellers_ordersn, "Remote Sellers Orders.csv")
export(Isolated_Sellers_State, "Remote Sellers States.csv")

Remote1 <- merge(Isolated_Sellers_State, Isolated_Sellers_ordersn, by="seller_id" )
Remote1
Isolated_Sellers_Revenue <- Isolated_Sellers_State_Olist %>% group_by(seller_id) %>% summarize(Total_Revenue = sum(payment_value)) %>% 
  select(seller_id, Total_Revenue) %>%print

Remote2 <- merge(Remote1, Isolated_Sellers_freight, by="seller_id")
Remote2

Remote3 <- merge(Remote2, Isolated_Sellers_Revenue, by="seller_id")
Remote3

Remote <- merge(Remote3, Isolated_Sellers_Avg_Payment, by="seller_id") # All remote or isolated sellers info
Remote
export(Remote, "Remote Sellers Info.csv")

Revenue_Top_100 <- head(Revenue_by_Sellers,100) # Top 100 sellers by revenue
Revenue_Top_100

Revenue_by_Sellers <- Olist %>% group_by(seller_id) %>% summarize(Total_Revenue = sum(payment_value)) %>% select(seller_id, Total_Revenue) %>% 
  arrange(desc(Total_Revenue)) %>% print

Revenue_Top_100 <- head(Revenue_by_Sellers,100) # Top 100 sellers by revenue
Revenue_Top_100

### Back to Freight Analysis:

Avg_Freight_Ratio <- Olist %>% group_by(seller_id) %>% summarize(Avg_Freight_Ratio = mean(freight_ratio))


Top_10_Sellers_Freight <- Avg_Freight_Ratio %>% filter(seller_id %in% c("4a3ca9315b744ce9f8e9374361493884", "6560211a19b47992c3666cc44a7e94c0",
                                                                        "1f50f920176fa81dab994f9023523100", "cc419e0650a3c5ba77189a1882b7556a",
                                                                        "da8622b14eb17ae2831f4ac5b9dab84a", "955fee9216a65b617aa5c0531780ce60",
                                                                      "1025f0e2d44d7041d6cf58b6550e0bfa", "7c67e1448b00f6e969d365cea6b010ab",
                                                                        "7a67c85e85bb2ce8582c35f2203ad736", "ea8482cd71df3c1969d7b9473ff13abc")) %>% print

export(Avg_Freight_Ratio, "Freight Ratios.csv")

High_Freight <- Avg_Freight_Ratio %>% filter(Avg_Freight_Ratio > 1.0) %>% arrange(desc(Avg_Freight_Ratio)) %>% print # freight ratio >=1.0!

High_Freight_Sellers <- head(High_Freight, 15)
High_Freight_Sellers

High_Freight_Sellers_Orders <- merge(Sellers_by_ordersn,High_Freight_Sellers, by="seller_id")
High_Freight_Sellers_Orders

Highest_Freight_Sellers_Performance <- High_Freight_Sellers_Orders %>% filter(Avg_Freight_Ratio != Inf) %>% print
export(Highest_Freight_Sellers_Performance, "Highest Freight.csv")

summary(Olist$payment_value)

summary(Isolated_Sellers_State_Olist)
names(Olist)


Highest_Freight_Location <- Sellers %>% filter(seller_id %in% c("403aa86912527d730337ffcb0fb096ab", "daeb5653dd96c1b11860f72209795012", "f3295428338a40977a03f555246a70f4",
                                                                "c68fb906c8f4b4b946d8386bfa6e5467", "cac876b37d3abcd6bd76caca30277996","1a3df491d1c4f1589fc2b934ada68bf2",
                                                                "c864036feaab8c1659f65ea4faebe1da", "117cfc326c6d50da67ca858ff5c0c852", "117cfc326c6d50da67ca858ff5c0c852",
                                                                "800214c63934acd05d47e84214db8ba8", "9dd59e43f0c9f0553244e8f9c2fb247e")) %>% print
Highest_Freight_Info <- merge(Highest_Freight_Sellers_Performance,Highest_Freight_Location, by="seller_id")

export(Highest_Freight_Info, "Highest freight Info.csv")
cor(Highest_Freight_Sellers_Performance$n, Highest_Freight_Sellers_Performance$Avg_Freight_Ratio) 





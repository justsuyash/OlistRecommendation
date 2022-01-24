setwd("~/Desktop/School 2.0/archive/Cleaned")
library(data.table)
library(dplyr)
#import files
final = fread('final.csv')
cust_data = final
#1. Initial Data prep ----------------
#took out all the columns I didn't need
cust_data$order_status = NULL
cust_data$payment_sequential = NULL
cust_data$payment_type = NULL
cust_data$payment_installments = NULL
cust_data$review_id = NULL
cust_data$review_score = NULL
cust_data$product_category_name = NULL
cust_data$product_name_lenght = NULL
cust_data$product_description_lenght = NULL
cust_data$product_photos_qty = NULL
cust_data$product_weight_g = NULL
cust_data$product_length_cm = NULL
cust_data$product_height_cm = NULL
cust_data$product_width_cm = NULL
cust_data$product_category_name_english = NULL


head(cust_data)

#2. Initial EDA-------------------
#looking at number of customers in each city and state
cities = table(cust_data$customer_city)
barplot(cities)
#too many cities to see in a bar graph
states = table(cust_data$customer_state)
states
sort(states, decreasing = T)
barplot(states)

#most popular states are SP (Sao Paulo 45M people), RJ (Rio de Janeiro 17M), and 
#MG (Minas Gerais 21M people )
brazil = fread('brazil info.csv')
class(brazil)
states_df = data.table(states)


#number of customers per order
orders_per_customer = data.table(table(cust_data$customer_id))
names(orders_per_customer) = c('customer_id', 'N')
summary(orders_per_customer)
hist(orders_per_customer$N, main = 'Count of orders by customer',
     xlab = 'Number of orders')
# number of customers who have multiple orders
repeats_custs = duplicated(cust_data$customer_id)
table(repeats_custs)
#the vast majority of the customers only had 1 order
#so I looked at customers that had more than 1 order
repeat_customers = orders_per_customer[N > 1]
nrow(repeat_customers)
hist(repeat_customers$N, main = '# of orders for repeat customers',
     xlab = 'Number of orders', labels = T)

#3. More data prep needed-----------------
#here I found out that some of the order ID's were repeated. Every time a customer had  
  #bought more than 1 item, the order items table would have a row for each item bought.
#So for example, if a customer bought 2 toothbrushes, there would be two rows with the exact
  #same data except for 1 column: "Order_Item_ID" which functioned as a counter 
#This means customers could buy multiple items and appear to be "repeat customers" when they had only made 1 order
#So I made a table that eliminated all the extra rows and summed them into 1 row.
Orders_unrepeated = cust_data %>%
  distinct(order_id, customer_id,
           product_id,seller_id,price,freight_value, .keep_all = TRUE)
head(Orders_unrepeated)
sapply(Orders_unrepeated, class)

#Orders unrepeated is a list of orders with every row a different product
Orders_unrepeated$NumberOfProducts = as.numeric(Orders_unrepeated$order_item_id)
Orders_unrepeated$order_item_id = NULL
sapply(Orders_unrepeated, class)
Orders_unrepeated$total_price = Orders_unrepeated$price * Orders_unrepeated$NumberOfProducts


#adds up the total price and number of products for each order id
Price_Per_Order = aggregate(Orders_unrepeated$price, by = list(Orders_unrepeated$order_id), FUN = sum)
Products_per_Order = aggregate(Orders_unrepeated$NumberOfProducts, by=list(Orders_unrepeated$order_id), FUN = sum)
names(Price_Per_Order) = c('order_id', 'transaction_price')
names(Products_per_Order) = c('order_id', 'Total_products_bought')
#cust_data2 has total sums of number of products and price
cust_data2 = data.table(merge(Price_Per_Order, Orders_unrepeated, by = 'order_id'))
cust_data2 = data.table(merge(Products_per_Order, cust_data2, by='order_id'))

#here all the duplicate orders are deleted
#this makes the table a summary of all orders, how much was paid, and how many items were bought
cust_data2 = cust_data2[!duplicated(order_id),]
#same number of rows means no repeat customers
length(unique(cust_data2$customer_id))
cust_data2$total_price = NULL #the total-price and number of products column are now outdated
cust_data2$NumberOfProducts = NULL #they have turned into Transaction Price, and Total Products Bought, respectively
cust_data2$product_id = NULL #since there are different product ids in an order, this column is wrong
head(cust_data2)

#some of the orders had an order_id, but do not appear in the order items table. This is why the NAs must be removed
table(is.na(cust_data2$seller_id)) #776 values have an order listed, but are not matched in the order items table
cust_data2 = cust_data2[!is.na(seller_id)]


#4. EDA----------
#4a. Where are the customers located? ------------
#I divided this question up into 3 sections: state, city, and zip code
#State
state_sales = data.table(table(cust_data2$customer_state))
names(state_sales) = c('Code', 'NumberOfSales')
sort(table(cust_data2$customer_state),decreasing = T)
barplot(table(cust_data2$customer_state))
#Unsurprisingly, the three states with the highest population had the most sales. 
  #Sao Paulo, Rio de Janeiro, and Minas Gerais had the largest number of orders, respectively. 
  #There was a large drop-off between these 3 states and the rest. 
brazil = left_join(brazil, state_sales, by ='Code')
brazil = mutate(brazil,sales_per_mil = NumberOfSales/Population*1000000)
state_sales_table = brazil %>%
  select(Code, sales_per_mil)
barplot(state_sales_table$sales_per_mil, names.arg = state_sales_table$Code,
        cex.names = .75)
state_sales_table[order(-sales_per_mil),]
#I also made a graph of the sales on a per capita basis. 
    #Sao Paulo and Rio de Janeiro were still in the top 2, but the Distrito Federal, a relatively small state, jumped into the top 3. 
    #A cluster of 5 other states, including Minas Gerais, was right below those 3.
#City
top_paying = cust_data2[transaction_price >0] %>%
  group_by(customer_city, customer_state) %>%
  summarize(Amt_Money = sum(transaction_price))
head(top_paying[order(-top_paying$Amt_Money),],10) #top 10 cities by amount paid
most_buying = cust_data2[Total_products_bought > 5] %>% #list of the number of products bought by city
  group_by(customer_state, customer_city) %>%
  summarize(result = sum(Total_products_bought))
head(most_buying[order(-most_buying$result),],10) #top 10 cities by number of sales
#The top 4 cities were Sao Paulo, Rio de Janeiro, Belo Horizonte, and Brasilia. 
  #Brasilia is in the Distrito Federal, which explains why that state had such a high per capita. 
  #Belo Horizonte is in Minas Gerais. 
  #These are four out of the six most populous cities in Brazil.
#One potential city for improvement is Fortaleza, which is the fifth most populated city in Brazil, but isn’t anywhere in the top 10 of sales.
#Zip Code
zip_codes = data.table(table(cust_data2$customer_zip_code_prefix))
names(zip_codes) = c('customer_zip_code_prefix', 'N')
zip_codes = zip_codes[order(-zip_codes$N)]
top_10_zip_codes = head(zip_codes, 10); top_10_zip_codes
top_10_zip_codes$customer_zip_code_prefix = as.numeric((top_10_zip_codes$customer_zip_code_prefix))
#There were too many zip codes to make a histogram of, so I looked at the top 10 zip codes. 
  #The top 5 were all in Rio de Janeiro, in fact, Rio had 6 of the top 10 zip codes. 
  #The others were in Espirito Santo, Minas Gerais (twice), and Sao Paulo.

#4b. What type of areas are the zip codes in? -------------------
#I had to do this manually. I looked up a couple of the top 10 zip codes. 
  #Most of the zip codes in Rio (22790, 22793, 22775, and 22631) are from around the Barra de Tijuca, which seems to be an upscale living area. 
  #Two others from Rio (24220 and 24230) are from the Guanabara Bay, near Icarai, a beach area. 

#4c. How many are returning customers?----------
#Initially I did an analysis based on the customer ID. 
  #I counted how many times each customer ID showed up in the data (see section 2 - Initial EDA)

#However, when I looked closer at the repeat customers, 
  #I saw that there was a new row in the data table for every item purchased. 
  #When I summed up the numbers, no customer was a repeat customer.
cust_data2[duplicated(customer_id),] 

#4d. Who are the customers that haven't purchased in the longest time?-----------
#This is hard to answer because there are no repeat customers. 
  #However, 103 customers had orders on 9/9/2017, which were the first orders in the database.
cust_data2[order_purchase_date == '2017-09-09']

#4e. Who are the top customers?
#Who paid the most money and bought the most products?
hist(cust_data2$transaction_price, labels = T)
hist(cust_data2$Total_products_bought, labels = T)
quantile(cust_data2$transaction_price, probs = seq(0,1,.05)) 
quantile(cust_data2$Total_products_bought,probs = seq(0,1,.05)) #95% of orders only had one item
percentile95_location = cust_data2[transaction_price >379.9] %>%
  group_by(customer_city, customer_state) %>%
  summarize(top_cust_count = n())
head(percentile95_location[order(-percentile95_location$top_cust_count),],10) 
#The top 5% of customers in terms of money paid more than $379.90 for their products (not including shipping). 
  #They were mostly from the top four cities Sao Paulo, Rio de Janeiro, Belo Horizonte, and Brasilia. 
#Over 95% of the customers only bought 1 item, which means customers typically do not buy multiple items at once
  #This is an interesting point that can be looked at down the line 

#box plot of the transaction price. Data is very right skewed 
boxplot(cust_data2$transaction_price)
head(cust_data2[order(-transaction_price)],10) %>%
  select(customer_id, Total_products_bought, transaction_price, customer_city,customer_state)
#The customers who paid a lot mostly bought one expensive item and were mostly in the states that had the most orders.

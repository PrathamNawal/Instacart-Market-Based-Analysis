#Load the datasets
library(readr)
aisles <- read_csv("~/Downloads/aisles.csv")
departments <- read_csv("~/Downloads/departments.csv")
order_products_prior <- read_csv("~/Downloads/order_products__prior.csv")
orders <- read_csv("~/Downloads/orders.csv")
products <- read_csv("~/Downloads/products.csv")
order_products_train <- read_csv("~/Downloads/order_products__train.csv")

#load the libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)


#Recoding some variables
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set = as.factor(orders$eval_set)
products$product_name = as.factor(products$product_name)


#order_products_train$user_id <- orders$user_id[match(order_products_train$order_id, orders$order_id)]

#order_products <- orders %>% inner_join(order_products_prior, by = "order_id")
gc()
#########################################################################
#lets take out test data from orders 
test = orders %>%
  filter(eval_set == "test")

#lets take prior users with prior and test by their user_id
total_products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) 

orders_train <- order_products_train %>%
  left_join(orders,by = "order_id") %>%
  left_join(total_products,by = "product_id") %>%
  select(-11,-14,-15)

orders_prior <- order_products_prior %>%
  left_join(orders,by = "order_id") %>%
  left_join(total_products,by = "product_id") %>%
  select(-11,-14,-15)

#Combining the datasets- prior and train
total_ordersset = rbind(orders_train,orders_prior)
head(total_ordersset,10)

test_history = total_ordersset[total_ordersset$user_id %in% test$user_id,]

#Customer-Frequency
user_freq <- orders %>%
  select(user_id, order_number, days_since_prior_order) %>%
  group_by(user_id) %>%
  summarise(total = max(order_number), 
            frequency = mean(days_since_prior_order, na.rm = TRUE))

#missing values
sapply(orders, function(x) sum(is.na(x)))
orders$days_since_prior_order[is.na(orders$days_since_prior_order)] <- 0

# Products ----------------------------------------------------------------
products <- products %>%
  inner_join(aisles,by = "aisle_id") %>%
  inner_join(departments,by = "department_id")

#orderUsers
ordersusers = data.frame(orders$user_id,orders$order_id)
colnames(ordersusers) = c("user_id","order_id")

#add user_id  to order_products_train
order_products_train <- order_products_train %>%
  left_join(ordersusers,by = "order_id")

order_products = inner_join(orders, order_products_prior,by= 'order_id')
order_products = order_products[order(order_products$user_id, order_products$order_number,order_products$product_id),]

#user prob
userpurchased = data.frame(table(order_products$user_id))
colnames(userpurchased) = c("user_id", "usertimesordered")

userreordered = order_products$reordered %>%
  aggregate(by = list(user_id=order_products$user_id),FUN = sum) 
colnames(userreordered) = c("user_id", "userreordered")

user_prob = cbind(userpurchased, userreordered)
user_prob[,3] = NULL

user_prob$reorderchance = user_prob$userreordered/user_prob$usertimesordered

user_prob$user_id = as.integer(user_prob$user_id)

#product prob
productspurchased = data.frame(table(order_products$product_id))
colnames(productspurchased) = c("product_id", "prodtimesordered")

productsreordered = order_products$reordered %>%
  aggregate(by = list(product_id= order_products$product_id), FUN = sum)
colnames(productsreordered) = c("product_id", "prodreordered")

product_prob = cbind(productspurchased, productsreordered)
product_prob[,3] = NULL

product_prob$prodreorderchance = product_prob$prodreordered/product_prob$prodtimesordered

product_prob$product_id = as.integer(product_prob$product_id)

############################################################################



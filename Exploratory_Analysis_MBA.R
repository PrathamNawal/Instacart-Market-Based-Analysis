#############################################################################
#Visualization of the data

#On which day people buy products
ggplot(orders,aes_string(x = orders$order_dow)) + geom_density() + geom_histogram(fill = "DarkSlateBlue",color = "Black",binwidth = 0.1) +
  theme_bw() + theme(text = element_text(size = 20)) + xlab("day of the week") + ylab("Count") +
  ggtitle("Day of the week V/s Order Count")

#mostly people order on sat and sunday

#At what time of the day do people buy products
ggplot(orders,aes_string(x = orders$order_hour_of_day)) + geom_density() + geom_histogram(fill = "CornSilk",color = "Black",binwidth = 0.5) +
  theme_bw() + theme(text = element_text(size = 20)) + xlab("Hour of the day") + ylab("Count") +
  ggtitle("Hour of the day V/s Order Count")

#people order between 9am to 8pm


#when do they order from days since prior order
ggplot(orders,aes_string(x = orders$days_since_prior_order)) + geom_density() + geom_histogram(fill = "DarkSlateBlue",color = "Black",binwidth = 0.1) +
  theme_bw() + theme(text = element_text(size = 20)) + xlab("No of days") + ylab("Count") +
  ggtitle("Day since prior order V/s Order Count")

#for every 9 and 30 days they order products

#How many days between two orders
ggplot(user_freq, aes(frequency)) +
  geom_histogram(col = "Blue")


#How many re-orders were there in proportion
tmp <- order_products %>% 
  group_by(reordered) %>% 
  summarize(count = n()) %>% 
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))
kable(tmp)

tmp %>% 
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  geom_bar(stat="identity")

#Nearly 59% orders involved re-orders

#Order-Distribution
par(mar = rep(2, 4))
library(MASS)
pie_orders <- table(orders$eval_set)
pie(pie_orders,col = c("red","yellow","green"))

#Products sold by each department
tmp3 <- order_products_prior %>%
  group_by(product_id) %>%
  summarise(count = n()) %>%
  left_join(products) %>%
  left_join(departments) %>%
  left_join(aisles) %>%
  group_by(department, aisle) %>%
  summarise(count2 = sum(count))

treemap(tmp3, index = c("department", "aisle"), vSize = "count2", title="",palette="Set2",border.col="#FFFFFF")

require(readr)
require(arules)
require(plyr)

order_products_prior_data<-read.csv("dataset/order_products__prior.csv", nrows=20000)
products_data<-read.csv("dataset/products.csv")
product_id_name_data<-order_products_prior_data[,1:2] #picking up first two rows
transection_data<-merge(product_id_name_data, products_data, by="product_id") #merging
transection_data<-arrange(transection_data, order_id) # ascending order

mydata<-transection_data[,c(2,3)] #dropping other columns
mydata[1:10,] # sneak peek

#drop columns if you want, as we will be dealing with only order id and product name/id. 
# will be choosing product name for sake of readability.

#Splitting the data :

dt <- split(mydata$product_name, mydata$order_id)

# Converting data to a class of transactions
dt2 = as(dt,"transactions")
#summary of this new dt2, uncomment if you want to,

#summary(dt2) # t
# sneak peek into  dt2 uncomment below line on your local machine, here this thing crashes which is a bummer.
#inspect(dt2)

#Let us look at frequencies:, uncomment if you want
#itemFrequency(dt2, type = "relative")

#Plotting 
itemFrequencyPlot(dt2,topN=20,type="absolute")

# in class
frequentItems <- eclat (dt2,
                        parameter = list(supp = 0.07, maxlen = 15))
inspect(frequentItems)
itemFrequencyPlot(dt2, topN=10,
                  type="absolute", main="Item Frequency")

# Rules can be generated as below:
rules <- apriori (dt2,
                  parameter = list(supp = 0.001, conf = 0.5))
rules_conf <- sort (rules, by="confidence",
                    decreasing=TRUE) # 'high-confidence' rules.
# show the support, lift and confidence for all rules
inspect(head(rules_conf))

# get rules that lead to buying 'Banana'
rules <- apriori (data=dt2, parameter=list (supp=0.001,conf = 0.08),
                  appearance = list (default="lhs",rhs="Banana"),
                  control = list (verbose=F))
# 'high-confidence' rules.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(head(rules_conf))

department_data<-read.csv("dataset/departments.csv")
product_id_name_department_id_data<-transection_data[,c(2,3,5)] #dropping other columns
transection_data_department<-merge(product_id_name_department_id_data, department_data, by="department_id") #merging
transection_data_department<-arrange(transection_data_department, order_id) # ascending order

mydata_department<-transection_data_department[,c(2,4)]
head(mydata_department)

dt_dep <- split(mydata_department$department, mydata_department$order_id)
dt_dep2 = as(dt_dep,"transactions")

itemFrequencyPlot(dt_dep2,topN=20,type="absolute")


# in class
frequentItems <- eclat (dt_dep2,
                        parameter = list(supp = 0.07, maxlen = 15))
inspect(frequentItems)
itemFrequencyPlot(dt_dep2, topN=10,
                  type="absolute", main="Item Frequency")

# Rules can be generated as below:
rules <- apriori (dt_dep2,
                  parameter = list(supp = 0.001, conf = 0.5))
rules_conf <- sort (rules, by="confidence",
                    decreasing=TRUE) # 'high-confidence' rules.
# show the support, lift and confidence for all rules
inspect(head(rules_conf))

# get rules that lead to buying 'Banana'
rules <- apriori (data=dt_dep2, parameter=list (supp=0.001,conf = 0.08),
                  appearance = list (default="lhs",rhs="Banana"),
                  control = list (verbose=F))
# 'high-confidence' rules.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(head(rules_conf))


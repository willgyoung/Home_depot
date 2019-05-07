library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


mysheets <- read_excel_allsheets('Analyst_Case_Study.xlsx')


Summary <- mysheets[[1]]
Product_info <- mysheets[[2]]
Carrier_Rates <- mysheets[[3]]
Historical_Orders <- mysheets[[4]]
Historical_Shipments <- mysheets[[5]]

Historical_Shipments$carrierXCost <- ifelse((Historical_Shipments$`Str Nbr` == 1) & (Historical_Shipments$Supplier == "A"), 2540, NA)
Historical_Shipments$carrierXCost <- ifelse((Historical_Shipments$`Str Nbr` == 2) & (Historical_Shipments$Supplier== "A"), 1640, Historical_Shipments$carrierXCost)
Historical_Shipments$carrierXCost <- ifelse((Historical_Shipments$`Str Nbr` == 1|2) & (Historical_Shipments$Supplier == "B"), 1200, Historical_Shipments$c)


library(data.table)

Historical_Shipments <- data.table(Historical_Shipments)

Historical_Shipments[`Str Nbr` == 1 & Supplier == "A", carrierYCost := 0.12 * Weight]
Historical_Shipments[`Str Nbr` == 2 & Supplier == "A", carrierYCost := 0.07 * Weight]
Historical_Shipments[`Str Nbr` == 1 & Supplier == "B", carrierYCost := 0.07 * Weight]
Historical_Shipments[`Str Nbr` == 2 & Supplier == "B", carrierYCost := 0.06 * Weight]

View(Historical_Shipments)

Historical_Shipments$Choice <- ifelse(Historical_Shipments$carrierXCost > Historical_Shipments$carrierYCost, "Carrier Y", "Carrier X")



Historical_Orders$estimated_orders <- ((Historical_Orders$`Order Qty` * 0.10) + Historical_Orders$`Order Qty`)
Historical_Orders$estimated_order_weight <- Historical_Orders$estimated_orders * Historical_Orders$`Product Weight`

Historical_Orders$hammer_orders <- ifelse(Historical_Orders$`Product Id` == 1|2, Historical_Orders$estimated_orders, NA)
Historical_Orders$hammer_weight <- (Historical_Orders$hammer_orders * 2.0)
Historical_Orders$estimated_total_weight <- Historical_Orders$estimated_order_weight + Historical_Orders$hammer_weight



estimated_hammers <- ((410665 * 0.10) + 410665)

productionCostA <- 0.8 * estimated_hammers
productionCostB <- 0.82 * estimated_hammers



library(tidyverse)
testDF <- Historical_Orders %>%
  group_by(`Order Dt`, `Str Nbr`, Supplier) %>%
  summarise(estimated_weight = sum(estimated_order_weight))

testDF <- ungroup(testDF)


testDF$hammer_weight <- Historical_Orders %>%
  group_by(`Order Dt`, Supplier) %>%
  summarise(hammer_weight = sum(hammer_weight))


hammer_list <- na.omit(Historical_Orders$hammer_weight)
hammer_list <- rep(hammer_list, each = 2)

testDF$hammer_weight <- hammer_list

testDF$total_weight <- testDF$estimated_weight + testDF$hammer_weight


estimated_orders <- Historical_Orders[, 8:ncol(Historical_Orders)]
testDF <- data.table(testDF)

testDF[Supplier == "A" & `Str Nbr` == 1, A_fixed_cost := 2540]
testDF[Supplier == "A" & `Str Nbr` == 2, A_fixed_cost := 1640]
testDF[Supplier == "A" & `Str Nbr` == 1, A_variable_cost := 0.12 * total_weight]
testDF[Supplier == "A" & `Str Nbr` == 2, A_variable_cost := 0.07 * total_weight]
testDF[Supplier == "B" & `Str Nbr` == 1, B_fixed_cost := 1200]
testDF[Supplier == "B" & `Str Nbr` == 2, B_fixed_cost := 1200]
testDF[Supplier == "B" & `Str Nbr` == 1, B_variable_cost := 0.07 * total_weight]
testDF[Supplier == "B" & `Str Nbr` == 2, B_variable_cost := 0.06 * total_weight]

testDF$A_cheapest <- ifelse(testDF$A_fixed_cost < testDF$A_variable_cost, testDF$A_fixed_cost, testDF$A_variable_cost)
testDF$B_cheapest <- ifelse(testDF$B_fixed_cost < testDF$B_variable_cost, testDF$B_fixed_cost, testDF$B_variable_cost)

testDF <- testDF[, -c(7:10)]

shipping_price <- data.frame(na.omit(testDF$A_cheapest), na.omit(testDF$B_cheapest))
names(shipping_price) <- c("A_cheapest", "B_cheapest")

total_ship_price <- lapply(shipping_price, sum)

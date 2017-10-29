library(lubridate)
library(dplyr)
products$DOB<-ymd(products$DOB)
products$DOB_year<-year(products$DOB)
products$DOB_year<-as.factor(products$DOB_year)
products$trans_year <- year(products$transactionDate)
products$trans_year <- as.factor(products$trans_year)
products<-products %>% group_by(Gender) %>% mutate(gen_sum = sum(sale_price_after_promo))
products<-products %>% group_by(State) %>% mutate(State_sum = sum(sale_price_after_promo))
products<-products %>% group_by(discountUsed) %>% mutate(discount_sum = sum(sale_price_after_promo))
products<-products %>% group_by(promo_code) %>% mutate(promo_code_sum = sum(sale_price_after_promo))
products<-products %>% group_by(trans_year) %>% mutate(trans_year_sum = sum(sale_price_after_promo))
products<-products %>% group_by(DOB_year) %>% mutate(DOB_year_sum = sum(sale_price_after_promo))
products$trans_month<- month(products$transactionDate)

##age
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
    calc.age = new_interval(dob, age.day) / duration(num = 1, units = units)
    if (floor) return(as.integer(floor(calc.age)))
    return(calc.age)
}




 

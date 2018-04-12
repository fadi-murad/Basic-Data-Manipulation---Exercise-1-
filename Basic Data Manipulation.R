
library(dplyr)
library(dummies)

#Creating a vector that contains an accurate company names:
name_col <- c(tolower(refine_original$company))
cor_name<- c()
for (i in 1:length(name_col)) {
  if(stringr::str_detect(name_col[i],"ps")==TRUE) {cor_name[i]="philips"} 
  else if (stringr::str_detect(name_col[i],"ak")==TRUE)  {cor_name[i]="akzo"}
  else if (stringr::str_detect(name_col[i],"van")==TRUE)  {cor_name[i]="van Houten"}
  else if (stringr::str_detect(name_col[i],"uni")==TRUE)  {cor_name[i]="unilever"}
  else {cor_name[i]}
}
#Creating data frame contains product code and product category in order to add product category field to the data set
product_ca <- data.frame(product_code=c("p","v","x","q"),product_category=c("Smartphone","TV","Laptop","Tablet"))


refine_clean <- 
  refine_original %>%  tbl_df() %>% 
  tidyr::separate('Product code / number',c("product_code","product_number"),by = "-") %>%
  dplyr::select(-1) %>% 
  dplyr::mutate(Company = cor_name) %>% 
  dplyr::left_join(product_ca,by = "product_code") %>% 
  tidyr::unite("full_address",4:6,sep=",")

#Adding dummy variables for company name field and product category field
company<-refine_clean
refine_clean<-cbind.data.frame(company,dummy(company$Company,sep = "_"))
product<-refine_clean
refine_clean<-cbind.data.frame(product,dummy(product$product_category,sep = "_"))

#export the output ot csv file
write.csv(refine_clean,"refine_clean.csv")


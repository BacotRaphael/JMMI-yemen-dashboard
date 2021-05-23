# helper function for the JMMI scripts - Data Merge and global in particular

harmonise.df <- function(df){
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub("_all", "", colnames(df))
  colnames(df) <- gsub("\\.|\\_\\.", "/", colnames(df))
  colnames(df) <- sub(".*?sell_", "sell_", colnames(df))
  colnames(df) <- sub(".*?supply_information/", "", colnames(df))
  colnames(df) <- sub("info/date_survey|date_survey", "date", colnames(df))
  df <- df %>% 
    map_if(is.factor, as.character) %>%
    as_tibble%>%
    mutate_at(vars(matches("calc|exchange|cost")), as.numeric) %>%
    mutate(jmmi_date = as.character(as.Date(as.yearmon(as.character(gsub("_", " ", jmmi))))),
           date = if ("date" %in% colnames(df)) ifelse(!is.na(date), date, jmmi_date) else {jmmi_date}) # Finish this mess to get date cleaned.
  return(df)
}

name_object<-function(df){
  name<-deparse(substitute((df)))
  return(name)
}

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}

# Archived pieces of code

############### OLD Code for consolidation of all datasets - Was too slow.
############################################################ major change
# col_name_initial<-c("fuel_gov_origin","wash_gov_origin","food_gov_origin", colnames(September_2020%>% dplyr::select(starts_with('calc_price_'),contains("cost_cubic_metere"), contains("exchange_rate_result"),starts_with("governorate_"),starts_with("district_"),-contains('market'))))
# data_all_JMMI<-as_tibble(data.frame(test="TEST"))
# data_all_JMMI[,col_name_initial] <- NA
# colnames_pulled_all<-as_tibble(data.frame(JMMI="TEST"))
# 
# #https://stackoverflow.com/questions/37360009/binding-values-from-function-to-global-environment-in-r
# col_pull<-function(df, list_of_df){
#   #name<- names(list_of_df[df])
#   call1 <-  sys.call(1)
#   call1[[1]] <- as.name("names")
#   call1 <- call1[1:2]
#   nm <- eval(call1)
#   name<-nm[!is.na(match(list_of_df,list(df)))]
#   
#   colnames(df)<-tolower(colnames(df))
#   colnames(df)<-gsub("_all","",colnames(df))
#   
#   df1<-df%>%
#     as_tibble()%>%
#     dplyr::select(starts_with('calc_price_'),contains("cost_cubic_meter"), contains("exchange_rate_result"),starts_with("governorate_"),starts_with("district_"),starts_with("fuel_gov_origin"),starts_with("wash_gov_origin"),starts_with("food_gov_origin"))%>%
#     #rename(replace=c(colnames(df)=( gsub("_normalized", "_normalised", colnames(df) ) )  ))%>%
#     mutate(as_tibble(),jmmi = name)%>%
#     map_if(is.factor,as.character)%>%
#     as_tibble()
#     
#     #colnames(df1)<-gsub(".*/","",colnames(df1))
#     colname_pull<-as_tibble(data.frame( holder = colnames(df1)))
#     names(colname_pull)<-name
#     colnames_pulled_all<<-as_tibble(rowr::cbind.fill(colnames_pulled_all, colname_pull, fill=NA ))
#   
#     data_all_JMMI <<- as_tibble(merge(df1,data_all_JMMI,all=T))
#   #return(data_all_JMMI)
# }
# 
# #debug(col_pull)
# #col_pull(November_2019)
# lapply((list_df), col_pull, list_of_df = list_df)
# #delete the test column
# data_all_JMMI<-dplyr::select(data_all_JMMI,-c(test, district_name, governorate_name))
# 
# total<-0
# count_nrows<-function(df){
#   nrow_count<-as.numeric(nrow(df))
#   total<<-as.numeric(nrow_count+total)
#   return(total)
# }
# 
# lapply(list_df,count_nrows)

#once you have the entire dataset created
#https://stackoverflow.com/questions/35366803/how-to-use-gsub-for-the-list-of-multiple-patterns-in-r
#make all the column numeric that are 
#https://stackoverflow.com/questions/25391975/grepl-in-r-to-find-matches-to-any-of-a-list-of-character-strings
#https://stackoverflow.com/questions/38695967/how-to-convert-certain-columns-only-to-numeric
#https://stackoverflow.com/questions/33061116/how-to-convert-a-column-of-date-string-to-dates-in-r

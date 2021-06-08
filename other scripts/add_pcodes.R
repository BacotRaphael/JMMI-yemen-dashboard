add.pcodes<-function(data){
  #library("dplyr")
  pcode.data <- read.csv("other scripts/pcodes/yem_admin_20171007.csv",header=T,sep=",", encoding = "UTF-8", check.names=F, stringsAsFactors=FALSE)
  pcode <- as.data.frame(pcode.data) %>%
    dplyr::select(-admin2Name_en, -admin2OldPcode) %>%
    dplyr::rename(governorate_name=admin1Name_en,
                  governorate_id=admin1Pcode,
                  district_name=admin2RefName_en,
                  district_id=admin2Pcode,
                  country_name=admin0Name_en,
                  country_id=admin0Pcode)
  data <- data %>% 
    dplyr::select(-any_of(c("country_id", "country_name", "district_name", "governorate_id", "governorate_name"))) %>%
    left_join(pcode, by="district_id")
  
  #admin1Name
  # pcode_merge1<-pcode[,c(colnames(pcode[c(4,3)]))]
  # pcode_merge1 <- unique(pcode_merge1)
  # dplyr::rename(pcode_merge1, governorate_name = admin1Name_en) -> admin1_merge
  # plyr::rename(admin1_merge, c("admin1Pcode" = "governorate_id")) -> admin1_merge

  
  #admin2Name
  # pcode_merge2<-pcode[,c(colnames(pcode[c(7,5)]))]
  # pcode_merge2 <- unique(pcode_merge2)
  # dplyr::rename(pcode_merge2, district_name = admin2Name_en) -> admin2_merge
  # plyr::rename(admin2_merge, c("admin2Pcode" = "district_id")) -> admin2_merge
  
  # admin2Name and admin1Name at once [keeping geographical coherence and assuming admin2Name will be right]
  
  
  #Merge
  # data <- dplyr::left_join(data, admin2_merge, by = "district_id") 
  # data <- dplyr::left_join(data, admin1_merge, by = "governorate_id")
  
  #Add Country name and code
  # data$country_name <- "Yemen"
  # data$country_ID <- "YE"
  
  return(data)
}



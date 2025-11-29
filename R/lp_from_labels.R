

# for internal use by drc_plan_HEKA()
lp_from_labels<-function(labels, pattern="[0-9]?[0-9,\\.]+", .navalue=0){
  labels %>% stringr::str_extract(pattern) %>% fill_last_non_empty() %>% as.numeric() %>% tidyr::replace_na(.navalue)
}

# from ephys2:
# mean_of_last<-function(df, cursor, n) data.frame( 
#   res=mean(getlast(df[,cursor],n)), 
#   relTime.start=getlast(df$relTime,n)[1],
#   relTime.end=getlast(getlast(df$relTime,n),1)
# )

# function to get last n elements of a vector
getlast<-function(x,n){rev(x)[1:n]}

# function to get the most recent non-empty vector element for a given index 
get_last_non_empty<-function(vector, n){
  non_empty=which(vector!="" )
  preceeding = non_empty <= n
  vector[rev(non_empty[ preceeding ])[1] ]
} 

# function to fill empty vector elements with the most recent preceding non-empty value
fill_last_non_empty<-function(vector){
  sapply(1:length(vector), function(n)  get_last_non_empty(vector, n) ) 
} 
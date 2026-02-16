extract_county <- function(vector) {
  vector<-vector
  # Define a regex pattern to match text between the third and fourth commas
  pattern <- "^([^,]*,){3}([^,]*)(,|$)"
  
  # Apply the regex pattern to extract the desired text
  matches <- regmatches(vector, regexec(pattern, vector))
  
  # Extract the matched group that contains the text between the third and fourth commas
  extracted_text <- sapply(matches, function(x) if (length(x) > 2) x[3] else NA)
  
  extracted_text<-substr(extracted_text, 2, nchar(extracted_text))
  
  
  
  # if(stringr::str_detect(pattern="County", string=extracted_text)){
  #   pattern<-"(.*?)(?=\\sCounty)"
  #   
  # matches <- regexpr(pattern, extracted_text, ignore.case = TRUE, perl = TRUE)
  # extracted_text <- regmatches(extracted_text, matches)
  # }
  
  
  return(extracted_text)
}
#Remove word County
counties<-lapply(X = counties, FUN = function(t) gsub(pattern = " County", replacement = "", x = t, fixed = TRUE))



counties<-extract_county(full$name)
cities<-full$city
df<-data.frame(counties,cities)

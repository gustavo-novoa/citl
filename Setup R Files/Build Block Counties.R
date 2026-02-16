library(tidycensus)

save_blocks<-function(state, county, year){
  
  state=state
  county=county
  year=year
  
  if(year==2000){
    
   b<-get_decennial(state = state,
    county = county,
    geography = 'block',
    year = year,
    sumfile='pl',
    variables = c(
                  'pop' = 'PL001001',  #total pop
                  'pop_white'='PL002005', #one race non hispanic
                  'pop_black'='PL002006',#one race non hispanic
                  'pop_asian'='PL002008',#one race non hispanic
                  'pop_nativeam'='PL002007',#one race non hispanic
                  'pop_hispanic'= 'PL002002', #total hispanic pop
                  'vap_white'='PL004005',
                  'vap_black'='PL004006',
                  'vap_nativeam'= 'PL004007',
                  'vap_asian' = 'PL004008',
                  'vap_hisp' ='PL004002',
                  'vap' = 'PL004001',
                  'vap_one_race' = 'PL004004'
                  
                  ),
    geometry = TRUE,
    output = "wide")
  }
  
  if(year==2010){
  b<-get_decennial(state = state,
                county = county,
                geography = 'block',
                year = year,
                variables = c(
                  
                  "pop" =       "P001001", # Total Pop
                  "pop_white"=  "P005003", #White only non hispanic pop
                  "pop_black"=  "P005004", #Black only non hispanic pop
                  "pop_nativeam"=  "P005005", #NA only non hispanic pop
                  "pop_asian"=  "P005006", #asian only non hispanic pop
                  "pop_hispanic"="P005010", #Total Hispanic pop
                  'vap_white' = 'P011005', #non hispanic
                  'vap_black' = 'P011006',#non hispanic
                  'vap_asian'=  'P011008',#non hispanic
                  'vap_hisp' =  'P011002', #hispanic
                  'vap' =       'P010001',  #exact same as P011001
                  'vap_one_race'='P010002'), 
                geometry = TRUE,
                output = "wide")
  }
  
  if(year==2020){
    b<-get_decennial(state = state,
                  county = county,
                  geography = 'block',
                  year = year,
                  variables = c(
                    
                    "pop" =       "P1_001N", # Total Pop
                    "pop_one_race"="P1_002N", #Total pop one race
                    "pop_white"=  "P2_005N", #White only non hispanic pop
                    "pop_black"=  "P2_006N", #Black only non hispanic pop
                    "pop_nativeam"=  "P2_007N", #NA only non hispanic pop
                    "pop_asian"=  "P2_008N", #Asian only non hispanic pop
                    "pop_hispanic"="P2_002N", #Total Hispanic pop
                    'vap_white' = 'P4_005N', #non hispanic
                    'vap_black' = 'P4_006N',#non hispanic
                    'vap_asian'=  'P4_008N',#non hispanic
                    'vap_nativeam'='P4_007N', #non hispanic 
                    'vap_hisp' =  'P4_002N', #hispanic
                    'vap' =       'P3_001N', 
                    'vap_one_race'='P3_002N'), 
                  geometry = TRUE,
                  output = "wide")
    
  }
  
  if(year!=2020 & year!=2010 & year!=2000)
    return("Invalid Year")
  else
    return(b)
  
}







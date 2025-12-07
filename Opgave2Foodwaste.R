##### FOODWAST opgaven 

install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)

################################################################################

api_key <- "SG_APIM_0JQAMC41TBEVB5D2Y4AVVM100K1HCG5Z4X26RPGA4XZ0B3F100N0"
oest_url <- "https://api.sallinggroup.com/v1/food-waste/?zip=2100"


###################### Hent butikker i et postnummer ###########################

resp_oest <- GET(oest_url, add_headers(Authorization = paste("Bearer", api_key)))


print(status_code(resp_oest))


data_oest <-  fromJSON(flatten = TRUE,content(resp_oest, "text", encoding = "UTF-8"))

str(data_oest)

df_oesterbro <- as.data.frame(data_oest)

str(df_oesterbro)

########## Netto ved blegdamsvej 

net_bleg <- subset(df_oesterbro, store.address.street == "Blegdamsvej 124")

net_bleg_rens <- net_bleg$clearances[[1]]


################################################################################

api_key <- "SG_APIM_0JQAMC41TBEVB5D2Y4AVVM100K1HCG5Z4X26RPGA4XZ0B3F100N0"
fred_url <- "https://api.sallinggroup.com/v1/food-waste/?zip=2000"


###################### Hent butikker i et postnummer ###########################

resp_fred <- GET(fred_url, add_headers(Authorization = paste("Bearer", api_key)))


print(status_code(resp_fred))


data_fred <-  fromJSON(flatten = TRUE,content(resp_fred, "text", encoding = "UTF-8"))

str(data_fred)

df_frederiksberg <- as.data.frame(data_fred)

str(df_frederiksberg)

########## Føtex ved ndr fasanvej

ftx_ndrf <- subset(df_frederiksberg, store.address.street == "Ndr. Fasanvej 25")

ftx_ndr_rens <- ftx_ndrf$clearances[[1]]


################################################################################

api_key <- "SG_APIM_0JQAMC41TBEVB5D2Y4AVVM100K1HCG5Z4X26RPGA4XZ0B3F100N0"
ball_url <- "https://api.sallinggroup.com/v1/food-waste/?zip=2750"


###################### Hent butikker i et postnummer ###########################

resp_ball <- GET(ball_url, add_headers(Authorization = paste("Bearer", api_key)))


print(status_code(resp_ball))


data_ball <-  fromJSON(flatten = TRUE,content(resp_ball, "text", encoding = "UTF-8"))

str(data_ball)

df_ballerup <- as.data.frame(data_ball)

str(df_ballerup)

########## Føtex ved Baltropvej
ftx_ball <- subset(df_ballerup, store.address.street == "Baltorpvej 2")

ftx_ball_rens <- ftx_ball$clearances[[1]]

##################################################################################


api_key <- "SG_APIM_0JQAMC41TBEVB5D2Y4AVVM100K1HCG5Z4X26RPGA4XZ0B3F100N0"
syd_url <- "https://api.sallinggroup.com/v1/food-waste/?zip=2450"


###################### Hent butikker i et postnummer ###########################

resp_syd <- GET(syd_url, add_headers(Authorization = paste("Bearer", api_key)))


print(status_code(resp_syd))


data_syd <- fromJSON(flatten = TRUE,content(resp_syd, "text", encoding = "UTF-8"))

str(data_syd)

df_sydhavn <- as.data.frame(data_syd)

str(df_sydhavn)

########## Netto ved Vestre Teglgade 

net_vest <- subset(df_sydhavn, store.address.street == "Vestre Teglgade 31-33")

net_vest_rens <- net_vest$clearances[[1]]

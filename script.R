library(RSelenium)
library(RCurl)
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)

docker_machine_ip <- "docker machine ip here"

click_Selector <- "#form1 > div.main_wrapper > div > div > div > div > div > div > div.ps-tabs.donotprint > ul > li:nth-child(5)"
owner_Selector <- '#details > div.property-data > div.propcard-section > span:nth-child(1)'
address_Selector <- "#details > div.property-data > div.propcard-section > span:nth-child(9)"
type_Selector <- "table.report-table:nth-child(11) > tbody:nth-child(1) > tr:nth-child(6) > td:nth-child(2)"

Failed <- "Failed.txt"

mybrowser <- remoteDriver(remoteServerAddr = docker_machine_ip, port = 4445L)
mybrowser$open()

df4 <- data.frame()

for(i in 1:4099) {
  print(i)
  tryCatch({
    
    urlEach <- paste0("http://gis.hcpafl.org/propertysearch/#/search/basic/address=", notscraped[i])
    mybrowser$navigate(urlEach)
    #navigate to the right page and save URL
    addresspage <- mybrowser$findElement(using = 'css', click_Selector)
    addresspage$clickElement()
    Sys.sleep(3) 
    urlresult <- as.character(addresspage$getCurrentUrl())
    
    #get owner information
    owner <- mybrowser$findElement(using= 'css', owner_Selector)
    ownerresult <- as.character(owner$getElementAttribute("innerText"))
    
    #get address information- as a way to clarify
    address <- mybrowser$findElement(using= 'css', address_Selector)
    addressresult <- as.character(address$getElementAttribute("innerText"))
    
    #get property type
    type <- mybrowser$findElement(using= 'css', type_Selector)
    typeresult <- as.character(type$getElementAttribute("innerText"))
    df4[i, 1] <- addressresult
    df4[i, 2] <- typeresult
    df4[i, 3] <- ownerresult
  }, error=function(e) {
    print("Failed")
    cat(notscraped[i], file=Failed, append=TRUE, sep = "\n")})
}  
save.image(file="100lovers.RDATA")


#getting df4 ready to merge

df4$V1<- sub("33\\d{3}", "", df4$V1)
df4$V1<- sub("TAMPA", "", df4$V1, ignore.case=TRUE)
df4$V1<- trimws(df4$V1)
df4$V1 <- toupper(df4$V1)
df4$V1<- sub("FL$", "", df4$V1)
df4$V1<- sub("[[:punct:]]", "", df4$V1)
df4$V1<- trimws(df4$V1)
df4$V1<- sub("AV$", "AVE",df4$V1)
df4$V1<- sub("  ", " ", df4$V1)

df4 <- (unique(df4))

addresslistscraped2 <- df4$V1
addresslistscraped2 <- unique(addresslistscraped2)
addresslistscraped1and2 <- c(addresslistscraped, addresslistscraped2)
addresslistscraped1and2 <- unique(addresslistscraped1and2)

notscraped2 <- setdiff(addresslist, addresslistscraped1and2)
notscraped2 <- notscraped2[-c(1:96)]
notscraped2 <- notscraped2[-c(3667:3905)]

df5 <- rbind(df3, df4)
df5 <- unique(df5)

df6 <- data.frame()
Failed2 <- "Failed2.txt"

for(i in 1:length(notscraped2)) {
  print(i)
  tryCatch({
    
    urlEach <- paste0("http://gis.hcpafl.org/propertysearch/#/search/basic/address=", notscraped2[i])
    mybrowser$navigate(urlEach)
    Sys.sleep(5) 
    #navigate to the right page and save URL
    addresspage <- mybrowser$findElement(using = 'css', click_Selector)
    addresspage$clickElement()
    Sys.sleep(5) 
    urlresult <- as.character(addresspage$getCurrentUrl())
    
    #get owner information
    owner <- mybrowser$findElement(using= 'css', owner_Selector)
    ownerresult <- as.character(owner$getElementAttribute("innerText"))
    
    #get address information- as a way to clarify
    address <- mybrowser$findElement(using= 'css', address_Selector)
    addressresult <- as.character(address$getElementAttribute("innerText"))
    
    #get property type
    type <- mybrowser$findElement(using= 'css', type_Selector)
    typeresult <- as.character(type$getElementAttribute("innerText"))
    df6[i, 1] <- addressresult
    df6[i, 2] <- typeresult
    df6[i, 3] <- ownerresult
    df6[i, 4] <- notscraped2[i]
    print(df6[i,])
  }, error=function(e) {
    print("Failed")
    cat(notscraped2[i], file=Failed2, append=TRUE, sep = "\n")})
} 

df5[,4] <- df5[,1]

for (i in 1:length(notscraped2)){
  print(i)
  if(is.na(notscraped2[i])){
  } else if(length(agrep(notscraped2[i], df5[,4], max.distance=0.01)!=0)) {
    match<-agrep(notscraped2[i], df5[,4], max.distance=0.001)[1]
    df5[match,4] <- notscraped[i]
    notscraped2[i]<-NA
    } else{}
}

df5$V3 <- gsub("\n", " ", df5$V3)

#Refining prop. type


df5[,2] <- as.factor(df5[,2])
table(df5[,2])

originallevels <- levels(df5$V2)

df11[,2] <- as.factor(df11[,2])
newlevels <- levels(df11$V2)

setdiff(newlevels, originallevels)

singlefamily <- data.frame()
commercialbuilding <- data.frame()
commercialland <- data.frame()
vacantland <- data.frame()
agricultureland <- data.frame()
other <- data.frame()

singlefamilyvec <- c("0100 SINGLE FAMILY R","0200 MH","0000 VACANT RESIDENTIAL","0400 CONDOMINIUM"
,"0800 MFR <10 UNITS","0106 TOWNHOUSE/VILLA","0006 VACANT TOWNHOME","0901 RESIDENTIAL HOA","2814 MHP D",
"HH HEADER", "0802 MULTI RES RENTAL UNITS", "0801 MULTI RES DWELLINGS", "1201 MIXED USE RES", "1203 MIXED USE MULTI FAM", 
"0902 CONDO HOA", "0508 MH CO-OP", "0340 MFR CLASS D", "0700 MISC RESIDENTIA", "1228 MIXED USE MH PARK", 
"0040 VACANT CONDO", "2820 RV PARK", "2815 MIGRANT HOUSING > 9 UNITS", "0350 MFR CLASS E", "0630 ALF C", "0610 ALF A", 
"0102 SFR BLD ARND MH")

vacantlandvec <- c("9900 VACANT ACREAGE", "9600 WETLANDS/LOWLANDS", "8200 PARKS AND RECREATION", 
                   "9400 RIGHT-OF-WAY", "9300 SUBSURF RIGHTS")

commerciallandvec <- c("1000 VACANT COMM", "4000 VACANT INDUS", "2800 PKG LOT (COMM)")

othervec <- c("7100 CHURCHES","7200 PRIVATE SCHOOL","8700 STATE", "8800 FEDERAL", "NN NOTE")

agriculturallandvec <- c("6000 PASTURE", "5100 CROPS", "6600 ORCHARD/CITRUS", "6700 POUL/BEES/FISH", "6800 DAIRIES/FEEDLTS")

commericialbuilvec <- c("2754 AUTO REPAIR D", "1211 MIXED USE RETAIL", "2753 AUTO REPAIR C", 
"1730 OFFICE 1 STY C", "1130 1 STY STORE C", "1740 OFFICE 1 STY D", "1227 MIXED USE AUTO"
, "4830 WAREHOUSE C", "1630 STRIP CENTER", "3300 NIGHT CLUBS", "6900 PLANT NURSERY", "1710 OFFICE 1 STY A"
, "1720 OFFICE 1 STY B", "2755 VEHICLE SALVAGE/STORAGE", "2103 RESTAURANT C", "1420 CONV STORE/GAS", 
"1217 MIXED USE OFFICE", "1410 CONV STORE", "2703 AUTO SALES C", "4810 WAREHOUSE A", "4700 MIN PROCESSING"
, "2503 SERV SHOP C", "2104 RESTAURANT D", "1999 MEDICAL OFF CONDO", "4905 EQUIPMENT STORAGE", "2752 AUTO REPAIR B"
, "1248 MIXED USE WAREHSE", "1899 OFF MULTI-STY CONDO", "1830 OFF MULT-STY C", "1910 MEDICAL OFF A", "1140 RETAIL SERVICES", 
"4104 LIGHT MFG D" ,"1799 OFFICE 1 STY CONDO", "2702 AUTO SALES B", "2504 SERV SHOP D", "7600 FUNERAL HOME"
, "2102 RESTAURANT B", "2720 SELF SERVICE CAR WASH", "1940 MEDICAL OFF D", "1820 OFF MULT-STY B", "1810 OFF MULT-STY A", 
"1199 1 STY RETAIL CONDO", "1120 1STY STORE B", "1040 VACANT COMM HOA", "4903 SCRAP METAL/MATERIALS RECYCLING",
"4901 BUILDING MATERIALS STORAGE - NEW AND USED", "4600 FOOD PROCESSING", "4400 PACKING PLANTS", "4103 LIGHT MFG C",
"3931 EXTEND STAY A", "3820 SEMI-PRIVATE GOLF COURSE", "3700 RACETRACK", "2502 SERV SHOP B", "1005 VACANT PRO PARK"
, "2300 FINANCIAL", "2203 FAST FOOD C", "2201 FAST FOOD A", "2010 MARINAS", "2000 TRANSIT TERMINALS", "7610 CEMETERY",
"1850 BROADCASTING FACILITY", "1840 OFF MULT-STY D", "1239 MIXED USE MOTEL", "0044 CONDO GARAGE", "7700 CLB/LDG/UN HALL", 
"9100 UTILITY", "4860 FLEX SERV B", "9070 LEASED/PORT", "4870 FLEX SERV C", "3923 LMTD SERV C", "3924 LMTD SERV D",
"8670 PORT AUTHORITY")

for (i in 1:nrow(df11)) {
  print(i)
  if(df11[i,2] %in% singlefamilyvec){
    singlefamily <- rbind(singlefamily, df11[i,])
  } else if (df11[i,2] %in% vacantlandvec) {
    vacantland <- rbind(vacantland, df11[i,])
  } else if (df11[i,2] %in% commerciallandvec) {
    commercialland <- rbind(commercialland, df11[i,])
  } else if (df11[i,2] %in% othervec) {
    other <- rbind(other, df11[i,])
  } else if (df11[i,2] %in% agriculturallandvec) {
    agricultureland <- rbind(agricultureland, df11[i,])
  } else if (df11[i,2] %in% commericialbuilvec) {
    commercialbuilding <- rbind(commercialbuilding, df11[i,])
  }
}

write.csv(singlefamily, "singlefamilyfinal.csv")
write.csv(commercialbuilding, "commercialbuildingfinal.csv")
write.csv(commercialland, "commercialland2.csv")
write.csv(vacantland, "vacantlandfinal.csv")
write.csv(agricultureland, "agriculturelandfinal.csv")
write.csv(other, "otherfinal.csv")
write.csv(rerun3, "failedaddresses.csv")
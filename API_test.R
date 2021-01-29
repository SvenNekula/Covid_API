#Loading packages
library(httr)
library(jsonlite)

#API request
dt <- GET("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/Coronaf%C3%A4lle_in_den_Bundesl%C3%A4ndern/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
dt
#Status: 200 --> GOOD!

rawToChar(dt$content)

#creating DF
datCov <- fromJSON(rawToChar(dt$content))
names(datCov)

head(datCov$fields)
head(datCov$features)

listCov19 <- datCov$features

dfCov19 <- as.data.frame(listCov19[[1]])
dim(dfCov19)
head(dfCov19)

#Using the data
sapply(dfCov19, class)


dfc <- dfCov19
dfc$LAN_ew_GEN

dfc$LAN_ew_GEN <- c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW",
                    "BY", "SL","BE", "BB", "MV", "SN", "ST", "TH")
dfc$LAN_ew_GEN



library(ggplot2)
library(cowplot)

cp100k <- ggplot(dfc, aes(reorder(LAN_ew_GEN, -cases7_bl_per_100k), 
                     cases7_bl_per_100k)) + geom_bar(stat="identity") + 
  ggtitle("Covid-19 in Germany") + xlab("State (short)") +
  ylab("new cases per 100.000 inhabitants (7 days)")

human_numbers <- function(x = NULL, smbl ="", signif = 1){
  humanity <- function(y){
    
    if (!is.na(y)){
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "k")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"m")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"bn")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x,humanity)
}

ct <- ggplot(dfc, aes(reorder(LAN_ew_GEN, -Fallzahl), 
                     Fallzahl)) + geom_bar(stat="identity") + 
  ggtitle("") + xlab("State (short)") +
  ylab("Total cases") +
  scale_y_continuous(labels = human_numbers)

plot_grid(cp100k, ct, labels="")









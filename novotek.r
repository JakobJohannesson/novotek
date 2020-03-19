library(borsdata)
library(stringr)
library(dplyr)

key<-"<api>"

aktier<-fetch_instruments(key=key)

aktier %>% filter(name=="Novotek")

year<-fetch_year(164,key = key)
r12<-fetch_r12(164,key = key)
kvartal<-fetch_quarter(164,key = key)



# städar upp datan
for(i in 1:33){
  year[,i]<-rev(year[,i])
  r12[,i]<-rev(r12[,i])
  kvartal[,i]<-rev(kvartal[,i])
  year[11,]<-r12[10,]
}


cagr<-(year$revenues[11]/year$revenues[1])^1/length(year$year)

# Visualiserar
library(ggplot2)

skr<-data.frame(year=year$year,revenue=year$revenues,
                ebit=year$profit_Before_Tax)


plot_labels <- data.frame(label = round(cagr,digits = 3)*100,
                          x = seq(2008.5,2018.5,1),
                          y = year$revenues[1:11]*1.3)


ggplot(data =  skr, aes(x=year, y=revenue))+ 
  geom_bar(stat = "identity", 
           fill = "dark green",   
           colour = "black")+ theme_bw()+ labs(
             x = "År",
             y = "Omsättning",
             title = "Novotek omsättning MKR mellan 2009 och 2019",
             caption = "Källa: Börsdata"
           )+ 
  theme(
    axis.title.y =
      element_text(
        angle = 0,
        hjust = 1,
        vjust = 0.5
      ),
    plot.title = element_text(hjust = 0.5)
  )+ theme(
    panel.grid.major.x =
      element_blank(),
    panel.grid.minor.x =
      element_blank(),
    panel.grid.major.y =
      element_line(color = "grey")
  )+
  scale_x_continuous(breaks=seq(2009,2019,1))+
  scale_y_continuous(breaks=seq(0,350,50), limits = c(0,350))+ 
  geom_text(data = plot_labels,
            aes(x = x[6], y = y[6], label = label), size=5, parse = TRUE)+
  annotate("segment", x = plot_labels$x[2], xend = plot_labels$x[10],
           y = year$revenues[2]*1.1,
           yend = year$revenues[10]*1.25,
           arrow = arrow(length = unit(1, "lines")), colour = "grey50")


#### DCF #### 

library(stringr)
library(readxl)
library(dplyr)
library(lubridate)


namn<-"data/novotek.xlsx" # namn på excel filen





#### Kurs utveckling - veckodata ####

figur<-function(namn){
  library(xts)
  kurs<- read_excel(namn, 
                    sheet = "PriceWeek")
  kurs<-kurs[-1,]
  kurs<-data.frame(kurs$Date,kurs$Openprice)
  x<-kurs
  rownames(kurs)<-kurs$kurs.Date
  kurs<-as.matrix(kurs)
  kurs<-kurs[,-1]
  kurs<-as.matrix(kurs)
  kurs<-as.data.frame(kurs)
  rownames(kurs)<-x$kurs.Date
  colnames(kurs)<-c("Price")
  kurs$Price<-as.numeric(as.character(kurs$Price))
  MTG_Kurs <- as.xts(kurs, descr='my new xts object')
  figur<-plot.xts(MTG_Kurs, main = namn)
  return(figur)
}
figur(namn=namn, skr = skr)




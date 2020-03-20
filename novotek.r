library(borsdata)
library(stringr)
library(dplyr)

# Api key

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


cagr<-(year$revenues[11]/year$revenues[1])^(1/length(year$year))-1

library(ggplot2)

skr<-data.frame(year=year$year,revenue=year$revenues,
                ebit=year$profit_Before_Tax,ebit_marg=year$profit_Before_Tax/year$revenues)


plot_labels <- data.frame(label = round(cagr,digits = 3)*100,
                          x = seq(2008.5,2018.5,1),
                          y = year$revenues[1:11]*1.3)


ggplot(data =  skr, aes(x=year, y=revenue))+ 
  geom_bar(stat = "identity", 
           fill = "dark green",   
           colour = "black")+ theme_bw()+ labs(
             x = "År",
             y = "Omsättning\n MKR",
             title = "Novotek omsättning och ebitmarginal mellan 2009 och 2019",
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
            aes(x = x[6], y = 300, label = label), size=5, parse = TRUE)+
  annotate("segment", x = plot_labels$x[2], xend = plot_labels$x[10],
           y = year$revenues[2]*1.1,
           yend = year$revenues[10]*1.25,
           arrow = arrow(length = unit(1, "lines")), colour = "grey50")+
  geom_text(aes(label=round(skr$ebit/skr$revenue, digits=3)*100),
            vjust=1.5, color="white", size=3.5)



cagr<-(year$revenues[11]/year$revenues[8])^(1/3)-1
plot_labels <- data.frame(label = round(cagr,digits = 3)*100,
                          x = seq(2008.5,2018.5,1),
                          y = year$revenues[1:11]*1.3)
ggplot(data =  skr, aes(x=year))+ 
  geom_bar(stat = "identity", 
           fill = "dark green",   
           colour = "black", aes(y=skr$revenue)) + theme_bw()+ labs(
             x = "År",
             y = "Omsättning\n MKR",
             title = "Novotek omsättning och ebitmarginal mellan 2009 och 2019",
             caption = "Källa: Börsdata"
           )+ 
  theme(
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
  geom_text(data = plot_labels,
            aes(x = 2017, y = 320,
                label = paste("cagr:",label)), size=5, parse = TRUE)+
  annotate("segment", x = 2016, xend = plot_labels$x[11],
           y = year$revenues[2]*1.02,
           yend = year$revenues[10]*1.35,
           arrow = arrow(length = unit(1, "lines")), colour = "grey50")+
  geom_line(aes(y=round(ebit_marg, digits=3)*2000)) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.0005,
                                         name = "Rörelsemarginal [%]",
                                         labels = c(0,5,10,15,20),
                    breaks = seq(0,0.2,0.05)))+
  geom_text(aes(x = year, y = revenue,
                label=round(skr$ebit/skr$revenue, digits=3)*100),
            vjust=1.5, color="white", size=3.5)


library(shiny)
library(ggplot2)
library(stringr)
library(readxl)
library(dplyr)
library(lubridate)



namn<-"data/novotek.xlsx" # namn på excel filen

# Läser in data

years <- read_excel(namn, 
                    sheet = "Year")
r12 <- read_excel(namn, 
                  sheet = "R12")
quarter<- read_excel(namn, 
                     sheet = "Quarter")




years_func<-function(years){
  years<-t(years)
  colnames(years)<-years[1,]
  years<-years[-1:-2,]
  years<-years[,colSums(!is.na(years)) > 0]
  years<-as.data.frame(years)
  for(i in c(1:ncol(years))) {
    years[,i] <- as.numeric(as.character(years[,i]))
  }
  years<-years[-10,]
  return(years)
}
r12_func<-function(r12){
  r12<-t(r12)
  colnames(r12)<-r12[1,]
  r12<-r12[-1:-2,]
  r12<-r12[,colSums(!is.na(r12)) > 0]
  r12<-as.data.frame(r12)
  for(i in c(1:ncol(r12))) {
    r12[,i] <- as.numeric(as.character(r12[,i]))
  }
  return(r12)
}
quarter_func<-function(quarter){
  quarter<-t(quarter)
  colnames(quarter)<-quarter[1,]
  quarter<-quarter[-1:-2,]
  quarter<-quarter[,colSums(!is.na(quarter)) > 0]
  quarter<-as.data.frame(quarter)
  for(i in c(1:ncol(quarter))) {
    quarter[,i] <- as.numeric(as.character(quarter[,i]))
  }
  return(quarter)
}

#datasets clean
years<-years_func(years = years)
#years<-years[-1:-3,]
r12<-r12_func(r12)
quarter<-quarter_func(quarter)

inputs<-function(){
  cagr<-0
  for(i in 1:length(years$Omsättning)-1){
    cagr[i]<-years$Omsättning[i+1]/years$Omsättning[i]
    cagr[i]<-cagr[i]-1
  }
  ebit_marg<-years$`Resultat Före Skatt`/years$Omsättning
  return(c(mean(cagr),mean(ebit_marg)))
}


DCF<-function(most_recent_annual, most_recent_quarter,wacc_start=seq(0.06,0.08,0.01), 
              growth=inputs()[1], ebitmarginal=inputs()[2], 
              aktiekurs=quarter$`Aktiekurs Snitt`[length(quarter$`Aktiekurs Snitt`)], capex_takt=0.065,
              dep_amor_takt=0.045, evig_start=0.02) {
  # Nettoskuld, antal aktier, aktiekurs
  Motiverat_varde<-0
  nettokassa_nettoskuld<-quarter$Nettoskuld[length(quarter$Nettoskuld)]
  antal_aktier<-quarter$`Antal Aktier`[length(quarter$`Antal Aktier`)]
  
  # Datum 
  date<-Sys.Date()
  date<-str_replace_all(date,"-","")
  date<-as.numeric(date)
  date<-ymd(date) #dagens datum
  most_recent_quarter<-ymd(most_recent_quarter) # ändra denna
  most_recent_annual<-ymd(most_recent_annual) # ändra denna
  
  #statiska inställningar
  skattesats<-0.214
  exit_start<-0.075
  exit_steg<-0.005
  evig_steg<-0.005
  wacc_steg<-0.01
  
  # Omsättning
  sales_annual<-years$Omsättning
  sales_kvartal<-c(quarter$Omsättning[length(quarter$Omsättning)-4],quarter$Omsättning[length(quarter$Omsättning)])
  lerre<-length(sales_annual)
  while (length(sales_annual) < (lerre+10)){
    sales_annual[length(sales_annual)+1]<-sales_annual[length(sales_annual)]*(1+growth)
  }
  
  sales_est<-sales_annual[(lerre+1):(lerre+10)]
  sales_annual<-sales_annual[-(lerre+1):-(lerre+10)]
  
  #EBIT
  
  EBIT_kvartal<-quarter$`Resultat Före Skatt`[length(quarter$`Resultat Före Skatt`)]
  EBIT_est<-sales_est*ebitmarginal
  
  # Capex
  capex<--quarter$`Kassaf Investeringsverk`[length(quarter$`Kassaf Investeringsverk`)]
  capex_est<-sales_est*capex_takt
  capex_est<-capex_est*(-1)
  
  # Av- och nedskrivningar
  
  dep_amor_est<-sales_est*dep_amor_takt
  terminal_dep_amor<-capex_est[length(capex_est)]*0.95
  dep_amor_kvartal<-quarter$EBITDA[length(quarter$EBITDA)]-quarter$`Resultat Före Skatt`[length(quarter$`Resultat Före Skatt`)]
  
  # EBITDA
  EBITDA<-EBIT_est+dep_amor_est
  terminal_EBITDA<-EBITDA[length(EBITDA)]
  
  Terminal_EBIT<-terminal_EBITDA-terminal_dep_amor 
  # Operativa omsättningstillgångar och skulder
  
  Operativa_omsattningstillgangar_annual<-years$`Summa Omsättningstillgångar`[length(years$`Summa Omsättningstillgångar`)]
  Operativa_omsattningstillgangar_kvartal<-quarter$`Summa Omsättningstillgångar`[length(quarter$`Summa Omsättningstillgångar`)]
  
  Operatvia_skulder_annual<-years$`Kortfristiga Skulder`[length(years$`Kortfristiga Skulder`)]
  Operatvia_skulder_kvartal<-quarter$`Kortfristiga Skulder`[length(quarter$`Kortfristiga Skulder`)]
  
  # Rörelsekapital
  Rorelsekapital_annual<-Operativa_omsattningstillgangar_annual-Operatvia_skulder_annual
  Rorelsekapital_kvartal<-Operativa_omsattningstillgangar_kvartal-Operatvia_skulder_kvartal
  
  rorelsekap_omsattningen<-Rorelsekapital_annual/sales_annual[length(sales_annual)] #Detta är en procentsats
  rorelsekap_est<-rorelsekap_omsattningen*sales_est
  
  
  
  # Fritt kassaflöde (FCFF)
  # NOPLAT
  NOPLAT_kvartal<-EBIT_kvartal*(1-skattesats)
  NOPLAT_est<-EBIT_est*(1-skattesats)
  terminal_NOPLAT<-Terminal_EBIT*(1-skattesats)
  # (+) Avskrivningar
  # (-) CapEx
  # (+/-) Förändring i rörelsekapital
  change_in_rorelsekapital<-Rorelsekapital_annual-Rorelsekapital_kvartal
  temp<-rorelsekap_est[1]-Rorelsekapital_annual
  temp2<-0
  for(i in 1:length(rorelsekap_est)){
    temp2[i]<-rorelsekap_est[i+1]-rorelsekap_est[i]
  }
  change_rorelse<-na.omit(c(temp,temp2))
  terminal_change<-change_rorelse[length(change_rorelse)]
  
  # Fritt kassaflöde (FCFF)
  FCFF<-NOPLAT_kvartal+dep_amor_kvartal-capex+change_in_rorelsekapital
  
  skillnad<-interval(start = most_recent_quarter, end = date)
  skillnad<-seconds_to_period(skillnad)
  antal_dagar<-skillnad@day/360
  skillnad<-interval(start = most_recent_quarter, end = (most_recent_annual+duration(1, units = "years")))
  skillnad<-seconds_to_period(skillnad)
  fracyear<-1-antal_dagar/(skillnad@day/360)
  
  est_FCFF<-(fracyear*sum(NOPLAT_est[1]+dep_amor_est[1]+capex_est[1]-change_rorelse[1])-FCFF) #denna är bara första skattningen
  est_FCF<-NOPLAT_est+dep_amor_est+capex_est-change_rorelse #denna är för alla andra förutom första 
  est_FCF<-est_FCF[-1] #tar bort den första i vektorn
  terminal_FCF<-terminal_NOPLAT+terminal_dep_amor+capex_est[length(capex_est)]-terminal_change
  est_FCFF<-c(est_FCFF, est_FCF) #slår samman
  
  skillnad<-interval(start = date, end = (most_recent_annual+duration(1, units = "years")))
  skillnad<-seconds_to_period(skillnad)
  diskonteringsperiod<-(skillnad@day/360)/2
  
  period2<-diskonteringsperiod*2+0.5
  sista_perioderna<-seq(period2+1,8+period2,1)
  diskonteringsperioderna<-c(diskonteringsperiod,period2, sista_perioderna)
  
  for(i in 1:length(wacc_start)){
    wacc7<-est_FCFF/(1+wacc_start[i])^diskonteringsperioderna
    
    # Värdering Gordon Growth
    
    Evigtillvaxt<-terminal_FCF+terminal_change+rorelsekap_est[length(rorelsekap_est)]-rorelsekap_est[length(rorelsekap_est)]*(1+evig_start)
    steg<-evig_start+1
    steg2<-1/(wacc_start[i]-evig_start)
    Terminal_varde<-Evigtillvaxt*steg*steg2
    implicit_exitmultipel<-Terminal_varde/EBITDA[length(EBITDA)]
    
    Diskonteringsfaktor<-1/(1+wacc_start[i])^diskonteringsperioderna[length(diskonteringsperioderna)]
    NPV_termialvarde<-Terminal_varde*Diskonteringsfaktor
    NPV_FCFF<-sum(wacc7)
    EV<-NPV_FCFF+NPV_termialvarde
    Varde_av_EK<-nettokassa_nettoskuld+EV
    (Motiverat_varde[i]<-Varde_av_EK/antal_aktier)
    Potential<-((Motiverat_varde/aktiekurs)-1)*100
  }
  resultat<-data.frame(Motiverat_varde, Potential, wacc_start, aktiekurs)
  return(resultat)
}
skr<-DCF(most_recent_annual = 20190306, most_recent_quarter = 20191022, 
         wacc_start = seq(0.06,0.10,0.01))


ui <- fluidPage(
  titlePanel("Gordon Grown Interaktiv Värdering"),
  fluidRow(
    column(3, 
           dateInput("annual", 
                     h3("Ange datum för senaste årsredovisningen"), 
                     value = "2019-03-22")),
    column(3, 
           dateInput("quarter", 
                     h3("Ange datum för senaste kvartalsrapporten"), 
                     value = "2019-11-05"))),
    column(3, sliderInput("aktiekurs", "Aktiekurs:",min = 1, 
                          max = skr$aktiekurs[1]*3,step=1,value=c(skr$aktiekurs[1]))),
    column(3, sliderInput("capex", "Capex:",min = 0.005,
                        max = 0.15,step=0.005,value=0.065)),
    column(3, sliderInput("dep_amor", "Av och nedskrivningar:",min = 0.005, 
                        max = 0.20,step=0.005,value=0.045)),
    column(3, sliderInput("evig_tillvxt", "Evigt tillväxt:",min = 0.00,
                          max = 0.05,step=0.005,value=0.02)),
  sidebarPanel( 
    sliderInput("num", "WACC:",min = 0.04, max = 0.12,step=0.005,value=c(0.06,0.09)),
    sliderInput("growth", "Omsättningstillväxt:",min = 0.02, max = round(inputs()[1]*3, digits = 1),step=0.005,value=inputs()[1]),
    sliderInput("ebitmarginal", "Rörelsemarginal (EBIT):",min = 0.04, max = round(inputs()[2]*3, digits = 1),step=0.005,value=inputs()[2])),
  mainPanel(plotOutput("plot2")))
 



server <- function(input,output){
  dat <- reactive({
    test<-DCF(most_recent_annual = input$annual,most_recent_quarter = input$quarter, 
              wacc_start = seq(min(input$num),max(input$num),0.005),
              growth = input$growth,
              ebitmarginal = input$ebitmarginal,
              dep_amor_takt = input$dep_amor,
              capex_takt = input$capex,
              evig_start = input$evig_tillvxt,
              aktiekurs = input$aktiekurs)
    print(test)
    test
  })
  
output$plot2<-renderPlot({
    ggplot(data=dat(), aes(x=wacc_start, y=Motiverat_varde)) +
      geom_bar(stat = "identity",                          
               fill = "orange",                       
               colour = "black") +
      geom_text(aes(label=round(Potential, digits=1)), vjust=1.5, color="white", size=6)+
      geom_hline(aes(yintercept = aktiekurs), color="darkblue", size=1)+
      theme_bw()+ 
      theme(axis.title.y = 
              element_text(angle = 0, 
                           hjust = 1, 
                           vjust = 0.5), 
            plot.title = 
              element_text(hjust = 0.5),
            panel.grid.major.x = 
              element_blank()) +
      scale_y_continuous(breaks = seq(0,signif(x=skr$aktiekurs[1],digits=1)*10,signif(x=skr$aktiekurs[1],digits=1)/2))+
      theme(panel.grid.major.y = element_line(color = "grey75")) +
      theme(panel.grid.major.x = element_line(color = "grey75")) +
      theme(panel.grid.minor.y = element_blank()) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold"))+
      xlab("Weighted Average Cost of Capital")+ 
      labs(title = "Gordon Growth DCF värdering", 
           y = "Aktiekurs", caption = "Källa: Börsdata (2019)")},height = 450,width = 500)}


shinyApp(ui, server)


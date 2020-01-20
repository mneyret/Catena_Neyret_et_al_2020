library(readr)
library(ggplot2)
library(lubridate)
library(nlme)
library(lsmeans)
library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(reshape2)
library(car)
library(MuMIn)
library(MASS)
library(readxl)
library(multcomp)
library(multcompView)
library(cowplot)
library(ggeffects)
library(Hmisc)
library(gridExtra)
library(grid)

######## Data preparation ######
##### Meteo data
## load data
Data_2015<- read_excel("~/Desktop/Projet_M2/Data/Analysis/Data/Erosion microplots/2015 Meteo_HuayLang_rain2.xls")
Data_2016<- read_excel("~/Desktop/Projet_M2/Data/Analysis/Data/Erosion microplots/2016 Meteo_HuayLang_rain3.xls")
All_es <- read.csv("~/Desktop/Projet_M2/Data/Raw_data/Microplot_erosion/All_état_surface.csv", sep=';')

Data_2015 = Data_2015[is.na(Data_2015$Date) == FALSE,]
Data_2016 = Data_2016[is.na(Data_2016$Date) == FALSE,]

Data_2015$ACC1 = Data_2015$ACC1*0.96
Data_2015$RTC = Data_2015$RTC*0.96

Data_2015$Correc.ACC = ifelse(!is.na(Data_2015$ACC1), Data_2015$ACC1 / Data_2015$Cumul_since_last_event,1)
Data_2015$Correc.RTC = ifelse(!is.na(Data_2015$RTC), Data_2015$RTC  / Data_2015$Cumul_since_last_event, 1)

Data_2015$All_manual_ok = ifelse(abs(Data_2015$ACC1- Data_2015$Cumul_since_last_event) / Data_2015$Cumul_since_last_event <0.5 &
                                 abs(Data_2015$RTC- Data_2015$Cumul_since_last_event) / Data_2015$Cumul_since_last_event <0.5, 1, 0   )
Data_2016$All_manual_ok = 1

######## Comparaison ACC - RTC -MTO #################
COMP = Data_2015[, c('Date', 'ACC1',"Paddy", 'Cumul_since_last_event', "WindDir_Avg" , "WindRun_Tot")]#, 'Correc.ACC', 'Correc.RTC')]
COMP[is.na(COMP$Cumul_since_last_event),]$Cumul_since_last_event = 0

COMP$ACCsimilar = abs(COMP$ACC1 - COMP$Cumul_since_last_event) / COMP$Cumul_since_last_event
COMP$ACCnook = ifelse(COMP$ACCsimilar < 0.3, 0,1  ) 
COMP$PADsimilar = abs(COMP$Paddy - COMP$Cumul_since_last_event) / COMP$Cumul_since_last_event
COMP$PADnook = ifelse(COMP$PADsimilar < 0.3,  0,1 ) 
COMP$Allnook = rowSums(COMP[,c("PADnook","ACCnook")], na.rm = T)

ppp = COMP[COMP$Cumul_since_last_event>0,]$Allnook

COMP$Month = COMP$Date  %>% 
  as.Date(origin = "1900-01-01")  %>% 
  format(format = '%y%m') 
COMP$Day = COMP$Date  %>% 
  as.Date(origin = "1900-01-01")  
COMP_melt = melt(COMP[, c(c('Day', 'Month', 'Date', 'Cumul_since_last_event', "Allnook", 'ACC1', "Paddy"))], id.var = c('Day', 'Month', 'Date', 'Cumul_since_last_event', "Allnook"))

COMP_melt[is.na(COMP_melt$value),]$value = 0

MTO_ACC_RTC2 =  ggplot(COMP_melt[#COMP_melt$Cumul_since_last_event>0 & 
                                  COMP_melt$Day>'2015-05-01',], aes(x = Day, y = Cumul_since_last_event)) + 
  #geom_line(data = COMP_melt[COMP_melt$variable == 'ACC1',], aes(x = Day, y = Cumul_since_last_event), alpha = 0.5, inherit.aes = F) +  
  geom_col(data = COMP_melt[COMP_melt$variable == 'ACC1',], aes(x = Day, y = Cumul_since_last_event), width = 1) +
  geom_point(data = COMP_melt[COMP_melt$Cumul_since_last_event>0 & 
                            #    COMP_melt$Day>'2015-05-01'
             COMP_melt$Allnook > 0,]
            , aes(Day, y = value, pch = variable),color = 'black', inherit.aes = FALSE, size = 2) +
  theme_bw(base_size = 14)+
  scale_shape_manual(breaks = c('ACC1', 'Paddy'), label = c('M YR', 'ORi ORs'), values = c(1, 16), name = 'Rain gauge ') +
  scale_linetype_manual(name = 'Meteo station')+
  xlab('Date') + ylab('Rainfall (mm)')

ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/MTO_ACC_RTC2.pdf',MTO_ACC_RTC2, width = 9, height = 4)

################ Formattage données #####################
# Pour reformater le tableau il vaut mieux séparer chaque variable : runoff, coef, N, K, sediments
  
### Pour 2015 ###

# on enlève les évènements pour lesquels les sédiments ont été mesurés sur <= 6 microplots
Data_2015$keepdetach = apply(Data_2015[, colnames(Data_2015)[grepl("Detach", colnames(Data_2015)) & ! grepl("DetachTot", colnames(Data_2015))]],
      1,
      function(x){length(x[x>0 & !is.na(x)])}) > 6

Meteo_2015 = Data_2015[,c("ID", "Date", "API_2", "API_eventstart" ,'Cumul','All_manual_ok', "Cumul_since_last_event", 'max_meanInt',"max_EI30_since_last_event", "Sum_EC_since_last_event", 'Correc.ACC', 'Correc.RTC', 'keepdetach')]

# Minimum rainfall to provoke runoff in all microplots
Ruiss_only = Data_2015[, colnames(Data_2015)[grepl("Ruiss", colnames(Data_2015))]]

Some_ruiss = rowSums(Ruiss_only, na.rm = T) # ruissellement non nul


Ruiss_only = Data_2015[, colnames(Data_2015)[grepl("Ruiss", colnames(Data_2015))]]
Some_ruiss = rowSums(Ruiss_only, na.rm = T)
count_ruiss = Ruiss_only
count_ruiss[count_ruiss>0 &!is.na(count_ruiss)] = 1
Meteo_2015$allruiss = rowSums(count_ruiss) == 12


Rain_2015 = Data_2015
Ruiss_2015 = melt(data.frame(Rain_2015[ , c('ID', colnames(Data_2015)[grepl("Ruiss", colnames(Data_2015))])])
                  , id.var = 'ID', value.name = 'Ruiss', variable.name = 'Microplot')
Ruiss_2015$Microplot = gsub('Ruiss_', '', Ruiss_2015$Microplot)

Sediments_2015 = melt(Rain_2015[,c('ID', colnames(Data_2015)[grepl("Detach", colnames(Data_2015)) & ! grepl("DetachTot", colnames(Data_2015))])]
                       , id.var = 'ID', value.name = 'Detachment', variable.name = 'Microplot')
Sediments_2015$Microplot = gsub('Detach_', '', Sediments_2015$Microplot)

Tot_det_2015 = melt(Rain_2015[,c('ID', colnames(Data_2015)[grepl("DetachTot", colnames(Data_2015))])]
                      , id.var = 'ID', value.name = 'Tot_Detachment', variable.name = 'Microplot')
Tot_det_2015$Microplot = gsub('DetachTot_', '', Sediments_2015$Microplot)

Meteo_2015$Month = Meteo_2015$Date  %>% 
  as.Date(origin = "1900-01-01")  %>% 
  format(format = '%y%m') 
Meteo_2015$Day = Meteo_2015$Date  %>% 
  as.Date(origin = "1900-01-01")  
Melt_2015 = Reduce(function(...) merge(..., all=TRUE), list(Meteo_2015,Ruiss_2015, Sediments_2015,Tot_det_2015))

Crop15 = gsub('[IS]*[1-9]*', '', Melt_2015$Microplot)
Melt_2015$max.EI30_corr = ifelse(Crop15 %in% c('M', 'YRU','YR', 'YRM'), Melt_2015$max_EI30_since_last_event*Melt_2015$Correc.ACC, Melt_2015$max_EI30_since_last_event*Melt_2015$Correc.RTC  )
Melt_2015$Cumul_corr = ifelse(Crop15 %in% c('M', 'YRU','YR', 'YRM'), Melt_2015$Cumul_since_last_event*Melt_2015$Correc.ACC, Melt_2015$Cumul_since_last_event*Melt_2015$Correc.RTC  )
Melt_2015$max_meanInt_corr = ifelse(Crop15 %in% c('M', 'YRU','YR', 'YRM'), Melt_2015$max_meanInt*Melt_2015$Correc.ACC, Melt_2015$max_meanInt*Melt_2015$Correc.RTC  )
Melt_2015$Ruiss_corr = Melt_2015$Ruiss
Melt_2015$Sum.EC_corr = ifelse(Crop15 %in% c('M', 'YRU','YR', 'YRM'), Melt_2015$Sum_EC_since_last_event*Melt_2015$Correc.ACC, Melt_2015$Sum_EC_since_last_event*Melt_2015$Correc.RTC  )


Melt_2015$Coeff = Melt_2015$Ruiss/ Melt_2015$Cumul_since_last_event
Melt_2015$Coeff_corr = Melt_2015$Ruiss/ Melt_2015$Cumul_corr

Melt_2015 = Melt_2015[Melt_2015$Day > "2015-05-01",]


## Pour 2016
Data_2016$Coeff.1 = rowSums(Data_2016[, colnames(Data_2016)[grepl("Ruiss", colnames(Data_2016))]] > Data_2016$Cumul_since_last_event)
Meteo_2016 = Data_2016[,c("ID", "Date","API_2", "API_eventstart","max_meanInt" ,'Cumul', "Cumul_since_last_event","All_manual_ok", "max_EI30_since_last_event", "Sum_EC_since_last_event", "Coeff.1")]

Ruiss_only = Data_2016[, colnames(Data_2016)[grepl("Ruiss", colnames(Data_2016))]]
Some_ruiss = rowSums(Ruiss_only, na.rm = T)
count_ruiss = Ruiss_only
count_ruiss[count_ruiss>0 &!is.na(count_ruiss)] = 1
min(Data_2016[rowSums(count_ruiss) == 12,]$Cumul_since_last_event, na.rm = T)
Data_2016[rowSums(count_ruiss) == 12,]$ID

Meteo_2016$allruiss = rowSums(count_ruiss) == 12

Rain_2016 = Data_2016

Ruiss_2016 = melt(Rain_2016[ , c('ID', colnames(Data_2016)[grepl("Ruiss", colnames(Data_2016))])]
                  , id.var = 'ID', value.name = 'Ruiss', variable.name = 'Microplot')
Ruiss_2016$Microplot = gsub('Ruiss_', '', Ruiss_2016$Microplot)

Sediments_2016 = melt(Rain_2016[,c('ID', colnames(Data_2016)[grepl("Detach", colnames(Data_2016)) & ! grepl("DetachTot", colnames(Data_2016))])]
                      , id.var = 'ID', value.name = 'Detachment', variable.name = 'Microplot')
Sediments_2016$Microplot = gsub('Detach_', '', Sediments_2016$Microplot)


Tot_det_2016 = melt(Rain_2016[,c('ID', colnames(Data_2016)[grepl("Detach", colnames(Data_2016))])]
                    , id.var = 'ID', value.name = 'Tot_Detachment', variable.name = 'Microplot')
Tot_det_2016$Microplot = gsub('DetachTot_', '', Tot_det_2016$Microplot)

Meteo_2016$Month = Meteo_2016$Date  %>% 
  as.Date(origin = "1900-01-01")  %>% 
  format(format = '%y%m') 
Meteo_2016$Day = Meteo_2016$Date  %>% 
  as.Date(origin = "1900-01-01")  
Melt_2016 = Reduce(function(...) merge(..., all=TRUE), list(Meteo_2016 ,Ruiss_2016, Sediments_2016))


Melt_2016 = Melt_2016[Melt_2016$Day > "2016-05-01",]
Crop16 = gsub('[IS]*[1-9]*', '', Melt_2016$Microplot)
Melt_2016$Tot_Detachment = Melt_2016$Detachment
Melt_2016$Ruiss_corr =  Melt_2016$Ruiss
Melt_2016$Coeff_corr =   Melt_2016$Coeff
Melt_2016$max.EI30_corr =  Melt_2016$max_EI30_since_last_event
Melt_2016$Cumul_corr =  Melt_2016$Cumul_since_last_event
Melt_2016$Sum.EC_corr =  Melt_2016$Sum_EC_since_last_event
Melt_2016$max_meanInt_corr = Melt_2016$max_meanInt

Melt_2016$Coeff = Melt_2016$Ruiss/ Melt_2016$Cumul_since_last_event
Melt_2016$Coeff_corr = Melt_2016$Ruiss/ Melt_2016$Cumul_corr
Melt_2016$keepdetach = TRUE

##### Put together
Melt_2015$Yr = rep('2015')
Melt_2016$Yr = rep('2016')

Melt_2015$Yr_date =  Melt_2015$Date - 42005.00
Melt_2016$Yr_date =  Melt_2016$Date - 42370.00

Melt_2015$ID = as.character(Melt_2015$ID)
Melt_2016$ID = as.character(Melt_2016$ID)

Melt_tot = rbind(Melt_2015[,intersect(colnames(Melt_2016), colnames(Melt_2015))]
                 , Melt_2016[,intersect(colnames(Melt_2016), colnames(Melt_2015))])

# Add slope data
slope = tapply(All_es$Slope.., All_es$Microplot, min, na.rm = T)
slope = slope[!is.na(slope)]
slope_rad = atan(slope/100)
slope_cos = cos(slope_rad)
Melt_tot$Slope = slope[Melt_tot$Microplot]
Melt_tot$Slope_cos = slope_cos[Melt_tot$Microplot]
Melt_tot$Sum.EC_cos = Melt_tot$Sum.EC_corr * Melt_tot$Slope_cos

Melt_tot$Crop = factor(gsub('[1-9]','',  Melt_tot$Microplot))
Melt_tot$Crop = factor(gsub('YR[UM]', 'YR',  Melt_tot$Crop), levels = c('M', 'YR', 'ORI', 'ORS'))


####### Extreme events ####### 
Melt_tot$EI_quantiles = factor(NA, levels = c('veryl', 'med', 'veryh'))
Melt_tot$Intensity_quantiles = factor(NA, levels = c('veryl', 'med', 'veryh'))

for (y in c(2015, 2016)){
Melt_tot[Melt_tot$Yr == y,]$EI_quantiles =  cut(Melt_tot[Melt_tot$Yr == y &Melt_tot$Cumul_corr>0,]$max_EI30_since_last_event, 
                                                c(0,
                                                  quantile(Melt_tot[Melt_tot$Yr == y &Melt_tot$Cumul_corr>0,]$max_EI30_since_last_event, c(0.20, 0.95), na.rm = T),
                                                  max(Melt_tot[Melt_tot$Yr == y &Melt_tot$Cumul_corr>0,]$max_EI30_since_last_event, na.rm = T))
                                                , labels = c('veryl', 'med', 'veryh'))
Melt_tot[Melt_tot$Yr == y,]$Intensity_quantiles =  cut(Melt_tot[Melt_tot$Yr == y &Melt_tot$Cumul_corr>0,]$max_meanInt, 
                                                c(0,
                                                  quantile(Melt_tot[Melt_tot$Yr == y &Melt_tot$Cumul_corr>0,]$max_meanInt, c(0.10, 0.95), na.rm = T),
                                                  max(Melt_tot[Melt_tot$Yr == y &Melt_tot$Cumul_corr>0,]$max_meanInt, na.rm = T))
                                                , labels = c('veryl', 'med', 'veryh'))
}



Melt_tot$Extreme =factor( ifelse(Melt_tot$EI_quantiles == 'veryh' | Melt_tot$Intensity_quantiles == 'veryh', 1, 0))

Melt_tot =Melt_tot[Melt_tot$Extreme == 0,]

############################################################
###################### Data analysis  ######################
############################################################
### Min rainfall ###
# 2015
D15_month = Melt_tot[Melt_tot$Yr == 2015 & Melt_tot$Extreme == 0,]
R_min_month = data.frame(do.call("rbind",lapply(split(D15_month, list(D15_month$Microplot, D15_month$Month,D15_month$Crop)), function(x)
  min(x[x$Ruiss>0.1 & !is.na(x$Ruiss),]$Cumul_corr, na.rm = T))))
colnames(R_min_month) = 'value'
library(stringr)
R_min_month[, c('Microplot','Month', 'Crop')] = str_split_fixed(rownames(R_min_month), '\\.', 3)[, c(1,2,3)]
R_min_month$micro =   gsub('^[A-TY]*[1234U][IS]*','',R_min_month$Microplot)
R_min_month[is.infinite(R_min_month$value),]$value = NA
R_min_month$Crop  = factor(R_min_month$Crop, levels = c('M', 'YR', 'ORI', 'ORS'))
R_nevent = melt(with(D15_month[D15_month$Ruiss>0.1,], tapply(Ruiss, list(Microplot, Month), function(x){length(x[!is.na(x)])})))
colnames(R_nevent) = c('Microplot', 'Month', 'N_event')
R_min_month = merge(R_nevent, R_min_month, by = c('Microplot', 'Month'))

Plot_min= ggplot(R_min_month, aes(y=value, x=factor(Month), group = micro))+ facet_wrap(Crop~., ncol = 2) + theme_bw(base_size = 14)+
  geom_col(position = "dodge", fill ='gray70', color = 'black') +
  geom_text(data = R_min_month, aes(y = value + 1.2, label = N_event),position = position_dodge(width = 1)) +
  ylab("Minimal rainfall volume (L/m2)") + xlab('Month')#+  scale_y_log10(breaks = c(10, 100, 500, 1000))
mod = lme(value~Month, data = R_min_month[!(R_min_month$Crop %in% c('ORI', 'ORS')) 
                                         & R_min_month$Month < 1510,], random = ~1|Microplot,na.action = na.omit )
plot(mod)
summary(mod)
ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/Ruiss_min_month_15.pdf', Plot_min, width = 6, height = 4)


R_month = tapply(D15_month$Ruiss_corr, list(D15_month$Crop,D15_month$Microplot, D15_month$Month), sum, na.rm = T)
P_month = tapply(D15_month$Cumul_corr, list(D15_month$Crop,D15_month$Microplot, D15_month$Month), sum, na.rm = T)

R_month_m = melt(R_month/P_month)
R_month_m$micro =   gsub('^[A-z]*[1-3][IS]*','',R_month_m$Var2)
Ruiss_month_15 = ggplot(R_month_m, aes(y=value, x=factor(Var3), group = micro))+ facet_wrap(Var1~., ncol = 2) + theme_bw()+
  geom_col(position = "dodge", fill ='gray70', color = 'black', width = 0.8) +
  ylab("Monthly runoff coefficient") + xlab('Month')#+  scale_y_log10(breaks = c(10, 100, 500, 1000))
ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/Ruiss_month_15.pdf', Ruiss_month_15, width = 6, height = 4)

D_month = tapply(D15_month$Tot_Detachment, list(D15_month$Crop,D15_month$Microplot, D15_month$Month), sum, na.rm = T)
D_month = melt(D_month)
D_month$micro =   gsub('^[A-z]*[1-3][IS]*','',R_month_m$Var2) 
Det_month_15 = ggplot(D_month, aes(y=value, x=factor(Var3), group = micro))+ facet_wrap(Var1~., ncol = 2) + theme_bw()+
  geom_col(position = "dodge", fill ='gray70', color = 'black') +
  ylab("Monthly soil detachment (g/m2)") + xlab('Month')#+  scale_y_log10(breaks = c(10, 100, 500, 1000))
ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/Det_month_15.pdf', Det_month_15, width = 6, height = 4)

######## Ruiss - Detach #######
Ruiss_Det_16 = ggplot(Melt_tot[Melt_tot$Yr == "2016" & Melt_tot$Extreme==0 & !(log10(Melt_tot$Detachment) < 0.5 & log10(Melt_tot$Ruiss) >0.75),],
                      aes(log10(Ruiss+1), log10(Detachment+1),  color = Crop, fill = Crop, linetype = Crop
                          , shape = Crop)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F) + theme_bw(base_size = 14) +
  ylab('Log soil detachment (g/m2)') + xlab('Log runoff (L)') +
  scale_fill_grey(start = .70 , end = 0.10, breaks = c('M', 'YR', 'ORI', 'ORS'))+
  scale_fill_grey(start = .80 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'), guide = FALSE)+
  scale_shape_manual(values = c(0, 1,15,16), breaks = c('M', 'YR', 'ORI', 'ORS'))+
  scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'longdash'), breaks = c('M', 'YR', 'ORI', 'ORS'))+
  scale_color_grey(start = .80 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS')) +
  annotate("text", y = c(7,6.8, 6.6,6.2)-3.5, x = 0.6, 
           label = c('log Runoff ***', 'Crop ***', 'log Runoff x Crop ***', 'R2= 86%'))

ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/Ruiss_Det_16.pdf', Ruiss_Det_16, width = 6, height = 4)

D_16 = Melt_tot[Melt_tot$Yr == "2016" &
                  !(log10(Melt_tot$Detachment) < 0.5 & log10(Melt_tot$Ruiss) >0.75)
                ,]
D_16$Ldet = log10(D_16$Detachment +1)
D_16$Lrui = log10(D_16$Ruiss+1)
mod_det_ruiss = lme(Ldet ~Lrui*Crop, data =D_16, random = ~1|Microplot, na.action = na.omit )
Anova(mod_det_ruiss)
summary(mod_det_ruiss)
qqnorm(residuals(mod_det_ruiss)); qqline(residuals(mod_det_ruiss))
r.squaredGLMM(mod_det_ruiss)
TukeyHSD(mod_det_ruiss)
cld(glht(mod_det_ruiss, linfct= mcp(Crop = 'Tukey')))
cld(lsmeans(mod_det_ruiss, 'Crop'))
cld(lstrends(mod_det_ruiss, 'Crop', var = 'Lrui'))







#######  Which rainfall parameters influence runoff and detachment ? ####### 
D_16_r = Melt_tot[Melt_tot$Yr == "2016" & !is.na(Melt_tot$Ruiss) 
                  & Melt_tot$allruiss == TRUE 
                & Melt_tot$Ruiss > 0
                  & Melt_tot$Cumul_corr >2
                 # & (Melt_tot$Crop %in% c('ORI', 'ORS'))
                  & !is.na(Melt_tot$Detachment)
                  #& Melt_tot$Detachment >0# &
                  #Melt_tot$Coeff_corr <1.1
                  ,]
D_16_r[, c('API_eventstart', 
           'Cumul_since_last_event', 
           'max_EI30_since_last_event',
           'Sum.EC_corr')] = scale(D_16_r[, c('API_eventstart', 
                                              'Cumul_since_last_event', 
                                              'max_EI30_since_last_event',
                                              'Sum.EC_corr')])

library(hier.part)
HP = hier.part(D_16_r$Detachment, D_16_r[,c('Crop', 
                                'API_eventstart', 
                                'Cumul_since_last_event', 
                                'max_EI30_since_last_event',
                                  'Sum.EC_corr', 'Yr_date')] )
barplot(HP$I.perc$I,names.arg = c('Crop', 'API', 'Cumul', 'EI30', 'EC', 'Yr_date'))
barplot(HP$IJ$J,names.arg = c('Crop', 'API', 'Cumul', 'EI30', 'EC', 'Yr_date'))

rand.hp(D_16_r$Ruiss, D_16_r[,c('Crop', 'API_eventstart', 'Cumul_since_last_event', 'max_EI30_since_last_event',
                                'Sum.EC_corr', 'Yr_date')])



#########
D_16_r$Crop = factor(D_16_r$Crop, levels = c('M', 'YR', 'ORI', 'ORS'))
D_16_r$group = factor(D_16_r$Crop, levels = c('M', 'YR', 'ORI', 'ORS'))

D_16_r$Microplot = factor(D_16_r$Microplot)

plot_mod = function(model,D,  xx, yy, lab_x, lab_y, lab, title = ''){
  Dnew = D
  Dnew$XX = D[,xx]
  Dnew$YY = D[,yy]
  gg = ggeffect(model, term = c(xx, 'Crop'))
  gg$group = factor(gg$group, levels = c('M', 'YR', 'ORI', 'ORS')) 
  gg$Crop = factor(gg$group, levels = c('M', 'YR', 'ORI', 'ORS')) 
  P = plot(gg, rawdata = FALSE, colors = "bw") + 
    geom_point(data = Dnew, aes(y = YY, x = XX, color = Crop, shape = Crop), inherit.aes = F) +
    geom_line(data = gg, aes(x = x, y= predicted, linetype=Crop, color=Crop), alpha = 0.2) +
    xlab(lab_x) + ylab(lab_y) + theme_bw(base_size = 10)+ggtitle(title)+
    scale_fill_grey(start = .80 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'), guide = FALSE)+
    scale_shape_manual(values = c(0, 1,15,16), breaks = c('M', 'YR', 'ORI', 'ORS'))+
    scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'longdash'), breaks = c('M', 'YR', 'ORI', 'ORS'))+
    scale_color_grey(start = .80 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'))
    #+
 #   annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,
 #            label = paste(lab, ptostar(Anova(model)[,3]), collapse = '\n')) +
#    annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,
#             label = paste('R2 =', round(r.squaredGLMM(model)[2],2)*100, '%'))
  return(P)
}

#### Runoff ####
lambdas =boxCox(D$Ruiss ~ D$Crop, data = D, family = "bcPower", lambda = seq(-1, 1, length = 20))
lambda= round(lambdas[['x']][which(lambdas[['y']] == max(lambdas[['y']]))], 2) 

D_16$Ruiss_bc = bcPower(D_16$Ruiss, lambda)

P_r_crop = ggplot(D_16, aes(Ruiss_bc, x = Crop, fill = Crop, shape = Crop, color = Crop)) + 
  theme_bw(base_size = 10)+
  geom_boxplot(outlier.shape = NA, alpha = 0.2) + 
  geom_jitter() + ggtitle(paste('l = ',lambda)) +
  scale_fill_grey(start = .80 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'), guide = FALSE)+
  scale_shape_manual(values = c(0, 1,15,16), breaks = c('M', 'YR', 'ORI', 'ORS'))+
  scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'longdash'), breaks = c('M', 'YR', 'ORI', 'ORS'))+
  scale_color_grey(start = .80 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS')) +
  ylab('Transformed runoff (L/m2)') + xlab('Land use') 

Ruiss_models = matrix(ncol = 4, nrow = 5)
labels = c('Rainfall (mm)', 'API', 'max EI30', 'EC cum', 'Date')
names(labels) = c('Cumul_corr', 'API_eventstart', 'max.EI30_corr', 'Sum.EC_corr', 'Yr_date')
plotlist_ruiss = list()
i = 0
for (X in c('Cumul_corr', 'API_eventstart', 'max.EI30_corr', 'Sum.EC_corr', 'Yr_date')){
  i = i+1
  print('------------------------------')
  print(X)
  D = D_16_r
  D$X = D[, X]
  
  lambdas =boxCox(D$Ruiss ~ D$X*D$Crop, data = D, family = "bcPower", lambda = seq(-1, 1, length = 100))
  lambda= round(lambdas[['x']][which(lambdas[['y']] == max(lambdas[['y']]))], 2)
  lambdaconfint = round(c(min( lambdas$x[lambdas$y > max(lambdas$y) - 1/2 * qchisq(.95,1)]),
                          max( lambdas$x[lambdas$y > max(lambdas$y) - 1/2 * qchisq(.95,1)])),2)
  
  D$Ruiss_bc = bcPower(D$Ruiss, lambda)
  m = lme(Ruiss_bc~ X*Crop, random = ~1|Microplot ,D, na.action=na.omit, method = 'ML')
  m2 = stepAIC(m, direction = 'both', trace = 0, k =  3.8)
  print(summary(m2))
  print(cld(lsmeans(m, specs = 'Crop')))
  
  
  Ruiss_models[i,] = c(X,
                       as.character(m2$call)[2],
                       round(r.squaredGLMM(m2)[2], 3),
                       paste(c(lambda, ' (', lambdaconfint[1],' ', lambdaconfint[2], ')' ), collapse = ''))
  tryCatch({
    print(cld(lstrends(m, var ='X', 'Crop')))
  }, error=function(e){'cld'})
 # tryCatch({
#    print(Anova(m, 'III'))
#  }, error=function(e){'Anova'})
  plot(resid(m), fitted(m))
  qqnorm(resid(m)); qqline(resid(m))
  
  print("lala")
  plotlist_ruiss[[i]] = plot_mod(m, D, 'X', 'Ruiss_bc', labels[X], title = paste('l = ',round(lambda, 2)), 'Transformed runoff (L/m2)', c('Rainfall', 'Crop', 'Rainfall x Crop'))
}

plotlist_ruiss[[1]] +  scale_color_manual(name = 'Land use', breaks = c('M', 'YR', 'ORI', 'ORS'), values  = c("tan1", "lightgreen", 'mediumpurple', 'royalblue'), guide = F) +
   scale_fill_manual(name = 'Land use', breaks = c('M', 'YR', 'ORI', 'ORS'), values = c("tan1", "lightgreen", 'mediumpurple', 'royalblue'), guide = F) 
  
#### Detachment ####
D_16_d = Melt_tot[Melt_tot$Yr == "2016" & !is.na(Melt_tot$Ruiss) 
                  & Melt_tot$allruiss == TRUE 
                  & Melt_tot$Ruiss > 0
                  & Melt_tot$Cumul_corr >2
                   & !(Melt_tot$Crop %in% c('ORI', 'ORS'))
                  #& Melt_tot$keepdetach == TRUE &
                  & !is.na(Melt_tot$Detachment)
                  & Melt_tot$Detachment >0# &
                  #Melt_tot$Coeff_corr <1.1
                  ,]

D_16_d$Crop = factor(D_16_d$Crop, levels = c('M', 'YR', 'ORI', 'ORS'))
D_16_d$group = factor(D_16_d$Crop, levels = c('M', 'YR', 'ORI', 'ORS'))

D_16_d$Microplot = factor(D_16_d$Microplot)


Det_models = matrix(ncol = 4, nrow = 5)
labels = c('Rainfall (mm)', 'API', 'max EI30', 'EC cum', 'Date')
names(labels) = c('Cumul_corr', 'API_eventstart', 'max.EI30_corr', 'Sum.EC_corr', 'Yr_date')
plotlist_det = list()
i = 0
for (X in c('Cumul_corr', 'API_eventstart', 'max.EI30_corr', 'Sum.EC_corr', 'Yr_date')){
  i = i+1
  print('------------------------------')
  print(X)
  D = D_16_d
  D$X = D[, X]
  
  lambdas =boxCox(D$Detachment ~ D$X*D$Crop, data = D, family = "bcPower", lambda = seq(-1, 1, length = 20))
  lambda= round(lambdas[['x']][which(lambdas[['y']] == max(lambdas[['y']]))], 2) 
  lambdaconfint = round(c(min( lambdas$x[lambdas$y > max(lambdas$y) - 1/2 * qchisq(.95,1)]),
                          max( lambdas$x[lambdas$y > max(lambdas$y) - 1/2 * qchisq(.95,1)])),2)
  D$Detachment_bc = bcPower(D$Detachment, lambda)
  m = lme(Detachment_bc~ X*Crop, random = ~1|Microplot ,D, na.action=na.omit, method = 'ML')
  m2 = stepAIC(m, direction = 'backward', k =  3.8)
  Det_models[i,] = c(X, as.character(m2$call)[2], round(r.squaredGLMM(m2)[2], 3),
                     paste(c(lambda, ' (', lambdaconfint[1],' ', lambdaconfint[2], ')' ), collapse = ''))
  tryCatch({
    print(cld(lstrends(m2, var ='X', 'Crop')))
  }, error=function(e){'cld'})
  tryCatch({
    print(Anova(m, 'III'))
  }, error=function(e){'Anova'})
  print("lala")
plotlist_det[[i]] = plot_mod(m, D, 'X', 'Detachment_bc', labels[X], title = paste('l = ',round(lambda, 2)), 'Transformed detachment (g/m2)', c('Rainfall', 'Crop', 'Rainfall x Crop'))
}

### Crop
lambdas =boxCox(D$Detachment ~ D$Crop, data = D, family = "bcPower", lambda = seq(-1, 1, length = 20))
lambda= round(lambdas[['x']][which(lambdas[['y']] == max(lambdas[['y']]))], 2) 
D_16$Detachment_bc = bcPower(D_16$Detachment, lambda)

P_det_crop = ggplot(D_16, aes(Detachment_bc, x = Crop, shape = Crop,color = Crop, fill = Crop)) + theme_bw(base_size = 10)+
  geom_boxplot(outlier.shape = NA, alpha = 0.2) + geom_jitter() + ggtitle(paste('l = ',lambda))+
  scale_fill_grey(start = .80 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'), guide = FALSE)+
  scale_shape_manual(values = c(0, 1,15,16), breaks = c('M', 'YR', 'ORI', 'ORS'))+
  scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'longdash'), breaks = c('M', 'YR', 'ORI', 'ORS'))+
  scale_color_grey(start = .80 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS')) +
  ylab('Transformed detachment (g/m2)') + xlab('Land use') #+
  annotate(geom ="text", x =seq(1:4), y= c(2.5, 2.5, 4,4), size = 8,
           label = c('a', 'a', 'b', 'b'))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(P_r_cum)
Det_D16 <- grid.arrange(arrangeGrob(  P_det_crop+ theme(legend.position="none"),
                                      plotlist_det[[1]] + theme(legend.position="none"),
                                      plotlist_det[[2]] + theme(legend.position="none"),
                                      plotlist_det[[3]] + theme(legend.position="none"),
                                      plotlist_det[[4]] + theme(legend.position="none"),
                                      plotlist_det[[5]] + theme(legend.position="none"),
                                     
                                      nrow=2, ncol = 3),
                          mylegend, ncol=2, widths=c(10, 1))

Ruiss_D16 <- grid.arrange(arrangeGrob(P_r_crop+ theme(legend.position="none"),
                                      plotlist_ruiss[[1]] + theme(legend.position="none"),
                                      plotlist_ruiss[[2]] + theme(legend.position="none"),
                                      plotlist_ruiss[[3]] + theme(legend.position="none"),
                                      plotlist_ruiss[[4]] + theme(legend.position="none"),
                                      plotlist_ruiss[[5]] + theme(legend.position="none"),
                                    
                                    nrow=2, ncol = 3),
                        mylegend, ncol=2, widths=c(10, 1))

ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/Ruiss_D16.pdf',Ruiss_D16, width = 8, height = 6)
ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/Det_D16.pdf',Det_D16, width = 8, height = 6)




############################################
nice_labels = c("Land use",'Rainfall (mm)', 'API', 'Max EI30', 'Cumulated EC', 'Date', 'API2', 'max int')
names(nice_labels) = c('Crop', "Cumul_corr", 'API_eventstart', 'max.EI30_corr', 'Sum.EC_cos','Yr_date', 'API_2', 'max_meanInt_corr')
G = vector("list", 9)
names(G) =  c("Crop", "Cumul_corr", 'API_eventstart', 'max.EI30_corr',  'Sum.EC_cos', 'Yr_date', 'API_2','max_meanInt_corr' )
for (i in names(nice_labels)){
  print(i)
  DD = data.frame(R = D_16$Ruiss,
                  Crop = as.character(D_16$Crop),
                  D = D_16$Yr_date,
                  V = D_16[,i],
                  Microplot = D_16$Microplot)
  if(i == "Crop"){m0 = lme(log10(R) ~ Crop, data = DD, random = ~1|Microplot, method = 'ML', na.action=na.omit )}
  else{m0 = lme(log10(R) ~ V*Crop, data = DD, random = ~1|Microplot, method = 'ML', na.action=na.omit)
  print(cld(lstrends(m0,"Crop", var ='V'), alpha = 0.05, Letters = LETTERS))
 # print(Anova(m0))
  m = stepAIC(m0, trace =0)
  print(Anova(m))
  
  #qqnorm(resid(m))
  #qqline(resid(m))
  #plot(fitted(m),resid(m))
  r = r.squaredGLMM(m0)[1]
  print(r)
  #plot(m) 
}
  if (i == 'Crop'){
    G[[i]] =  ggplot(DD, aes(y = log10(R), x = Crop, color = Crop,  fill = Crop)) + 
      geom_boxplot(alpha = 0.5)+ theme_bw(base_size = 14)+
      geom_jitter()+  
      scale_fill_grey(start = .85 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'))+
      scale_color_grey(start = .85 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'))+
      xlab(nice_labels[i]) + ylab('log10 runoff (L/m2)') + ggtitle('')+
      annotate("text", x = 1:4, y = 2,size = 5, label =c('a','a', 'b', 'b')) +
      annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,label = paste('R2 =', round(r,2)*100, '%'))
  }
  else {G[[i]] = tryCatch(ploteffect(m,m0, i),
            error = function(e) {
             G[[i]] =  ggplot(DD, aes(y = log10(R), x = V, color = Crop)) + geom_point(alpha = 0.5)+ theme_bw(base_size = 14) + ggtitle('')+
               scale_fill_grey(start = .85 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'))+
               scale_color_grey(start = .85 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'))+
               xlab(nice_labels[i]) + ylab('log10 runoff (g/m2)') + 
               #scale_y_log10() +
               annotate(geom ="text",x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, label = paste(c(nice_labels[i], 'Crop', paste(nice_labels[i], 'x Crop')), ptostar(Anova(m0)[,3]), collapse = '\n'),
                        size = 3) +
               annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,label = paste('R2 =', round(r,2)*100, '%'))
             
            } )}
# plot(G[[i]])
}
Ruiss_D16 <- grid.arrange(arrangeGrob(G[[1]] + theme(legend.position="none"),
                                      G[[2]] + theme(legend.position="none"),
                                      G[[3]] + theme(legend.position="none"),
                                      G[[4]] + theme(legend.position="none"),
                                      G[[5]] + theme(legend.position="none"),
                                      G[[6]] + theme(legend.position="none"),
                                      nrow=2, ncol = 3),
                          mylegend, ncol=2, widths=c(10, 1))

ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/Ruiss_D16.pdf',Ruiss_D16, width = 8, height = 6)



ptostar = function(x){
 s = ifelse(x > 0.05, "n.s", 
         ifelse(x > 0.01,'*',
               ifelse(x > 0.001,'**', '***')))
 return(s)}
  
ploteffect = function(m,m0, i){
  library(ggeffects)
  GG = ggpredict(m, term = c('V', 'Crop'))
  GG$group = factor(  GG$group, levels = c('M', 'YR', 'ORI', 'ORS')) 
  P = plot(GG, rawdata = TRUE, colors = "bw") + 
    geom_line(data = GG, aes(x = x, y= predicted, linetype=group, color=group)) +
    #scale_y_log10() +
   # scale_linetype(c("solid", 'longdash', 'dotdash', 'dashed'), breaks = c('M', 'YR', 'ORI', 'ORS')) +
    xlab(nice_labels[i]) + ylab('log10 soil detachment (g/m2)') + theme_bw(base_size = 14) +ggtitle('')+
    scale_fill_grey(start = .85 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'))+
    scale_color_grey(start = .85 , end = 0, breaks = c('M', 'YR', 'ORI', 'ORS'))+
    annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,label = paste(c(nice_labels[i], 'Crop', paste(nice_labels[i], 'x Crop')), ptostar(Anova(m0)[,3]), collapse = '\n')) +
    annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,label = paste('R2 =', round(r,2)*100, '%'))

return(P)}


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(G[[1]])


# Detachment
D_15 = Melt_tot[Melt_tot$Extreme == 0 & 
  Melt_tot$Yr == "2016" & Melt_tot$Detachment>0,]
D_15 = D_15[!is.na(D_15$ID),]
D_15[, c('Ruiss_c','Detachment_c', 'API_eventstart_c', 'max.EI30_corr_c', 'Sum.EC_corr_c', 'max_meanInt_corr_c','Date_c', 'Cumul_corr_c', 'Yr_date_c', 'Sum_EC_since_last_event_c', 'max_EI30_since_last_event_c')] = 
  scale(D_15[, c('Ruiss','Detachment', 'API_eventstart', 'max.EI30_corr', 'Sum.EC_corr','max_meanInt_corr', 'Date', 'Cumul_corr', 'Yr_date', 'Sum_EC_since_last_event', 'max_EI30_since_last_event')], scale = TRUE)

nice_labels = c("Land use",'Rainfall (mm)', 'API', 'Max EI30', 'Cumulated EC', 'Date')
names(nice_labels) = c('Crop', "Cumul_corr", 'API_eventstart', 'max.EI30_corr', 'Sum.EC_corr','Yr_date')
G = vector("list", 6)
names(G) =  c("Crop", "Cumul_corr", 'API_eventstart', 'max.EI30_corr', 'Sum.EC_corr','Yr_date')
for (i in names(nice_labels)){
  print(i)
  DD = data.frame(R = D_15[D_15$All_manual_ok == 1 & D_15$keepdetach == TRUE,]$Detachment,
                  Crop = D_15[D_15$All_manual_ok == 1 & D_15$keepdetach == TRUE,]$Crop,
                  V = D_15[D_15$All_manual_ok == 1 &  D_15$keepdetach == TRUE,i],
                  Microplot = D_15[D_15$All_manual_ok == 1&  D_15$keepdetach==TRUE,]$Microplot)
  if(i == "Crop"){m0 = lme(log10(R) ~ Crop, data = DD, random = ~1|Microplot, method = 'ML', na.action = na.omit)}
  else{m0 = lme(log10(R) ~ V*Crop, data = DD, random = ~1|Microplot, method = 'ML',na.action = na.omit)
  print(cld(lstrends(m0, 'Crop', var = 'V')))}
  print(Anova(m0))
  m = stepAIC(m0, trace =0)
  qqnorm(resid(m))
  qqline(resid(m))
  r = r.squaredGLMM(m)[1]
  plot(m)
  if (i == 'Crop'){
    G[[i]] =  ggplot(DD, aes(y = log10(R), x = Crop, color = Crop,  fill = Crop)) + geom_boxplot(alpha = 0.5)+theme_bw(base_size = 14)+
      geom_jitter()+ scale_color_brewer(palette = "Set1")+scale_fill_brewer(palette = "Set1")+
      xlab(nice_labels[i]) + ylab('log10 soil detachment (g)') + ggtitle('')+
      annotate("text", x = 1:4, y = 4,size = 5, label =c('a', 'a', 'b', 'b')) +
      annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,label = paste('R2 =', round(r,2)*100, '%'))
  }
  else {
    G[[i]] = tryCatch(ploteffect(m,m0, i),
                          error = function(e) {
                            G[[i]] =  ggplot(DD, aes(y = log10(R), x = V, color = Crop)) + geom_point(alpha = 0.5)+theme_bw(base_size = 14)+ggtitle('')+
                              scale_color_brewer(palette = "Set1")+scale_fill_brewer(palette = "Set1")+xlab(nice_labels[i]) + ylab('log10 Detachment (g)') + 
                              annotate(geom ="text",x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, label = paste(c(nice_labels[i], 'Crop', paste(nice_labels[i], 'x Crop')), ptostar(Anova(m0)[,3]), collapse = '\n'),
                                       size = 3) +
                              annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,label = paste('R2 =', round(r,2)*100, '%'))
                            
                          } )}
  plot(G[[i]])
}

ptostar = function(x){
  s = ifelse(x > 0.05, "n.s", 
             ifelse(x > 0.01,'*',
                    ifelse(x > 0.001,'**', '***')))
  return(s)}

ploteffect = function(m,m0, i){P = plot(ggeffect(m, term = c('V', 'Crop'), colors = 'bw'), rawdata = TRUE) + 
  xlab(nice_labels[i]) + ylab('log10 Detachment (g)') + theme_bw(base_size = 14) +ggtitle('')+
  annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,label = paste(c(nice_labels[i], 'Crop', paste(nice_labels[i], 'x Crop')), ptostar(Anova(m0)[,3]), collapse = '\n')) +
  annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,label = paste('R2 =', round(r,2)*100, '%'))

return(P)}
ploteffect(m,m0, i)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(G[[1]])

Det_D16 <- grid.arrange(arrangeGrob(G[[1]] + theme(legend.position="none"),
                                      G[[2]] + theme(legend.position="none"),
                                      G[[3]] + theme(legend.position="none"),
                                      G[[4]] + theme(legend.position="none"),
                                      G[[5]] + theme(legend.position="none"),
                                      G[[6]] + theme(legend.position="none"),
                                      nrow=2, ncol = 3),
                          mylegend, ncol=2, widths=c(10, 1))

ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/Det_D16.pdf',Det_D16, width = 8, height = 6)


# What if we split among periods
q5 = quantile(Melt_tot[Melt_tot$Yr =="2015" & Melt_tot$Ruiss>0 & !is.na(Melt_tot$Ruiss),]$Yr_date, c(0,0.33, 0.66,1))
q6 = quantile(Melt_tot[Melt_tot$Yr =="2016" & Melt_tot$Ruiss>0 & !is.na(Melt_tot$Ruiss),]$Yr_date, c(0,0.33, 0.66,1))

date_cut5  =c(42200, 42277)#( '2015-07-15', '2015-09-30')), origin = "1900-01-01")

date_cut6  = c(42566,42643)#as.numeric(as.Date(c( '2016-07-15', '2016-09-30')), origin = "1900-01-01")

Melt_tot$part_moonsoon = NA
Melt_tot$part_moonsoon = ifelse(
  Melt_tot$Yr == '2015', ifelse(Melt_tot$Day < as.Date("2015-07-15"), "early",
                                ifelse(Melt_tot$Day < as.Date("2015-09-30"), "mid",'late')),
  ifelse(Melt_tot$Day < as.Date("2016-07-15"), "early",
         ifelse(Melt_tot$Day < as.Date("2016-09-30"), "mid",'late')))
#Melt_tot$part_moonsoon = factor(Melt_tot$part_moonsoon, levels = c('early', 'mid', 'late'))
Melt_tot$part_moonsoon = ifelse(
  Melt_tot$Yr == '2015', ifelse(Melt_tot$Day < as.Date("2015-09-01"), "early",'late'),
  ifelse(Melt_tot$Day < as.Date("2016-09-01"), "early",'late'))
Melt_tot$part_moonsoon = factor(Melt_tot$part_moonsoon, levels = c('early', 'late'))


Melt_tot = Melt_tot[!is.na(Melt_tot$Ruiss),]

ggplot(Melt_tot, aes(y =log10(Coeff), x = Crop )) + geom_boxplot() + facet_wrap(Yr~part_moonsoon)

Melt_tot$CropMoon = factor(paste(Melt_tot$Crop, Melt_tot$part_moonsoon), levels = c(
  'M early',   'M late' , 
  'YR early'  ,  'YR late' ,'ORI early' ,  'ORI late' ,  'ORS early'  ,'ORS late' ))
mod_15 = lme(log10(Ruiss)~CropMoon, data = Melt_tot[Melt_tot$Yr == 2016 & Melt_tot$All_manual_ok == TRUE &
                                               Melt_tot$Ruiss >7 ,], random = ~1|Microplot, na.action = na.omit)
plot(mod_15)
qqnorm(resid(mod_15));qqline(resid(mod_15))

L= cld(glht(mod_15, linfct= mcp(CropMoon = 'Tukey')))

ggplot(Melt_tot[Melt_tot$Yr == 2016 & Melt_tot$All_manual_ok == TRUE &
                  Melt_tot$Ruiss > 1 ,], aes(y =log10(Coeff), x = CropMoon )) + geom_boxplot() +
  annotate("text", y = 0.5, x = 1:8, label = L$mcletters$monospacedLetters)


mod_15 = lme(log10(Detachment)~Crop, data = Melt_tot[Melt_tot$Detachment > 0 & Melt_tot$Yr == 2015 & 
                                                  Melt_tot$All_manual_ok == TRUE  &
                                                  Melt_tot$part_moonsoon == 'early' ,], random = ~1|Microplot, na.action = na.omit)
qqnorm(resid(mod_15));qqline(resid(mod_15))
Anova(mod_15)

ggplot(Melt_tot[Melt_tot$Ruiss > 0,], aes(log10(Ruiss), fill = Crop)) + geom_histogram() + facet_wrap(Yr~part_moonsoon)


DD = D_15[D_15$All_manual_ok == 1 & D_15$Cumul>7,]
mod_coeff = lme(log10(Ruiss) ~ 
                #  Yr_date_c   *Crop
               # +     Cumul_corr_c*Crop +
                 #(API_eventstart_c+Cumul_corr_c+
                    (Sum.EC_corr_c) *Crop# +
                 # API_eventstart_c
                ,
                random = ~1|Microplot,
                data = DD,#D_16[D_16$All_manual_ok == 1 & D_16$Cumul_corr>6.1,],
                , method = 'ML'
               ,na.action = na.omit)

qqnorm(resid(mod_coeff))
qqline(resid(mod_coeff))
plot(mod_coeff)
Anova(mod_coeff)
summary(mod_coeff)

mod_runoff_15 =lme(log10(Ruiss+1) ~ 
                    Yr_date_c   *Crop
                    +     Cumul_corr_c*Crop +
                     max_meanInt_corr_c + Crop +
                     API_eventstart_c
                   ,
                   random = ~1|Microplot,
                   data = D_15[D_15$All_manual_ok == 1,],
                   , method = 'ML',)
vif(mod_runoff_15)
cor(D_15$Cumul_corr_c, D_15$Sum.EC_corr_c)
mod_runoff_15_bis =  update( mod_runoff_15, weights = varIdent(form = ~ 1 | Crop) )
leveneTest(residuals(mod_runoff_15_bis) ~ D_15[D_15$All_manual_ok == 1,]$Crop)
plot(ggeffect(mod_runoff_15_bis, terms = c('API_eventstart_c', 'Crop')))

Anova(mod_runoff_15_bis)
summary(mod_runoff_15_bis)
qqnorm(resid(mod_runoff_15))
qqline(resid(mod_runoff_15))
r.squaredGLMM(mod_runoff_15_bis)

# 2015 - Detachment

mod_det_15_bis =lme(log10(Detachment+1) ~ #API_eventstart_c +
                 #     (
               #    Cumul_corr_c
                  Yr_date_c# )
                * Crop 
#+Sum.EC_corr_c ,
                    ,random = ~1|Microplot,
                    data = D_15[D_15$All_manual_ok == 1 & D_15$keepdetach == TRUE & !is.na(D_15$Detachment) ,][R<0.6,]
                    , method = 'ML')
leveneTest(residuals(mod_det_15_bis) ~D_15[D_15$All_manual_ok == 1 & D_15$keepdetach == TRUE & !is.na(D_15$Detachment) ,][R<0.6,]$Crop)
R = resid(mod_det_15)
plot(mod_det_15)

qqnorm(resid(mod_det_15_bis))
qqline(resid(mod_det_15_bis))
Anova(mod_det_15_bis)
summary(mod_det_15_bis)
qqnorm(resid(mod_runoff_15_bis))
qqline(resid(mod_runoff_15_bis))
vif(mod_det_15_bis)

cld(lsmeans(mod_runoff_15_bis, pairwise~Crop))
paste(round(summary(lsmeans(mod_det_15_bis, 'Crop'))[,2],2), "\\textsuperscript{\\emph{}}", sep = "", collapse = " & ")

K = cld(lstrends(mod_runoff_15_bis,"Crop", "Sum.EC_corr_c"), alpha = 0.05, Letters = LETTERS)
K = cld(lstrends(mod_det_15_bis,"Crop", "Sum.EC_corr_c"), alpha = 0.05, Letters = LETTERS)

paste(ifelse(K[,2]<0.1,format(K[,2],scientific = TRUE, digits = 2), round(K[,2],2)),
      "$\\pm$",
      ifelse(K[,3]<0.1,format(K[,3],scientific = TRUE, digits = 2), round(K[,3],2)),
      "\\textsuperscript{\\emph{",
      K[,7], 
      "}}",
      collapse = '&')

paste(round(summary(lstrends(mod_detach_class, 'Crop',  "Yr_date_c"))[,2],2), "\\textsuperscript{\\emph{}}", sep = "", collapse = " & ")

stargazer(mod_runoff_15_bis,mod_det_15_bis, title="Regression Results",
          dep.var.labels=c("Log Runoff", "Log detachment"),
          #covariate.labels=c("Handling of Complaints","No Special Privileges",
          #                   "Opportunity to Learn","Performance-Based Raises","Too Critical","Advancement"),
          omit.stat=c("LL","ser","f"),
          add.lines=list(c("R2m", r.squaredGLMM(mod_runoff_class)[1]), c('R2c', r.squaredGLMM(mod_runoff_class)[2])),
          single.row=FALSE, digits = 2, align = TRUE)



# 2016 - runoff
D_16 = Melt_tot[Melt_tot$Extreme == 0 & Melt_tot$Yr == "2016" & !is.na(Melt_tot$Ruiss)  ,]
D_16 = D_16[!is.na(D_16$ID),]

D_16[, c('Ruiss_c','Detachment_c', 'API_eventstart_c', 'max.EI30_corr_c', 'Sum.EC_corr_c', 'Date_c', 'Cumul_corr_c', 'Yr_date_c', 'Sum_EC_since_last_event', 'max_EI30_since_last_event')] = 
  scale(D_16[, c('Ruiss','Detachment', 'API_eventstart', 'max.EI30_corr', 'Sum.EC_corr', 'Date', 'Cumul_corr', 'Yr_date', 'Sum_EC_since_last_event', 'max_EI30_since_last_event')], scale = TRUE)

View(D_16[!complete.cases(D_16[, c('Ruiss', 'API_eventstart_c', 'Cumul_corr_c', 'Yr_date_c', 'Sum.EC_corr_c', 'Crop')]),])

mod_runoff_16 =lme(log10(Ruiss+1) ~ API_eventstart_c +
                     (Cumul_corr_c +   
                        Yr_date_c )*Crop +Sum.EC_corr_c
                   ,
                   random = ~1|Microplot,
                   data = D_16,
                   , method = 'ML')

MOD_CUM = lmer(log10(Ruiss+1) ~ Cumul_corr_c  +(1|Microplot),
               data = D_16)
MOD_Date = lmer(log10(Ruiss+1) ~ Yr_date_c  +(1|Microplot),
               data = D_16)
MOD_DateCUM = lmer(log10(Ruiss+1) ~ Yr_date_c*Cumul_corr_c+(1|Microplot),
               data = D_16)
anova(MOD_CUM)
anova(MOD_Date)
anova(MOD_DateCUM)
cor(D_16$Yr_date_c, D_16$Cumul_corr_c)

mod_runoff_16 =lmer(log10(Ruiss+1) ~
                     (Yr_date_c +   
                         Cumul_corr_c)*Crop + API_eventstart_c +Sum.EC_corr_c
                   +(1|Microplot),
                   data = D_16,
                   )
mod_runoff_16_bis =  update( mod_runoff_16, weights = varIdent(form = ~ 1 | Crop) )
leveneTest(residuals(mod_runoff_16_bis) ~ D_16$Crop)

anova(mod_runoff_16)
summary(mod_runoff_16)
Anova(mod_runoff_16_bis, 'III')
summary(mod_runoff_16_bis)
qqnorm(resid(mod_runoff_16_bis))
qqline(resid(mod_runoff_16_bis))
vif(mod_runoff_16_bis)
r.squaredGLMM(mod_runoff_16_bis)



plot(ggeffect(mod_det_16_bis, terms = c('API_eventstart_c','Crop')))
# 2016 - Detachment

mod_det_16 =lme(log10(Detachment+1) ~ API_eventstart_c +
                  (Cumul_corr_c+ Yr_date_c +Sum.EC_corr_c)*Crop ,
                random = ~1|Microplot,
                data = D_16[ !is.na(D_16$Detachment) ,]
                , method = 'ML')
leveneTest(residuals(mod_det_16) ~D_16[ !is.na(D_16$Detachment) ,]$Crop)
mod_det_16_bis =  update( mod_det_16, weights = varIdent(form = ~ 1 | Crop) )
R = resid(mod_det_15)
plot(mod_det_16_bis)

qqnorm(resid(mod_det_16))
qqline(resid(mod_det_16))
Anova(mod_det_15_bis)
summary(mod_runoff_15r)
vif(mod_det_16_bis)

cld(lsmeans(mod_det_16_bis, pairwise~Crop))
paste(round(summary(lsmeans(mod_det_16_bis, 'Crop'))[,2],2), "\\textsuperscript{\\emph{}}", sep = "", collapse = " & ")

Kr = cld(lstrends(mod_runoff_16_bis,"Crop", "Sum.EC_corr_c"), alpha = 0.05, Letters = LETTERS)
Kd = cld(lstrends(mod_det_16_bis,"Crop", "Sum.EC_corr_c"), alpha = 0.05, Letters = LETTERS)

R = paste(ifelse(Kr[,2]<0.1,format(Kr[,2],scientific = TRUE, digits = 2), round(Kr[,2],2)),
      "\\textsuperscript{\\emph{", Kr[,7],  "}}", collapse = '&')
D = paste(ifelse(Kd[,2]<0.1,format(Kd[,2],scientific = TRUE, digits = 2), round(Kd[,2],2)),
   "\\textsuperscript{\\emph{",Kd[,7],   "}}", collapse = '&')
Rse = paste('(',ifelse(Kr[,3]<0.1,format(Kr[,3],scientific = TRUE, digits = 2), round(Kr[,3],2)),')',
            collapse = '&')
Dse = paste('(',ifelse(Kd[,3]<0.1,format(Kd[,3],scientific = TRUE, digits = 2), round(Kd[,3],2)),')',
            collapse = '&')
paste(R, D, sep = '&')
paste(Rse, Dse, sep = '&')


paste(#"(",
  format(summary(lsmeans(mod_runoff_16_bis, 'Crop'))[,2],digits = 2, scientific = TRUE), "\\textsuperscript{\\emph{}}",
    #  ")", 
  sep = "", collapse = " & ")

stargazer(mod_runoff_16_bis,mod_det_16_bis, title="Regression Results",
          dep.var.labels=c("Log Runoff", "Log detachment"),
          #covariate.labels=c("Handling of Complaints","No Special Privileges",
          #                   "Opportunity to Learn","Performance-Based Raises","Too Critical","Advancement"),
          omit.stat=c("LL","ser","f"),
          add.lines=list(c("R2m", r.squaredGLMM(mod_runoff_class)[1]), c('R2c', r.squaredGLMM(mod_runoff_class)[2])),
          single.row=FALSE, digits = 2, align = TRUE)

#### Lien avec les caractéristiques de surface ####
### Variation of surface among LU ###
All_es2 = All_es[1:48,c(1:6,14:32)]


coeff_corr = tapply(Melt_tot[Melt_tot$Extreme == 0 & Melt_tot$All_manual_ok == TRUE,]$Ruiss, list(Melt_tot[Melt_tot$Extreme == 0& Melt_tot$All_manual_ok == TRUE,]$Microplot, Melt_tot[Melt_tot$Extreme == 0& Melt_tot$All_manual_ok == TRUE,]$part_moonsoon),sum,na.rm = T  )/
  tapply(Melt_tot[Melt_tot$Extreme == 0 & Melt_tot$All_manual_ok == TRUE,]$Cumul_corr, list(Melt_tot[Melt_tot$Extreme == 0& Melt_tot$All_manual_ok == TRUE,]$Microplot, Melt_tot[Melt_tot$Extreme == 0& Melt_tot$All_manual_ok == TRUE,]$part_moonsoon),sum, na.rm = T )

det_corr = tapply(Melt_tot[Melt_tot$Extreme == 0,]$Tot_Detachment, list(Melt_tot[Melt_tot$Extreme == 0,]$Microplot, Melt_tot[Melt_tot$Extreme == 0,]$part_moonsoon),sum,na.rm = T  )
 

All_es2$CRUST = All_es2$ST + All_es2$ERO + All_es2$G

All_es2[,c(7:ncol(All_es2))] = scale(All_es2[,c(7:ncol(All_es2))])
All_es2$Month = factor(ifelse(All_es2$Month == "August", 'Aug', 'MayJune'))

All_es2$coeff_corr= ifelse(All_es2$Month == "Aug", as.numeric(coeff_corr[as.character(All_es2$Microplot), 'mid']),
                           as.numeric(coeff_corr[as.character(All_es2$Microplot), 'early']))


All_es2$det_corr= ifelse(as.character(All_es2$Month) == "Aug", as.numeric(det_corr[as.character(All_es2$Microplot), 'mid']),
                         as.numeric(det_corr[as.character(All_es2$Microplot), 'early']))


All_es_melt_surface = melt(All_es[, (colnames(All_es) %in% c('Fag' ,'FG','ST','ERO','G','Charcoals','Residues','Month','Year', 'Plot', 'Microplot', 'Crop'))],
                        id.vars = c('Month','Year', 'Plot', 'Microplot', 'Crop'))
All_es_mean = aggregate(All_es_melt_surface$value, by = list(All_es_melt_surface$Month,All_es_melt_surface$Year,All_es_melt_surface$Plot,All_es_melt_surface$Crop, All_es_melt_surface$variable), mean)

colnames(All_es_mean) = c('Month', 'Year', 'Plot', 'Crop', 'variable', 'value')

All_es_mean$Month = as.character(All_es_mean$Month)

All_es_mean[All_es_mean$Month %in% c('May', 'June'),]$Month = 'MayJune'

All_es_melt_surface$Month = as.character(All_es_melt_surface$Month)
All_es_melt_surface[All_es_melt_surface$Month %in% c('May', 'June'),]$Month = 'MayJune'

All_es_melt_surface$Crop = factor(All_es_melt_surface$Crop, c('M', 'YR', 'ORI', 'ORS'))
All_es_melt_surface$Month = factor(All_es_melt_surface$Month, c('MayJune', 'August', 'Oct'))
All_es_melt_surface$Microplot_n = gsub('[A-z1-9]*([1-9]{1})', '\\1', All_es_melt_surface$Microplot)

Surface15 = ggplot(All_es_melt_surface[All_es_melt_surface$variable != 'Charcoals' & All_es_melt_surface$Year == "2015",], aes(x = Microplot_n, y=value, fill = variable, color = variable )) +
  geom_col() + facet_grid( Crop ~ Year +  Month) +
  scale_color_grey(start = 0.5, end = 0.5, guide = FALSE) +
  theme_bw(base_size = 14) +  ylab('Proportion (%)') + xlab('Microplot repetition') +
  scale_fill_grey(name = '',start = 0.95, end = 0, breaks = c('Fag', 'FG', 'ST', 'ERO', 'G', 'Residues'),
                                      labels = c('Free aggregates', 'Free gravels', 'Structural crust', 
                                                                  'Erosion crust', 'Gravel crust', 'Crop residues'))
 # scale_fill_brewer(palette="Spectral", breaks = c('Fag', 'FG', 'ST', 'ERO', 'G', 'Residues'),
  #                                      labels = c('Free aggregates', 'Free gravels', 'Structural crust', 
   #                                               'Erosion crust', 'Gravel crust', 'Crop residues'))
Surface16 = ggplot(All_es_melt_surface[All_es_melt_surface$variable != 'Charcoals' & All_es_melt_surface$Year == "2016",], aes(x = Microplot_n, y=value, fill = variable, color = variable )) +
  geom_col() + facet_grid( Crop ~ Year+ Month) + 
  theme_bw(base_size = 14) +  
  ylab('') + xlab('Microplot repetition') +
  scale_color_grey(start = 0.5, end = 0.5, guide = FALSE) +
  scale_fill_grey(name = '', start = 0.95, end = 0, breaks = c('Fag', 'FG', 'ST', 'ERO', 'G', 'Residues'),
                   labels = c('Free aggregates', 'Free gravels', 'Structural crust', 
                              'Erosion crust', 'Gravel crust', 'Crop residues'))

#  scale_fill_brewer(palette="Spectral", breaks = c('Fag', 'FG', 'ST', 'ERO', 'G', 'Residues'),
#                    labels = c('Free aggregates', 'Free gravels', 'Structural crust', 
#                               'Erosion crust', 'Gravel crust', 'Crop residues'))
legend <- g_legend(Surface15 + theme(legend.position='bottom'))


Surface = grid_arrange_shared_legend(Surface15, Surface16, ncol = 2,
                                     position = "bottom") 
plot(Surface)
ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/Surface_per_micro.pdf',Surface, width = 8, height = 6)


All_es_melt_carac$Microplot_n = gsub('[A-z1-9]*([1-9]{1})', '\\1', All_es_melt_carac$Microplot)
All_es_melt_carac$test = paste(All_es_melt_carac$Microplot_n,All_es_melt_carac$variable) 
All_es_melt_carac$Crop = factor(All_es_melt_carac$Crop , levels = c('M', 'YR', 'ORI', 'ORS'))
cover15 = ggplot(All_es_melt_carac[!is.na(All_es_melt_carac$Month) & !(All_es_melt_carac$variable %in% c('CH', 'WH'))  & All_es_melt_carac$Year == "2015",],
                 aes(x = Microplot_n, y=value, fill = variable, color = variable, group = test )) +
  geom_col(position = position_dodge()) + facet_grid( Crop ~ Year +  Month) +
  theme_bw(base_size = 14) +  ylab('Proportion (%)') + xlab('Microplot repetition') +
  scale_color_grey(start = 0.2, end = 0.7, labels = c('Crop cover', 'Weed cover'), name = "") +
  scale_fill_grey(start =  0.2, end = 0.7,  labels = c('Crop cover', 'Weed cover'), name = "") 
  
  
cover16 = ggplot(All_es_melt_carac[!is.na(All_es_melt_carac$Month) & !(All_es_melt_carac$variable %in% c('CH', 'WH'))  & All_es_melt_carac$Year == "2016",],
                 aes(x = Microplot_n, y=value, fill = variable, color = variable, group = test )) +
  geom_col(position = position_dodge()) + facet_grid( Crop ~ Year +  Month) +
  scale_color_grey(start = 0.2, end = 0.7, breaks = c('CC', 'WC'), labels = c('Crop cover', 'Weed cover'), name = "") +
  scale_fill_grey(start =  0.2, end = 0.7, breaks = c('CC', 'WC'), labels = c('Crop cover', 'Weed cover'), name = "") +
  
  theme_bw(base_size = 14) +  ylab('Proportion (%)') + xlab('Microplot repetition') 

cropcover = grid_arrange_shared_legend(cover15, cover16, ncol = 2,
                                     position = "bottom") 

ggsave('/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/Cover.pdf',cropcover,  width = 8, height = 6)



### Lien avec le runoff ###

Mat_annual_month = corstarsl(as.matrix(All_es[All_es$Crop %in% c('M', 'YR') & All_es$Month != 'August',c(7:17,19:29)]))
xtable(Mat_annual_month)
Mat_or_month     = corstarsl(as.matrix(All_es[!(All_es$Crop %in% c('M', 'YR')) &  All_es$Month == 'August',c(7:15,17:27)]))
xtable(Mat_or_month)

All_es_agg = aggregate(All_es, list(All_es$Microplot,All_es$Year) , mean, na.rm = T)
All_es_agg = All_es_agg[, c(1,2,9:17, 20:31)]

All_es_agg$tot_crust = rowSums(All_es_agg[, c('ST','ERO','G')])
All_es_agg$tot_litter = All_es_agg$Residues + All_es_agg$WC
All_es_agg$G / All_es_agg$tot_crust

Mat_annual = corstarsl(as.matrix(All_es_agg[ES_crop %in% c('M', 'YR'),3:25]))
xtable(Mat_annual)

Mat_or = corstarsl(as.matrix(All_es_agg[!(ES_crop %in% c('M', 'YR')),3:25]))
xtable(Mat_or)

All_es_agg$Cropyear = paste(gsub('[0-9]', '', All_es_agg[,1]), All_es_agg[,2])
ggplot(All_es_agg[!(ES_crop %in% c('M', 'YR')),], aes(yearly_coeff, x=WC, color = Cropyear)) + geom_point()

ES_crop = gsub('[1-9]', '', All_es_agg[,1])
mod = stepAIC(lm(yearly_coeff ~ Fag + FG + Charcoals + ST + ERO + G +Algae + Mosses + WC + tot_crust + CC + Ped. + Pedhx , data = All_es_agg[ES_crop %in% c('M', 'YR'),]))
summary(mod)
Anova(mod, type = 3)
vif(mod)


####### Meteo ########
library(readr)


Meteo_month <- read_delim("~/Desktop/Projet_M2/Documents/Papers/Article 2/Data_code/Meteo_month.csv", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)
Meteo_month$Date = as.Date(Meteo_month$Date, format = "%m/%d/%y")


Monthly_coeff = melt(tapply(Melt_tot$Ruiss_corr, list(Melt_tot$Microplot, Melt_tot$Month), sum, na.rm = T) / 
                       tapply(Melt_tot$Cumul_corr, list(Melt_tot$Microplot, Melt_tot$Month), sum, na.rm = T) )
Monthly_coeff[, 1] = gsub('OR[24]', 'OR', Monthly_coeff[, 1])
Monthly_coeff[, 1] = gsub('YR[MU]', 'YR', Monthly_coeff[, 1])
Monthly_coeff[,1] = gsub('M1([1-3])', 'M\\1', Monthly_coeff[, 1])
Monthly_coeff$Crop = gsub('[1-9]', '', Monthly_coeff[, 1])

# Test significant variations for each month / crop
Monthly_coeff$Crop = as.character(Monthly_coeff$Crop)
Letters_month = matrix(nrow = 8, ncol = 5)
k = 0
for (i in seq(1605, 1611)){
  k = k+1
  print('----------------------------------')
  print(i)
  D = Monthly_coeff[Monthly_coeff$Var2 ==i,]
  print(unique(D$Crop ))
  D$Crop = factor( D$Crop, levels = c('M', 'YR', 'ORI', 'ORS'))
  mod = aov(value ~Crop, D)
#  print(summary(mod))
  L = cld(glht(mod, linfct = mcp(Crop = "Tukey")))
  print(c(as.character(i), L$mcletters$monospacedLetters))
  Letters_month[k,] = c(as.character(i), L$mcletters$monospacedLetters)
}
colnames(Letters_month) = c('YrMonth', 'M', 'YR', 'ORI', 'ORS')
Letters_month = data.frame(Letters_month)
Letters_month_m = melt(Letters_month, id.vars = 'YrMonth', measure.vars = c('M', 'YR', 'ORI', 'ORS') ) 

Monthly_coeff$MonthCrop = factor(paste(Monthly_coeff$Var2, Monthly_coeff$Crop))
mod = lm(value ~MonthCrop, Monthly_coeff[Monthly_coeff$Crop %in% c('M', 'YR'),])
summary(mod)

Monthly_coeff_agg = aggregate(Monthly_coeff$value, list(Monthly_coeff$Crop,Monthly_coeff$Var2), mean, na.rm = T)
colnames(Monthly_coeff_agg)[3] = "mean"
Monthly_coeff_sd = aggregate(Monthly_coeff$value, list(Monthly_coeff$Crop,Monthly_coeff$Var2), sd, na.rm = T)
colnames(Monthly_coeff_sd)[3] = "sd"

Coeff_agg = merge(Monthly_coeff_agg,Monthly_coeff_sd, all = TRUE )
M_coeff = dcast(Coeff_agg, Group.2 ~Group.1, value.var = c('mean'))
colnames(M_coeff)[1] = 'YrMonth'

SD_coeff = dcast(Coeff_agg, Group.2 ~Group.1, value.var = c('sd'))
colnames(SD_coeff)[1] = c('YrMonth')


Meteo_tot_m = merge(M_coeff, Meteo_month, all = TRUE)
Meteo_tot = merge(M_coeff, Meteo_month, all = TRUE)

Meteo_tot_sd = merge(SD_coeff, Meteo_month, all = TRUE)

Meteo_long_m = melt(Meteo_tot_m, id.vars = colnames(Meteo_tot)[c(1,6:14)] )
Meteo_long_sd = melt(Meteo_tot_sd, id.vars = colnames(Meteo_tot)[c(1,6:14)] )

Meteo_long_m$variable = factor(Meteo_long_m$variable , levels = c('M', 'YR', 'ORI', 'ORS'))
Meteo_long_sd$variable = factor(Meteo_long_sd$variable , levels = c('M', 'YR', 'ORI', 'ORS'))

colnames(Meteo_long_m)[12] = 'Mean'
colnames(Meteo_long_sd)[12] = 'SD'


Meteo_long  = merge(Meteo_long_m, Meteo_long_sd, all = TRUE)

## Plot 

plot_cumul = ggplot(Meteo_tot[Meteo_tot$YrMonth>1505,], aes(y=Cumul, x = Date)) +
  geom_col(fill = 'grey80') + 
  theme_bw(base_size = 10) +
  xlab('') + ylab('Rainfall\n(mm)')+
  geom_point( aes(y=s5mm*10, x=Date), color = 'black')+geom_line(aes(y=s5mm*10, x=Date), color = 'gray30')+
  scale_y_continuous(sec.axis = sec_axis(~./10, name = 'Number of\nevents > 5mm')) +
  geom_vline(xintercept = Management[Management$Management == "Observation",]$Date, linetype = "dashed")

plot_int = ggplot(Meteo_tot[Meteo_tot$YrMonth>1505,], aes(y=R, x = Date)) +
  geom_col(fill = 'grey55') +theme_bw(base_size = 10) +
  xlab('') + ylab('Rainfall erosivity\n(KJ.mm/m2/h)') +
 # geom_point( aes(y=Rdens*200, x=Date), color = 'black', shape = 17)+geom_line( aes(y=Rdens*200, x=Date), color = 'gray30')+
 # scale_y_continuous(sec.axis = sec_axis(~./200, name = "Erosivity density\n(MJ/m2/h)")) #+

  #theme(axis.text.x=element_text(size=7,  angle = 90), axis.title=element_text(size=9,face="bold")) +
  geom_vline(xintercept = Management[Management$Management == "Observation",]$Date, linetype = "dashed")
  

g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 


Management <- read_csv("~/Desktop/Projet_M2/Documents/Papers/Article 2/Data_code/Management.csv")
Management$Date = as.Date(Management$Date, format = "%m/%d/%y")
Management$LUnum = as.numeric(factor(Management$Crop, levels = c('M', 'YR', 'OR')))/17+0.7

Meteo_long = 
  merge(Meteo_long,Letters_month_m,  by = c('YrMonth', 'variable'))


Meteo_long$value = gsub(' ', '', Meteo_long$value)

Meteo_long$Date2 = factor(format(Meteo_long$Date, "%b"), levels = c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov'))

plot_coeff =  
  ggplot(Meteo_long[Meteo_long$Yr == 2016,], aes(y=Mean, x = Date2, group = variable,
                                                  fill = variable, color = variable), color = "black" ) +
  geom_col(position = "dodge") + theme_bw()+
  geom_text( aes( label= value, y = Mean + 0.02), position = position_dodge(width = 1), cex = 8,colour = 'black')  +
  scale_color_grey(name = 'Land use', start = 0.4, end = 0.8, guide = FALSE) +
  scale_color_manual(name = 'Land use', breaks = c('M', 'YR', 'ORI', 'ORS'), values  = c("tan1", "lightgreen", 'mediumpurple', 'royalblue'), guide = F) +
  scale_fill_manual(name = 'Land use', breaks = c('M', 'YR', 'ORI', 'ORS'), values = c("tan1", "lightgreen", 'mediumpurple', 'royalblue'), guide = F) +
  theme(  panel.background = element_rect(fill = "transparent",colour = NA),
          axis.text = element_text(colour = "black", size = rel(2)),
          title = element_text(color = "black", size = rel(2)),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
    xlab('') + ylab('Monthly runoff\ncoefficient')# +
   scale_fill_grey(name = 'Land use', start = 0.85, end = 0) +
#  guides(fill=guide_legend(nrow=2, byrow = TRUE, title.position='top', order = 1))
legend1 <- g_legend(plot_coeff) 

   png('/Users/Margot/Desktop/Projet_M2/Documents/Presentations/SFE18/coeff.png',width=600,height=600,units="px",bg = "transparent")
   plot(plot_coeff)
   dev.off()
   

meteo = plot_grid(plot_cumul, plot_int,
                #  plot_coeff + theme(legend.position = "none"),
                  ncol = 1, align = c("v"))
all_met = plot_grid(meteo, legend1, ncol = 1, rel_heights = c(5,1))

ggsave("/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/meteo.pdf", all_met)
ggsave("/Users/Margot/Desktop/Projet_M2/Documents/Papers/Article 2/Figures/coeff_2016.pdf", plot_coeff)










#### Now let's add plant cover data
Rain_2017[!is.na(Rain_2017$Date_microplot),]

Plant_cov = read.csv2("~/Desktop/Projet_M2/Data/Raw_data/Microplot_erosion/Areas_tot.csv", sep = '\t')
Plant_cov$Plot_Treat = paste(Plant_cov$Plot, Plant_cov$Traitement)
Plant_cov[Plant_cov$Plot == 'OR3',]$Plot = 'OR2'
Plant_cov[Plant_cov$Traitement == 'H',]$Traitement = 'N'
Plant_cov$Plants = as.numeric(as.character(Plant_cov$Plants))

mod_plants = lme(Plants~Traitement*Plot, data = Plant_cov, random = ~1|Microplot)
summary(mod_plants)

ggplot(Plant_cov, aes(x = Date, y = Plants, group = Microplot, color = Plot_Treat)) +# geom_smooth(span = 0.3) +
  geom_point() + theme_bw()+
  geom_smooth(span = 0.4) +
  scale_x_continuous(breaks = datevalues, limits = c(datevalues[24], datevalues[36]),
                                                                                                                                   labels =format(as.Date(datevalues,origin = "1900-01-01"), format = '%b') )
library( simecol)
new_date = unique(Rain_2017_since[!is.na(Rain_2017_since$Cumul_since_last)  & Rain_2017_since$Day > 42707,]$Day)
sample_dat = data.frame('Day' = rep(new_date, 12), 'Microplot' = rep(unique(Plant_cov$Microplot), each = 24))
sample_dat$Plants = NA

for (mic in unique(Plant_cov$Microplot)){
  D = Plant_cov[Plant_cov$Microplot == mic,]
  for (v in c( "Plants")){
    ipc <- loess(Plants~Date, D,  span = 0.4)
    pred = predict(ipc, data.frame(Date = new_date), se = TRUE)$fit
    sample_dat[sample_dat$Microplot == mic,'Plants'] = pred
}
}

ggplot(data = sample_dat, aes(x = Day, y = Plants, color = Microplot)) + theme_bw() + geom_line()+
  #geom_smooth(alpha = 0.1, data = pl, aes(x = Date2, y = value, group = Microplot), formula= y ~ poly(x, 1), span = 0.5,size = 0.1) + 
  theme_bw()



Rain_2017_since$Microplot =   gsub('\\.', '', Rain_2017_since$Microplot)
all_dat = merge(sample_dat, Rain_2017_since ,by = c('Microplot', 'Day'))
all_dat$BigSmall = factor(ifelse( all_dat$Cumul_since_last <20.2,"Cumul < 20.2","Cumul > 20.2"))
all_dat$Pl = factor(gsub('[NH]', '', all_dat$Plot))
all_dat$when = factor(ifelse( all_dat$Day <42879,"Beg","End"))
all_dat$Date = as.Date(all_dat$Date,origin = "1900-01-01")
median(all_dat$Day)
ml = lm(sqrt(coef.ruiss) ~Plants*when*pl, data = all_dat)
all_dat$res =NA
all_dat[!is.na(all_dat$Ruiss),]$res = residuals(ml)

mod0 = stepAIC(lme(sqrt(coef.ruiss)~  Plants*Pl+Date, data = all_dat, random = ~1|Microplot, na.action = na.omit, method = 'ML'))
mod =(lme(Plants ~ Pl,  data = all_dat, random = ~1|Microplot, na.action = na.omit, method = 'ML'))
mod =(lme(sqrt(coef.ruiss) ~ when*Plants,  data = all_dat, random = ~1|Microplot, na.action = na.omit, method = 'ML'))

anova(mod0)
r.squaredGLMM(mod)
summary(mod0)
ggplot(all_dat, aes(x=Date, y=sqrt(coef.ruiss), color = Pl)) + geom_smooth(method = lm)+ geom_point() + theme_bw()+
 # annotate('text', y = 0.9, x = 42850, label = 'Plot n.s \n Date ***\nPlot x Date ***\nR2 = %') +
  xlab('Study plot') + ylab('Runoff coefficient (%)') +
  scale_color_discrete(guide=FALSE)+ 
  scale_x_date( date_breaks = "1 month")



summary(mod)
anova(mod)

plot(allEffects(mod0))
plot(mod0)
qqnorm(mod0)

all_dat[all_dat$BigSmall == "Cumul < 20.2", ]$res = residuals(mod0)
summary(lme(res ~Plants, data = all_dat[all_dat$BigSmall == "Cumul < 20.2", ], random = ~1|Microplot))
ggplot(all_dat[all_dat$BigSmall == "Cumul < 20.2", ], aes(y = coef.ruiss, x = Plants, color = Plot)) + geom_point()


##### État de surface ###
All_es <- read.csv("~/Desktop/Projet_M2/Data/Raw_data/Microplot_erosion/All_état_surface.csv", sep=';')
All_es = All_es[,1:30]
All_es$Microplot = as.character(gsub('YR','YRM',All_es$Microplot))

boxplot(All_es$Slope.. ~ All_es$Crop)
kruskal.test(All_es[All_es$Month == 'August',]$Slope.., All_es[All_es$Month == 'August',]$Year)
require(PMCMR)
posthoc.kruskal.nemenyi.test(x=All_es[All_es$Month == 'August',]$Slope.., g=All_es[All_es$Month == 'August',]$Crop, dist="Tukey")

ES_melt = melt(All_es, id.vars = c('Month', 'Month_num', 'Year', 'Plot', 'Crop', 'Microplot'))

##### usefull functions ####
corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

corstarsl(swiss[,1:4])
xtable(corstarsl(swiss[,1:4]))

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +  
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend ,ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

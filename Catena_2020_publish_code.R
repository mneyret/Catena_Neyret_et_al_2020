library(car)
library(cowplot)
library(data.table)
library(dplyr)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(grid)
library(gridExtra)
library(hier.part)
library(Hmisc)
library(lme4)
library(lmerTest)
library(lubridate)
library(MASS)
library(mgsub)
library(multcomp)
library(multcompView)
library(MuMIn)
library(nlme)
library(plyr)
library(readr)
library(readxl)
library(reshape2)
library(stringr)
library(tidyr)
library(tidyverse)
library(mgsub)


Main_color_palette = c('#fdae61','#d7191c','#abdda4','#2b83ba')

#### Input data ####

# This is the runoff and detachment data with rainfall event-specific meteorological data (e.g. total rainfall).
# Biaised measurements (i.e. runoff > 50L or runoff coefficient > 2, see methods in the paper) have already been corrected.
Runoff_data_2015 = read.csv(Runoff_data_2015, "Runoff_data_2015.csv")
Runoff_data_2016 = read.csv(Runoff_data_2016, "Runoff_data_2016.csv")
Runoff_data_2017_18 = read.csv(Runoff_data_2017_18, "Runoff_data_2017_18.csv")

# This is the monthly meteorological data
Meteo_month <- read_delim("Meteo_month.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# This is the description of soil surface characteristics
Surface_charac<- data.frame(read_delim("~/Desktop/Research/Projet_M2/Data/Raw_data/Microplot_erosion/Surface_characteristics.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE))

# This is the raw plant cover data.
Microplot_areas <- read_delim("Microplot_cover_areas.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)






#---------------------------#


#### Plot rainfall and rainfall intensity plots ####
Meteo_month$Date = as.Date(Meteo_month$Date, format = "%m/%d/%y")

Meteo_month$fil = 'Rainfall height'
Meteo_month$fil = factor(Meteo_tot$fil, levels = c('Rainfall height', 'Rainfall erosivity'))
Meteo_month[49,] = NA
Meteo_month[49, c('Date',  'fil')] = c('2015-01-15',  'Rainfall erosivity')
Meteo_month[49, 'Cumul'] = 0.0000001
Meteo_month$col = 'Number of events'
Meteo_month$li = 'Sampling dates'

plot_cumul = ggplot(Meteo_month, aes(y=Cumul, x = Date)) +
  geom_col(fill = "grey80") + 
  scale_color_manual(values = c('black'), breaks = c('Number of events'), labels = c('Number of events > 5 mm'), name = "")  +
  scale_fill_manual(values = c('grey55', 'grey80'), breaks = c('Rainfall height', 'Rainfall erosivity'), 
                    labels = c('Rainfall height (mm)', 'Rainfall erosivity (KJ.mm/m2/h)'), name = "")  +
  scale_linetype_manual(values = c('dashed'), breaks = c('Observation'), labels = c('Surface observation dates'), name = "")  +
  guides(color=guide_legend(title=NULL, nrow = 1), fill=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL,nrow = 1)) +
  theme_bw(base_size = 10) +
  theme(legend.position = 'bottom', legend.box = "horizontal", legend.direction = 'vertical') +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  xlab('') +
  ylab('Rainfall (mm)')+
  geom_point(aes(y=s5mm*10, x=Date, color = col), fill = NA) + 
  geom_line(aes(y=s5mm*10, x=Date, color = col), fill = NA) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = 'Number of\nevents > 5mm')) 

plot_int = ggplot(Meteo_month, aes(y=R, x = Date)) +
  geom_col(fill = 'grey80') +
  theme_bw(base_size = 10) +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  xlab('') + ylab(expression(paste("R ("~10~kJ.mm/m^2/h~")", sep = ''))) 

meteo_leg = get_legend(plot_cumul)
meteo = plot_grid(plot_grid(plot_cumul + theme(legend.position = 'none'),plot_int, ncol=1,
                            align="v", labels = c('(a)', '(b)')),
                  meteo_leg, rel_heights = c(10,2), ncol = 1)

#ggsave('Fig1ab.pdf', meteo, width = 9, height = 6)





#### Effect of land use and rainfall on runoff and soil detachment ####

# Yearly runoff and detachment per microplot
Runoff_data_2015$MinMidHigh = factor(ifelse(Runoff_data_2015$Cumul_corr <= 25, 'Min', ifelse(Runoff_data_2015$Cumul_corr <= 50,'Med','High')), c('Min', 'Med', 'High'))
Runoff_data_2016$MinMidHigh = factor(ifelse(Runoff_data_2016$Cumul_corr <= 25, 'Min', ifelse(Runoff_data_2016$Cumul_corr <= 50,'Med','High')), c('Min', 'Med', 'High'))
Runoff_data_2017_18$MinMidHigh = factor(ifelse(Runoff_data_2017_18$Cumul_corr <= 25, 'Min', ifelse(Runoff_data_2017_18$Cumul_corr <= 50,'Med','High')), c('Min', 'Med', 'High'))

# number of events
with(Runoff_data_2015, tapply(Cumul_corr, list(Microplot, MinMidHigh),function(x){length(x[x>0])}))
with(Runoff_data_2016, tapply(Cumul_corr, list(Microplot, MinMidHigh),function(x){length(x[x>0])}))
with(Runoff_data_2017_18, tapply(Cumul_corr, list( Microplot,Year, MinMidHigh),function(x){length(x[x>0])}))


# This is Table 2 in the manuscript

table_indiv_runoff = round(rbind(
  do.call("cbind", list(
    tapply(Runoff_data_2015$Cumul_corr, list( Runoff_data_2015$Microplot, Runoff_data_2015$MinMidHigh), sum, na.rm = T),
    tapply(Runoff_data_2015$Ruiss, list( Runoff_data_2015$Microplot,Runoff_data_2015$MinMidHigh), sum, na.rm = T),
    tapply(Runoff_data_2015$Tot_Detachment, list( Runoff_data_2015$Microplot, Runoff_data_2015$MinMidHigh), sum, na.rm = T)
  )),
  do.call("cbind", list(
    tapply(Runoff_data_2016$Cumul_corr, list( Runoff_data_2016$Microplot, Runoff_data_2016$MinMidHigh), sum, na.rm = T),
    tapply(Runoff_data_2016$Ruiss, list( Runoff_data_2016$Microplot, Runoff_data_2016$MinMidHigh), sum, na.rm = T),
    tapply(Runoff_data_2016$Tot_Detachment, list( Runoff_data_2016$Microplot, Runoff_data_2016$MinMidHigh), sum, na.rm = T)
  )),
  do.call("cbind", list(
    tapply(Runoff_data_2017_18[Runoff_data_2017_18$Year == '17',]$Cumul_corr, list( Runoff_data_2017_18[Runoff_data_2017_18$Year == '17',]$Microplot, Runoff_data_2017_18[Runoff_data_2017_18$Year == '17',]$MinMidHigh), sum, na.rm = T),
    tapply(Runoff_data_2017_18[Runoff_data_2017_18$Year == '17',]$Ruiss,      list( Runoff_data_2017_18[Runoff_data_2017_18$Year == '17',]$Microplot, Runoff_data_2017_18[Runoff_data_2017_18$Year == '17',]$MinMidHigh), sum, na.rm = T),
    tapply(Runoff_data_2017_18[Runoff_data_2017_18$Year == '17',]$Cumul_corr, list( Runoff_data_2017_18[Runoff_data_2017_18$Year == '17',]$Microplot, Runoff_data_2017_18[Runoff_data_2017_18$Year == '17',]$MinMidHigh), sum, na.rm = T)
  )),
  do.call("cbind", list(
    tapply(Runoff_data_2017_18[Runoff_data_2017_18$Year == '18',]$Cumul_corr, list( Runoff_data_2017_18[Runoff_data_2017_18$Year == '18',]$Microplot, Runoff_data_2017_18[Runoff_data_2017_18$Year == '18',]$MinMidHigh), sum, na.rm = T),
    tapply(Runoff_data_2017_18[Runoff_data_2017_18$Year == '18',]$Ruiss,      list( Runoff_data_2017_18[Runoff_data_2017_18$Year == '18',]$Microplot, Runoff_data_2017_18[Runoff_data_2017_18$Year == '18',]$MinMidHigh), sum, na.rm = T),
    tapply(Runoff_data_2017_18[Runoff_data_2017_18$Year == '18',]$Cumul_corr, list( Runoff_data_2017_18[Runoff_data_2017_18$Year == '18',]$Microplot, Runoff_data_2017_18[Runoff_data_2017_18$Year == '18',]$MinMidHigh), sum, na.rm = T)
  )))
  , 2)


#### Monthly runoff and detachment per microplot ####
# The following code calculates the average monthly runoff coefficient for each microplot, then test for differences in coefficients 
# between the different treatments and within each month.
# Then all figures for runoff and detachment are drawn 

Runoff_data_2015_2 = Runoff_data_2015
Runoff_data_2016_2 = Runoff_data_2016[!is.na(Runoff_data_2016$Month),]
setDT(Runoff_data_2015_2)
setDT(Runoff_data_2016_2)
Monthly_coeff = rbind(
  Runoff_data_2015_2[, list(Year = '2015', Coeff = sum(Ruiss, na.rm = T)/sum(Cumul_corr, na.rm = T) *100, Tot_Detachment= sum(Tot_Detachment, na.rm = T)), list(Month, Microplot)],
  Runoff_data_2016_2[, list(Year = '2016', Coeff = sum(Ruiss, na.rm = T)/sum(Cumul_corr, na.rm = T) *100, Tot_Detachment= sum(Tot_Detachment, na.rm = T)), list(Month, Microplot)])

Monthly_coeff[, Crop := mgsub(Microplot, c('OR[24]', 'YR[MU]', '[1-9]'), c('OR', 'YR', '')) ]
Monthly_coeff[, MonthCrop := factor(paste(Month, Crop))]

Monthly_coeff$MonthCrop = factor(paste(Monthly_coeff$Var2, Monthly_coeff$Crop))
Monthly_coeff$Month = factor(Monthly_coeff$Month)

# Stat model 
Monthly_coeff[, Late := as.numeric(format(Monthly_coeff$Date, '%m')) >= 8]

labels = matrix(ncol = 4)
colnames(labels) = c('Month','Crop', 'label_ruiss', 'label_detach')
  for (month in unique(Monthly_coeff[!(Monthly_coeff$Month %in% c('1512', '1612')),]$Month)){
   D = Monthly_coeff[Monthly_coeff$Month == month,]
   mod1 = lm(Coeff ~ Crop, D)
   mod2 = lm(Tot_Detachment ~ Crop, D)
   Add = cbind('Month' =rep(month, 4), 
              'Crop' = as.character(emmeans::CLD(emmeans(mod1, ~Crop), Letters = LETTERS)[, c( 1)]),
              'label_ruiss' = gsub(' ', '', tolower(emmeans::CLD(emmeans(mod1, ~Crop), Letters = LETTERS)[, c( 7)])),
              'label_detach' = gsub(' ', '', tolower(emmeans::CLD(emmeans(mod2, ~Crop), Letters = LETTERS)[, c( 7)]))
  )
  labels = rbind(labels, Add)
  }

Monthly_coeff_agg = Monthly_coeff[, list(Tot_Detachment = mean(Tot_Detachment, na.rm = T), Coeff = mean(Coeff, na.rm = T), Year = unique(Year)), list(Crop, Month)] 
Monthly_coeff_agg = Monthly_coeff_agg[, Date := as.Date(paste(Month, '01'), "%y%m%d")]
Monthly_coeff_agg$Crop = factor(Monthly_coeff_agg$Crop, levels = c('M', 'YR', 'ORI', 'ORS'))
ruiss_melt = merge(Monthly_coeff_agg, data.frame(labels)[, c("Month", "Crop", 'label_ruiss', 'label_detach')])
ruiss_melt$pos_ruiss =  ruiss_melt$Coeff + 3
ruiss_melt$pos_det=  ruiss_melt$Tot_Detachment + 50
ruiss_melt = ruiss_melt[ruiss_melt$Date %in% c(seq(from = as.Date('2015-05-01'), to = as.Date('2015-12-01'), by = 'month'),
                                               seq(from = as.Date('2016-05-01'), to = as.Date('2016-12-01'), by = 'month')),]

coeff_2016 = ggplot(ruiss_melt[ruiss_melt$Year == "2016",], aes(x = Date, y = Coeff, group = Crop, fill = Crop)) + 
  scale_fill_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'), labels = c('M', 'YR', bquote(OR[i]),  bquote(OR[s])), name = 'Land use') +
  geom_col( position = position_dodge(width = 28), color = NA) + theme_bw() + 
  xlab('Month') +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + ylab('Runoff coefficient (%)') +
  geom_text(aes(y = pos_ruiss, label = label_ruiss, group = Crop, fill = Crop), position = position_dodge(width = 28), color = 'black') +
  theme(legend.position = "bottom", legend.box = 'vertical') +
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.4))

coeff_2015 = ggplot(ruiss_melt[ruiss_melt$Year == "2015",], aes(x = Date, y = Coeff, group = Crop, fill = Crop)) + 
  scale_fill_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'), labels = c('M', 'YR', bquote(OR[i]),  bquote(OR[s])), name = 'Land use') +
  geom_col( position = position_dodge(width = 28), color = NA) + theme_bw() + 
  xlab('Month') +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + ylab('Runoff coefficient (%)') +
  geom_text(aes(y = pos_ruiss, label = label_ruiss, group = Crop, fill = Crop), position = position_dodge(width = 28), color = 'black') +
  theme(legend.position = "bottom", legend.box = 'vertical') +
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.4))

detach_2015 = ggplot(ruiss_melt[ruiss_melt$Year == "2015",], aes(x = Date, y = Tot_Detachment, group = Crop, fill = Crop, color = is.problem)) +
  scale_fill_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'), labels = c('M', 'YR', bquote(OR[i]),  bquote(OR[s])), name = 'Land use') +
  geom_col(position = 'dodge',  color = NA) + theme_bw() + 
  xlab('Month') +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + ylab(expression(Soil~detachment~(g/m^2))) +
  geom_text(aes(y = pos_det, label = label_detach, group = Crop, fill = Crop),position = position_dodge(width = 28), color = 'black') +
  theme(legend.position = "bottom", legend.box = 'vertical') +
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.4))

#ggsave('Fig.2a.pdf',coeff_2015, width = 6, height =5)
#ggsave('Fig.2b.pdf',coeff_2016, width = 6, height =5)
#ggsave('Fig.2c.pdf',detach_2015, width = 6, height =5)




####  Which rainfall parameters influence runoff and detachment ?  ####
# The following code fits model of runoff and detachment with land use type and meteorological data, then plots the corresponding figures.

# This is just an helper function to draw the following plots
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
    scale_fill_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use')+
    scale_shape_manual(values = c(0, 1,15,16), breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use')+
    scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'longdash'), breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use')+
    scale_color_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use')
  #+
  #   annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,
  #            label = paste(lab, ptostar(Anova(model)[,3]), collapse = '\n')) +
  #    annotate(geom ="text", x =Inf, y= -Inf, hjust = 1.1, vjust =-0.5, size = 3,
  #             label = paste('R2 =', round(r.squaredGLMM(model)[2],2)*100, '%'))
  return(P)
}


## 1. The response variable is Runoff
# We keep only raifall height above 2mm and for which all microplots generated runoff
D_16_r = data.frame(Runoff_data_2016[!is.na(Runoff_data_2016$Ruiss) 
                   & Runoff_data_2016$Cumul_since_last_event > 2
                   & Runoff_data_2016$Ruiss > 0
                   ,])

D_16_r$Crop = factor(D_16_r$Crop , c('M', 'YR', 'ORI', 'ORS'))

## Models 


# 1.1 Explanatory variable = Crop

# We chose the best lambda for Box Cox transformation
lambdas = boxCox(D_16_r$Ruiss ~ D_16_r$Crop, data = D_16_r, family = "bcPower", lambda = seq(-1, 1, length = 20))
lambda = round(lambdas[['x']][which(lambdas[['y']] == max(lambdas[['y']]))], 2) 
D_16_r$Ruiss_bc = bcPower(D_16_r$Ruiss, lambda)

mod_crop = lme(Ruiss_bc ~ Crop, random = ~1|Microplot , D_16_r, na.action=na.omit)
cld(emmeans(mod_crop, ~ Crop))
Anova(mod_crop)
r.squaredGLMM(mod_crop)

P_r_crop = ggplot(D_16_r, aes(Ruiss_bc, x = Crop, fill = Crop, shape = Crop, color = Crop)) + 
  theme_bw(base_size = 10)+
  geom_boxplot(outlier.shape = NA, alpha = 0.4) + 
  geom_jitter() + ggtitle(bquote(paste(lambda, ' = ',.(round(lambda, 2))))) +
  scale_fill_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use')+
  scale_shape_manual(values = c(0, 1,15,16), breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use')+
  scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'longdash'), breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use')+
  scale_color_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use') +
  ylab(expression(Transformed~runoff~(L/m^2))) + xlab('Land use') 


# 1.2 Explanatory variable = Meteorological variables. (Commented: use only for supplementary information)
Ruiss_models = matrix(ncol = 4, nrow = 5)
labels = c(expression(Rainfall~(mm))#, expression(API), expression(max~EI30~(kJ.mm/m^2/h)), expression(EC~cum~(kJ/m^2/h))
           , 'Date')
names(labels) = c('Cumul_corr', #'API_eventstart', 'max_EI30_since_last_event', 'Sum_EC_since_last_event', 
                  'Yr_Date')
plotlist_ruiss = list()
i = 0
for (X in c('Cumul_corr', #'API_eventstart', 'max_EI30_since_last_event', 'Sum_EC_since_last_event', 
            'Yr_Date')){
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
  m = lmer(Ruiss_bc~ X*Crop +  (1|Microplot) , D, na.action=na.omit, method = 'ML')
  m2 = get_model(step(m, direction = 'both', trace = 0))
  print(Anova(m2))
  print(r.squaredGLMM(m2))
    tryCatch({
    print(cld(lstrends(m, var ='X', 'Crop')))
  }, error=function(e){'cld'})
  plot(resid(m), fitted(m))
  qqnorm(resid(m)); qqline(resid(m))
  print("lala")
  print( labels[X])
  plotlist_ruiss[[i]] = plot_mod(model = m, D= D, xx= 'X', yy= 'Ruiss_bc', lab_x = labels[X], title = bquote(paste(lambda, ' = ',.(round(lambda, 2)))), lab_y = expression(Transformed~runoff~(L/m^2)), c('Rainfall', 'Crop', 'Rainfall x Crop'))
}

mylegend<-get_legend(P_r_crop)
Ruiss_D16 <- grid.arrange(arrangeGrob(P_r_crop+ theme(legend.position="none"),
                                      plotlist_ruiss[[1]] + theme(legend.position="none"),
                                      plotlist_ruiss[[2]] + theme(legend.position="none"),
                                    # plotlist_ruiss[[3]] + theme(legend.position="none"),
                                    # plotlist_ruiss[[4]] + theme(legend.position="none"),
                                    # plotlist_ruiss[[5]] + theme(legend.position="none"),
                                      
                                      nrow=1, ncol = 3),
                          mylegend, ncol=2, widths=c(10, 1))
#ggsave('Fig3a.pdf',Ruiss_D16, width = 8, height = 3)


## 2. Response variable = Detachment 
# We keep only data for which rainfall height >2mm, and detachment >0
D_16_d = data.frame(Runoff_data_2016[!is.na(Runoff_data_2016$Ruiss) 
                   & Runoff_data_2016$Cumul_since_last_event >2
                   & !is.na(Runoff_data_2016$Detachment)
                   & Runoff_data_2016$Detachment >0,])
D_16_d$Crop = factor(D_16_d$Crop, levels = c('M', 'YR', 'ORI', 'ORS'))


### 2.1 Explanatory variable = Crop
lambdas =boxCox(D_16_d$Detachment ~ D_16_d$Crop, data = D_16_d, family = "bcPower", lambda = seq(-1, 1, length = 20))
lambda= round(lambdas[['x']][which(lambdas[['y']] == max(lambdas[['y']]))], 2) 
D_16_d$Detachment_bc = bcPower(D_16_d$Detachment, lambda)

mod_crop = lme(Detachment_bc ~ Crop, random = ~1|Microplot , D_16_d, na.action=na.omit)
cld(emmeans(mod_crop, ~ Crop))
Anova(mod_crop)
r.squaredGLMM(mod_crop)

P_det_crop = ggplot(D_16_d, aes(Detachment_bc, x = Crop, shape = Crop,color = Crop, fill = Crop)) + theme_bw(base_size = 10)+
  geom_boxplot(outlier.shape = NA, alpha = 0.3) + geom_jitter() + ggtitle(bquote(paste(lambda, ' = ',.(round(lambda, 2)))))+
  scale_fill_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use')+
  scale_shape_manual(values = c(0, 1,15,16), breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use')+
  scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'longdash'), breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use')+
  scale_color_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'), name = 'Land use') +
  ylab(expression(Transformed~detachment~(g/m^2))) + xlab('Land use') #+
annotate(geom ="text", x =seq(1:4), y= c(2.5, 2.5, 4,4), size = 8,
         label = c('a', 'a', 'b', 'b'))


### 2.2 Explanatory variable = Meteorological variables
Det_models = matrix(ncol = 4, nrow = 5)
labels = c(expression(Rainfall~(mm)),# expression(API), expression(max~EI30~(kJ.mm/m^2/h)), expression(EC~cum~(kJ/m^2/h)),
           'Date')
names(labels) = c('Cumul_since_last_event',# 'API_eventstart', 'max_EI30_since_last_event', 'Sum_EC_since_last_event', 
                  'Yr_Date')
plotlist_det = list()
i = 0
for (X in c('Cumul_since_last_event', #'API_eventstart', 'max_EI30_since_last_event', 'Sum_EC_since_last_event', 
            'Yr_Date')){
  i = i+1
  print('------------------------------')
  print(X)
  D = D_16_d
  D$X = D[, X]
  #lambda = 1
  lambdas =boxCox(D$Detachment ~ D$X*D$Crop, data = D, family = "bcPower", lambda = seq(-1, 1, length = 20))
  lambda= round(lambdas[['x']][which(lambdas[['y']] == max(lambdas[['y']]))], 2) 
  lambdaconfint = round(c(min( lambdas$x[lambdas$y > max(lambdas$y) - 1/2 * qchisq(.95,1)]),
                          max( lambdas$x[lambdas$y > max(lambdas$y) - 1/2 * qchisq(.95,1)])),2)
  D$Detachment_bc = bcPower(D$Detachment, lambda)
  m = lmer(Detachment_bc~ X*Crop + (1|Microplot) ,D, na.action=na.omit, method = 'ML')
  m2 = get_model(step(m, direction = 'backward', trace = 0))
  print(Anova(m2))
  print(r.squaredGLMM(m2))
  Det_models[i,] = c(X, as.character(m2$call)[2], round(r.squaredGLMM(m2)[2], 3),
                     paste(c(lambda, ' (', lambdaconfint[1],' ', lambdaconfint[2], ')' ), collapse = ''))

  print("lala")
  plotlist_det[[i]] = plot_mod(m, D, 'X', 'Detachment_bc', labels[X], title = bquote(paste(lambda, ' = ',.(round(lambda, 2)))), expression(Transformed~detachment~(g/m^2)), c('Rainfall', 'Crop', 'Rainfall x Crop'))
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-get_legend(P_r_crop)
Det_D16 <- grid.arrange(arrangeGrob(  P_det_crop+ theme(legend.position="none"),
                                      plotlist_det[[1]] + theme(legend.position="none"),
                                       plotlist_det[[2]] + theme(legend.position="none"),
                                      # plotlist_det[[3]] + theme(legend.position="none"),
                                      # plotlist_det[[4]] + theme(legend.position="none"),
                                     # plotlist_det[[5]] + theme(legend.position="none"),
                                      
                                      nrow=1, ncol = 3),
                        mylegend, ncol=2, widths=c(10, 1))

#ggsave('Fig3b.pdf',Det_D16, width = 8, height = 3)



#### Relationship between runoff and detachment ####
# The following code analyses and plots the relationship between (log-transformed) runoff and (log-transformed) soil detachment in 2016

Runoff_data_2016$lRu = log(Runoff_data_2016$Ruiss)
Runoff_data_2016$lDet = log(Runoff_data_2016$Detachment)

# Difference between rubber tree plantation and other land-use types
Runoff_data_2016$isOR = factor(ifelse(Runoff_data_2016$Crop %in% c('ORI', 'ORS'), 'OR', 'YR  M'), levels = c('YR  M', 'OR'))

mod_ruiss_detll = lmer(lDet~ log(Ruiss)*Crop + (1|Microplot), data = Runoff_data_2016[Runoff_data_2016$Detachment > 0 & Runoff_data_2016$Ruiss >0,])
mod = lmer(lDet~ lRu*isOR + (1|Microplot), data = Runoff_data_2016[Runoff_data_2016$Detachment > 0 & Runoff_data_2016$Ruiss >0,])
cld(emtrends(mod, var =  'lRu', ~isOR, data = Runoff_data_2016[Runoff_data_2016$Detachment > 0 & Runoff_data_2016$Ruiss >0 ,]))
ggdat = ggpredict(mod_ruiss_detll, c('Ruiss [exp]', 'Crop'))
ggdat$lDet = exp(ggdat$predicted)
ggdat$lRu = ggdat$x

ggdat$Det_high = exp(ggdat$conf.high)
ggdat$Det_low = exp(ggdat$conf.low)
ggdat$Crop = factor(ggdat$group, levels = c('M', 'YR', 'ORI', 'ORS'))
ggdat$isOR = factor(ifelse(ggdat$Crop %in% c('ORI', 'ORS'), 'OR', 'YR  M'), levels = c('YR  M', 'OR'))
ggdat = ggdat[!(ggdat$isOR != 'OR' & ggdat$x > 20),]

RDplot = ggplot(Runoff_data_2016[!is.na(Runoff_data_2016$Crop) & Runoff_data_2016$Detachment > 0,], 
                aes(Ruiss, Detachment,  color = Crop , shape = Crop)) + facet_wrap(~isOR) +
  theme_bw(base_size = 14) +
  geom_point(alpha = 0.8) + theme_bw(base_size = 14) +
  ylab(expression(Soil~detachment~(g/m^2))) + xlab(expression(Runoff~(L/m^2))) +
  scale_fill_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'), labels = c('M', 'YR',bquote(OR[I]), bquote(OR[S])), name = 'Land use')+
  scale_shape_manual(values = c(0, 1,15,16), breaks = c('M', 'YR', 'ORI', 'ORS'),labels = c('M', 'YR',bquote(OR[I]), bquote(OR[S])), name = 'Land use')+
  scale_color_manual(values = Main_color_palette, breaks = c('M', 'YR', 'ORI', 'ORS'),labels = c('M', 'YR',bquote(OR[I]), bquote(OR[S])), name = 'Land use') +
  geom_ribbon(data = ggdat, aes(x = lRu, y = lDet, ymin = Det_low, ymax = Det_high, group  = Crop, fill = Crop ), inherit.aes = FALSE, alpha = 0.3 ) 

#ggsave('Fig4.pdf', RDplot, width = 6, height = 4)





##### Effect of soil surface characteristics ##### 
# The following code uses data on soil surface characteristics

Surface_charac$Tot_crust = Surface_charac$ST + Surface_charac$ERO +Surface_charac$G 
Surface_charac$CropOR = factor(ifelse(Surface_charac$Crop %in% c('M', 'YR'), 'noOR', 'OR'))

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

## First we plot the descriptive figure of the proportion of all surface attributes ##
{
  # Bar plots surface
  { 
    Surface_charac_melt_surface = melt(Surface_charac[, (colnames(Surface_charac) %in% c( 'Runoff_coeff','Month_plot', 'Fag' , 'Microplot','FG','ST','ERO','G','Charcoals','Residues','Month','Year', 'Plot', 'Crop'))],
                               id.vars = c('Microplot', 'Month','Month_plot', 'Year', 'Plot', 'Microplot', 'Crop'))
    
    Surface_charac_melt_surface = aggregate(Surface_charac_melt_surface$value, 
                                    by = list(Surface_charac_melt_surface$Microplot, Surface_charac_melt_surface$variable,  Surface_charac_melt_surface$Month_plot, Surface_charac_melt_surface$Month, Surface_charac_melt_surface$Year, Surface_charac_melt_surface$Plot, Surface_charac_melt_surface$Crop), mean)
    colnames(Surface_charac_melt_surface) = c('Microplot', 'variable','Month_plot', 'Month', 'Year', 'Plot', 'Crop', 'value')
    Surface_charac_melt_surface$Crop2 = gsub('[1-9]', '', Surface_charac_melt_surface$Plot)
    Surface_charac_melt_surface$Crop2 = factor(gsub('YRM', 'YR', Surface_charac_melt_surface$Crop2), levels = c('M', 'YR', 'ORI', 'ORS', 'ORH', 'ORNH'))
    Surface_charac_melt_surface$variable = factor(Surface_charac_melt_surface$variable, levels = c('Residues', 'Fag', 'FG', 'ST', 'ERO', 'G') )
    
    surface_palette = c('#99d594',  '#91bfdb', '#e0f3f8','#fee08b', '#fc8d59', '#d73027')
    
    Surface_charac_melt_surface_1516 = Surface_charac_melt_surface[Surface_charac_melt_surface$Year %in% c( 2015, 2016) & Surface_charac_melt_surface$variable != 'Charcoals',]
    Surface_charac_melt_surface_1516 = Surface_charac_melt_surface_1516[!is.na(Surface_charac_melt_surface_1516$Year),]
    Surface_charac_melt_surface_1516$Microplot_num =     substrRight(Surface_charac_melt_surface_1516$Microplot, 1)

    
    Surface_15_16 = ggplot(Surface_charac_melt_surface_1516
                           , aes(x = Microplot_num, y= value, fill = variable, color = variable )) + 
      geom_col() + facet_grid( Crop ~  Year + Month_plot) +
      scale_color_manual(values = surface_palette, guide = FALSE) +
      theme_bw(base_size = 14) +  ylab('Proportion (%)') + xlab('Month') +
      scale_fill_manual(values = surface_palette, breaks = c('Residues', 'Fag', 'FG', 'ST', 'ERO', 'G'),
                        labels = c( 'Crop residues', 'Free aggregates', 'Free gravels', 'Structural crust', 
                                    'Erosion crust', 'Gravel crust'), name = '')
    
    Surface_charac_melt_surface_1718 = Surface_charac_melt_surface[Surface_charac_melt_surface$Year %in% c( 2017, 2018) & Surface_charac_melt_surface$variable != 'Charcoals',]
    Surface_charac_melt_surface_1718 = Surface_charac_melt_surface_1718[!is.na(Surface_charac_melt_surface_1718$Year) & !is.na(Surface_charac_melt_surface_1718$Crop2) ,]
    
    Surface_charac_melt_surface_1718$Microplot_num = gsub('OR[24]*','' , Surface_charac_melt_surface_1718$Microplot)
    Surface_charac_melt_surface_1718$Microplot_num = gsub('N*H','' , Surface_charac_melt_surface_1718$Microplot_num)
    
    Surface_17_18 = ggplot(Surface_charac_melt_surface_1718
                           , aes(x = Microplot_num, y= value, fill = variable, color = variable )) + 
      geom_col() + facet_grid( Plot ~  Year + Month_plot ) +
      scale_color_manual(values = surface_palette, guide = FALSE) +
      theme_bw(base_size = 14) +  ylab('Proportion (%)') + xlab('Microplot repetition') +
      scale_fill_manual(values = surface_palette, breaks = c('Residues', 'Fag', 'FG', 'ST', 'ERO', 'G'),
                        labels = c( 'Crop residues', 'Free aggregates', 'Free gravels', 'Structural crust', 
                                    'Erosion crust', 'Gravel crust'), name = '')
  }
}

#ggsave('Fig5a.pdf',cropcover,  width = 8, height = 6)
#ggsave('Fig5b.pdf',Surface_17_18,  width = 8, height = 6)
  

## Now we compare the relationship between runoff coefficient and soil surface depending on the time (early or late) in the rainy season ##
  
  setDT(Runoff_data_2015); setDT(Runoff_data_2016); setDT(Runoff_data_2017_18)
  
  late_early_coeff =  rbind(
    Runoff_data_2015[, list(Year = "2015", Coeff = 100*sum(Ruiss, na.rm = T)/ sum(Cumul_corr, na.rm = T)), list(Microplot, Late = Day > "2015-08-15")],
    Runoff_data_2016[, list(Year = "2016", Coeff = 100*sum(Ruiss, na.rm = T)/ sum(Cumul_corr, na.rm = T)), list(Microplot, Late = Day > "2016-08-15")],
    Runoff_data_2017_18[Runoff_data_2017_18$Year == "17", list(Year = "2017", Coeff = 100*sum(Ruiss, na.rm = T)/ sum(Cumul_corr, na.rm = T)), list(Microplot, Late = Start > "2017-08-15")],
    Runoff_data_2017_18[Runoff_data_2017_18$Year == "18", list(Year = "2018", Coeff = 100*sum(Ruiss, na.rm = T)/ sum(Cumul_corr, na.rm = T)), list(Microplot, Late = Start > "2018-08-15")])
  
  
Surface_charac2 = Surface_charac
Surface_charac2[Surface_charac2$Year == "2016" & Surface_charac2$Month == 'August' & !is.na( Surface_charac2$Month), c('Fag' ,'FG','ST','ERO','G','Tot_crust','Charcoals','Residues')] =
     (Surface_charac2[Surface_charac2$Year == "2016" & Surface_charac2$Month == 'August' & !is.na( Surface_charac2$Month), c('Fag' ,'FG','ST','ERO','G','Tot_crust','Charcoals','Residues')] + Surface_charac2[Surface_charac2$Year == "2016" & Surface_charac2$Month == 'Oct' & !is.na( Surface_charac2$Month), c('Fag' ,'FG','ST','ERO','G','Tot_crust','Charcoals','Residues')]) / 2

Surface_charac2 = Surface_charac[!is.na(Surface_charac$Late),]


Surface_charac_melt_surface = melt(Surface_charac2[, (colnames(Surface_charac2) %in% c( 'Late', 'Semi.annual.coeff','Fag' ,'FG','ST','ERO','G','Tot_crust','Charcoals','Residues','Month','Year', 'Plot', 'Microplot', 'Crop'))],
                           id.vars = c('Month','Year', 'Plot', 'Microplot', 'Crop','Late', 'Semi.annual.coeff' ))

Surface_charac_cast = dcast(Surface_charac_melt_surface, Month +Year+  Plot+ Crop+ Late + Semi.annual.coeff + Microplot~ variable, mean)
Surface_charac_cast$Crop = factor(Surface_charac_cast$Crop, levels = c('M', 'YR', 'ORI', 'ORS', 'OR'))
Surface_charac_cast$CropOR = factor(ifelse(Surface_charac_cast$Crop %in%  c('M', 'YR'), 'noOR', 'OR'))
Surface_charac_cast$Late = as.factor(Surface_charac_cast$Late)

# Runoff and crust
mod = get_model(step(lmer(Semi.annual.coeff ~ Tot_crust*Late+ (1|Microplot) , Surface_charac_cast[!(Surface_charac_cast$Crop %in% c('M', 'YR')),])))
mod = get_model(step(lmer(Semi.annual.coeff ~ Residues*Late+ (1|Microplot)  , Surface_charac_cast[!(Surface_charac_cast$Crop %in% c('M', 'YR')),])))

qqnorm(residuals(mod)); qqline(residuals(mod))
Anova(mod)
summary(mod)
emtrends(mod, ~   Late, var = 'Residues')
plot(ggeffect(mod, c('Residues',  'Late')), rawdata = TRUE) 

Surface_charac_cast$Crop2 = factor(ifelse(Surface_charac_cast$CropOR == 'OR', 'OR', as.character(Surface_charac_cast$Crop)), levels = c('M', 'YR','OR'))

Coeff_residues = ggplot(Surface_charac_cast, aes(y = Semi.annual.coeff, x = Tot_crust, colour = Crop2, fill = Crop2, shape = factor(Year), group = CropOR)) +
  xlab('% Crust') + ylab('Runoff coefficient (%)') +
  facet_wrap(~ Late) +
  geom_smooth(method = 'lm', color = 'black', fill = "black", alpha = 0.4) + 
  geom_point(data =Surface_charac_cast, aes(colour = Crop2, fill = Crop2), size = 0.8) +
  scale_color_manual(values =  Main_color_palette[c(1, 2, 4)], breaks = c('M', 'YR', 'OR'), name = 'Land use') +
  scale_fill_manual(values = Main_color_palette[c(1, 2, 4)], breaks = c('M', 'YR', 'OR'), name = 'Land use') +
  theme_bw() +
  theme(legend.position = 'none')

#ggsave(Coeff_residues, file = 'FigS2.pdf', width = 4, height = 2.5)


##### Effect of plant cover ##### 
# The following code uses data on plant cver of all microplots in 2017-2018 to estimate the change in runoff due to plant cover

Microplot_areas[!is.na(Microplot_areas$ok),]$Area = NA
Microplot_areas$Microplot = gsub(' ', '', Microplot_areas$Microplot)
Microplot_areas$Date = as.Date(Microplot_areas$Date, format = c('%d/%m/%y') )
Microplot_areas$Date_num = as.numeric(Microplot_areas$Date)
# We separate it in different time periods between the herbicides application to have a better fit
Microplot_areas$timepart = ifelse(Microplot_areas$Date < as.Date("2017-06-15"), 'a', 
                                  ifelse(Microplot_areas$Date < as.Date("2017-08-21"), 'b', 
                                         ifelse(Microplot_areas$Date < as.Date("2018-05-29"), 'c',
                                                ifelse(Microplot_areas$Date < as.Date("2018-09-29"),'d', 'e'))))
# Predict new values
Pred_areas = Microplot_areas %>%
  nest( - Microplot, -timepart) %>%
  mutate(mods = map(data, ~ loess(Area ~Date_num, data = ., span = 0.6)), 
         preds = map(mods, predict, newdata = data.frame(Date_num = 
                                                           seq(as.numeric(as.Date("2017-01-01")),as.numeric(as.Date("2019-01-01")),1))))  %>% #, 

  unnest(preds) 
Pred_areas[!is.na(Pred_areas$preds) & Pred_areas$preds < 0,]$preds = 0
Pred_areas$Date_num = rep(seq(as.numeric(as.Date("2017-01-01")),as.numeric(as.Date("2019-01-01")),1), 12*5)
Pred_areas$Date = as.Date(Pred_areas$Date_num, "1970-01-01")
Runoff_data_melt$Microplot = as.character(Runoff_data_melt$Microplot) 


# Now we merge obtained data with microplot runoff data
All_data = merge(Runoff_data_2017_18, Pred_areas[,c('Microplot', 'Date_num','preds')], by = c('Microplot', 'Date_num'))

All_data$Plot = factor(All_data$Plot)
All_data$Date = as.Date(All_data$Date_num, "1970-01-01")
All_data$Coeff = All_data$Ruiss/All_data$Cumul_corr
All_data$MonthYear = factor(format(as.Date(All_data$Date_num, "1970-01-01"), '15%b%y'), levels = c("15Jan17","15Feb17", "15Mar17", "15Apr17", "15May17", "15Jun17", "15Jul17", "15Aug17", "15Sep17" ,"15Oct17",
                                                                                                   "15Nov17", "15Dec17", "15Jan18", "15Feb18", "15Mar18", "15Apr18", "15May18", "15Jun18", "15Jul18","15Aug18", "15Sep18", "15Oct18", "15Nov18", "15Dec18", "15Jan19"))

All_data$sqrtPluie = sqrt(All_data$Cumul_corr)
All_data$sqrtRuiss = sqrt(All_data$Ruiss)

# We separate the soil cover into three classes based on 30% quantile distribution
All_data$coverclass = factor(cut(All_data$preds, c(0, 2.9 , 31.2, 100), labels = c('low', 'med', 'high')), levels = c('low', 'med', 'high'))

# We run the models only on data with cumul larger than 0 and between May and November (ie. rainy season)
Data_use = All_data[!is.na(All_data$Ruiss) & !is.na(All_data$preds) & !is.na(All_data$Cumul_corr) &  All_data$Cumul_corr > 0 & All_data$MonthYear %in% c('15May17', '15Jun17', '15Jul17', '15Aug17', '15Sep17','15Oct17', '15Nov17', '15May18', '15Jun18', '15Jul18', '15Aug18', '15Sep18', '15Oct18', '15Nov18'),]

mod_cover = lmer(sqrtRuiss~ coverclass*sqrtPluie*Plot  + (1|Microplot) + (1|MonthYear), Data_use, REML = FALSE)
Anova(mod_cover)
cld(emmeans(mod_cover, ~ coverclass , data = Data_use))

#### Plot runoff rainfall | cover
GG1 = data.frame(ggeffect(mod_cover, c('sqrtPluie','coverclass', 'Plot')))
GG1$sqrtPluie = GG1$x
GG1$sqrtRuiss = GG1$predicted
GG1$coverclass = factor(GG1$group, levels = c('low', 'med', 'high'))
GG1$Plot= GG1$facet
Data_use$sqrtCoeff = sqrt(Data_use$Coeff)

plot_runoff_rainfall = ggplot(Data_use, 
                              aes(sqrtRuiss, x = sqrtPluie, color = factor(coverclass))) + theme_bw() +
  geom_point()  + facet_wrap(~Plot) +
  geom_ribbon(data = GG1, aes(ymin = conf.low, ymax = conf.high, x = sqrtPluie, fill = factor(coverclass)), inherit.aes = FALSE, alpha = 0.2, color = NA) +
  geom_line(data = GG1, aes(y = sqrtRuiss, x = sqrtPluie, color = factor(coverclass)), inherit.aes = FALSE, lwd = 1.2)  +
  xlab(expression(sqrt(Rainfall~height~(mm)))) +
  ylab(expression(sqrt(Runoff))) + theme(legend.position = 'bottom', legend.title.align = 0.5) +
  guides(colour = guide_legend(title.position = "top"))  +
  scale_color_manual(values = c("#7fcdbb","#1d91c0","#0c2c84"), breaks = c('low', 'med','high'), labels = c("0-3", "3-31",">31"), name = "% cover by plants") +
  scale_fill_manual(values = c("#7fcdbb","#1d91c0","#0c2c84"),  breaks = c('low', 'med','high'), labels = c("0-3", "3-31",">31"),name = "% cover by plants") 
#ggsave(plot = plot_runoff_rainfall, file = 'Fig9.pdf',width = 6, height = 4)



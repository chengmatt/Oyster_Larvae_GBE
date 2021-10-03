#Creator: Matthew LH. Cheng
#Purpose: for creating figures and analyses for stasse et al. 2021
#Date created: 7/26/2021
#Date updated: 10/3/2021

#load in libraries needed
library(tidyverse)
library(patchwork)
library(lubridate)
library(here)
library(lemon)

# cleaning and setting up data physchem 2018 - 2020 --------------------------------------------
#load in physchem data
data_1<-read_csv(here("Data", "Physchem_GBE_2018-2020", "GRBGBWQ.csv"), skip = 2)#skipping first two rows 

#only selecting 1:10 character strings to remove time stamps and to only keep dates
data_1$DateTimeStamp<-substr(data_1$DateTimeStamp, 1,10)

#force dates into lubridate format
data_1$DateTimeStamp<-mdy(data_1$DateTimeStamp)

#filter physchem to time frame of when 2020 sampling was done and create new object with 2018 data
data_2018<-data_1 %>% 
  dplyr::select(DateTimeStamp, Temp, Sal, pH) %>% 
  rename(Date = DateTimeStamp,
         Salinity = Sal) %>% 
  filter(Date >= "2018-06-09" & Date <= "2018-11-05")

#Do the same for 2019 and 2020

#data for 2019
data_2019<-data_1 %>% 
  dplyr::select(DateTimeStamp, Temp, Sal, pH ) %>% 
  rename(Date = DateTimeStamp,
         Salinity = Sal) %>% 
  filter(Date >= "2019-06-09" & Date <= "2019-11-05")

#data for 2020
data_2020<-data_1 %>% 
  dplyr::select(DateTimeStamp, Temp, Sal, pH ) %>% 
  rename(Date = DateTimeStamp,
         Salinity = Sal) %>% 
  filter(Date >= "2020-06-09" & Date <= "2020-11-05")

#join these three datasets together into one common dataset
phys_data<-full_join(data_2018, data_2019) %>% 
  full_join(.,data_2020)

#Group and average by dates because values were taken every several hours
#Drop any missing values
phys_data<-phys_data %>% 
  group_by(Date) %>% 
  drop_na() %>% 
  summarize(Temp = mean(Temp),
            Salinity = mean(Salinity),
            pH = mean(pH))


# Figure 2 - Physchem -----------------------------------------------------
#plots show physchem data over time
#Write function so it forces all of these dates to be the same year (AKA same x scale) for nice plotting
same_year <- function(x) {
  year(x) <- 2000
  return(x)
}

#Temperature plots
a<-ggplot(phys_data, aes(x = same_year(Date),y = Temp, group = as.factor(year(Date))))+
  geom_line(aes(linetype = as.factor(year(Date))), size = 1.1)+
  theme_classic()+
  annotate(geom="text",x=as.Date("2000-06-10"),
           y=Inf,label="A",fontface="bold", size = 8, vjust = 2, hjust = 2)+
  labs(x = "",y = "Temperature (°C)", linetype = "Year")+
  theme(axis.title = element_text(size = 19.5), axis.text.x = element_blank(), legend.text = element_text(size = 17, color = 'black'), legend.title = element_text(size = 17, color = 'black'), legend.position = c(0.93,0.74), axis.ticks = element_line (colour = "black", size = 1),  axis.line = element_line(color = "black", size = 1),legend.key.width=unit(2.3,"cm"),axis.text.y = element_text(size = 18, color = 'black'))

#pH plots
b<-ggplot(phys_data, aes(x = same_year(Date),y = pH, group = as.factor(year(Date))))+
  geom_line(aes(linetype = as.factor(year(Date))), size = 1.1)+
  theme_classic()+
  annotate(geom="text",x=as.Date("2000-06-10"),
           y=Inf,label="B",fontface="bold", size = 8, vjust = 2,hjust = 2)+
  labs(x = "",y = "pH")+
  theme(axis.title = element_text(size = 19.5), axis.text.x = element_blank(), legend.position = 'none', axis.ticks = element_line (colour = "black", size = 1),  axis.line = element_line(color = "black", size = 1),axis.text.y = element_text(size = 18, color = 'black'))

#Salinity plots
c<-ggplot(phys_data, aes(x = same_year(Date),y = Salinity, group = as.factor(year(Date))))+
  geom_line(aes(linetype = as.factor(year(Date))), size = 1.1)+
  theme_classic()+
  annotate(geom="text",x=as.Date("2000-06-10"),
           y=Inf,label="C",fontface="bold", size = 8, vjust = 2,hjust = 2)+
  labs(x = "Month",y = "Salinity")+
  theme(axis.title = element_text(size = 19.5, color = 'black'), axis.text = element_text(size = 18, color = 'black'), legend.position = 'none', axis.ticks = element_line (colour = "black", size = 1),  axis.line = element_line(color = "black", size = 1))

#combine plots and save to figures folder
a / b / c 
ggsave(path = here("Figures"), filename = "fig2a,2b,2c.tiff", dpi = 500)


# Figure 3 ----------------------------------------------------------------
#plots show larval densities over time and by site
#read in oyster data (3 separate files)
dat_2018<-read_csv(here("Data", "Oyster_Counts", "OneData.csv"))#2018 data
dat_2019<-read_csv(here("Data", "Oyster_Counts", "TwoData.csv"))#2019 data
dat_2020<-read_csv(here("Data", "Oyster_Counts", "ThreeData.csv"))#2020 data

#combine and join datasets
dat_all<-full_join(dat_2018, dat_2019) %>% 
  full_join(.,dat_2020)

#force dates to lubridate format for plotting
dat_all$Date<-mdy(dat_all$Date)

#make a year column for all dates and Month Days (for plotting purposes)
dat_all<-dat_all %>% 
  mutate(Year = year(Date),
         Date_same_year = same_year(Date)) 

#make text annotations for plotting
#make annotations
dat_text <- data.frame(
  label = c("2018", "2019", "2020"),
  Year = c("2018", "2019", "2020"))

#Dhinge plot
d<-dat_all %>% 
  ggplot(.,aes(x = Date_same_year, y = D))+
  geom_line(aes(linetype = Site), size = 0.75)+
  geom_text(dat_text, mapping = aes(x = as.Date("2000-06-25"), y = Inf, label = label), vjust = 1.75, size = 8)+
  geom_errorbar(aes(ymin = D-D_SE, ymax = D+D_SE), size = 0.75, width = 1.75, alpha = 0.8)+
  facet_rep_wrap(~Year, ncol  = 1)+
  theme_classic()+
  labs(x = "Month",y = bquote("Average D-hinge larval count"~m^-3~''))+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20, color = 'black'),  legend.position = 'none', axis.ticks = element_line (colour = "black", size = 1),  axis.line = element_line(color = "black"), strip.text.x = element_blank(), strip.background = element_blank(),
        axis.title.x = element_text(vjust = -0.1))+
  scale_x_date(date_labels = "%b", limits = c(as.Date("2000-06-09"), as.Date("2000-11-05")))+
  scale_y_continuous(limits = c(0, 30000))

#Veliger plot
v<-dat_all %>% 
  ggplot(.,aes(x = Date_same_year, y = V))+
  geom_line(aes(linetype = Site),size = 0.75)+
  geom_text(dat_text, mapping = aes(x = as.Date("2000-06-25"), y = Inf, label = label), vjust = 1.75, size = 8)+
  geom_errorbar(aes(ymin = V-V_SE, ymax = V+V_SE), size = 0.75, width = 1.75, alpha = 0.8)+
  facet_rep_wrap(~Year, ncol  = 1)+
  theme_classic()+
  labs(x = "Month",y = bquote("Average veliger larval count"~m^-3~''))+
  theme(axis.title = element_text(size = 25), axis.text = element_text(size = 20, color = 'black'), legend.text = element_text(size = 17, color = 'black'), legend.title = element_text(size = 18, color = 'black'), legend.position = c(0.8,0.875), axis.ticks = element_line (colour = "black", size = 1),  axis.line = element_line(color = "black"),legend.key.width=unit(2.3,"cm"), legend.background = element_rect(fill = 'NA'), strip.text.x = element_blank(), strip.background = element_blank(),
        axis.title.x = element_text(vjust = -0.1))+
  scale_y_continuous(limits=c(0,15))+
  scale_x_date(date_labels = "%b", limits = c(as.Date("2000-06-09"), as.Date("2000-11-05")))

#plot tobether Dhinge and Veliger
d | v 

#save figure
ggsave(path = here("Figures"), filename = "fig_3.tiff", dpi = 500 )


# Figure 4 ----------------------------------------------------------------
#Plots of boxplots compared across years

#ANOVA for D hinge and Year
DYear<-aov(log(D+0.05)~as.factor(Year), data = dat_all)
summary(DYear)
TukeyHSD(DYear)

#ANOVA for Veliger  and Year
VYear<-aov(log(V+0.05)~as.factor(Year), data = dat_all)
summary(VYear)

TukeyHSD(VYear)



# Figure 4 Analysis -------------------------------------------------------

#Dhinge Plot
a<-dat_all %>% 
  ggplot(.,aes(x = factor(Year), y = D))+
  geom_boxplot(width = 0.7)+
  geom_jitter(width = 0.25, size = 2, alpha = 0.75)+
  annotate('text', x = 1, y = 12000, label = "a", size = 6)+
  annotate('text', x = 2, y = 3800, label = "ab", size = 6)+
  annotate('text', x = 3, y = 3800, label = "b", size = 6)+
  annotate('text', x = 0.65, y = Inf, label = "A", size = 7, vjust = 3, fontface = 'bold')+
  
  theme_classic()+
  labs(x = 'Year', y = bquote("D-hinge larval count"~m^-3~""))+
  theme(axis.title = element_text(size = 17), 
        axis.text = element_text(size = 15, color = 'black'), axis.ticks = element_line (colour = "black", size = 1),  axis.line = element_line(color = "black", size = 1))

#Veliger plot
b<-dat_all %>% 
  ggplot(.,aes(x = factor(Year), y = V))+
  geom_boxplot(width = 0.7)+
  geom_jitter(width = 0.25, size = 2, alpha = 0.75)+
  annotate('text', x = 1, y = 2, label = "a", size = 6)+
  annotate('text', x = 2, y = 1, label = "b", size = 6)+
  annotate('text', x = 3, y = 3, label = "a", size = 6)+
  annotate('text', x = 0.65, y = Inf, label = "B", size = 7, vjust = 3, fontface = 'bold')+
  
  theme_classic()+
  labs(x = 'Year', y = bquote("Veliger larval count"~m^-3~""))+
  theme(axis.title = element_text(size = 17), 
        axis.text = element_text(size = 15, color = 'black'), axis.ticks = element_line (colour = "black", size = 1),  axis.line = element_line(color = "black", size = 1))
#Plot both
a |b 
#save figure
ggsave(path = here("Figures"), filename = "fig_4.tiff", dpi = 500)

# Figure 5  -------------------------------------------------------
#fitting linear model
#load in data for all aggregated
data<-read_csv(here("Data","Oyster_counts","Combined_All.csv"))

#summarize dat all for lm model
dat_all<-dat_all %>% 
  select(Date, D, V) %>% 
  group_by(Date) %>% 
  summarize(D = mean(D),
            V = mean(V))
#join physchem data to dat_all dataset
#make dates to lubridate format
data$Date<-mdy(data$Date)
#joining data and selecting col
physchem<-data %>% 
  select(Date,Temp, Salinity, pH, Year)
#join data
dat_all<-full_join(dat_all, physchem)

#make prediciton fr mod3
mod3<-lm(log(D+0.01)~Temp, data = dat_all,na.action = "na.fail")
#predicitons
predictions<-predict(mod3, interval = 'confidence', level = 0.95)
#bind predicitons
new_pred<-cbind(predictions, data)
#plot
new_pred %>% 
  ggplot(.,aes(x = Temp, y = exp(fit)-0.01))+
  geom_line(aes(y= exp(fit)-0.01),size = 1.5 )+
  geom_point(aes(y = Dhinge), alpha = 0.75, size = 2)+
  geom_ribbon(aes(ymin = exp(lwr)-0.01, ymax = exp(upr)-0.01),alpha = 0.4)+
  labs(x = "Temperature (°C)",  y = bquote('log D-hinge larval count '~m^-3~''))+
  theme_classic()+
  theme(axis.title = element_text(size = 25), 
        axis.text = element_text(size = 23, color = 'black'), 
        axis.ticks = element_line (colour = "black", size = 1), 
        axis.line = element_line(color = "black", size = 1))+
  ylim(0,1450)#make plot prettier - remove the two large values there....

#save figure
ggsave(path = here("Figures"), filename = "fig_5.tiff", dpi = 500)


# Figure 6 ----------------------------------------------------------------
#load in spat data
spat<-read_csv(here("Data","Oyster_Counts", "Spat.csv"))
#lubridate dates
spat$Date<-mdy(spat$Date)
#plot
spat %>% 
  ggplot(.,aes(x = Date , y= Spat))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin = Spat - SE, ymax = Spat+ SE), size = 0.75, width = 2)+
  theme_classic()+
  labs(x = "Month (2020)", y = "Average spat settlement"~m^-2~"")+
  theme(axis.title = element_text(size = 25, color = 'black'), axis.text = element_text(size = 22, color ='black'), axis.line = element_line(size = 1))

#save image
ggsave(path = here("Figures"), filename = "figure_6.tiff", dpi = 500)


# Summary stats -----------------------------------------------------------
#get range and mean
dat_all %>% 
  group_by(Year) %>% 
  summarize(meanD = mean(D),
            meanV = mean(V),
            minD = min(D),
            maxD = max(D),
            minV = min(V),
            maxV = max(V),
            seD = sd(D)/sqrt(35),
            seV = sd(V)/sqrt(35),
            median_D = median (D),
            median_V = median(V))

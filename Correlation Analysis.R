library('dplyr')
library('ggplot2')
library('data.table')
library('reshape2')
library('RColorBrewer')
library('patchwork')
library('tidytext')
library('lmtest')
library('ivreg')
library('stargazer')
library('tseries')
library('moments')
library('car')
library('caret')
library('sandwich')
stringency<-OxCGRT_stringency
write.csv(stringency,file='~/Documents/科研/death data/stringency.csv')
cn_data<-read.csv('~/Documents/科研/death data/cn_death_data.csv')
age<-covdeath<-read.csv('~/Documents/科研/death data/aging_with_region.csv')
ghs<-read.csv('~/Documents/科研/ghs.csv')

# regression across the globe
# age
plot(age[,1],age[,2])
cor.test(age[,1],age[,2],method='spearman')
summary(lm(age[,1]~age[,2]))
age<-age[age$Aging.rate!=0&age$Aging.rate!='#N/A',]

# ghs
ghs<-ghs[ghs$Year==2021,]
ghs<-ghs[,c(1,3)]
colnames(ghs)[c(1,2)]<-c('Entity','ghs')

# first regression
reg<-merge(tot_death[,c(1:3)],age,by='Entity')
reg<-merge(reg,ghs,by='Entity')
reg<-merge(reg,stringency[,c(1,2)],by='Entity')
reg$Aging.rate<-as.numeric(reg$Aging.rate)
reg<-merge(reg,cum_p[,c(1,2)],by='Entity')
reg<-reg[,c(1,2,3,7,8,9)]
colnames(reg)[c(2:6)]<-c('Excess death','Reported COVID death','Aging rate',
                         'GHS index','Stringency index')
cor.test(reg$Aging.rate,reg$ghs) #r=0.63
cor.test(reg$cum_p,reg$`GHS index`) #r=-0.35
summary(lm(log(`Excess death`)~log(`Reported COVID death`)+
                   `Aging rate`+`GHS index`+`Stringency index`,data=reg))
reg1<-lm(log(`Excess death`)~log(`Reported COVID death`)+
                 `Aging rate`+`GHS index`+`Stringency index`,data=reg)
write.csv(reg,file='~/Documents/科研/death data/reg.csv')   

# 逐步回归分析
summary(step(reg1))
drop1(step(reg1))

# test for OLS assumptions
shapiro.test(log(reg[,2])) #p=0.24
bptest(reg1) #p<0.05
vif(reg1) #<2

# robust se
cov1 <- vcovHC(reg1, type = "HC1")
reg1_se_r <-coeftest(reg1, vcov=cov1)
reg1_robust <- sqrt(diag(cov1))

stargazer(reg1, type="html", 
          dep.var.labels=c("Log(Excess death)"),
          covariate.labels=c("Intercept","Log(Reported COVID death)",
                             'Aging rate','GHS index','Stringency index'),
          digits=4,
          single.row=FALSE,
          intercept.bottom=FALSE,
          out= "reg1.html")


# rank stringency
stringency$s_rank<-rank(stringency[,2])
stringency$s_percentile<-stringency$s_rank/length(stringency$s_rank)

# stringency vs cum_p
colnames(stringency)[1]='Entity'
stringency<-merge(stringency,cum_p,by='Entity')
colnames(stringency)[c(6,7)]<-c('p_rank','p_percentile')
stringency[,6]<-rank(stringency[,5])
stringency[,7]<-stringency[,6]/length(stringency[,6])

hist(stringency$`average stringency`)
hist(stringency$cum_p)
plot(stringency$`average stringency`,stringency$cum_p)
cor.test(stringency$`average stringency`,stringency$cum_p,
         method='spearman') #防疫严格与否与p无关

# stringency vs cov death
colnames(tot_death)[1]='Entity'
stringency<-merge(stringency,tot_death,by='Entity')
stringency<-stringency[,c(1:7,9)]
hist(stringency[,8])
cor.test(stringency$`average stringency`,stringency[,8],
         method='spearman') #防疫严格和新冠死亡人数中正相关r=0.4
plot(stringency[,2],stringency[,8])

# stringency of favorable countries
country<-c('China','United Kingdom','United States')
stringency[stringency$Entity==country,c(1,2,4,5,7)]

#stringency vs ratio
strin_ratio<-merge(co_ex_ratio,stringency,by='Entity')
cor.test(stringency[,4],stringency[,5],
         method='spearman') 

#stringency vs p plot
least10<-stringency[order(stringency[,2])[1:10],c(1,2,5)]
colnames(least10)[c(2,3)]<-c('stringency index','cumulative excess mortality rate')
least10<-melt(least10)
least_plot<-ggplot(least10,aes(x=Entity,
                   y=ifelse(variable=='cumulative excess mortality rate',value,-value),
                   fill=variable))+
        geom_bar(stat='identity')+
        coord_flip()+
        geom_text(aes(label=round(value,2),
                  hjust=ifelse(variable=='cumulative excess mortality rate',-0.4,1.1)),
                  size=2)+
        labs(x='Country',y=element_blank(),
             title='Cumulative excess mortality rates of 10 countries with smallest stringency index')+
        theme(axis.title=element_text(size=8,face="plain",color="black"),
              axis.text = element_text(size=8,face="plain",color="black"),
              legend.title = element_blank(),
              legend.background = element_blank())

# highest 10 stringency 
top10<-stringency[order(stringency[,2],decreasing=T)[1:10],c(1,2,5)]
colnames(top10)[c(2,3)]<-c('stringency index','cumulative excess mortality rate')
top10<-melt(top10)
top_plot<-ggplot(top10,aes(x=Entity,
                   y=ifelse(variable=='cumulative excess mortality rate',value,-value),
                   fill=variable))+
        geom_bar(stat='identity')+
        coord_flip()+
        geom_text(aes(label=round(value,2),
                      hjust=ifelse(variable=='cumulative excess mortality rate',-0.4,1.1)),
                  size=2)+
        labs(x='Country',y=element_blank(),
             title='Cumulative excess mortality rates of 10 countries with highest stringency index')+
        theme(axis.title=element_text(size=8,face="plain",color="black"),
              axis.text = element_text(size=8,face="plain",color="black"),
              legend.title = element_blank(),
              legend.background = element_blank())
least_plot/top_plot+labs(y='stringency index / cumulative excess mortality rate')
# plot cum excess death vs covid death
#world
world_cum<-aggregate(cum[complete.cases(cum),][,c(4,5)],
                     by=list(cum[complete.cases(cum),]$Day),sum,na.rm=TRUE)
colnames(world_cum)[c(1,2,3)]<-c('Day','Cumulative excess deaths','Cumulative COVID deaths')
world_cum<-melt(world_cum,id='Day')
world_cum$Day<-as.Date(world_cum$Day)

ggplot(world_cum, aes(x =Day, y = value,color=variable) )+
        geom_area(aes(fill=variable),alpha=0.5,position="identity")+ 
        geom_line(size=1)+
        scale_x_date(date_labels = "%Y-%m",date_breaks = "4 month")+
        labs(x="Time",y="Total Death",
             title='World Cumulative COVID deaths vs Cumulative excess deaths')+
        theme( axis.title=element_text(size=10,face="plain",color="black"),
               axis.text = element_text(size=10,face="plain",color="black"),
               legend.position = c(0.15,0.8),
               legend.title = element_blank(),
               legend.background = element_blank())

# china cum
cn_data1<-cn_data[,c(2,4,7)]
colnames(cn_data1)[c(2,3)]<-c('Cumulative COVID deaths','Cumulative excess deaths')
colnames(cn_data1)[1]<-'date'
cn_data1<-melt(cn_data1,id='date')
cn_data1$date<-as.Date(cn_data1$date)

ggplot(cn_data1, aes(x =date, y = value,color=variable) )+
        geom_area(aes(fill=variable),alpha=0.5,position="identity")+ 
        geom_line(size=1)+
        scale_x_date(date_labels = "%Y-%m",date_breaks = "4 month")+
        labs(x="Time",y="Total Death",
             title='China Cumulative COVID deaths vs Cumulative excess deaths')+
        theme(axis.title=element_text(size=10,face="plain",color="black"),
               axis.text = element_text(size=10,face="plain",color="black"),
               legend.position = c(0.15,0.8),
               legend.title = element_blank(),
               legend.background = element_blank())

# Excess mortality and covid death 

# weekly_death table
weekly_death<-covdeath[,c(1,3,4,7)]
weekly_death<-weekly_death[complete.cases(weekly_death),]
colnames(weekly_death)[c(1:4)]<-c('Entity','date','Excess death','COVID death')
weekly_death<-weekly_death[weekly_death$Entity!='China',]
weekly_death$date<-as.Date(weekly_death$date)
cn_weekly$Entity='China'
cn_bind<-cn_weekly[,c(12,1,7,2)]
cn_bind$date<-as.Date(cn_bind$date)
weekly_death<-rbind(weekly_death,cn_bind)
weekly_death[weekly_death$Entity=='China',]

# correlations
cor.test(weekly_death[,3],weekly_death[,4],method='spearman')
summary(lm(weekly_death[,3]~weekly_death[,4]))
plot(weekly_death[,3],weekly_death[,4])
Entity<-c()
r<-c()
for (country in unique(weekly_death$Entity)){
        Entity=append(Entity,country)
        r=append(r,cor(weekly_death[weekly_death$Entity==country,3],weekly_death[weekly_death$Entity==country,4]
                       ,method='spearman'))
}
cor<-data.frame(Entity=Entity,r=r)

# covid/excess ratio
co_ex_ratio<-aggregate(weekly_death[,c(3,4)],by=list(weekly_death$Entity),sum)
co_ex_ratio$ratio<-co_ex_ratio[,3]/co_ex_ratio[,2]
colnames(co_ex_ratio)[1]='Entity'
r_ratio<-merge(co_ex_ratio,cor,by='Entity')
cor.test(r_ratio[,4],r_ratio[,5]) #r和cov.ex 无关
k<-c("China",'Brazil','Japan','United States','Germany','India')
r_ratio[r_ratio$Entity==c('China'),]

length(unique(raw$Entity))
# time series
plot_weekly<-function(Entity) {
        weekly<-weekly_death[weekly_death$Entity==Entity,c(2,3,4)]
        r<-cor(weekly[,2],weekly[,3],method='spearman')
        r<-round(r,2)
        weekly<-melt(weekly,id='date')
        weekly$date<-as.Date(weekly$date)
        
        ggplot(weekly, aes(x =date, y = value,color=variable) )+
                geom_line(size=0.5)+
                scale_x_date(date_labels = "%Y-%m",date_breaks = "4 month")+
                labs(y=element_blank(),
                     x=element_blank(),
                     title=paste(Entity,'( r =',r,')'))+
                theme( axis.title=element_text(size=8,face="plain",color="black"),
                       axis.text = element_text(size=5,face="plain",color="black"),
                       legend.title=element_blank(),
                       title=element_text(size=8),
                       legend.text = element_blank(),
                       legend.position = 'none',
                       legend.background = element_blank())
}
#China India Brazil USA Japan Germany
plot_weekly("United Kingdom")
plot_weekly("Japan")+labs(y='Weekly deaths')+
        theme(legend.position =c(0.3,0.9),
                           legend.text = element_text(size=5),
                           legend.key.size = unit(c(0.8,0.5),'lines'))+
        plot_weekly("China")+plot_weekly("United States")+
        plot_weekly("Brazil")+labs(y='Weekly deaths')+
        plot_weekly("Germany")+plot_weekly("India")

# test Our world in data and the Economist's excess death
OWID<-raw[,c(1,7)]
OWID<-aggregate(OWID[complete.cases(OWID),],
          by=list(OWID[complete.cases(OWID),]$Entity),max,na.rm=TRUE)
ECO<-cum[,c(1,4)]
ECO<-aggregate(ECO[complete.cases(ECO),],
                by=list(ECO[complete.cases(ECO),]$Entity),max,na.rm=TRUE)
OWvsECO<-merge(ECO,OWID,by='Entity')
OWvsECO<-OWvsECO[,c(1,3,5)]
colnames(OWvsECO)[c(2,3)]<-c('ECO excess','OW excess')
plot(OWvsECO[,3])
wilcox.test(OWvsECO[,3],OWvsECO[,2]) #p=0.29, ECO and OW not different






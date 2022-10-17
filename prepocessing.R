raw<-read.csv('~/Documents/科研/death data/excess-mortality-raw-death-count-single-series.csv')
cum<-read.csv('~/Documents/科研/death data/excess-deaths-cumulative-economist-single-entity (1).csv')
pscore<-read.csv('~/Documents/科研/death data/excess-mortality-p-scores-projected-baseline-by-age.csv')
covdeath<-read.csv('~/Documents/科研/death data/excess-deaths-daily-economist-single-entity.csv')
who_cov<-read.csv('~/Documents/科研/who data/WHO-COVID-19-global-data.csv')
library(ggplot2)
library(dplyr)
library(data.table)

# data pre-processing 

# build china death model, using death data from 2010-2019, 10 yrs before covid
cn_year<-c(2010:2019)
cn_death<-c(948,957,963,969,974,975,977,986,993,998)
mod1<-lm(cn_death~cn_year)
summary(mod1)

#predict death in 2020, 2021, 2022
cn_data<-data.frame(cn_year=c(2020,2021,2022))
cn_predictd_death_10k<-predict(mod1,cn_data)

# china data frame
cn_data<-data.frame(cn_year=c('2020-12-31','2021-12-31','2022-12-31'),cn_predictd_death_10k)
as.Date(pscore$Day)
as.Date(cn_data$cn_year)
cn_data$reported_death_10k<-c(996,1014,1062)
cn_data$p<-((cn_data$reported_death_10k-cn_data$cn_predictd_death_10k)/cn_data$cn_predictd_death_10k)*100
cn_data$excess_death_10k<-cn_data$reported_death_10k-cn_data$cn_predictd_death_10k
cn_data$cumulative_excess_death_10k<-cumsum(cn_data$excess_death_10k)
cn_data$cumulative_reported_death_10k<-cumsum(cn_data$reported_death_10k)
cn_data$cumulative_p<-(cn_data$cumulative_excess_death_10k/cn_data$cumulative_reported_death_10k)*100
cn_data$reported_covid_death<-c(4634,5650,15848)
cn_data$cumulative_covid_death<-cumsum(cn_data$reported_covid_death)

length(unique(raw$Entity))
length(unique(covdeath$Entity))

#raw cumulated excess death and cumulated p
colnames(raw)[4]<-'projected_death'
colnames(raw)[5]<-'reported_death'
raw$excess_mortality<-raw[,5]-raw[,4]
as.Date(raw$Day)
raw<-raw[order(raw$Entity,raw$Day),]
cum_excess_mortality<-summarise(group_by(raw,Entity),cum_excess_mortality=cumsum(excess_mortality))
raw[,7]<-cum_excess_mortality[,2]
cum_projected_death<-summarise(group_by(raw,Entity),cum_projected_death=cumsum(projected_death))
raw[,8]<-cum_projected_death[,2]
raw$cum_p<-(raw$cum_excess_mortality/raw$cum_projected_death)*100

#cumulative p percentile calculation
raw[complete.cases(raw$cum_p),]
cum_p<-aggregate(raw[complete.cases(raw$cum_p),]$cum_p,
          by=list(raw[complete.cases(raw$cum_p),]$Entity),max,na.rm=TRUE)
colnames(cum_p)[c(1,2)]<-c("Entity",'cum_p')
cn_cum_p<-data.frame(Entity='China',cum_p=cn_data[3,8])
cum_p<-rbind(cum_p,cn_cum_p)
quantile(cum_p$cum_p)
cum_p$rank<-rank(cum_p$cum_p)
cum_p$percentile_rank<-(cum_p$rank/length(cum_p$cum_p))*100

# total excess mortality-total covid death for each country
tot_death<-aggregate(cum[complete.cases(cum),][,4:7],
                 by=list(cum[complete.cases(cum),]$Entity),max,na.rm=TRUE)

# 改中国死亡数据
tot_death[tot_death$Group.1=='China',][,3]<-25868

# weekly data of China
who_cn<-who_cov[who_cov$Country=='China',][,c(1,7,8)]
# frollsum(who_cn[,2],n=7)

i=7
weekly_cov_death<-c()
while (i <= (length(who_cn[,2]))){
        new<-sum(who_cn[(i-6):i,2])
        weekly_cov_death<-append(weekly_cov_death,new)
        i = i+7
}
dates<-c(as.Date('2020-01-01'),seq.Date(from=as.Date('2020/1/6'),format='%Y%m%d',by='week',
               length.out=141))
cn_weekly<-data.frame(dates,weekly_cov_death)
cn_weekly$culumative_cov_death<-cumsum(cn_weekly$weekly_cov_death)

# calculate weekly projected death 
cn_weekly[1:52,4]<-(945.933+5.103*10)/52
cn_weekly[53:104,4]<-(945.933+5.103*11)/52
cn_weekly[105:142,4]<-(945.933+5.103*12)/52
colnames(cn_weekly)[4]<-'projected_death_10k'
cn_weekly$cum_projected_death<-cumsum(cn_weekly[,4])


# cum and weekly excess death of china
cn_temp<-cum[cum$Entity=='China',]
cn_temp<-head(cn_temp,142)
cum_excess_death<-cn_temp[,4]
cn_weekly<-cbind(cn_weekly,cum_excess_death)
cn_weekly$weekly_excess_death[1]<-cn_weekly$cum_excess_death[1]
for (i in c(2:length(cn_weekly[,2]))){
        cn_weekly[i,7]<-cn_weekly[i,6]-cn_weekly[(i-1),6]
}
colnames(cn_weekly)[5]<-'cum_projected_death_10k'

# p and cum p of china
cn_weekly$p<-(cn_weekly[,7]/(cn_weekly[,4]*10000))*100
cn_weekly$cum_p<-(cn_weekly[,6]/(cn_weekly[,5]*10000))*100
cum_p[121,2]<-cn_weekly[142,9]

# adjusting ranks of china
cum_p$rank<-rank(cum_p[,2])
cum_p$percentile_rank<-cum_p$rank/length(cum_p[,2])

# total death (calculated indirectly by excess death + projected death)
cn_weekly$weekly_total_reported_death_10k<-((cn_weekly[,4]*10000)+cn_weekly[,7])/10000
cn_weekly$cum_total_death_10k<-cumsum(cn_weekly$weekly_total_reported_death_10k)

write.csv(cn_weekly,file='~/Documents/科研/death data/cn_death_data.csv')
read.csv('~/Documents/科研/death data/cn_death_data')
        
        
        
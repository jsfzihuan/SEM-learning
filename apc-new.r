###############################################################################
#*********************CM-APC in Hong Kong**************************************
#***************  Created by Shaofei Jin   ************************************
###############################################################################

#Packages requied====
library(Epi)
library(reshape2)
library(ggplot2)
library(segmented)
library(sysfonts)

#Function for peridic sum====

Periodsum=function(
  x,
  interval
){
  # x=death.hk
  # interval=5
  datalength=dim(x)[1] 
  n_max=datalength %/% interval
  x_0=x[1:interval,]
  x_01=colSums(x_0)
  x_01=as.data.frame(x_01)
  x_01$x_01[1]=x[1,1]
  x_01=t(x_01)
  for (i in 2:(n_max)){
    x_i_name=paste("x_",i,sep="")
    x_i=x[((i-1)*interval+1):(i*interval),]
    # print(x_i)
    #x_i1=colSums(x_i)
    x_i1=apply(x_i,2,FUN="sum")
    x_i1=as.data.frame(x_i1)
    x_i1$x_i1[1]=x[(i-1)*interval+1,1]
    names(x_i1)[1]=x_i_name
    x_i1=t(x_i1)
    x_01=rbind(x_01,x_i1)
  }
  x_01=as.data.frame(x_01)
  return(x_01)
}

P=theme_bw()+
  theme(text=element_text(family="Arial",size=10))+
  theme(strip.text=element_text(family="Arial",size=10))+
  theme(strip.background = element_rect(fill="white"))+
  theme(strip.text=element_text(size=10))+
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank())
# Part I ==== case numbers of CM in the HK

death.hk=read.csv("data/death_hk_num.csv",header = T)
incid.hk=read.csv("data/inci_hk_num.csv",header = T)
death.male=read.csv("data/death_male_num.csv",header = T)
death.female=read.csv("data/death_female_num.csv",header = T)
incid.male=read.csv("data/inci_male_num.csv",header = T)
incid.female=read.csv("data/inci_female_num.csv",header = T)

# Five year interval sum
death.hk_5sum=Periodsum(x=death.hk,interval = 5)
incid.hk_5sum=Periodsum(x=incid.hk,interval = 5)
death.male_5sum=Periodsum(x=death.male,interval = 5)
death.female_5sum=Periodsum(x=death.female,interval = 5)
incid.male_5sum=Periodsum(x=incid.male,interval = 5)
incid.female_5sum=Periodsum(x=incid.female,interval = 5)

# Bingding them, here, I do not know how to convert the style name, then, I use
#a slow but effective method to create them. Write, then read out.
five_sum=rbind(death.hk_5sum,incid.hk_5sum,death.male_5sum,
               death.female_5sum,incid.male_5sum,incid.female_5sum)
write.csv(five_sum,"five_sum.csv")
five_sum=read.csv("five_sum.csv",header = T)

# Creat the year interval and sytle.
 year_interval1=c("1983-1987","1988-1992","1993-1997",
                 "1998-2002","2003-2007","2008-2012")
year_interval=c(1985,1990,1995,2000,2005,2010)
style_name=c("Total mortality","Total incidence","Male mortality",
             "Female mortality","Male incidence","Female incidence")

year_interval=rep(year_interval,6)
year_interval1=rep(year_interval1,6)
style_name=rep(style_name,each=6)

five_sum=cbind(year_interval1,year_interval,style_name,five_sum)

# Picked the age group from 25 years
five_sum_plot=five_sum[,c(1,2,3,10:22)]

#melt the data 
five_sum_plot=melt(five_sum_plot,id=c("year_interval1",
                                      "year_interval","style_name"))

#create the age-groups
age_group=c("25-29","30-34","35-39","40-44","45-49",
            "50-54","55-59","60-64","65-69","70-74",
            "75-79","80-84","85+")
age_group=rep(age_group,each=36)
five_sum_plot=cbind(five_sum_plot,age_group)

age_group_large_1=rep(c("25-44"),24*6)
age_group_large_2=rep(c("45-64"),24*6)
age_group_large_3=rep(c("65-85+"),30*6)
age_group_large=c(age_group_large_1,age_group_large_2,
                      age_group_large_3)

# age_group_large=rep(age_group_large_0,6)

five_sum_plot=cbind(five_sum_plot,age_group_large)

five_sum_plot_large=aggregate(value~year_interval1+year_interval+style_name+age_group_large,
                              data = five_sum_plot,FUN="sum")
#Plot the figures
five_sum_plot_large$style_name=factor(five_sum_plot_large$style_name,
                                      ordered = T,
                                      levels = c("Total mortality",
                                                 "Total incidence",
                                                 "Male mortality",
                                                 "Male incidence",
                                                 "Female mortality",
                                                 "Female incidence"))

ggplot(five_sum_plot_large,aes(x=year_interval,y=value,linetype=age_group_large))+
  geom_line()+
  facet_wrap(~style_name,nrow = 3,scales = "free_y")+
  P+
  labs(x="Period",y="Cases")+
  labs(linetype="Age group (years)")
  
aggregate(value~style_name+age_group_large,
          data=five_sum_plot_large, FUN="sum")


# APC model

death.hk_asr=read.csv("data/death_hk_asr.csv",header = T)
incid.hk_asr=read.csv("data/inci_hk_asr.csv",header = T)
death.male_asr=read.csv("data/death_male_asr.csv",header = T)
death.female_asr=read.csv("data/death_female_asr.csv",header = T)
incid.male_asr=read.csv("data/inci_male_asr.csv",header = T)
incid.female_asr=read.csv("data/inci_female_asr.csv",header = T)
style_name=c("Total mortality","Total incidence","Male mortality",
             "Female mortality","Male incidence","Female incidence")

cmdata_acp=rbind(death.hk_asr,incid.hk_asr,
                 death.male_asr,death.female_asr,
                 incid.male_asr,incid.female_asr)

style_name_apc=rep(style_name,each=33)

cmdata_acp=cbind(style_name_apc,cmdata_acp)
cmdata_acp_trun=cmdata_acp[,c(1,2,8:20)]

cmdata_acp_trun_MEAN=aggregate(cbind(X27.5,X32.5,X37.5,X42.5,X47.5,
                                     X52.5,X57.5,X62.5,X67.5,X72.5,
                                     X77.5,X82.5,X87.5)~style_name_apc,
                               data=cmdata_acp_trun,
                               FUN="mean")
cmdata_acp_trun_MEAN=transform(cmdata_acp_trun_MEAN,
                                X2544=(X27.5*7.93+X32.5*7.61+X37.5*7.15+X42.5*6.59)/
                                  (7.93+7.61+7.15+6.59),
                                X4564=(X47.5*6.04+X52.5*5.37+X57.5*4.55+X62.5*3.72)/
                                  (6.04+5.37+4.55+3.72),
                                X6585=(X67.5*2.96+X72.5*2.21+X77.5*1.52+X82.5*0.91+
                                         X87.5*0.63)/(2.96+2.21+1.52+0.91+0.63))


# AAPC for the large age groups 25-45, 45-65, and 65+,
# write it later cause i wanna to use a new R package.
cmdata_acp_trun_large=transform(cmdata_acp_trun,
                          X2544=(X27.5*7.93+X32.5*7.61+X37.5*7.15+X42.5*6.59)/
                            (7.93+7.61+7.15+6.59),
                          X4564=(X47.5*6.04+X52.5*5.37+X57.5*4.55+X62.5*3.72)/
                            (6.04+5.37+4.55+3.72),
                          X6585=(X67.5*2.96+X72.5*2.21+X77.5*1.52+X82.5*0.91+
                                   X87.5*0.63)/(2.96+2.21+1.52+0.91+0.63))
# Trends plot in overall/25-44/45-64/65-85+
# write.csv(cmdata_acp_trun_large,"trend.csv")
trend_plot=read.csv("trend.csv",header = T)
trend_plot=melt(trend_plot,id=c("style_name_apc","Year"))
trend_plot$data=trend_plot$value
trend_plot$data[trend_plot$value == 0.0] <- 0.0000001

trend_plot$style_name_apc=factor(trend_plot$style_name_apc,
                                      ordered = T,
                                      levels = c("Total mortality",
                                                 "Total incidence",
                                                 "Male mortality",
                                                 "Male incidence",
                                                 "Female mortality",
                                                 "Female incidence"))
trend_plot$variable=as.character(trend_plot$variable)
trend_plot$Age=trend_plot$variable
trend_plot$Age[trend_plot$variable =="Age.25.44"]="25-44"
trend_plot$Age[trend_plot$variable =="Age.45.64"]="45-64"
trend_plot$Age[trend_plot$variable =="Age.85."]="65-85+"
trend_plot$Age[trend_plot$variable =="Overall"]="Overall"
ggplot(trend_plot[which(trend_plot$Age != "Overall"),],
       aes(x=Year,y=value,colour=Age))+
  geom_line()+
  geom_point(size=2)+
  #geom_point(data=trend_plot,aes(x=Year,y=value,shape=variable,group=1))+
  facet_wrap(~style_name_apc,nrow = 3)+
  scale_y_log10(breaks=c(0.1,1,2,5,10))+
  P+
  labs(x="Year of diagnosis or death")+
  #ylab(expression(atop("Age standardized rate", "per 1000 person-years")))+
  ylab(expression("Age standardized rate (per 100000 person-years)"))+
  labs(colour="Age group (years)")

ggsave("figure1.png",dpi=900)

trend_plot[which(trend_plot$Year==2015),]
cmdata_acp_trun_large_trend=cmdata_acp_trun_large[,c(1,2,16:18)]
cmdata_acp_trun_large_trend=melt(cmdata_acp_trun_large_trend,
                                 id=c("style_name_apc","X"))
cmdata_acp_trun_large_trend$X=as.numeric(cmdata_acp_trun_large_trend$X)
for (i in levels(cmdata_acp_trun_large_trend$style_name_apc)){
  #for (j in c("X6585","X4564")){
  for (j in levels(cmdata_acp_trun_large_trend$variable)){
    trend_aapc=
      cmdata_acp_trun_large_trend[which(cmdata_acp_trun_large_trend$style_name_apc==i & cmdata_acp_trun_large_trend$variable==j),]
    trend_i=lm(value~X,data=trend_aapc)
    print(i)
    print(j)
   # trend_i_1=segmented.lm(trend_i)
    try(trend_i_1 <- segmented.lm(trend_i,
                           control=seg.control(display=F,
                                               K=3,
                                               stop.if.error=FALSE,
                                               n.boot=0, it.max=50)))
    print(summary(trend_i_1))
    #plot.segmented(trend_i_1)
    aapc(ogg = trend_i_1,parm=1,
         wrong.se = F)
  }
}


#APC analysis
#Data cleaning to fit the requiement of the APC

cmdata_acp_trun_apc=melt(cmdata_acp_trun,id=c("style_name_apc","X"))
cmdata_acp_trun_apc$variable=substr(cmdata_acp_trun_apc$variable,2,5)
cmdata_acp_trun_apc$variable=as.numeric(cmdata_acp_trun_apc$variable)
cmdata_acp_trun_apc$Y=rep(100000,2574)
names(cmdata_acp_trun_apc)[1:5]=c("Style","P","A","D","Y")

cmdata_acp_plot=matrix(NA,nrow=0,ncol = 6)
cmdata_acp_name_age=as.matrix(rep("Age",13))
cmdata_acp_name_per=as.matrix(rep("Period",33))
cmdata_acp_name_coh=as.matrix(rep("Cohort",93))
cmdata_acp_name=rbind(cmdata_acp_name_age,
                      cmdata_acp_name_per,
                      cmdata_acp_name_coh)

for (i in levels(cmdata_acp_trun_apc$Style)){
  cmdata_acp_trun_apc_i=
    cmdata_acp_trun_apc[which(cmdata_acp_trun_apc$Style==i),]
  apc_style=apc.fit(cmdata_acp_trun_apc_i,
                    A=A,P=P,D=D,Y=Y,
                    npar=5, model="ns",
                    parm="APC",
                    ref.c = 1940,
                    ref.p = 1983,
                    scale=10^5,
                    print.AOV = F)

  # cmdata_acp_plot_i=rbind(apc_style$Age,
  #                         apc_style$Per,
  #                         apc_style$Coh)
  # cmdata_acp_plot_i=cbind(cmdata_acp_plot_i,
  #                         cmdata_acp_name,rep(i,139))
  # 
  # cmdata_acp_plot=rbind(cmdata_acp_plot,cmdata_acp_plot_i)
  print(i)
  print(apc_style$Anova)
  #apc.plot(apc_style ,ci=T,shade=T)
}

cmdata_acp_plot1=as.vector(cmdata_acp_plot)

cmdata_acp_plot1=as.data.frame(cmdata_acp_plot1)
cmdata_acp_plot1$Age=as.numeric(as.vector(cmdata_acp_plot1$Age))
cmdata_acp_plot1$Rate=as.numeric(as.vector(cmdata_acp_plot1$Rate))
cmdata_acp_plot1$`2.5%`=as.numeric(as.vector(cmdata_acp_plot1$`2.5%`))
cmdata_acp_plot1$`97.5%`=as.numeric(as.vector(cmdata_acp_plot1$`97.5%`))

names(cmdata_acp_plot1)[1:6]=c("Year","RR","LRR","URR","Effect","Sex")

cmdata_acp_plot1$Sex=as.character(cmdata_acp_plot1$Sex)



# Here, the function of transform is not right,DONT know why
cmdata_acp_plot1$mort <- substr(cmdata_acp_plot1$Sex,
                                nchar(cmdata_acp_plot1$Sex)-9,
                                nchar(cmdata_acp_plot1$Sex))
cmdata_acp_plot1$sex1 <- substr(cmdata_acp_plot1$Sex,
                                1,
                                nchar(cmdata_acp_plot1$Sex)-10)

ggplot(cmdata_acp_plot1[which(cmdata_acp_plot1$Effect !="Age"),],
       aes(x=Year,y=RR,colour=sex1))+
  geom_line()+
  facet_grid(mort~Effect,scales = "free")+P+
  labs(colour="")+
  ylab("Rate ratio")+
  guides(fill=guide_legend(title=NULL))
  #theme(legend.position=c(0.15,0.82))
ggsave("figure2.png",dpi=900)

ggplot(cmdata_acp_plot1[which(cmdata_acp_plot1$Effect =="Age"),],
       aes(x=Year,y=RR,colour=sex1))+
  geom_line()+
  labs(x="Age",y="Age effect")+
  facet_grid(mort~.,scales = "free")+P+
  labs(colour=NULL)+
  guides(fill=guide_legend(title=NULL))+
  theme(legend.position=c(0.15,0.82))
ggsave("figure3.png",dpi=900)

ggsave("Figure3.pdf",width = 8, height= 10, units = "cm")

write.csv(cmdata_acp_plot1,"acpplot1.csv")






#****Data cleaning====
# For total====
# 第一步 清洗数据
death.hk=read.csv("data/total_death.csv",
                  header=T)
death.hk=Periodsum(x=death.hk,interval = 5)
death.hk=death.hk[,-c(2:6)]
death.hk1=melt(death.hk,id="X")
names(death.hk1)[1:3] <- c("P","A1","D")

incidence.hk=read.csv("data/total_inc.csv",
                      header=T)

incidence.hk=Periodsum(x=incidence.hk,interval = 5)
incidence.hk=incidence.hk[,-c(2:6)]
incidence.hk1=melt(incidence.hk,id="X")
names(incidence.hk1)[1:3] <- c("P","A1","D")

pop.hk=read.csv("data/total_popu.csv",
                header=T)
pop.hk=Periodsum(x=pop.hk,interval = 5)
pop.hk=pop.hk[,-c(2:6)]
# pop.hk=pop.hk[c(1,6,11,16,21,26,31),-c(2:8)]
pop.hk1=melt(pop.hk,id="X")
names(pop.hk1)[1:3] <- c("P","A1","Y")

# 第二步：计算各自的APC值
# 2.1 death
death=cbind(death.hk1,pop.hk1)
death=death[,c(1,2,3,6)]
as.character(death$A1)
death$A=substr(death$A1,2,5)
death$A=as.numeric(death$A)

death.fit=apc.fit1(death,A=death$A,P=death$P,D=death$D,Y=(death$Y),
                  npar=7, model="ns",
                  parm="APC",
                  ref.c = 1940,
                  ref.p = 1983,
                  scale=10^5)
plot.apc(death.fit ,ci=T,shade=T)
attach(death)
bl.rate <- tapply( D, list(A,P), sum ) /
            tapply( Y, list(A,P), sum )
bl.rate

bl.rate1=bl.rate[,c(1,6,11,16,21,26)]
rateplot( bl.rate*10^6,ann=T )
a=Cplot( bl.rate1*10^6 )

# 2.2 Incidence 
incidence=cbind(incidence.hk1,pop.hk1)
incidence=incidence[,c(1,2,3,6)]

as.character(incidence$A1)
incidence$A=substr(incidence$A1,2,5)
incidence$A=as.numeric(incidence$A)
incidence$style="Incidence"
incidence.fit=apc.fit1(incidence,
                      A=incidence$A,
                      P=incidence$P,
                      D=incidence$D,
                      Y=(incidence$Y),
                  npar=7, model="ns",
                   parm="APC",
                  ref.c = 1940,
                  ref.p = 1983,
                  scale=10^5)
incidence.fit$Age

apc.plot(incidence.fit,ci=T,shade=T)

D_incidence=with(incidence,tapply(D,list(A,P),sum))
Y_incidence=with(incidence,tapply(Y,list(A,P),sum))
R_table=(D_incidence/Y_incidence)*(10^5)
b=seq(1,31,5)
age=seq(5,18,2)
rateplot(R_table[1:18,b],which=c("AP"),ann=T)
rateplot(R_table[age,b],which=c("CA"),ann=T)
par( mfrow=c(2,2) )
rateplot(R_table,ann=T,
         a.thin = seq( 1, 87.5, 40 ),
         p.thin = seq( 1, 31, 40 ),
         c.thin = seq( 2, 31+87.5 - 1, 40 ))
# For Male====
# 第一步 清洗数据
death.male.hk=read.csv("C:/paper/chenling/hk/data/datanew/male_death.csv",
                  header=T)
death.male.hk1=melt(death.male.hk,id="X")
names(death.male.hk1)[1:3] <- c("P","A1","D")

incidence.male.hk=read.csv("C:/paper/chenling/hk/data/datanew/male_inc.csv",
                      header=T)
incidence.male.hk1=melt(incidence.male.hk,id="X")
names(incidence.male.hk1)[1:3] <- c("P","A1","D")

pop.male.hk=read.csv("C:/paper/chenling/hk/data/datanew/male_popu.csv",
                header=T)
pop.male.hk1=melt(pop.male.hk,id="X")
names(pop.male.hk1)[1:3] <- c("P","A1","Y")

# 第二步：计算各自的APC值
# 2.1 death
death.male=cbind(death.male.hk1,pop.male.hk1)
death.male=death.male[,c(1,2,3,6)]
as.character(death.male$A1)
death.male$A=substr(death.male$A1,2,5)
death.male$A=as.numeric(death.male$A)

death.male.fit=apc.fit(death.male,
                       A=death.male$A,
                       P=death.male$P,
                       D=death.male$D,
                       Y=(death.male$Y),
                  npar=7, model="ns",
                  parm="APC",
                  ref.c = 1960.5,
                  ref.p = 2000,
                  scale=10^5)
plot.apc( death.male.fit ,ci=T,shade=T)
death.male.fit$Age

# 2.2 Incidence 
incidence.male=cbind(incidence.male.hk1,pop.male.hk1)
incidence.male=incidence.male[,c(1,2,3,6)]

as.character(incidence.male$A1)
incidence.male$A=substr(incidence.male$A1,2,5)
incidence.male$A=as.numeric(incidence.male$A)
incidence.male.fit=apc.fit(incidence.male,
                           A=incidence.male$A,
                           P=incidence.male$P,
                           D=incidence.male$D,
                           Y=(incidence.male$Y),
                      npar=7, model="ns",
                      parm="APC",
                      ref.c = 1960.5,
                      ref.p = 2000,
                      scale=10^5)
incidence.male.fit$Coh

apc.plot(incidence.male.fit,ci=T,shade=T)
# For Female====
# 第一步 清洗数据
death.female.hk=read.csv("C:/paper/chenling/hk/data/datanew/female_death.csv",
                       header=T)
death.female.hk1=melt(death.female.hk,id="X")
names(death.female.hk1)[1:3] <- c("P","A1","D")

incidence.female.hk=read.csv("C:/paper/chenling/hk/data/datanew/female_inc.csv",
                           header=T)
incidence.female.hk1=melt(incidence.female.hk,id="X")
names(incidence.female.hk1)[1:3] <- c("P","A1","D")

pop.female.hk=read.csv("C:/paper/chenling/hk/data/datanew/female_popu.csv",
                     header=T)
pop.female.hk1=melt(pop.female.hk,id="X")
names(pop.female.hk1)[1:3] <- c("P","A1","Y")

# 第二步：计算各自的APC值
# 2.1 death
death.female=cbind(death.female.hk1,pop.female.hk1)
death.female=death.female[,c(1,2,3,6)]
as.character(death.female$A1)
death.female$A=substr(death.female$A1,2,5)
death.female$A=as.numeric(death.female$A)

death.female.fit=apc.fit(death.female,
                       A=death.female$A,
                       P=death.female$P,
                       D=death.female$D,
                       Y=(death.female$Y),
                       npar=c(10,10,10), model="ns",
                       parm="APC",
                       ref.c = 1960.5,
                       ref.p = 2000,
                       scale=10^5)
plot.apc( death.female.fit ,ci=T,shade=T)
death.female.fit$Age

# 2.2 Incidence 
incidence.female=cbind(incidence.female.hk1,pop.female.hk1)
incidence.female=incidence.female[,c(1,2,3,6)]

as.character(incidence.female$A1)
incidence.female$A=substr(incidence.female$A1,2,5)
incidence.female$A=as.numeric(incidence.female$A)
incidence.female=incidence.female[,c(1,3,4,5)]

incidence.female$Y=as.numeric(incidence.female$Y)
incidence.female.fit=apc.fit(incidence.female,
                           A=incidence.female$A,
                           P=incidence.female$P,
                           D=incidence.female$D,
                           Y=incidence.female$Y,
                           npar=7, model="ns",
                           parm="APC",
                           ref.c = 1960.5,
                           ref.p = 2000,
                           scale=10^5)

incidence.female.fit$Per
a=plot.apc( incidence.female.fit ,ci=T,shade=T)


        # plot.apc(incidence.female.fit,ci=T,shade=T,
        #          r.txt = "Rate per 100,000 person-years", 
        #          cp.lab = seq(1900,1983,20))
        # par(mar=c(4,4,1,4))
        # bf=apc.frame(cp.lab = seq(1900,1983,20),
        #              a.lab = seq(0,90,10),
        #              r.lab=seq(0.01,10),
        #              a.txt = "Age",
        #              cp.txt = "Calendar time",
        #              rr.txt = "Rate ratio",
        #              sides = c(1,2,4))
apc.lines(incidence.female.fit,frame.par = bf)
results=rbind(death.fit$Per,death.fit$Coh,
              incidence.fit$Per,incidence.fit$Coh,
              death.male.fit$Per,death.male.fit$Coh,
              incidence.male.fit$Per,incidence.male.fit$Coh,
              death.female.fit$Per,death.female.fit$Coh,
              incidence.female.fit$Per,incidence.female.fit$Coh)
write.csv(results,
          file="C:/paper/chenling/hk/data/APC1.csv")


rawdata=cbind(death$P,death$A,death$Y,death$D,
              incidence$D,
              death.male$Y,
              death.male$D,
              incidence.male$D,
              death.female$Y,
              death.female$D,
              incidence.female$D)
rawdata1=as.data.frame(rawdata)
names(rawdata1)[1:11]=c("P","A","Y-T","D-T","I-T",
                       "Y-M","D-M","I-M",
                       "Y-F","D-F","I-F")
write.csv(rawdata1,
          file="C:/paper/chenling/hk/data/rawdata.csv")

# Figure 1 in the paper====
setwd("C:/paper/chenling/hk/data")

age.asr=read.csv("C:/paper/chenling/hk/data/age-asr.csv",
                 header=T)
age.asr=subset(age.asr,Sex != "Total")
age.asr$Sex=factor(age.asr$Sex,
                   levels=c("Female","Male"),ordered = T)
age.asr$Age=factor(age.asr$Age,levels = c("0-4","5-9","10-14","15-19",
                                          "20-24","25-29","30-34","35-39",
                                          "40-44","45-49","50-54","55-59",
                                          "60-64","65-69","70-74","75-79",
                                          "80-84","85+"),ordered = T)
age.case=read.csv("C:/paper/chenling/hk/data/cases_hk-1.csv",
                  header=T)
age.case$Sex=factor(age.case$Sex,
                    levels=c("Female","Male"),ordered = T)
age.case$Age=factor(age.case$Age,levels = c("0-4","5-9","10-14","15-19",
                                            "20-24","25-29","30-34","35-39",
                                            "40-44","45-49","50-54","55-59",
                                            "60-64","65-69","70-74","75-79",
                                            "80-84","85+"),ordered = T)
source("C:/paper/chenling/paper/multiplot.r")  

p1=ggplot(age.asr, aes(x=factor(Age), y=round(ASR,2),group=Sex,colour=Sex)) +
  facet_grid(. ~ style)+
  #geom_errorbar(aes(ymin=lower, ymax=upper), width=.2)+
  geom_line(size=1)+ 
  geom_point(size=2)+
  # geom_ribbon(data=age.asr,
  #             aes(x=factor(Age),ymin=lower, ymax=upper,group=Sex,fill=Sex),
  #             alpha=0.4)+
  # scale_fill_manual(values=c("red","green"))+
  guides(colour=guide_legend(title=NULL))+
  scale_colour_grey()+
  # labs(linetype="")+
  labs(x="",y="ASR")+
  # ylim(0,7)+
  scale_y_continuous(breaks=seq(0, 10, 1))+
  theme_bw()+
  theme(text=element_text(size=10),
        panel.grid.major.x=element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_text(angle = 90,vjust = 0.5)
        # axis.text.x=element_blank()
  )+
  
  theme(legend.position=c(0.5,1),legend.justification=c(0,0.85),
        legend.direction = "horizontal",
        legend.text=element_text(size=10),
        legend.key.width = unit(0.2, "in"),
        legend.key.height = unit(0.1, "in"))+
  theme(legend.background=element_blank()) + # Remove overall border
  theme(legend.key=element_blank()) # Remove border around each item
#theme(plot.margin=unit(c(0,0.01,0,0.01),"cm"))

p2=ggplot(age.case, aes(x=Age, y=Cases,fill=Sex))+
  facet_grid(. ~ style)+
  geom_bar(position = "dodge",stat="identity")+ 
  guides(fill=guide_legend(title=NULL))+
  scale_fill_grey()+
  labs(x="Age group",y="Cases")+
  theme_bw()+
  theme(text=element_text(size=10),
        #panel.grid.major.y=elment_line(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_text(angle = 90,vjust = 0.5))+
  theme(legend.position=c(0.5,1),legend.justification=c(0,0.85),
        legend.direction="horizontal",
        legend.text=element_text(size=10),
        legend.key.width = unit(0.2, "in"),
        legend.key.height = unit(0.1, "in"))+
  theme(legend.background=element_blank()) + # Remove overall border
  theme(legend.key=element_blank()) # Remove border around each item
#theme(plot.margin=unit(c(0,0.01,0,0),"cm"))
d= multiplot(p1,p2,cols=1)
ggsave("figure.png",d,width=8,height = 6,units="in")

# figure 2 fatality ratio====
ratio=read.csv("C:/paper/chenling/hk/data/fatality ratio.csv",
               header=T)
ratio$Sex=factor(ratio$Sex,levels=c("Female","Male"),ordered = T)
# ratio$ratio=as.numeric(ratio$ratio)
source("C:/paper/chenling/paper/multiplot.r") 
p1=ggplot(ratio, aes(x=factor(Period), y=ratio,fill=Sex)) +
  geom_bar(position = "dodge",stat="identity")+ 
  scale_fill_grey()+
  labs(x="Period",y="Case fatality ratio")+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+
  theme(text=element_text(size=10),
        panel.grid.major.x=element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()
  )+
  guides(fill=guide_legend(title=NULL))+
  theme(legend.position=c(0,0.95),legend.justification=c(0,0.85),
        legend.text=element_text(size=8),
        legend.key.width = unit(0.3, "in"),
        legend.key.height = unit(0.2, "in"))+
  theme(legend.background=element_blank()) + # Remove overall border
  theme(legend.key=element_blank())
ratio1=read.csv("C:/paper/chenling/hk/data/fatality ratio-ages.csv",
               header=T)
ratio1$Sex=factor(ratio1$Sex,levels=c("Total","Female","Male"),ordered = T)
ratio1$Age=factor(ratio1$Age,levels = c("30-34","35-39",
                                          "40-44","45-49","50-54","55-59",
                                          "60-64","65-69","70-74","75-79",
                                          "80-84","85+"),ordered = T)

p2=ggplot(ratio1, aes(x=factor(Age), y=Ratio,group=Sex,linetype=Sex)) +
  geom_line(size=1)+ 
  geom_point(size=2)+
 # scale_fill_grey()+
  labs(x="Age group",y="Case fatality ratio")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values=c("solid", "dotted","twodash"))+
  theme_bw()+
  theme(text=element_text(size=10),
        #panel.grid.major.x=element_blank(),
        #panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank()
  )+
  geom_hline(yintercept = 0.4)+
  guides(linetype=guide_legend(title=NULL))+
  theme(legend.position=c(0,0.95),legend.justification=c(0,0.85),
        legend.text=element_text(size=8),
        legend.key.width = unit(0.3, "in"),
        legend.key.height = unit(0.2, "in"))+
  theme(legend.background=element_blank()) + # Remove overall border
  theme(legend.key=element_blank())
p=multiplot(p1,p2,cols=2)
# Figure 3 effect====
apc=read.csv("C:/paper/chenling/hk/data/APC1.csv",
             header = T)
names(apc)
#Figure 3.1 period effect====

apc$Sex=factor(apc$Sex,
                     levels=c("Total","Female","Male"),ordered = T)
#which.max(apc$upper)
ggplot(subset(apc,Effects=="Per")
       , aes(x=Year, y=RR)) +
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.8) +
  facet_grid(Style ~ Sex,scales="free_y")+
  geom_line(size=0.5)+
   scale_y_sqrt(breaks=(0:4))+
  #xlim(1890,1985)+
  geom_hline(yintercept=1,lty=2)+
  labs(x="Year",y="Rate ratio")+
  theme_bw()+
  theme(text=element_text(size=10),
        panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        axis.text.x=element_text(angle=0))
# ggsave("figure3.pdf",width=6,height = 4,units="inch")
ggplot(subset(apc,Effects=="Coh" & Year < 1985)
       , aes(x=Year, y=RR)) +
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.8) +
  scale_y_sqrt(breaks=(0:10))+
  #scale_y_log10(breaks=10^(0.1:10))+
  facet_grid(Style ~ Sex,scales="free_y")+
  geom_line(size=0.5)+
  xlim(1890,1985)+
  #ylim(0,9)+
  geom_hline(yintercept=1,lty=2)+
  labs(x="Cohort birth year",y="Rate ratio")+
  theme_bw()+
  theme(text=element_text(size=10),
        panel.grid.major=element_blank(),
       # panel.grid.minor=element_blank(),
        axis.text.x=element_text(angle=0))



# rate-plot figures====
rateplot(bl.rate)
attach(incidence.female)
as.numeric(incidence.female$D)
as.numeric(incidence.female$Y)

bl.rate <- tapply(D, list(A,P), sum ) /tapply(Y, list(A,P), sum )
bl.rate

par( mfrow=c(2,2) )
a= rateplot( bl.rate*10^6 )

png("a.png")







library(agricolae)
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
out <- LSD.test(model,"virus", p.adj="bonferroni")














CMdata=read.csv("C:/paper/chenling/hk/data/rawdata.csv",header=T)
cmdata0=CMdata[,c("A","P","YT","IT")]
names(cmdata0)[3:4]=c("Y","D")
apc.result1=apc.fit(cmdata0,
                    npar=7, model="ns",
                    parm="APC",
                    ref.c = 1960.5,
                    ref.p = 2000,
                    scale=10^5)
plot.apc(apc.result1,ci=T,
         r.txt = "Rate per 100,000 person-years")

incidence.fit=apc.fit(incidence,
                      A=incidence$A,
                      P=incidence$P,
                      D=incidence$D,
                      Y=incidence$Y,
                  npar=7, model="ns",
                  parm="APC",
                  ref.c = 1960.5,
                  ref.p = 2000,
                  scale=10^5)
plot.apc(incidence.fit,ci=T,
         r.txt = "Rate per 100,000 person-years")

 all(cmdata0$A==incidence$A)
 all(cmdata0$P==incidence$P)
 all(cmdata0$D==incidence$D)
 all(cmdata0$Y==incidence$Y)
 
summary(dbase)
cor(cbi[,5:16]) #Arbitrairement, utiliser fin aver et CBIE semble 
set<-dbase
set<-set[which(set$year<1996),]
set<-na.omit(set)
set<-set[-which(set$exec==0 | set$exec==-999), ]
set$exec<-as.factor(set$exec)   

summary(set)

#CBI inflation en Europe, plm
#Should I run fixed or random effects ?
#Durbin Wu Hausman test evaluates that.
phtest(form, data=setEU,method="chisq")
#If H0 is rejected, there is one model that is inconsistent, it will be the random one ->H1 : Use FE
#If it isn't rejected, both are supposedly consistent, but fixed will be inconsistent. ->H0 : Use RE


pdwtest(decomp,data=set)#Check for serial correlation

aggregate(setEU$inf,by=list(setEU$country),FUN=sd)%>%plot

bptest(decompb,data=new,studentize=TRUE) #Check for time heteroskedasticity, default studentize is true (more robust)



setEU<-set[which(set$cont=="europe"),]
setAS<-set[which(set$cont=="asia"),]
setAF<-set[which(set$cont=="africa"),]
setN_A<-set[which(set$cont=="northam"),]

set$execrlc<-as.factor(set$execrlc)
ocde <- c("Canada","USA","UK","Denmark","Iceland","Norway","Turkey","Spain",
               "Portugal","France","Ireland","Belgium","Germany","Greece","Sweden","Switzerland","Austria",
               "Netherlands","Luxembourg","Italy","Japan","Finland","Australia","New Zealand","Mexico","Czech Republic")




plot(setEU$inf)
options(scipen=6)
set<-droplevels(set)

aggregate(set$country,by=list(set$cont),FUN=count)
d1 <- (aggregate(log(set$gpc),by=list(set$country),FUN=mean)) #Income per capita mean
d2 <- (aggregate(log(set$gdp),by=list(set$country),FUN=mean)) #Gdp mean

density(d1$x) %>% plot#Income per capita kernel
density(d2$x) %>% plot#GDP kernel


set$exec<-as.character(set$exec)
set$exec<-as.factor(set$exec)
summary(set$exec)
set <- within(set, exec <- relevel(exec, ref = "Center"))


#_____________________________________????___________________________________________# 

#Pas vraiment utilisé, je ne saisis plus la pertinence de la variable gpc plutot que gdp et pop 
panel <- plm(modgpc,model="within",effect='time',set)
summary(panel,vcov=vcovBK(panel,cluster='time'))
avg_slopes(panel)
plot_slopes(panel,variables='obj',condition="exec",vcov=vcovBK(panel,cluster='time'))




#_____________________________________1______________________________________________# 

#Baseline regressions for the whole set with various methods
inf ~ (1 + exec) * (CBIE) + gpc + hyp + laginf
setocde <- set[which(set$country %in% ocde),]
panel <- plm(inf ~ ( exec) * (CBIE) + gpc + hyp + laginf, method='within', effect="twoways",data=setocde)
summary(panel,vcov=vcovBK(panel,cluster="time"))
# CLUSTER BY TIME TO ACCOUNT FOR CROSS-SECTIONAL HETEROSKEDASTICITY
panelvcov<-vcovBK(panel,cluster="time")
sepan<-sqrt(diag(panelvcov))
stargazer(panel,type="text",se=list(sepan),title="Régression sur l'échantillon principal")

p <- avg_slopes(panel,by="exec",vcov=vcovBK(panel,cluster='time'),newdata=setocde)
print(p[which(p$p.value<0.11),],style="data.frame")



plot_slopes(panel,variables='obj',condition="exec",vcov=vcovBK(panel,cluster='time'))+
  ggtitle("Effet marginal d'indépendance des objectifs - Intervalle à 5%")+
  geom_hline(yintercept=0)



#_____________________________________2______________________________________________# 

#We only run regressions separating left, Center and right - POLITY_LESS VERSIONS
stock <- set
set <- setocde
setleft<-set[which(set$exec=="Left"),]
setright <- set[which(set$exec=="Right"),]
setcenter <- set[which(set$exec=="Center"),]

leftpanel <- plm(dexec,model = "within",effect='twoways',data = setleft)
rightpanel <- plm(dexec,model="within",effect="twoways",data=setright)
centerpanel<- plm(dexec,model="within",effect="twoways",data=setcenter)

VCleft<-vcovBK(leftpanel,cluster='time')
VCright<-vcovBK(rightpanel,cluster='time')
VCcenter<-vcovBK(centerpanel,cluster='time')
seleft<-sqrt(diag(VCleft))
seright<-sqrt(diag(VCright))
secenter<-sqrt(diag(VCcenter))
stargazer(leftpanel,rightpanel,centerpanel,
          column.labels =c('Left','Right','Center'),
          type = "text",title="Régressions par orientation",
          se = list(seleft,seright,secenter))
avg_slopes(leftpanel,vcov=vcovBK(leftpanel,cluster='time') )
avg_slopes(rightpanel,vcov=vcovBK(rightpanel,cluster='time') )
avg_slopes(centerpanel,vcov=vcovBK(centerpanel,cluster='time') )
#_____________________________________3______________________________________________# 

#We only run regressions separating left, Center and right - POLITY VERSIONS

data<-new
dataleft<-data[which(data$exec=="Left"),]
dataright <- data[which(data$exec=="Right"),]
datacenter <- data[which(data$exec=="Center"),]
leftpanel <- plm(unidimpol2,model = "within",effect='twoways',data = dataleft)
rightpanel <- plm(unidimpol2,model="within",effect="twoways",data=dataright)
centerpanel<- plm(unidimpol,model="within",effect="twoways",data=datacenter)
VCleft<-vcovBK(leftpanel,cluster='time')
VCright<-vcovBK(rightpanel,cluster='time')
VCcenter<-vcovBK(centerpanel,cluster='time')
seleft<-sqrt(diag(VCleft))
seright<-sqrt(diag(VCright))
secenter<-sqrt(diag(VCcenter))
stargazer(leftpanel,rightpanel,centerpanel,
          column.labels =c('Left','Right','Center'),
          type = "text",title="Régressions par orientation",
          se = list(seleft,seright,secenter))

avg_slopes(leftpanel,vcov=vcovBK(leftpanel,cluster='time'))
avg_slopes(centerpanel,vcov=vcovBK(centerpanel,cluster='time'))
avg_slopes(rightpanel,vcov=vcovBK(rightpanel,cluster='time'))
plot_slopes(leftpanel,variables='CBIE',condition='polity',vcov=vcovBK(leftpanel,cluster='time'))+
  ggtitle('left')
plot_slopes(rightpanel,variables='CBIE',condition='polity',vcov=vcovBK(rightpanel,cluster='time'))+
  ggtitle('right')
plot_slopes(centerpanel,variables='CBIE',condition='polity',vcov=vcovBK(centerpanel,cluster='time'))+
  ggtitle('center')

hist(datacenter$polity)
cor(new$gpc,new$polity)

#_____________________________________4______________________________________________# 


#GDP per capita ( GPC ) differentiated dataset
#dataset
set$exec<-relevel(set$exec,ref="Center")
meansgpc<-aggregate(set$gpc,by=list(set$country),FUN=mean)
quant<-quantile(meansgpc$x,probs=seq(0,1,0.25))
lset1b<-list("name"=meansgpc$Group.1[meansgpc$x<quant[2]])
set1b <- set[which(set$country %in% lset1b$name),]
lset2b <- list("name" = meansgpc$Group.1[which(meansgpc$x>= quant[2] &meansgpc$x<quant[3])])
set2b <- set[which(set$country %in% lset2b$name),]
lset3b <- list("name" = meansgpc$Group.1[which(meansgpc$x>= quant[3] &meansgpc$x<quant[4])])
set3b <- set[which(set$country %in% lset3b$name),]
lset4b <- list("name" = meansgpc$Group.1[which(meansgpc$x>= quant[4] &meansgpc$x<quant[5])])
set4b <- set[which(set$country %in% lset4b$name),]

#model
panel1b <- plm(unidim,model = "within",effect='twoways',data = set1b)
panel2b <- plm(unidim,model = "within",effect='twoways',data = set2b)
panel3b <- plm(unidim,model = "within",effect='twoways',data = set3b)
panel4b <- plm(unidim2,model = "within",effect='twoways',data = set4b) #n'inclut pas hyp car pas d'observations hyp
#VC Matrix
vcovpanel1b<-vcovBK(panel1b,cluster='time')
vcovpanel2b<-vcovBK(panel2b,cluster='time')
vcovpanel3b<-vcovBK(panel3b,cluster='time')
vcovpanel4b<-vcovBK(panel4b,cluster='time')
se1b<-sqrt(diag(vcovpanel1b))
se2b<-sqrt(diag(vcovpanel2b))
se3b<-sqrt(diag(vcovpanel3b))
se4b<-sqrt(diag(vcovpanel4b))


stargazer(panel1b,panel2b,panel3b,panel4b, type = "text",title="Régressions par quantile de revenu par personne",
          se = list(se1b,se2b,se3b,se4b))

plot_slopes(panel1b,variables='CBIE',condition="exec",vcov=vcovBK(panel1b,cluster='time'))+
  ggtitle("Régressions par quantile de revenu par personne - groupe 1" )+geom_hline(yintercept=2.8)
plot_slopes(panel2b,variables='CBIE',condition="exec",vcov=vcovBK(panel2b,cluster='time'))+
  ggtitle("Régressions par quantile de revenu par personne - groupe 2" )
plot_slopes(panel3b,variables='CBIE',condition="exec",vcov=vcovBK(panel3b,cluster='time'))+
  ggtitle("Régressions par quantile de revenu par personne - groupe 3" )
plot_slopes(panel4b,variables='CBIE',condition="exec",vcov=vcovBK(panel4b,cluster='time'))+
  ggtitle("Régressions par quantile de revenu par personne - groupe 4" )

#bandes de confiance à 5%
avg_slopes(panel1b,variables="CBIE",by='exec',vcov = vcovBK(panel1b,cluster='time'),newdata = set1b)
avg_slopes(panel2b,variables="CBIE",by='exec',vcov = vcovBK(panel2b,cluster='time'),newdata = set2b)
avg_slopes(panel3b,variables="CBIE",by='exec',vcov = vcovBK(panel3b,cluster='time'),newdata = set3b)
avg_slopes(panel4b,variables="CBIE",by='exec',vcov = vcovBK(panel4b,cluster='time'),newdata = set4b)



#_____________________________________5______________________________________________# 

#Dividing per group of polity scale (Democracy - Autocracy)
meanspol<-aggregate(new$polity,by=list(new$country),FUN=mean)
quant<-quantile(meanspol$x,probs=seq(0,1,0.25))
lnew1<-list("name"=meanspol$Group.1[meanspol$x<quant[2]])
new1 <- new[which(new$country %in% lnew1$name),]
lnew2 <- list("name" = meanspol$Group.1[which(meanspol$x>= quant[2] &meanspol$x<quant[3])])
new2 <- new[which(new$country %in% lnew2$name),]
lnew3 <- list("name" = meanspol$Group.1[which(meanspol$x>= quant[3] &meanspol$x<quant[4])])
new3 <- new[which(new$country %in% lnew3$name),]
lnew4 <- list("name" = meanspol$Group.1[which(meanspol$x>= quant[4] &meanspol$x<=quant[5])])
new4 <- new[which(new$country %in% lnew4$name),]

panel1 <- plm(unidim,model = "within",effect='twoways',data = new1)
panel2 <- plm(unidim,model = "within",effect='twoways',data = new2)
panel3 <- plm(unidim,model = "within",effect='twoways',data = new3)
panel4 <- plm(unidim2,model = "within",effect='twoways',data = new4)

# CLUSTER BY TIME TO ACCOUNT FOR CROSS-SECTIONAL HETEROSKEDASTICITY
vcovpanel2<-vcovBK(panel2,cluster='time')
vcovpanel1<-vcovBK(panel1,cluster='time')
vcovpanel3<-vcovBK(panel3,cluster='time')
vcovpanel4<-vcovBK(panel4,cluster='time')

se1<-sqrt(diag(vcovpanel1))
se2<-sqrt(diag(vcovpanel2))
se3<-sqrt(diag(vcovpanel3))
se4<-sqrt(diag(vcovpanel4))

stargazer(panel1,panel2,panel3,panel4, type = "text",title="Régressions par quantile de polity",
          se = list(se1,se2,se3,se4))

avg_slopes(panel1,variables="CBIE",by='exec',vcov = vcovBK(panel1,cluster='time'),newdata = new1)
avg_slopes(panel2,variables="CBIE",by='exec',vcov = vcovBK(panel2,cluster='time'),newdata = new2)
avg_slopes(panel3,variables="CBIE",by='exec',vcov = vcovBK(panel3,cluster='time'),newdata = new3)
avg_slopes(panel4,variables="CBIE",by='exec',vcov = vcovBK(panel4,cluster='time'),newdata = new4)



#_____________________________________6______________________________________________# 
#Baseline regressions for the whole set with various methods
panel <- plm(decomp, method='within', effect="twoways",data=set)
summary(panel,vcov=vcovBK(panel,cluster="time"))
# CLUSTER BY TIME TO ACCOUNT FOR CROSS-SECTIONAL HETEROSKEDASTICITY
panelvcov<-vcovBK(panel,cluster="time")
sepan<-sqrt(diag(panelvcov))
stargazer(panel,type="text",se=list(sepan),title="Régression sur l'échantillon principal")
avg_slopes(panel,by="exec",vcov=vcovBK(panel,cluster='time'),newdata=set)
plot_slopes(panel,variables='obj',condition="exec",vcov=vcovBK(panel,cluster='time'))+
  ggtitle("Effet marginal d'indépendance des objectifs - Intervalle à 5%")+
  geom_hline(yintercept=0)




#_____________________________________7______________________________________________# 

#Par GPC avec CBIE multidimensionel - model (decomp)


#model
panel1b <- plm(decomp,model = "within",effect='twoways',data = set1b)
panel2b <- plm(decomp,model = "within",effect='twoways',data = set2b)
panel3b <- plm(decomp,model = "within",effect='twoways',data = set3b)
panel4b <- plm(decomp2,model = "within",effect='twoways',data = set4b) #n'inclut pas hyp car pas d'observations hyp
#VC Matrix
vcovpanel1b<-vcovBK(panel1b,cluster='time')
vcovpanel2b<-vcovBK(panel2b,cluster='time')
vcovpanel3b<-vcovBK(panel3b,cluster='time')
vcovpanel4b<-vcovBK(panel4b,cluster='time')
se1b<-sqrt(diag(vcovpanel1b))
se2b<-sqrt(diag(vcovpanel2b))
se3b<-sqrt(diag(vcovpanel3b))
se4b<-sqrt(diag(vcovpanel4b))

stargazer(panel1b,panel2b, type = "text",title="Régressions par quantile de revenu par personne",
          se = list(se1b,se2b))
stargazer(panel1b,panel2b,panel3b,panel4b, type = "text",title="Régressions par quantile de revenu par personne",
          se = list(se1b,se2b,se3b,se4b))
#Certains coefficients ne sont pas calculés dans ce tableau, probablement par colinéarité trop forte.
#Regardons les matrices de covariance pour voir quelles variables sont redondantes.
rquery.cormat(set1b[,12:17])
rquery.cormat(set2b[,12:17])
rquery.cormat(set3b[,12:17])
rquery.cormat(set4b[,12:17])
summary(panel1b)
plot_slopes(panel1b,condition="exec",vcov=vcovBK(panel1b,cluster='time'))+
  ggtitle("Régressions par quantile de revenu par personne - groupe 1" )
plot_slopes(panel2b,variables='CBIE',condition="exec",vcov=vcovBK(panel2b,cluster='time'))+
  ggtitle("Régressions par quantile de revenu par personne - groupe 2" )
plot_slopes(panel3b,variables='CBIE',condition="exec",vcov=vcovBK(panel3b,cluster='time'))+
  ggtitle("Régressions par quantile de revenu par personne - groupe 3" )
plot_slopes(panel4b,variables='CBIE',condition="exec",vcov=vcovBK(panel4b,cluster='time'))+
  ggtitle("Régressions par quantile de revenu par personne - groupe 4" )

#bandes de confiance à 5%
p <- avg_slopes(panel1b,by='exec',vcov = vcovBK(panel1b,cluster='time'),newdata = set1b)#,hypothesis="b4-b8=0")
p <- avg_slopes(panel2b,by='exec',vcov = vcovBK(panel2b,cluster='time'),newdata = set2b)
p <- avg_slopes(panel3b,by='exec',vcov = vcovBK(panel3b,cluster='time'),newdata = set3b)
p <- avg_slopes(panel4b,by='exec',vcov = vcovBK(panel4b,cluster='time'),newdata = set4b)
p

names(panel1b$coefficients)
print(p[which(p$p.value<0.01),],style="data.frame")
set4b %>% ggplot(aes(year,CBIE,group=country,color=country))+geom_point()+geom_line()
#Issue with this model ~ It can't evaluate AME if there are estimates lacking.
#Instead of having four groups, we could try with two.
setb1 <- rbind(set1b,set2b)
setb2 <- rbind(set3b,set4b)
setb1 <- pdata.frame(setb1)
setb2 <- pdata.frame(setb2)

panelb1 <- plm(decomp,model = "within",effect='twoways',data = setb1)
panelb2 <- plm(decomp,model = "within",effect='twoways',data = setb2)
p <- avg_slopes(panelb1,by='exec',vcov = vcovBK(panelb1,cluster='time'),newdata = setb1)
print(p[which(p$p.value<0.11),],style="data.frame")
p <- avg_slopes(panelb2,by='exec',vcov = vcovBK(panelb2,cluster='time'),newdata = setb2)
print(p[which(p$p.value<0.11),],style="data.frame")
options(scipen=999)

#_____________________________________8______________________________________________# 
#Multidimensional CBIE separated per party

leftpanel <- plm(dimexec,model = "within",effect='twoways',data = setleft)
rightpanel <- plm(dimexec,model="within",effect="twoways",data=setright)
centerpanel<- plm(dimexec,model="within",effect="twoways",data=setcenter)

VCleft<-vcovBK(leftpanel,cluster='time')
VCright<-vcovBK(rightpanel,cluster='time')
VCcenter<-vcovBK(centerpanel,cluster='time')
seleft<-sqrt(diag(VCleft))
seright<-sqrt(diag(VCright))
secenter<-sqrt(diag(VCcenter))
stargazer(leftpanel,rightpanel,centerpanel,
          column.labels =c('Left','Right','Center'),
          type = "text",title="Régressions par orientation",
          se = list(seleft,seright,secenter))


p <- avg_slopes(leftpanel,vcov = vcovBK(leftpanel,cluster='time'),newdata = setleft)
p[which(p$p.value<0.11),]
p <- avg_slopes(rightpanel,vcov = vcovBK(rightpanel,cluster='time'),newdata = setright)
p[which(p$p.value<0.11),]
p <- avg_slopes(centerpanel,vcov = vcovBK(centerpanel,cluster='time'),newdata = setcenter)
p[which(p$p.value<0.11),]

plot_slopes(leftpanel,variables='obj',condition="gpc",vcov=vcovBK(leftpanel,cluster='time'))+
  ggtitle("Régressions par quantile de revenu par personne - groupe 4" )




#_____________________________________9______________________________________________# 
#Multidimensional CBIE separated per party - with polity
leftpanel <- plm(multidimpol,model = "within",effect='twoways',data = dataleft)
rightpanel <- plm(multidimpol,model="within",effect="twoways",data=dataright)
centerpanel<- plm(multidimpol2,model="within",effect="twoways",data=datacenter)
VCleft<-vcovBK(leftpanel,cluster='time')
VCright<-vcovBK(rightpanel,cluster='time')
VCcenter<-vcovBK(centerpanel,cluster='time')
seleft<-sqrt(diag(VCleft))
seright<-sqrt(diag(VCright))
secenter<-sqrt(diag(VCcenter))
stargazer(leftpanel,rightpanel,centerpanel,
          column.labels =c('Left','Right','Center'),
          type = "text",title="Régressions par orientation",
          se = list(seleft,seright,secenter))

p <- avg_slopes(leftpanel,vcov=vcovBK(leftpanel,cluster='time'))
p[which(p$p.value<0.11),]
p <- avg_slopes(rightpanel,vcov=vcovBK(rightpanel,cluster='time'))
p[which(p$p.value<0.11),]
p <- avg_slopes(centerpanel,vcov=vcovBK(centerpanel,cluster='time'))
p[which(p$p.value<0.11),]



plot_slopes(leftpanel,variables='rep',condition='polity',vcov=vcovBK(leftpanel,cluster='time'))+
  ggtitle('left-rep')
plot_slopes(rightpanel,variables='obj',condition='polity',vcov=vcovBK(rightpanel,cluster='time'))
plot_slopes(centerpanel,variables='lend',condition='polity',vcov=vcovBK(centerpanel,cluster='time'),
            conf_level = 0.95)+
  ggtitle('center-lend 10%')

ggplot(data.frame(fulldata), aes(x =gpc , y = exec, fill = exec)) +
  geom_density_ridges() +
  theme_ridges() + 
  ggtitle("Comparaison des densités de revenus par habitant  ")+ 
  theme(legend.position = "none")+
  scale_fill_viridis_d(option='G')

dataright %>% ggplot(aes(lend,inf))+geom_point()




#_____________________________________10_____________________________________________# 
#Separate higher and lower polity et
data <- new
apoldata<-data[which(data$polity<=0),]
poldata<-data[which(data$polity>0),]
ggplot(data.frame(poldata), aes(x = CBIE, y = exec, fill = exec)) +
  geom_density_ridges() +
  theme_ridges() + 
  ggtitle("Comparaison des densités de CBIE-poldata")
  theme(legend.position = "none")

setleft<-poldata[which(poldata$exec=="Left"),]
setright <- poldata[which(poldata$exec=="Right"),]
setcenter <- poldata[which(poldata$exec=="Center"),]

leftpanel <- plm(dexec,model = "within",effect='twoways',data = setleft)
rightpanel <- plm(dexec,model="within",effect="twoways",data=setright)
centerpanel<- plm(dexec,model="within",effect="twoways",data=setcenter)

VCleft<-vcovBK(leftpanel,cluster='time')
VCright<-vcovBK(rightpanel,cluster='time')
VCcenter<-vcovBK(centerpanel,cluster='time')
seleft<-sqrt(diag(VCleft))
seright<-sqrt(diag(VCright))
secenter<-sqrt(diag(VCcenter))
stargazer(leftpanel,rightpanel,centerpanel,
          column.labels =c('Left','Right','Center'),
          type = "text",title="Régressions par orientation- Polity Index bas (poldata)",
          se = list(seleft,seright,secenter))             
             
#___________________________11___________________________________#


data <- fulldata[,-1]
data <- pdata.frame(data,index=c('country','year'))
data$leadinf <- lead(data$inf,1L)
data$leadinf <- log(data$leadinf)
data$inf <- log(data$inf)

data <- data[which(is.na(data$inf)==FALSE),]
data <- data[which(is.na(data$leadinf)==FALSE),]






#___________________________11____________________________________#
library(panelvar)
#,index=c('country','year')
fdata <- data.frame(fulldata[c('country','year','CBIE','inf','exec'
                               ,'gpc','hyp','fin','lend','board','obj',
                               'policy','rep')])
fdata$year <- as.numeric(fdata$year)
fdata$country <- as.character(fdata$country)

bfdata <- makeBalancedPanel(fdata, idname='country', tname='year', return_data.table = FALSE)

mod <- pvarfeols(dependent_vars =c('fin','lend','board','obj','policy','rep','inf'),
                 lags = 2,
                 exog_vars=c('gpc'),
                 data =bfdata,
                 panel_identifier = c("country","year"))
summary(mod)
irfmod <- girf(model=mod, n.ahead=4,ma_approx_steps=4)
cl <- bootstrap_irf(model=mod,
                    n.ahead=4,
                    typeof_irf = "IRF",
                    confidence.band=0.95,
                    nof_Nstar_draws = 100,
                    mc.cores = 1)

plot(irfmod)





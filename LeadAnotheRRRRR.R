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
phtest(decomp, data=newb,method="chisq")
#If H0 is rejected, there is one model that is inconsistent, it will be the random one ->H1 : Use FE
#If it isn't rejected, both are supposedly consistent, but fixed will be inconsistent. ->H0 : Use RE

#Check for serial correlation
#If there is serial correlation, woolridge advises to use first-difference (cf10.7.1)
pdwtest(decompb,data=newb,model="within")
?pdwtest
#Applying Woolridge test comparing first difference and Fixed-effects
pdwtest(multidimpol,data=newb,h0="fe")
aggregate(setEU$inf,by=list(setEU$country),FUN=sd)%>%plot
unidimpol <- leadinf ~ (1 + polity) * (CBIE) + gpc + inf 
multidimpol <- leadinf ~ (1 + polity) * (board + policy + obj + lend + fin +rep) + gpc +inf+lag(inf)+lag(inf,2)
unidimb  <- leadinf ~ (1+exec)*(CBIE)+gpc+lag(inf)

bptest(multidimpol,data=setright,studentize=TRUE) #Check for time heteroskedasticity, default studentize is true (more robust)


unidim <- leadinf ~ exec * CBIE + gpc + inf + polity+ (year)
panel <- plm(unidim, method='within', effect="twoways",data=newb)
panel%>% summary


setEU<-set[which(set$cont=="europe"),]
setAS<-set[which(set$cont=="asia"),]
setAF<-set[which(set$cont=="africa"),]
setN_A<-set[which(set$cont=="northam"),]

set$execrlc<-as.factor(set$execrlc)




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


quantinf
data <- data[]
setA <- read_csv("data974.csv")
setA <- pdata.frame(setA[,-1])
setB <- read_csv("data925.csv")
setB <- pdata.frame(setB[,-1])

#_____________________________________1______________________________________________# 
#Baseline regressions for the whole set with various methods
data$exec <- as.factor(data$exec)
unidim<- leadinf ~  exec * CBIE 
unidimb <- leadinf ~ exec*CBIE +gpc +  inf + polity

panela <- plm(unidim, method='within', effect="twoways",data=setA)
panelb <- plm(unidimb, method='within', effect="twoways",data=setB)


# CLUSTER BY TIME TO ACCOUNT FOR CROSS-SECTIONAL HETEROSKEDASTICITY
sebaseline <- sqrt(diag(panel$vcov))
panelvcova<-vcovBK(panela,cluster="time")
sepana<-sqrt(diag(panelvcova))
panelvcovb<-vcovBK(panelb,cluster="time")
sepanb<-sqrt(diag(panelvcovb))


stargazer(panela,panela,panelb,type="latex",se=list(sebaseline,sepana,sepanb),title="Régression sur l'échantillon principal")


pa <-avg_slopes(panela,by='exec',vcov=vcovBK(panela,cluster='time'),newdata = setA)
pb <-avg_slopes(panelb,by='exec',vcov=vcovBK(panelb,cluster='time'),newdata = setB)
pa


pa <- pa[,c(1,3,4,7)]
pb <- pb[,c(1,3,4,7)]

print(pa[which(pa$term %in% dime),],style="data.frame")
print(pb[which(pb$term %in% dime),],style="data.frame")

print(xtable(pa[which(pa$term %in% dime),], type = "latex",caption="Effets marginaux - modèle 1 (2)"), file = "pa.tex")
print(xtable(pb[which(pb$term %in% dime),], type = "latex",caption="Effets marginaux - modèle 1 (3)"), file = "pb.tex")





plot_slopes(panel,variables='obj',condition="exec",vcov=vcovBK(panel,cluster='time'))+
  ggtitle("Effet marginal d'indépendance des objectifs - Intervalle à 5%")+
  geom_hline(yintercept=0)


#_____________________________________2______________________________________________# 

#We only run regressions separating left, Center and right - POLITY_LESS VERSIONS
setsave <- set #save some version.
set <- setB
setleft<-set[which(set$exec=="Left"),]
setright <- set[which(set$exec=="Right"),]
setcenter <- set[which(set$exec=="Center"),]


dexec <- leadinf ~ CBIE 


dexec <- leadinf ~ CBIE* gpc 
dexecb<- leadinf ~ CBIE * gpc*polity+ inf

dexec <- leadinf ~ CBIE*polity
dexecpol <- leadinf ~ CBIE*polity * gpc + inf 

dexecgpcpol <- leadinf ~ CBIE*polity* gpc + inf 


leftpanel <- plm(dexecb,model = "within",effect='twoways',data = setleft)
rightpanel <- plm(dexecb,model="within",effect="twoways",data=setright)#,vcov=vcovBK(rightpanel,cluster='time'))
centerpanel<- plm(dexecb,model="within",effect="twoways",data=setcenter)#,vcov=vcovBK(centerpanel,cluster='time'))



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

p1 <- avg_slopes(leftpanel,vcov=vcovBK(leftpanel,cluster='time'),newdata = setleft)
p2 <- avg_slopes(centerpanel,vcov=vcovBK(centerpanel,cluster='time'),newdata = setcenter)
p3 <- avg_slopes(rightpanel,vcov=vcovBK(rightpanel,cluster='time'),newdata = setright)

p1 <- p1[,c(1,3,4,6)]
p2 <- p2[,c(1,3,4,6)]
p3 <- p3[,c(1,3,4,6)]
print(p1[which(p1$term %in% dime),],style="data.frame")
print(p2[which(p2$term %in% dime),],style="data.frame")
print(p3[which(p3$term %in% dime),],style="data.frame")

print(xtable(p1[which(p1$term %in% dime),], type = "latex",caption="left"), file = "p1.tex")
print(xtable(p2[which(p2$term %in% dime),], type = "latex",caption="center"), file = "p2.tex")
print(xtable(p3[which(p3$term %in% dime),], type = "latex",caption="right"), file = "p3.tex")

mean(setcenter$polity)
mean(setright$polity)
mean(setleft$polity)
plot_slopes(leftpanel,variables='CBIE',condition=c('polity'),vcov=vcovBK(leftpanel,cluster='time'))+ ggtitle('left')
plot_slopes(centerpanel,variables='CBIE',condition=c('polity'),vcov=vcovBK(centerpanel,cluster='time'))+ ggtitle('center')
plot_slopes(rightpanel,variables='CBIE',condition=c('polity'),vcov=vcovBK(rightpanel,cluster='time'))+ ggtitle('right')

p <- p2[which(p2$p.value<0.11),c(1,3,4,7)]
p <- p3[which(p3$p.value<0.11),c(1,3,4,7)]
ptot <- rbind(p1,p2,p3)



#_____________________________________3______________________________________________# 

#We only run regressions separating left, Center and right - POLITY VERSIONS


data<-newb
dataleft<-data[which(data$exec=="Left"),]
dataright <- data[which(data$exec=="Right"),]
datacenter <- data[which(data$exec=="Center"),]
leftpanel <- plm(unidimpol,model = "within",effect='twoways',data = dataleft)
rightpanel <- plm(unidimpol,model="within",effect="twoways",data=dataright)
centerpanel<- plm(unidimpol,model="within",effect="twoways",data=datacenter)
VCleft<-vcovBK(leftpanel,cluster='time')
VCright<-vcovBK(rightpanel,cluster='time')
VCcenter<-vcovBK(centerpanel,cluster='time')
seleft<-sqrt(diag(VCleft))
seright<-sqrt(diag(VCright))
secenter<-sqrt(diag(VCcenter))
stargazer(leftpanel,rightpanel,centerpanel,
          column.labels =c('Left','Right','Center'),
          type = "latex",title="Régressions par orientation",
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
set <- data
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
  ggtitle("Régressions par quantile de revenu par personne - groupe 1" )+geom_hline(yintercept=0)
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
meanspol<-aggregate(newb$polity,by=list(newb$country),FUN=mean)
quant<-quantile(meanspol$x,probs=seq(0,1,0.25))
lnew1<-list("name"=meanspol$Group.1[meanspol$x<quant[2]])
new1 <- newb[which(newb$country %in% lnew1$name),]
lnew2 <- list("name" = meanspol$Group.1[which(meanspol$x>= quant[2] &meanspol$x<quant[3])])
new2 <- newb[which(newb$country %in% lnew2$name),]
lnew3 <- list("name" = meanspol$Group.1[which(meanspol$x>= quant[3] &meanspol$x<quant[4])])
new3 <- newb[which(newb$country %in% lnew3$name),]
lnew4 <- list("name" = meanspol$Group.1[which(meanspol$x>= quant[4] &meanspol$x<=quant[5])])
new4 <- newb[which(newb$country %in% lnew4$name),]

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


ggplot(aes(polity,gpc),data=newb)+geom_point()
#____________________________________6______________________________________________# 
dime <- list('CBIE','board','policy','obj','rep','fin','lend')
 
data <- setB
qt <- quantile(data$inflation,0.95)
data <- data[which(data$inflation<qt),]



#Multidimensionnel
#Baseline regressions for the whole set with various methods
decomp <- leadinf ~ (exec) * (board + policy + obj + lend + fin + rep) 
decompb <- leadinf ~ (exec) * (board + policy + obj + lend + fin + rep) + gpc + polity + inf

panel <- plm(decompb, method='within', effect="twoways",data=data)

# CLUSTER BY TIME TO ACCOUNT FOR CROSS-SECTIONAL HETEROSKEDASTICITY
panelvcov<-vcovBK(panel,cluster="time")
sepan<-sqrt(diag(panelvcov))
stargazer(panel,type="text",se=list(sepan),title="Régression sur l'échantillon principal")

p <- avg_slopes(panel,by="exec",vcov=vcovBK(panel,cluster='time'),newdata = data)
p <- p[which(p$p.value<=0.10),c(1,3,4,7)]
print(p[which(p$term %in% dime),],style="data.frame")
print(xtable(p[which(p$term %in% dime),1:4], type = "latex"), file = "p.tex")
xtable(p[which(p$term %in% dime),c(1,3,4,5,7)], type = "latex")

plot_slopes(panel,variables='obj',condition="exec",vcov=vcovBK(panel,cluster='time'))+
  ggtitle("Effet marginal d'indépendance des objectifs - Intervalle à 5%")+
  geom_hline(yintercept=0)


#________________________#


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

#Divisons plutot en deux groupes

setb1 <- pdata.frame(rbind(set1b,set2b),index=c('country','year'))
setb2 <- pdata.frame(rbind(set3b,set4b),index=c('country','year'))
summary(setb1)
panelb1 <- plm(decomp2,model = "within",effect='twoways',data = setb1)
panelb2 <- plm(decomp2,model = "within",effect='twoways',data = setb2)
p <- avg_slopes(panelb1,by='exec',vcov = vcovBK(panelb1,cluster='time'),newdata = setb1)
print(p[which(p$p.value<0.11),],style="data.frame")
p <- avg_slopes(panelb2,by='exec',vcov = vcovBK(panelb2,cluster='time'),newdata = setb2)
print(p[which(p$p.value<0.11),],style="data.frame")

summary(panelb2)




plot_slopes(panelb1,variables='obj',condition="exec",vcov=vcovBK(panel,cluster='time'))+
  +   ggtitle("Effet marginal d'indépendance des objectifs - Intervalle à 5%")+
  +   geom_hline(yintercept=0)







#_____________________________________7______________________________________________# 
#Multidimensional CBIE separated per party

hist(data$inflation)

data <- setB
qt <- quantile(data$inflation,0.95)
qt <- 300
data <- data[which(data$inflation<qt),]
dim(data)

dim(setB)
hist(setcenter$inflation)
sqrt(var(setcenter$leadinf))

set <- data
setleft<-set[which(set$exec=="Left"),]
setright <- set[which(set$exec=="Right"),]
setcenter <- set[which(set$exec=="Center"),]

dimexec <- leadinf ~ (board + policy + obj + lend + fin + rep) 
dimexecb <- leadinf ~ (board + policy + obj + lend + fin + rep) + polity + gpc + inf
partinc <- leadinf ~ (board + policy + obj + lend + fin + rep)* gpc + polity  + inf
partdem <- leadinf ~ (board + policy + obj + lend + fin + rep)* polity + gpc + inf
partdemb <- leadinf ~ (board + policy + obj + lend + fin +rep )* polity *gpc + inf

leftpanel1 <- plm(dimexecb,model = "within",effect='twoways',data = setleft)
rightpanel1 <- plm(dimexecb,model="within",effect="twoways",data=setright)
centerpanel1<- plm(dimexecb,model="within",effect="twoways",data=setcenter)

leftpanel2 <- plm(dimexecb,model = "within",effect='twoways',data = setleft)
rightpanel2 <- plm(dimexecb,model="within",effect="twoways",data=setright)
centerpanel2<- plm(dimexecb,model="within",effect="twoways",data=setcenter)

#rquery.cormat(setcenter[,4:9])
#summary()
VCleft1<-vcovBK(leftpanel1,cluster='time')
VCright1<-vcovBK(rightpanel1,cluster='time')
VCcenter1<-vcovBK(centerpanel1,cluster='time')
seleft1<-sqrt(diag(VCleft1))
seright1<-sqrt(diag(VCright1))
secenter1<-sqrt(diag(VCcenter1))

VCleft2<-vcovBK(leftpanel2,cluster='time')
VCright2<-vcovBK(rightpanel2,cluster='time')
VCcenter2<-vcovBK(centerpanel2,cluster='time')
seleft2<-sqrt(diag(VCleft2))
seright2<-sqrt(diag(VCright2))
secenter2<-sqrt(diag(VCcenter2))



stargazer(leftpanel1,rightpanel1,centerpanel1,
          column.labels =c('Left A','Right A','Center A'),
          type = "text",title="Régressions par orientation - filtré à la de Haan & Kooi",
          se = list(seleft1,seright1,secenter1))

p1 <- avg_slopes(leftpanel,vcov=vcovBK(leftpanel,cluster='time'),newdata = setleft)
p2 <- avg_slopes(rightpanel,vcov=vcovBK(rightpanel,cluster='time'),newdata = setright)
p3 <- avg_slopes(centerpanel,vcov=vcovBK(centerpanel,cluster='time'),newdata = setcenter)
p1 <- p1[which(p1$p.value<=0.10),]
p2 <- p2[which(p2$p.value<=0.10),]
p3 <- p3[which(p3$p.value<=0.10),]

print(p1[,c(1,3,6)],style="data.frame")
print(p2[,c(1,3,6)],style="data.frame")
print(p3[,c(1,3,6)],style="data.frame")
print(xtable(p1[which(p1$term %in% dime),c(1,3,4,7)], type = "latex"), file = "p1.tex")
print(xtable(p2[which(p2$term %in% dime),c(1,3,4,7)], type = "latex"), file = "p2.tex")
print(xtable(p3[which(p3$term %in% dime),c(1,3,4,7)], type = "latex"), file = "p3.tex")


#Comparer mes coefficients
coefleft <- coeftest(centerpanel,vcovBK(centerpanel,cluster="time"))[1:6,]
coefright <- coeftest(rightpanel,vcovBK(rightpanel,cluster="time"))[1:6,]
variables <- rownames(coefleft)
z_stats <- numeric(length(variables))
p_values <- numeric(length(variables))

for (i in seq_along(variables)) {
  var <- variables[i]
  
  beta1 <- coefleft[var, "Estimate"]
  se1 <- coefleft[var, "Std. Error"]
  
  beta2 <- coefright[var, "Estimate"]
  se2 <- coefright[var, "Std. Error"]
  
  z_stats[i] <- (beta1 - beta2) / sqrt(se1^2 + se2^2)
  p_values[i] <- 2 * (1 - pnorm(abs(z_stats[i])))
}

results <- data.frame(
  Variable = variables,
  Z_Statistic = z_stats,
  P_Value = p_values)
xtable(results)




#_____________________________________8______________________________________________# 
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
p <- avg_slopes(centerpanel,vcov=vcovBK(centerpanel,cluster='time'))
p[which(p$p.value<0.11),]
p <- avg_slopes(rightpanel,vcov=vcovBK(rightpanel,cluster='time'))
p[which(p$p.value<0.11),]



plot_slopes(leftpanel,variables='rep',condition='polity',vcov=vcovBK(leftpanel,cluster='time'))+
  ggtitle('left-rep')
plot_slopes(rightpanel,variables='lend',condition='polity',vcov=vcovBK(rightpanel,cluster='time'),
            conf_level = 0.9)+
  ggtitle('right-lend - 10%')
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





#_____________________________________9______________________________________________# 
#Separate higher and lower polity et
data <- newb
apoldata<-data[which(data$polity<=0),]
poldata<-data[which(data$polity>0),]

ggplot(data.frame(poldata), aes(x = gpc, y = exec, fill = exec)) +
  geom_density_ridges() +
  theme_ridges() + 
  ggtitle("Comparaison des densités de CBIE-poldata")
theme(legend.position = "none")

dexec <- leadinf ~  CBIE * gpc + polity + inf

setleft<-apoldata[which(apoldata$exec=="Left"),]
setright <- apoldata[which(apoldata$exec=="Right"),]
setcenter <- apoldata[which(apoldata$exec=="Center"),]

leftpanel <- plm(dexec,model = "within",effect='twoways',data = setleft)
rightpanel <- plm(dexec,model="within",effect="twoways",data=setright)
centerpanel<- plm(dexec,model="within",effect="twoways",data=setcenter)

dexec <- leadinf ~ exec*CBIE  + inf + polity
panel <- plm(dexec,model = "within",effect='twoways',data = apoldata)

avg_slopes(leftpanel,by='exec',vcov=vcovBK(leftpanel,cluster='time'))#,data=apoldata)
avg_slopes(rightpanel,by='exec',vcov=vcovBK(rightpanel,cluster='time'))#,data=apoldata)
avg_slopes(centerpanel,by='exec',vcov=vcovBK(centerpanel,cluster='time'))#,data=apoldata)

plot_slopes(panel,variable="CBIE",condition='exec',vcov=vcovBK(panel,cluster='time'))
apoldata %>% ggplot(aes(year,CBIE,group=country,color=exec))+geom_point()+geom_line()

plot_slopes(leftpanel,variables='CBIE',condition=c('gpc','polity'),vcov=vcovBK(leftpanel,cluster='time'))+ ggtitle('left')
plot_slopes(centerpanel,variables='CBIE',condition=c('gpc','polity'),vcov=vcovBK(centerpanel,cluster='time'))+ ggtitle('center')
plot_slopes(rightpanel,variables='CBIE',condition=c('gpc','polity'),vcov=vcovBK(rightpanel,cluster='time'))+ ggtitle('right')




VCleft<-vcovBK(leftpanel,cluster='time')
VCright<-vcovBK(rightpanel,cluster='time')
VCcenter<-vcovBK(centerpanel,cluster='time')
seleft<-sqrt(diag(VCleft))
seright<-sqrt(diag(VCright))
secenter<-sqrt(diag(VCcenter))
stargazer(leftpanel,rightpanel,centerpanel,
          column.labels =c('Left','Right','Center'),
          type = "latex",title="Régressions par orientation- Polity Index haut (poldata)",
          se = list(seleft,seright,secenter))                    


p <- avg_slopes(leftpanel,by="exec",vcov=vcovBK(leftpanel,cluster='time'),newdata = setleft)
p <- avg_slopes(rightpanel,by="exec",vcov=vcovBK(rightpanel,cluster='time'),newdata = setright)
p <- avg_slopes(centerpanel,by="exec",vcov=vcovBK(centerpanel,cluster='time'),newdata = setcenter)
p <- p[,c(1,3,4,7)]
print(p,style="data.frame")
print(xtable(p[which(p$term %in% dime),], type = "latex",caption='Efets marginaux - Modèle 5 (i) - Gauche'), file = "p.tex")






avg_slopes(leftpanel,vcov=vcovBK(leftpanel,cluster='time'))
avg_slopes(centerpanel,vcov=vcovBK(centerpanel,cluster='time'))
avg_slopes(rightpanel,vcov=vcovBK(rightpanel,cluster='time'))
plot_slopes(rightpanel,vcov=vcovBK(rightpanel,cluster='time'))


#___________________________10___________________________________#


data <- fulldata[,-1]
data <- pdata.frame(data,index=c('country','year'))
data$leadinf <- lead(data$inf,1L)
data$leadinf <- log(data$leadinf)
data$inf <- log(data$inf)

data <- data[which(is.na(data$inf)==FALSE),]
data <- data[which(is.na(data$leadinf)==FALSE),]

#__________________________________________________#
gset <- newb
gset$par <- 0
gset$exec <- as.factor(gset$exec)
boxplot(newb$inf~newb$exec)

ggplot(data.frame(newb), aes(x =CBIE , y = exec, fill = exec)) +
  geom_density_ridges() +
  theme_ridges() + 
  ggtitle("Comparaison des densités de revenus par habitant  ")+ 
  theme(legend.position = "none")+
  scale_fill_viridis_d(option='G')

newb%>% ggplot(aes(gpc,polity))+geom_point()

adata <- aggregate(cbind(inf,CBIE)~country, data=new,FUN=mean)
adata %>% ggplot(aes(CBIE,inf))+ geom_point()

adata <- aggregate(exec~country,data=new,FUN=ischange)

count(adata[which(length(adata[,3])==1),],)
count(newb$exec)
count(is.integer(adata$exec[,2]))

ischange <- function(patata){
  sum(patata$exec[-1] != patata$exec[-length(patata$exec)])}


countlist <- unique(data$country)

res <- list()  # You can also use a data frame if preferred
for (i in 1:length(countlist)){
  token <- str(countlist[i])
  dset <- data[which(data$country==token),]
  count_change <- ischange(dset)
  print(cbind(token,count_change))
  }




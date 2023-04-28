# Загружаю необходимые библиотеки

library("memisc")
library("dplyr")
library("psych")
library("lmtest") 
library("sjPlot") 
library("sgof")
library("ggplot2") 
library("foreign") 
library("hexbin") 
library("rlms") 
library(devtools)
devtools::install_github("bdemeshev/rlms")
library("readxl")
library("plm")
library("stargazer")

# Открываю экселевский файл с б/д

HCI <-read_excel("HCI_comp_regression_4.xlsx")
glimpse(HCI)

# Отбираю нужные для регрессии переменные
# и убираю все строки, в которых хотя бы одно NA

str(HCI)
HCI2 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, GDPpc, Urb, C_1_Sur5, GovhspAB, PrivhshmC, HoushspD)
str(HCI2)
HCI2.2 <- HCI2 %>% filter_all(all_vars(!is.na(.)))
HCI2.2_p <-pdata.frame(HCI2.2, index=c("cn_id","Year"))

m2_1 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB))
m2_2 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(PrivhshmC))
m2_3 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(HoushspD))
m2_4 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB)+log(PrivhshmC))
m2_5 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB)+log(PrivhshmC)+log(HoushspD))# эта модель лучше по adj. R2 и не сильно хуже по AIC и BIC
m2_6 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB)+log(HoushspD)) 
m2_7 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(PrivhshmC)+log(HoushspD))

mtable(m2_1,m2_2,m2_3,m2_4,m2_5,m2_6,m2_7, summary.stats=c("adj. R-squared", "AIC","BIC"))
gqtest(m2_5, order.by = ~ GovhspAB, data =  HCI2.2_p, fraction = 0.2) 
gqtest(m2_5, order.by = ~ HoushspD, data =  HCI2.2_p, fraction = 0.2)

fe2_5 <- plm(data = HCI2.2_p, C_1_Sur5~GovhspAB+PrivhshmC+HoushspD, model = "within")
re2_5 <- plm(data = HCI2.2_p, C_1_Sur5~GovhspAB+PrivhshmC+HoushspD, model = "random")

summary(fe2_5) # p-value: 0.0046669
summary(re2_5) # p-value: 2.7506e-05

phtest(fe2_5,re2_5)
plmtest(re2_5, type=c("bp"))

m2_5_d1 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB)+log(PrivhshmC)+log(HoushspD)+Cluster)
m2_5_d2 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB)*Cluster+log(PrivhshmC)+log(HoushspD))
m2_5_d3 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB)*Cluster+log(PrivhshmC)*Cluster+log(HoushspD))
m2_5_d4 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB)*Cluster+log(PrivhshmC)*Cluster+log(HoushspD)*Cluster) 
m2_5_d5 <- lm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB)*Cluster+log(PrivhshmC)+log(HoushspD)*Cluster) # лучшая

waldtest(m2_5,m2_5_d1) # H_0 rejected
waldtest(m2_5_d1,m2_5_d2) # H_0 rejected
waldtest(m2_5_d2,m2_5_d3) # H_0 accepted (из-за того что PrivshmC различий по кластерам нет)
waldtest(m2_5_d2,m2_5_d4) # H_0 rejected
waldtest (m2_5_d2,m2_5_d5) # H_0 rejected
waldtest(m2_5_d5,m2_5_d4) # accepted

mtable(m2_5_d5,m2_5_d4, summary.stats=c("adj. R-squared", "AIC","BIC"))

m2_5_d5_GMM <- pgmm(log(C_1_Sur5)~lag(log(C_1_Sur5), 1:2)+lag(log(GovhspAB)*Cluster, 0:1)+log(PrivhshmC)+
                      lag(log(HoushspD)*Cluster, 0:1) | lag(log(C_1_Sur5), 2:7), data = HCI2.2_p, effect = "twoways", model = "twosteps")

fe2_5_d5 <- plm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB)*Cluster+log(PrivhshmC)+log(HoushspD)*Cluster + Cluster, model = "within")
re2_5_d5 <- plm(data = HCI2.2_p, log(C_1_Sur5)~log(GovhspAB)*Cluster+log(PrivhshmC)+log(HoushspD)*Cluster + Cluster, model = "random")
phtest(fe2_5_d5, re2_5_d5)

pFtest (fe2_5_d5,m2_5_d5)

plmtest(re2_5_d5, type=c("bp"))

summary(fe2_5_d5)




# Тест Хаусмана:
phtest(fe2,re2)
help(phtest)


# Тест Тест Бройша-Пагана (Lagrange multiplier test):
plmtest(re2, type=c("bp"))

# Проверяем влияние социальных расходов на дожитие детей до 5

HCI3<-dplyr::select(HCI, Country, cn_id, Cluster, Year, Urb, GDPpc, C_1_Sur5,Govsocsp_USD2017)
HCI3.2<-HCI3 %>% filter_all(all_vars(!is.na(.)))
HCI3.2p <-pdata.frame(HCI3.2, index=c("cn_id","Year"))
m3 <- lm(data=HCI3.2p, log(C_1_Sur5)~log(Govsocsp_USD2017))
summary(m4)
m3_v1 <- lm(data=HCI3.2p, log(C_1_Sur5)~log(Govsocsp_USD2017)+log(GDPpc))
m3_v2 <- lm(data=HCI3.2p, log(C_1_Sur5)~log(Govsocsp_USD2017)+log(GDPpc)+log(Urb))

mtable(m3,m3_v1,m3_v2, summary.stats=c("R-squared", "adj. R-squared","AIC","BIC"))
m3_he <- augment(m3, HCI3.2p)
qplot(data=m3_he, log(Govsocsp_USD2017), abs(.resid), color=Country)+facet_grid(~Cluster)

gqtest(m3, order.by = ~ Govsocsp_USD2017, data =  HCI3.2p, fraction = 0.2) # H_0 accepted

m3_d1 <- lm(data=HCI3.2p, log(C_1_Sur5)~log(Govsocsp_USD2017)+Cluster)
m3_d2 <- lm(data=HCI3.2p, log(C_1_Sur5)~log(Govsocsp_USD2017)*Cluster)

waldtest(m3,m3_d1) # H_0 rejected
waldtest(m3_d1,m3_d2) # H_0 rejected
summary(m3_d2)

fe3_d2 <- plm(data=HCI3.2p, log(C_1_Sur5)~log(Govsocsp_USD2017)*Cluster, model = "within")
re3_d2 <- plm(data=HCI3.2p, log(C_1_Sur5)~log(Govsocsp_USD2017)*Cluster, model = "random")

phtest(fe3_d2,re3_d2) # accepted
plmtest(re3_d2, type=c("bp")) # rejected

summary(re3_d2)

-0.02862-0.01148
0.00048+0.00385

# Проверяем влияние государственнхы расх на уч на TNER_prim

HCI5<-dplyr::select(HCI, Country, cn_id, Cluster, Year, GDPpc, Urb, C_2.1.1_TNERpr,Gvsp_pr)
HCI5.2<-HCI5 %>% filter_all(all_vars(!is.na(.)))
HCI5.2p<-pdata.frame(HCI5.2, index=c("cn_id","Year"))
head(HCI5.2p)
m5 <- lm(data=HCI5.2p, C_2.1.1_TNERpr~Gvsp_pr)
summary(m5)


gqtest(m5, order.by = ~ Gvsp_pr, data =  HCI5.2p, fraction = 0.2) # accepted

m5_he <- augment(m5, HCI5.2p)
qplot(data=m5_het, log(Gvsp_pr), abs(.resid), color=Country)+facet_grid(~Cluster)

m5_d1 <- lm(data=HCI5.2p, C_2.1.1_TNERpr~Gvsp_pr+Cluster)
m5_d2 <- lm(data=HCI5.2p, C_2.1.1_TNERpr~Gvsp_pr*Cluster)

mtable(m5_d1,m5_d2, summary.stats=c("R-squared", "adj. R-squared","AIC","BIC"))

waldtest(m5,m5_d1) # H_0 rejected 
waldtest(m5_d1,m5_d2) # H_0 accepted

re5_d1 <- plm(data=HCI5.2p, C_2.1.1_TNERpr~Gvsp_pr+Cluster, model = "random")
fe5_d1 <- plm(data=HCI5.2p, C_2.1.1_TNERpr~Gvsp_pr+Cluster, model = "within")
phtest(fe5_d1, re5_d1) # Accepted
plmtest(re5_d1, type=c("bp")) # Rejected

summary(m5_d1)

#Проверяем влияние Repetition rate на TNER_prim

HCI6<-dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.1_TNERpr,C_2.1.1_RepRpr)
HCI6.2<-HCI6 %>% filter_all(all_vars(!is.na(.)))
HCI6.2p<-pdata.frame(HCI6.2, index=c("cn_id","Year"))
m6<-lm(data=HCI6.2p, C_2.1.1_TNERpr~C_2.1.1_RepRpr)
summary(m6)

m6_d1 <- lm(data=HCI6.2p, C_2.1.1_TNERpr~C_2.1.1_RepRpr+Cluster)
m6_d2 <- lm(data=HCI6.2p, C_2.1.1_TNERpr~C_2.1.1_RepRpr*Cluster)

waldtest(m6,m6_d1) # H_0 принимается
waldtest(m6,m6_d2) # H_0 принимается

fe6 <- plm(data=HCI6.2p, C_2.1.1_TNERpr~C_2.1.1_RepRpr, model="within")
summary(fe6)
re6 <- plm(data=HCI6.2p, C_2.1.1_TNERpr~C_2.1.1_RepRpr, model="random")
summary (re6)

qplot(data=HCI6.2p, C_2.1.1_TNERpr, C_2.1.1_RepRpr)
phtest(fe6,re6)
plmtest(re6, type=c("bp"))

# проверяем влияние TNER_prim на HLO_prim_read

HCI7<-dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.1_TNERpr,C_2.2.1_HLOPririd)
HCI7.2<-HCI7 %>% filter_all(all_vars(!is.na(.)))
HCI7.2_p<-pdata.frame(HCI7.2, index=c("cn_id","Year"))
m7<- lm(data=HCI7.2_p, C_2.2.1_HLOPririd~C_2.1.1_TNERpr)
summary(m7)
qplot(data=HCI7.2_p, C_2.1.1_TNERpr, C_2.2.1_HLOPririd)
fe7 <-plm(data=HCI7.2_p, C_2.2.1_HLOPririd~C_2.1.1_TNERpr, model="within")
re7<-plm(data=HCI7.2_p, C_2.2.1_HLOPririd~C_2.1.1_TNERpr, model="random")
phtest(fe7,re7)
summary(fe7)
fixef(fe7)

pFtest(fe7,m7)

# проверяетм влияние на TNER_low seqondary Gov funding per student sec и houshold funding

str(HCI)
HCI8<-dplyr::select(HCI, Country, cn_id, Cluster, Year,C_2.1.2_TNERlsec,Gvsp_sec)
HCI8.2<-HCI8 %>% filter_all(all_vars(!is.na(.)))
HCI8.2_p<-pdata.frame(HCI8.2, index=c("cn_id","Year"))
m8<- lm(data=HCI8.2_p,C_2.1.2_TNERlsec~Gvsp_sec)
summary(m8)

gqtest(m8, order.by = ~ Gvsp_sec, data =  HCI8.2_p, fraction = 0.2) # Accepted

m8_d1 <- lm(data=HCI8.2_p,C_2.1.2_TNERlsec~Gvsp_sec+Cluster)
m8_d2 <- lm(data=HCI8.2_p,C_2.1.2_TNERlsec~Gvsp_sec*Cluster)

waldtest(m8,m8_d1) # H_0 accepted
waldtest(m8,m8_d2) # H_0 accepted

fe8 <-plm(data=HCI8.2_p, C_2.1.2_TNERlsec~Gvsp_sec, model="within")
re8 <-plm(data=HCI8.2_p, C_2.1.2_TNERlsec~Gvsp_sec, model="random")
phtest(fe8,re8)

plmtest(re8, type=c("bp"))
summary(re8)

q8<-qplot(data=HCI8.2_p,C_2.1.2_TNERlsec,Gvsp_sec)
q8 + stat_smooth(method = "lm")

HCI9<-dplyr::select(HCI, Country, cn_id, Cluster, Year,C_2.1.2_TNERlsec,Houssp_sec)
HCI9.2<-HCI9 %>% filter_all(all_vars(!is.na(.)))
HCI9.2_p<-pdata.frame(HCI9.2, index=c("cn_id","Year"))
m9 <-lm(data=HCI9.2_p, C_2.1.2_TNERlsec~Houssp_sec)
summary(m9)

gqtest(m9, order.by = ~ Houssp_sec, data =  HCI9.2_p, fraction = 0.2) # Accepted 

m9_d1 <- lm(data=HCI9.2_p, C_2.1.2_TNERlsec~Houssp_sec+Cluster)
m9_d2 <- lm(data=HCI9.2_p, C_2.1.2_TNERlsec~Houssp_sec*Cluster)

waldtest(m9,m9_d1) # H_0 accepted
waldtest(m9,m9_d2) # H_0 rejected
waldtest(m9_d1,m9_d2) # H_0 rejected

summary(m9_d2)

fe9_d2 <- plm(data=HCI9.2_p, C_2.1.2_TNERlsec~Houssp_sec*Cluster, model="within")
re9_d2 <- plm(data=HCI9.2_p, C_2.1.2_TNERlsec~Houssp_sec*Cluster, model="random")

summary (fe9)
summary (re9)
phtest(fe9_d2,re9_d2) # H_0 rejected
pFtest(fe9_d2,m9_d2)

summary(fe9_d2)

# Проверяем влияние repetition rate на TNER_low seqondary

HCI10 <- dplyr::select(HCI, Country, cn_id, Cluster, Year,C_2.1.2_TNERlsec,C_2.1.2_RepRlsec)
HCI10.2<-HCI10 %>% filter_all(all_vars(!is.na(.)))
HCI10.2_p<-pdata.frame(HCI10.2, index=c("cn_id","Year"))
glimpse(HCI10.2_p)
m10<-lm(data=HCI10.2_p, C_2.1.2_TNERlsec~C_2.1.2_RepRlsec)
summary(m10)
fe10<-plm(data=HCI10.2_p, C_2.1.2_TNERlsec~C_2.1.2_RepRlsec, model="within")
re10 <-plm(data=HCI10.2_p, C_2.1.2_TNERlsec~C_2.1.2_RepRlsec, model="random")
phtest(fe10,re10)
pFtest(fe10,m10)

summary(fe10)

#Проверяем влияние TNER_low seqondary на HLO test scores

HCI11 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.2_TNERlsec,C_2.2.2_HLOSecmath )
HCI11.2 <- HCI11 %>% filter_all(all_vars(!is.na(.)))
HCI11.2_p <-pdata.frame(HCI11.2, index=c("cn_id","Year"))
glimpse(HCI11.2_p)
head(HCI11.2_p)
m11 <-lm(data=HCI11.2_p, C_2.2.2_HLOSecmath~C_2.1.2_TNERlsec)
summary(m11)
fe11<-plm(data=HCI11.2_p, C_2.2.2_HLOSecmath~C_2.1.2_TNERlsec, model="within")
re11 <-plm(data=HCI11.2_p, C_2.2.2_HLOSecmath~C_2.1.2_TNERlsec, model="random")
phtest(fe11,re11)

pFtest(fe11,m11)
summary(fe11)

HCI12 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.2_TNERlsec,C_2.2.3_HLOSecscie )
HCI12.2 <- HCI12 %>% filter_all(all_vars(!is.na(.)))
HCI12.2_p <-pdata.frame(HCI12.2, index=c("cn_id","Year"))
glimpse(HCI12.2_p)
m12 <-lm(data=HCI12.2_p, C_2.2.3_HLOSecscie~C_2.1.2_TNERlsec)
summary(m12)
fe12<-plm(data=HCI12.2_p, C_2.2.3_HLOSecscie~C_2.1.2_TNERlsec, model="within")
re12<-plm(data=HCI12.2_p, C_2.2.3_HLOSecscie~C_2.1.2_TNERlsec, model="random")
summary(fe12) # p-value: 0.66459
summary(re12) # p-value: 0.45442

# проверяетм влияние на TNER_up seqondary Gov funding per student sec и houshold funding

HCI13 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.3_TNERupsec,Gvsp_sec)
HCI13.2 <- HCI13 %>% filter_all(all_vars(!is.na(.)))
HCI13.2_p <-pdata.frame(HCI13.2, index=c("cn_id","Year"))
glimpse(HCI13.2_p)
m13<-lm(data=HCI13.2_p, log(C_2.1.3_TNERupsec)~log(Gvsp_sec))
summary(m13)

gqtest(m13, order.by = ~ Gvsp_sec, data =  HCI13.2_p, fraction = 0.2) # Accepted

m13_d1 <- lm(data=HCI13.2_p, log(C_2.1.3_TNERupsec)~log(Gvsp_sec)+Cluster)
m13_d2 <- lm(data=HCI13.2_p, log(C_2.1.3_TNERupsec)~log(Gvsp_sec)*Cluster)

mtable(m13_d1,m13_d2, summary.stats=c("R-squared", "adj. R-squared","AIC","BIC"))

waldtest(m13,m13_d1) # H_0 accepted
waldtest(m13,m13_d2) # H_0 accepted

fe13<-plm(data=HCI13.2_p, log(C_2.1.3_TNERupsec)~log(Gvsp_sec), model = "within")
re13<-plm(data=HCI13.2_p, log(C_2.1.3_TNERupsec)~log(Gvsp_sec), model = "random")
summary(fe13)# p-value: 0.057768
summary(re13)# p-value: 1.0185e-05

phtest(fe13,re13)
plmtest(re13, type=c("bp"))

summary (re13)

HCI14 <-dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.3_TNERupsec,Houssp_sec)
HCI14.2 <- HCI14 %>% filter_all(all_vars(!is.na(.)))
HCI14.2_p <-pdata.frame(HCI14.2, index=c("cn_id","Year"))
glimpse(HCI14.2_p)
m14<-lm(data=HCI14.2_p, C_2.1.3_TNERupsec~Houssp_sec)
summary(m14)

m14_d1 <- lm(data=HCI14.2_p, C_2.1.3_TNERupsec~Houssp_sec+Cluster)
m14_d2 <- lm(data=HCI14.2_p, C_2.1.3_TNERupsec~Houssp_sec*Cluster)

waldtest(m14,m14_d1) # H_O accepted 
waldtest(m14,m14_d2) # H_0 accepted

fe14<-plm(data=HCI14.2_p, C_2.1.3_TNERupsec~Houssp_sec, model = "within")
re14<-plm(data=HCI14.2_p, C_2.1.3_TNERupsec~Houssp_sec, model = "random")
summary (fe14) # p-value: 0.85197
summary (re14) # p-value: 0.55514

phtest(fe14,re14)
plmtest(re14, type=c("bp"))

# проверяетм влияние TNER_up secondary на HLO test scores C_2.2.2_HLOSecmath и C_2.2.3_HLOSecscie:

str(HCI)
HCI15 <-dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.3_TNERupsec,C_2.2.2_HLOSecmath)
HCI15.2 <- HCI15 %>% filter_all(all_vars(!is.na(.)))
HCI15.2_p <-pdata.frame(HCI15.2, index=c("cn_id","Year"))
glimpse(HCI15.2_p)
m15 <- lm(data=HCI15.2_p, C_2.2.2_HLOSecmath~C_2.1.3_TNERupsec)
summary (m15)

fe15 <- plm(data=HCI15.2_p, C_2.2.2_HLOSecmath~C_2.1.3_TNERupsec, model = "within")
re15 <- plm(data=HCI15.2_p, C_2.2.2_HLOSecmath~C_2.1.3_TNERupsec, model = "random")

summary(fe15) # p-value: 0.30297
summary(re15) # p-value: 0.054348

phtest(fe15,re15)
plmtest(re15, type=c("bp"))

HCI16 <-dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.3_TNERupsec,C_2.2.3_HLOSecscie)
HCI16.2 <- HCI16 %>% filter_all(all_vars(!is.na(.)))
HCI16.2_p <-pdata.frame(HCI16.2, index=c("cn_id","Year"))
glimpse(HCI16.2_p)

m16 <- lm(data=HCI16.2_p, C_2.2.3_HLOSecscie~C_2.1.3_TNERupsec)
summary(m16)
fe16 <- plm(data=HCI16.2_p, C_2.2.3_HLOSecscie~C_2.1.3_TNERupsec,model = "within")
re16 <- plm(data=HCI16.2_p, C_2.2.3_HLOSecscie~C_2.1.3_TNERupsec,model = "random")

summary(fe16) # p-value: 0.056934
summary(re16) # p-value: 0.00302

phtest(fe16,re16)
plmtest(re16, type=c("bp"))

# Проверяем влияние Гос расходов по ступеням на RepRpr, RepRlsec, RepRupsec

HCI17 <-dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.1_RepRpr,Gvsp_pr)
HCI17.2 <- HCI17 %>% filter_all(all_vars(!is.na(.)))
HCI17.2_p <-pdata.frame(HCI17.2, index=c("cn_id","Year"))
glimpse(HCI17.2_p)

m17 <- lm(data=HCI17.2_p, C_2.1.1_RepRpr~Gvsp_pr)
summary(m17)

gqtest(m17, order.by = ~ Gvsp_pr, data =  HCI17.2_p, fraction = 0.2) # Accepted

m17_d1 <- lm(data=HCI17.2_p, C_2.1.1_RepRpr~Gvsp_pr+Cluster)
m17_d2 <- lm(data=HCI17.2_p, C_2.1.1_RepRpr~Gvsp_pr*Cluster)

waldtest(m17,m17_d1) # H_0 rejected
waldtest(m17_d1,m17_d2) # H_0 rejected

summary(m17_d2)

fe17_d2 <- plm(data=HCI17.2_p, C_2.1.1_RepRpr~Gvsp_pr*Cluster, model = "within")
re17_d2 <- plm(data=HCI17.2_p, C_2.1.1_RepRpr~Gvsp_pr*Cluster, model = "random")

phtest(fe17_d2,re17_d2)
plmtest(re17_d2, type=c("bp"))

summary(re17_d2)

# Добавляем дамми-переменную кластер и смотрим, есть ли статистически значимые различия м-ду кластерами

m17 <- lm(data=HCI17.2_p, C_2.1.1_RepRpr~Gvsp_pr)
m17_d1 <- lm(data=HCI17.2_p, C_2.1.1_RepRpr~Gvsp_pr+Cluster)
m17_d2 <- lm(data=HCI17.2_p, C_2.1.1_RepRpr~Gvsp_pr*Cluster)

waldtest (m17,m17_d1) # H_0: лучше m17 (без dummy) отвергаетсется, т.е. статистические разлиичя между кластерами есть
waldtest(m17_d1,m17_d2) # H_0: m17_d1 лучше, чем m17_d2 (с большим dummy) отвергается, т.е. есть влияние кластеров и на расходы 

re17_d1 <- plm(data=HCI17.2_p, C_2.1.1_RepRpr~Gvsp_pr+Cluster, model = "random")
re17_d2 <- plm(data=HCI17.2_p, C_2.1.1_RepRpr~Gvsp_pr*Cluster, model = "random")

waldtest(re17, re17_d1) # H_0 не отвергается
waldtest(re17_d1,re17_d2) # H_0 не отвергается

q_m17 <- qplot(data = HCI17.2_p, Gvsp_pr, C_2.1.1_RepRpr)
q_m17 + facet_grid(~Cluster) 

HCI18 <-dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.1_RepRpr,Houssp_pr)
HCI18.2 <- HCI18 %>% filter_all(all_vars(!is.na(.)))
HCI18.2_p <-pdata.frame(HCI18.2, index=c("cn_id","Year"))

m18 <- lm(data=HCI18.2_p, C_2.1.1_RepRpr~Houssp_pr)
summary(m18) 

m18_d1 <- lm(data=HCI18.2_p, C_2.1.1_RepRpr~Houssp_pr+Cluster)
m18_d2 <- lm(data=HCI18.2_p, C_2.1.1_RepRpr~Houssp_pr*Cluster)
mtable(m18, m18_d1, m18_d2)

q_m18 <- qplot(data=HCI18.2_p, Houssp_pr, C_2.1.1_RepRpr)
q_m18+aes(col=Country)+facet_grid(~Cluster)

waldtest(m18,m18_d1) # H_0 отвергается
waldtest(m18_d1,m18_d2) # H_0 отвергается

summary(m18_d2)


str(HCI)


HCI190 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.2_RepRlsec,Gvsp_sec)
HCI190.2 <- HCI190 %>% filter_all(all_vars(!is.na(.)))
HCI190.2_p <-pdata.frame(HCI190.2, index=c("cn_id","Year"))

m190 <- lm(data=HCI190.2_p, C_2.1.2_RepRlsec~Gvsp_sec)
summary(m190)

gqtest(m190, order.by = ~ Gvsp_sec, data =  HCI190.2_p, fraction = 0.2)

m190_d1 <- lm(data=HCI190.2_p, C_2.1.2_RepRlsec~Gvsp_sec+Cluster)
m190_d2 <- lm(data=HCI190.2_p, C_2.1.2_RepRlsec~Gvsp_sec*Cluster)

waldtest(m190,m190_d1)
waldtest(m190,m190_d2)

mtable(m190,m190_d1,m190_d2, summary.stats=c("R-squared", "adj. R-squared","AIC","BIC"))

fe190_d2 <- plm(data=HCI190.2_p, C_2.1.2_RepRlsec~Gvsp_sec*Cluster, model = "within")
re190_d2 <- plm(data=HCI190.2_p, C_2.1.2_RepRlsec~Gvsp_sec*Cluster, model = "random")

phtest(fe190_d2,re190_d2)
plmtest(re190_d2, type=c("bp")) 

summary(re190_d2)

HCI19 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.2_RepRlsec,Houssp_sec)
HCI19.2 <- HCI19 %>% filter_all(all_vars(!is.na(.)))
HCI19.2_p <-pdata.frame(HCI19.2, index=c("cn_id","Year"))

9.36-5.3


m19 <- lm(data=HCI19.2_p, C_2.1.2_RepRlsec~Houssp_sec)
summary(m19)

gqtest(m19, order.by = ~ Houssp_sec, data =  HCI19.2_p, fraction = 0.2) # Accepted

m19_d1 <- lm(data=HCI19.2_p, C_2.1.2_RepRlsec~Houssp_sec+Cluster)
m19_d2 <- lm(data=HCI19.2_p, C_2.1.2_RepRlsec~Houssp_sec*Cluster)

waldtest(m19,m19_d1) #H_0 rejected
waldtest(m19,m19_d2) #H_0 rejected

summary (m19_d2)

fe19_d2 <- plm(data=HCI19.2_p, C_2.1.2_RepRlsec~Houssp_sec*Cluster, model = "within")
re19_d2 <- plm(data=HCI19.2_p, C_2.1.2_RepRlsec~Houssp_sec*Cluster, model = "random")

phtest(fe19_d2,re19_d2) # H_O assepted
plmtest(re19_d2, type=c("bp"))

summary (m19_d2)

HCI20 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.3_RepRupsec,Gvsp_sec)
HCI20.2 <- HCI20 %>% filter_all(all_vars(!is.na(.)))
HCI20.2_p <-pdata.frame(HCI20.2, index=c("cn_id","Year"))

m20 <- lm(data=HCI20.2_p, log(C_2.1.3_RepRupsec)~log(Gvsp_sec))
summary(m20)

gqtest(m20, order.by = ~ Gvsp_sec, data =  HCI20.2_p, fraction = 0.2) 

m20_d1 <- lm(data=HCI20.2_p, C_2.1.3_RepRupsec~Gvsp_sec+Cluster)
m20_d2 <- lm(data=HCI20.2_p, C_2.1.3_RepRupsec~Gvsp_sec*Cluster)

waldtest(m20,m20_d1) # H_0 rejected
waldtest(m20,m20_d2) # H_0 rejected

summary(m20_d2)

fe20_d2 <- plm(data=HCI20.2_p, C_2.1.3_RepRupsec~Gvsp_sec*Cluster, model = "within")
re20_d2 <- plm(data=HCI20.2_p, C_2.1.3_RepRupsec~Gvsp_sec*Cluster, model = "random")

phtest(fe20_d2,re20_d2) # H_0 accepted
plmtest(re20_d2, type=c("bp")) #H_0 accepted

summary(re20_d2)

qq20 <- qplot(data=HCI20.2_p,Gvsp_sec, C_2.1.3_RepRupsec)
qq20 + facet_grid(~Cluster)

str(HCI)

HCI200 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.1.3_RepRupsec,Houssp_sec)
HCI200.2 <- HCI200 %>% filter_all(all_vars(!is.na(.)))
HCI200.2_p <-pdata.frame(HCI200.2, index=c("cn_id","Year"))

m200 <- lm(data=HCI200.2_p, C_2.1.3_RepRupsec~Houssp_sec)
summary(m200)

m200_d1 <- lm(data=HCI200.2_p, C_2.1.3_RepRupsec~Houssp_sec+Cluster)
m200_d2 <- lm(data=HCI200.2_p, C_2.1.3_RepRupsec~Houssp_sec*Cluster)

waldtest(m200,m200_d1) # H_0 rejected
waldtest(m200_d1,m200_d2) # H_0 rejected

fe200_d2 <- plm (data=HCI200.2_p, C_2.1.3_RepRupsec~Houssp_sec*Cluster, model = "within")
re200_d2 <- plm (data=HCI200.2_p, C_2.1.3_RepRupsec~Houssp_sec*Cluster, model = "random")

phtest(fe200_d2,re200_d2) # H_0 rejected
pFtest(fe200_d2,m200_d2) # H_0 accepted

summary(fe200_d2)

# Проверяем обобщенных Всемирным банком результатов образовательных тестов и государственных расходов на 1 и 2 ступ

str(HCI)
HCI21 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.2_TscWB,Gvsp_pr, C_2.1.1_RepRpr, )
HCI21.2 <- HCI21 %>% filter_all(all_vars(!is.na(.)))
HCI21.2_p <-pdata.frame(HCI21.2, index=c("cn_id","Year"))

m21 <- lm(data=HCI21.2_p, C_2.2_TscWB~Gvsp_pr + Gvsp_sec)
summary(m21)
m21_1 <- lm(data=HCI21.2_p, C_2.2_TscWB~Gvsp_pr)
summary(m21_1)

waldtest(m21_1, m21)
mtable(m21_1, m21, summary.stats=c("R-squared", "adj. R-squared","AIC","BIC"))

m21_v <- lm(data=HCI21.2_p, C_2.2_TscWB~Gvsp_pr + C_2.1.1_RepRpr)
m21_v2 <- lm(data=HCI21.2_p, C_2.2_TscWB~Gvsp_pr + C_2.1.1_RepRpr + C_2.1.2_RepRlsec)

mtable(m21_v, m21_v2, summary.stats=c("R-squared", "adj. R-squared","AIC","BIC"))

gqtest(m21_v, order.by = ~ Gvsp_pr, data =  HCI21.2_p, fraction = 0.2)

fe21_1 <- plm(data=HCI21.2_p, C_2.2_TscWB~Gvsp_pr, model = "within")
re21_1 <- plm(data=HCI21.2_p, C_2.2_TscWB~Gvsp_pr, model = "random")
summary(fe21_1) # p-value: 0.99847
summary(re21_1) # p-value: 0.11895



m21_2 <- lm(data=HCI21.2_p, C_2.2_TscWB~Gvsp_sec)
summary(m21_2)

mtable(m21,m21_1,m21_2, summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "p", "Log-likelihood", "Deviance", "AIC","BIC", "N"))

HCI22 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.2.1_HLOPririd, Gvsp_pr)
HCI22.2 <- HCI22 %>% filter_all(all_vars(!is.na(.)))
HCI22.2_p <-pdata.frame(HCI22.2, index=c("cn_id","Year"))

m22 <- lm(data = HCI22.2_p, C_2.2.1_HLOPririd~Gvsp_pr)
summary(m22)

gqtest(m22, order.by = ~ Gvsp_pr, data =  HCI22.2_p, fraction = 0.2)

m22_d1 <- lm(data = HCI22.2_p, C_2.2.1_HLOPririd~Gvsp_pr+Cluster)
m22_d2 <- lm(data = HCI22.2_p, C_2.2.1_HLOPririd~Gvsp_pr*Cluster)

waldtest(m22,m22_d1) # H_0 rejected
waldtest(m22_d1,m22_d2) #H_0 accepted

mtable(m22_d1,m22_d2,summary.stats=c("adj. R-squared","AIC","BIC"))

fe22_d1 <- plm(data = HCI22.2_p, C_2.2.1_HLOPririd~Gvsp_pr+Cluster, model = "within")
re22_d1 <- plm(data = HCI22.2_p, C_2.2.1_HLOPririd~Gvsp_pr+Cluster, model = "random")

summary(fe22_d1)
summary(re22_d1)

phtest(fe22_d1,re22_d1) # H_0 rejected
pFtest(fe22_d1,m22_d1)

summary(m22_d1)


qplot(data = HCI22.2_p,Gvsp_pr,C_2.2.1_HLOPririd, color=Cluster)

HCI23 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.2.1_HLOPririd, C_2.1.1_RepRpr) 
HCI23.2 <- HCI23 %>% filter_all(all_vars(!is.na(.)))
HCI23.2_p <-pdata.frame(HCI23.2, index=c("cn_id","Year"))

m23 <- lm(data = HCI23.2_p, C_2.2.1_HLOPririd~C_2.1.1_RepRpr)
summary(m23)

fe23 <- plm(data = HCI23.2_p, C_2.2.1_HLOPririd~C_2.1.1_RepRpr, model = "within")
re23 <- plm(data = HCI23.2_p, C_2.2.1_HLOPririd~C_2.1.1_RepRpr, model = "random") 

summary(fe23) #p-value: 0.4951
summary(re23) #p-value: 0.059658

phtest(fe23,re23)
pFtest(fe23,m23)

str(HCI)
HCI24 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, GDPpc, Urb, C_2.2.2_HLOSecmath, Gvsp_sec) 
HCI24.2 <- HCI24 %>% filter_all(all_vars(!is.na(.)))
HCI24.2_p <-pdata.frame(HCI24.2, index=c("cn_id","Year"))

m24 <- lm(data = HCI24.2_p, log(C_2.2.2_HLOSecmath)~log(Gvsp_sec))
summary (m24)
m24_v <- lm(data = HCI24.2_p, log(C_2.2.2_HLOSecmath)~ log(GDPpc)+ log(Gvsp_sec) )
summary (m24_v)

mtable(m24_v, m24, summary.stats=c("R-squared", "adj. R-squared","AIC","BIC"))

gqtest(m24_v, order.by = ~ Gvsp_sec, data =  HCI24.2_p, fraction = 0.2) # rejected

mtable(m24_v, m24, summary.stats=c("R-squared", "adj. R-squared","AIC","BIC"))
waldtest(m24_v,m24) # H_0 accepted

brbr <- augment(m24, HCI24.2_p)
brbr_v <- augment(m24_v, HCI24.2_p)
glimpse(brbr)
qplot(data=brbr, log(Gvsp_sec), abs(.resid), color=Country)+facet_grid(~Cluster)
qplot(data=brbr_v, log(Gvsp_sec), abs(.resid), color=Country)+facet_grid(~Cluster)


m24_d1 <- lm(data = HCI24.2_p, log(C_2.2.2_HLOSecmath)~ log(GDPpc)+ log(Gvsp_sec)+Cluster ) # best model
m24_d2 <- lm(data = HCI24.2_p, log(C_2.2.2_HLOSecmath)~ log(GDPpc)+ log(Gvsp_sec)*Cluster )

waldtest(m24_v,m24_d1) # H_0 accepted
waldtest(m24_v,m24_d2) # H_0 accepted

fe24_v <- plm(data = HCI24.2_p, log(C_2.2.2_HLOSecmath)~ log(GDPpc)+ log(Gvsp_sec),model = "within")
re24_v <- plm(data = HCI24.2_p, log(C_2.2.2_HLOSecmath)~ log(GDPpc)+ log(Gvsp_sec),model = "random")


summary(fe24_d1) #p-value: 0.23323
summary(re24_d1) #p-value: 0.00015111

phtest(fe24_v,re24_v) # H_0 rejected
pFtest(fe24_v,m24_v) # H_0 accepted
mtable(m24_d1,m24_d2)

summary(fe24_v)


str(HCI)
HCI25 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.2.2_HLOSecmath, C_2.1.2_RepRlsec) 
HCI25.2 <- HCI25 %>% filter_all(all_vars(!is.na(.)))
HCI25.2_p <-pdata.frame(HCI25.2, index=c("cn_id","Year"))

m25 <- lm(data = HCI25.2_p, C_2.2.2_HLOSecmath~C_2.1.2_RepRlsec)
summary(m25)

fe25 <- plm(data = HCI25.2_p, C_2.2.2_HLOSecmath~C_2.1.2_RepRlsec, model = "within")
re25 <- plm(data = HCI25.2_p, C_2.2.2_HLOSecmath~C_2.1.2_RepRlsec, model = "random")

summary(fe25) #p-value: 0.81176
summary(re25) #p-value: 0.50803

phtest(fe25,re25)
plmtest(re25, type=c("bp"))

str(HCI)
HCI26 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.2.2_HLOSecmath, C_2.1.3_RepRupsec) 
HCI26.2 <- HCI26 %>% filter_all(all_vars(!is.na(.)))
HCI26.2_p <-pdata.frame(HCI26.2, index=c("cn_id","Year"))

m26 <- lm(data = HCI26.2_p, C_2.2.2_HLOSecmath~C_2.1.3_RepRupsec)
summary(m26)

fe26 <- plm(data = HCI26.2_p, C_2.2.2_HLOSecmath~C_2.1.3_RepRupsec, model = "within")
re26 <- plm(data = HCI26.2_p, C_2.2.2_HLOSecmath~C_2.1.3_RepRupsec, model = "random")

summary(fe26) # p-value: 0.74769
summary(re26) # p-value: 0.6356

phtest(fe26,re26)
plmtest(re26, type=c("bp"))


str(HCI)
HCI27 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.2.3_HLOSecscie, Gvsp_sec) 
HCI27.2 <- HCI27 %>% filter_all(all_vars(!is.na(.)))
HCI27.2_p <-pdata.frame(HCI27.2, index=c("cn_id","Year"))

m27 <- lm(data = HCI27.2_p, C_2.2.3_HLOSecscie~ Gvsp_sec)

gqtest(m27, order.by = ~ Gvsp_sec, data =  HCI27.2_p, fraction = 0.2) # rejected

summary(m27)
qplot(data = HCI27.2_p, C_2.2.3_HLOSecscie, Gvsp_sec)

m27_d1 <- lm(data = HCI27.2_p, C_2.2.3_HLOSecscie~ Gvsp_sec+Cluster)
m27_d2 <- lm(data = HCI27.2_p, C_2.2.3_HLOSecscie~ Gvsp_sec*Cluster)

waldtest(m27,m27_d1) # H_0 rejected
waldtest(m27_d1,m27_d2) # H_0 accepted
mtable(m27_d1,m27_d2, summary.stats=c("adj. R-squared"))

fe27_d1 <- plm(data = HCI27.2_p, C_2.2.3_HLOSecscie~ Gvsp_sec+Cluster, model = "within")
re27_d1 <- plm(data = HCI27.2_p, C_2.2.3_HLOSecscie~ Gvsp_sec+Cluster, model = "random")

summary(fe27) # p-value: 0.509
summary(re27) # p-value: 0.022897

phtest(fe27_d1,re27_d1) # rejected
plmtest(re27, type=c("bp"))
pFtest(fe27_d1,m27_d1) # H_0 accepted

summary(fe27_d1)


str(HCI)
HCI28 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.2.3_HLOSecscie, C_2.1.2_RepRlsec) 
HCI28.2 <- HCI28 %>% filter_all(all_vars(!is.na(.)))
HCI28.2_p <-pdata.frame(HCI28.2, index=c("cn_id","Year"))

m28 <- lm(data = HCI28.2_p, C_2.2.3_HLOSecscie~ C_2.1.2_RepRlsec)
summary(m28)

fe28 <- plm (data = HCI28.2_p, C_2.2.3_HLOSecscie~ C_2.1.2_RepRlsec, model = "within")
re28 <- plm (data = HCI28.2_p, C_2.2.3_HLOSecscie~ C_2.1.2_RepRlsec, model = "random")

summary(fe28) # p-value: 0.80397
summary(re28) # p-value: 0.33853

phtest(fe28,re28)
plmtest(re28, type=c("bp"))


str(HCI)
HCI29 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_2.2.3_HLOSecscie, C_2.1.3_RepRupsec) 
HCI29.2 <- HCI29 %>% filter_all(all_vars(!is.na(.)))
HCI29.2_p <-pdata.frame(HCI29.2, index=c("cn_id","Year"))

m29 <- lm(data = HCI29.2_p, C_2.2.3_HLOSecscie~ C_2.1.3_RepRupsec)
summary(m29)

fe29 <- plm (data = HCI29.2_p, C_2.2.3_HLOSecscie~ C_2.1.3_RepRupsec, model = "within")
re29 <- plm (data = HCI29.2_p, C_2.2.3_HLOSecscie~ C_2.1.3_RepRupsec, model = "random")

summary(fe29) # p-value: 0.52235
summary(re29) # p-value: 0.66982

phtest(fe29,re29)
plmtest(re29, type=c("bp"))

# Влияние на School life expectancy primary гос расходов

str(HCI)
HCI30 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C.2.1.2_Schlifepr, Gvsp_pr) 
HCI30.2 <- HCI30 %>% filter_all(all_vars(!is.na(.)))
HCI30.2_p <-pdata.frame(HCI30.2, index=c("cn_id","Year"))

m30 <- lm(data = HCI30.2_p, C.2.1.2_Schlifepr~ Gvsp_pr)
summary(m30)

m30_d1 <- lm(data = HCI30.2_p, C.2.1.2_Schlifepr~ Gvsp_pr+Cluster)
m30_d2 <- lm(data = HCI30.2_p, C.2.1.2_Schlifepr~ Gvsp_pr*Cluster)

waldtest(m30,m30_d1)# H_0 accepted
waldtest(m30,m30_d2) # H_0 accepted
mtable(m30,m30_d1,m30_d2)

fe30 <- plm(data = HCI30.2_p, C.2.1.2_Schlifepr~ Gvsp_pr, model = "within")
re30 <- plm(data = HCI30.2_p, C.2.1.2_Schlifepr~ Gvsp_pr, model = "random")

summary(fe30) # p-value: 0.25586
summary(re30) # p-value: 0.054502

phtest(fe30,re30)
plmtest(re30, type=c("bp"))

str(HCI)
HCI31 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C.2.1.2_Schlifepr, C_2.1.1_RepRpr) 
HCI31.2 <- HCI31 %>% filter_all(all_vars(!is.na(.)))
HCI31.2_p <-pdata.frame(HCI31.2, index=c("cn_id","Year"))

m31 <- lm(data = HCI31.2_p, C.2.1.2_Schlifepr~ C_2.1.1_RepRpr)
summary(m31)
qplot(data = HCI31.2_p, C.2.1.2_Schlifepr, C_2.1.1_RepRpr)

fe31 <- plm(data = HCI31.2_p, C.2.1.2_Schlifepr~ C_2.1.1_RepRpr, model = "within")
re31 <- plm(data = HCI31.2_p, C.2.1.2_Schlifepr~ C_2.1.1_RepRpr, model = "random")

summary(fe31) # p-value: 0.87375
summary(re31) # p-value: 0.019175

phtest(fe31,re31)
pFtest(fe31,m31)


str(HCI)
HCI32 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C.2.1.2_Schlifesec, Gvsp_sec) 
HCI32.2 <- HCI32 %>% filter_all(all_vars(!is.na(.)))
HCI32.2_p <-pdata.frame(HCI32.2, index=c("cn_id","Year"))

m32 <- lm(data = HCI32.2_p, C.2.1.2_Schlifesec~ Gvsp_sec)
summary(m32)

m32_d1 <- lm(data = HCI32.2_p, C.2.1.2_Schlifesec~ Gvsp_sec+Cluster)
m32_d2 <- lm(data = HCI32.2_p, C.2.1.2_Schlifesec~ Gvsp_sec*Cluster)

waldtest(m32,m32_d1) #H_0 accepted
waldtest(m32,m32_d2) #H_0 rejected
waldtest(m32_d1,m32_d2) #H_0 rejected

qplot(data = HCI32.2_p, C.2.1.2_Schlifesec, Gvsp_sec)

fe32_d2 <- plm (data = HCI32.2_p, C.2.1.2_Schlifesec~ Gvsp_sec*Cluster, model = "within")
re32_d2 <- plm (data = HCI32.2_p, C.2.1.2_Schlifesec~ Gvsp_sec*Cluster, model = "random")

summary(fe32) # p-value: 0.039347
summary(re32) # p-value: 0.014657

phtest(fe32_d2,re32_d2) # H_0 rejected
pFtest(fe32_d2,m32_d2) # H_0 accepted

summary(fe32_d2)

str(HCI)
HCI33 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C.2.1.2_Schlifesec, Houssp_sec) 
HCI33.2 <- HCI33 %>% filter_all(all_vars(!is.na(.)))
HCI33.2_p <-pdata.frame(HCI33.2, index=c("cn_id","Year"))

m33 <- lm(data = HCI33.2_p, C.2.1.2_Schlifesec~ Houssp_sec)
summary(m33)

m33_d1 <- lm(data = HCI33.2_p, C.2.1.2_Schlifesec~ Houssp_sec+Cluster)
m33_d2 <- lm(data = HCI33.2_p, C.2.1.2_Schlifesec~ Houssp_sec*Cluster)

waldtest(m33,m33_d1) # H_0 rejected
waldtest(m33_d1,m33_d2) # H_0 accepted
mtable(m33_d1,m33_d2)

fe33_d1 <- plm (data = HCI33.2_p, C.2.1.2_Schlifesec~ Houssp_sec+Cluster, model = "within")
re33_d1 <- plm (data = HCI33.2_p, C.2.1.2_Schlifesec~ Houssp_sec+Cluster, model = "random")

summary (fe33) # p-value: 0.82615
summary (re33) # p-value: 0.029538

phtest(fe33_d1,re33_d1) # H_0 rejected
pFtest(fe33_d1,m33_d1)

summary(fe33_d1)

str(HCI)
HCI34 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, Schlifetert, Houssp_tert) 
HCI34.2 <- HCI34 %>% filter_all(all_vars(!is.na(.)))
HCI34.2_p <-pdata.frame(HCI34.2, index=c("cn_id","Year"))

m34 <- lm(data = HCI34.2_p, Schlifetert~ Houssp_tert)
summary(m34)
qplot(data = HCI34.2_p, Schlifetert, Houssp_tert)

m34_d1 <- lm(data = HCI34.2_p, Schlifetert~ Houssp_tert+Cluster)
m34_d2 <- lm(data = HCI34.2_p, Schlifetert~ Houssp_tert*Cluster)

waldtest(m34,m34_d1) # H_0 accepted
waldtest(m34_d1,m34_d2) # H_0 accepted

fe34 <- plm (data = HCI34.2_p, Schlifetert~ Houssp_tert, model = "within")
re34 <- plm (data = HCI34.2_p, Schlifetert~ Houssp_tert, model = "random")

summary(fe34) # p-value: 8.8121e-05
summary(re34) # p-value: 0.66775

phtest(fe34,re34)
pFtest(fe34,m34)

str(HCI)
HCI340 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, Schlifetert, Gvsp_tert, GDPpc) 
HCI340.2 <- HCI340 %>% filter_all(all_vars(!is.na(.)))
HCI340.2_p <-pdata.frame(HCI340.2, index=c("cn_id","Year"))

m340 <- lm(data = HCI340.2_p, log(Schlifetert)~log(GDPpc)+ log(Gvsp_tert))
summary(m340)

gqtest(m340, order.by = ~ Gvsp_tert, data =  HCI340.2_p, fraction = 0.2) # accepted

m340_d1 <- lm(data = HCI340.2_p, log(Schlifetert)~log(GDPpc)+ log(Gvsp_tert)+Cluster)
m340_d2 <- lm(data = HCI340.2_p, log(Schlifetert)~log(GDPpc)+ log(Gvsp_tert)*Cluster)

waldtest(m340,m340_d1) #H_0 rejected
waldtest(m340,m340_d2) #H_0 rejected
mtable(m340,m340_d1,m340_d2, summary.stats=c("adj. R-squared"))

fe340 <- plm(data = HCI340.2_p, log(Schlifetert)~log(GDPpc)+ log(Gvsp_tert), model = "within")
re340 <- plm(data = HCI340.2_p, log(Schlifetert)~log(GDPpc)+ log(Gvsp_tert), model = "random")

phtest(fe340,re340) # H_0 rejected
pFtest(fe340, m340)

summary(fe340)


# Adult (15-60) survival rate

str(HCI)
HCI35 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_3.1_Adsurv, Schlifetert) 
HCI35.2 <- HCI35 %>% filter_all(all_vars(!is.na(.)))
HCI35.2_p <-pdata.frame(HCI35.2, index=c("cn_id","Year"))

m35 <- lm(data = HCI35.2_p, C_3.1_Adsurv~ Schlifetert)
summary(m35)
qplot(data = HCI35.2_p, Schlifetert, C_3.1_Adsurv)

m35_d1 <- lm(data = HCI35.2_p, C_3.1_Adsurv~ Schlifetert+Cluster)
m35_d2 <- lm(data = HCI35.2_p, C_3.1_Adsurv~ Schlifetert*Cluster)

waldtest(m35,m35_d1) # H_0 accepted
waldtest(m35,m35_d2) # H_0 rejected
waldtest(m35_d1,m35_d2)
mtable(m35,m35_d1,m35_d2,summary.stats=c("adj. R-squared"))

fe35_d2 <- plm(data = HCI35.2_p, C_3.1_Adsurv~ Schlifetert*Cluster, model = "within")
re35_d2 <- plm(data = HCI35.2_p, C_3.1_Adsurv~ Schlifetert*Cluster, model = "random")

summary(fe35) # p-value: 0.00040476
summary(re35) # p-value: 3.1377e-06

phtest(fe35_d2,re35_d2)
plmtest(re35_d2, type=c("bp"))

summary(re35_d2)

str(HCI)
HCI36 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_3.1_Adsurv, GovhspAB, PrivhshmC, HoushspD) 
HCI36.2 <- HCI36 %>% filter_all(all_vars(!is.na(.)))
HCI36.2_p <-pdata.frame(HCI36.2, index=c("cn_id","Year"))

m36.1 <- lm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB)
m36.2 <- lm(data = HCI36.2_p, C_3.1_Adsurv~PrivhshmC)
m36.3 <- lm(data = HCI36.2_p, C_3.1_Adsurv~HoushspD)
m36.4 <- lm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB+ PrivhshmC)
m36.5 <- lm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB+ PrivhshmC + HoushspD)
m36.6 <- lm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB + HoushspD)
summary (m36.1)
summary(m36.2)
summary(m36.3)
summary(m36.4)
summary(m36.5) # лучшая по adj. R-squared и BIC, штраф за высокий AIC не слишком большой
summary(m36.6)

gqtest(m36.5, order.by = ~ GovhspAB, data =  HCI36.2_p, fraction = 0.2)

waldtest(m36.4,m36.5) # H_0 rejected

mtable(m36.1,m36.2,m36.3,m36.4,m36.5,m36.6, summary.stats=c("R-squared", "adj. R-squared","AIC","BIC"))

m36.5_d1 <- lm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB+ PrivhshmC + HoushspD+ Cluster)
m36.5_d2 <- lm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB*Cluster+ PrivhshmC + HoushspD)
m36.5_d3 <- lm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB*Cluster+ PrivhshmC*Cluster + HoushspD)
m36.5_d4 <- lm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB*Cluster+ PrivhshmC*Cluster + HoushspD*Cluster) # лучшая
m36.5_d5 <- lm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB*Cluster+ PrivhshmC + HoushspD*Cluster)

waldtest(m36.5,m36.5_d1) # H_0 rejected
waldtest(m36.5_d1,m36.5_d2) # H_0 rejected
waldtest(m36.5_d2, m36.5_d3) # H_0 accepted
waldtest(m36.5_d2,m36.5_d4) # H_0 rejected
waldtest(m36.5_d4,m36.5_d5) # H_0 accepted

mtable(m36.5_d2, m36.5_d4, m36.5_d5, summary.stats=c("adj. R-squared"))

fe36.5_d4 <- plm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB*Cluster+ PrivhshmC*Cluster + HoushspD*Cluster, model = "within")
re36.5_d4 <- plm(data = HCI36.2_p, C_3.1_Adsurv~GovhspAB*Cluster+ PrivhshmC*Cluster + HoushspD*Cluster, model = "random")

summary(fe36.5) # p-value: 0.010229
summary(re36.5) # p-value: 8.623e-05

phtest(fe36.5_d4,re36.5_d4) # H_0 accepted
plmtest(re36.5_d4, type=c("bp")) # H_0 accepted

summary(re36.5_d4)

0.694 + 0.129

str(HCI)
HCI37 <- dplyr::select(HCI, Country, cn_id, Cluster, Year, C_3.1_Adsurv, Govsocsp_USD2017, GDPpc, Urb) 
HCI37.2 <- HCI37 %>% filter_all(all_vars(!is.na(.)))
HCI37.2_p <-pdata.frame(HCI37.2, index=c("cn_id","Year"))

m37 <- lm(data = HCI37.2_p, C_3.1_Adsurv~Govsocsp_USD2017 + Urb)
summary(m37)

gqtest(m37, order.by = ~ Govsocsp_USD2017, data =  HCI37.2_p, fraction = 0.2) # Accepted

m37_d1 <- lm(data = HCI37.2_p, C_3.1_Adsurv~Govsocsp_USD2017 + Urb +Cluster)
m37_d2 <- lm(data = HCI37.2_p, C_3.1_Adsurv~Govsocsp_USD2017*Cluster + Urb)

waldtest(m37,m37_d1) # H_0 rejected
waldtest(m37_d1,m37_d2) # H_0 accepted
mtable(m37,m37_d1,m37_d2,summary.stats=c("adj. R-squared"))

fe37_d1 <- plm(data = HCI37.2_p, C_3.1_Adsurv~Govsocsp_USD2017 + Urb +Cluster, model = "within")
re37_d1 <- plm(data = HCI37.2_p, C_3.1_Adsurv~Govsocsp_USD2017 + Urb +Cluster, model = "random")

summary(fe37) # p-value: 0.0381
summary(re37) # p-value: 0.002997

phtest(fe37_d1,re37_d1) # H_0 accepted
plmtest(re37_d1, type=c("bp")) # H_0 accepted

summary(re37_d1)

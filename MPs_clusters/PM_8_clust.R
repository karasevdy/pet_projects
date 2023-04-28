# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

library("lubridate") # работа с датами

library("sandwich") # vcovHC, vcovHAC
library("lmtest") # тесты
library("car") # еще тесты
library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("broom") # манипуляции
library("ggplot2") # графики

library("quantmod") # загрузка с finance.google.com
library("rusquant") # загрузка с finam.ru
library("sophisthse") # загрузка с sophist.hse.ru
library("Quandl") # загрузка с Quandl

devtools::install_github("dgrtwo/broom")
devtools::install_github("cran/bstats")
devtools::install_github("bdemeshev/sophisthse")
devtools::install_github("arbuzovv/rusquant")

# Работа с датами с помощью пакета "lubridate"
x <- c("2012-04-15", "2011-08-17")
y <- ymd(x)
y
y+days(20)
y - years(10)
day(y)
month(y)
vignette("lubridate")

#

x <- rnorm(5)
x
y <- ymd("2014-01-01")+days(0:4)
y
# упорядочевание наблюдей x по временному ряду y 
ts <- zoo(x,order.by = y)
ts

lag(ts,1)

diff(ts)


# ряд с ежеквартальными данными
ts2 <- zooreg(x,start = as.yearqtr("2014-01"), freq = 4)
ts2

# ряд с ежемесячными данными 
ts3 <- zooreg(x,start = as.yearmon("2014-01"), freq = 12)
ts3

data("Investment")
help("Investment")

start(Investment)
end(Investment)
time(Investment)
coredata(Investment)

dna <- Investment
dna[1,2] <- NA
dna[5,3] <- NA
dna

# Методы заполнения NA (линейная апроксимация - сложит два по краям и возьмет среднее)
na.approx(dna)
# Метод заполнения NA (вставить предшествующее значений)
na.locf(dna)

# загрузка данных с сайта sophist.hse

a <- sophisthse("POPNUM_Y")
a

# quandl
b <- Quandl("FRED/GNP")
b

# finance.google.com - больше не предоставляет, вместо него yahoo
Sys.setlocale("LC_TIME","C") # чтобы не было различий в формате дат м-ду США (yy.dd.mm) и РФ (yy.mm.dd)
getSymbols(Symbols = "AAPL", from="2010-01-01", to="2014-02-03", src = "yahoo")

head(AAPL)
tail(AAPL)

getSymbols(Symbols = "GAZP", from="2011-01-11", to="2014-09-09",src="mfd")


head(GAZP)

help(rusquant)

plot(AAPL)
autoplot(AAPl[,1:4])
autoplot(AAPL[,1:4],facets = NULL)

chartSeries(AAPL)

d <- as.zoo(Investment)
d <- as.data.frame (Investment)
d
autoplot(d[,1:2],facets = NULL)
d <- d %>% filter_all(all_vars(!is.na(.)))
d

# во временных рядах логично предполагать автокорреляцию, т.к. сегодня завсист от вчера
# поборемся с ней 

model <- lm(data=d, RealInv~RealInt+RealGNP)

summary(model)
coeftest(model)
confint(model)

#добавляем остатки в модель 
d_aug <- augment(model, d)
glimpse(d_aug)
qplot(data=d_aug,lag(.resid),.resid)

# при автокорреляции проверке гипотез (о значимости коэффициентов), которую R делает по умолчанию, доверять нельзя.

vcov(model) # эта корреляционная матрица при автокорреляции не состоятельна
vcovHAC(model) # эта корреляционная матрица состоятельна и при автокорреляции, и при гетероскедастичности

coeftest(model, vcov. = vcovHAC(model))
conftable <- coeftest(model, vcov. = vcovHAC(model))
ci <- data.frame(estimate=conftable[,1], se_ac=conftable[,2])
ci <- mutate(ci, left_95=estimate-1.96*se_ac, right_95=estimate+1.96*se_ac)

ci
# из-за эффекта автокорреляции часть оценок меннее точны, доверительные интервалы расширились

# тест Дарбина-Уотсона
# H_0 - нет автокорреляции (1-го порядка)

dwt(model) # H_0 rejected
res <- dwt(model)
res$dw
res$p
res$r # оценка автокорреляции остатков сомдели согласно D-W теста

# тест BG
# H_0 нет автокорреляции (n-ного порядка)
bgtest(model, order = 2) # H_0 не отвергается p-value= 0.1393

devtools::install_github('lachlandeer/hayashir')
library(hayashir) 
data(griliches)

head(griliches)

gr <- griliches
str(gr)
m_gr <- lm(data=gr, log_wage_80 ~ age_80 + iq_score + education_80 + experience_80)
summary(m_gr)

vcov(m_gr)
-2.651401e-07+3.101489e-07

matrix1 <-  vcovHC(m_gr, type = "HC3")
matrix2 <- vcovHC(m_gr, type = "HC1")
matrix3 <- vcovHC(m_gr, type = "HC4m")
matrix4 <- vcovHC(m_gr, type = "HC5")

coeftest(m_gr)
coeftest(m_gr, vcov. = matrix1) # Std. Error (experience_80) = 0.0044312
coeftest(m_gr, vcov. = matrix2) # Std. Error (experience_80) = 0.0043926
coeftest(m_gr, vcov. = matrix3) # Std. Error (experience_80) = 0.0044409
coeftest(m_gr, vcov. = matrix4) # Std. Error (experience_80) = 0.0044097

bptest(m_gr)
bptest(m_gr, data=gr, varformula = ~ age_80)

gqtest(m_gr, order.by = ~ iq_score, data = gr, fraction = 0.2)

data(Solow)

devtools::install_github('Ecdat/data/Solow.rda')
data(Solow)
library("rlms")

devtools::install_github("vincentarelbundock/Rdatasets")
data(Solow)

Solow <-read_excel("Solow.xlsx")
glimpse(Solow)
head(Solow)
m_solow <- lm(data = Solow, q~k+A)
summary(m_solow)

vcov(m_solow)
vcovHAC(m_solow)

9.204408e-05-8.759004e-05

1.178608e-05-8.402555e-06

dwt(m_solow)
sol_res <- dwt(m_solow)
sol_res$dw
bgtest(m_solow, order = 3)


Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="yahoo")
plot(MSFT$MSFT.Close, main = "")

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "AAPL",from="2010-01-01", to="2014-02-03",src="yahoo")
plot(AAPL$AAPL.Close, main = "")

getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="yahoo")
plot(MSFT$MSFT.Close, main = "")

getSymbols(Symbols = "INTC",from="2010-01-01", to="2014-02-03",src="yahoo")
plot(INTC$INTC.Close, main = "")

getSymbols(Symbols = "GOOG",from="2010-01-01", to="2014-02-03",src="yahoo")
plot(GOOG$GOOG.Close, main = "")

Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "GOOG",from="2010-01-01", to="2014-02-03",src="yahoo")
getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="yahoo")

ms_cl <- MSFT$MSFT.Close
ms_cl
MSFT <- read_excel("MSFT.xlsx")
str(MSFT)

m_msft <- lm(data = MSFT, MSFT_cl~lag1+lag2)
summary(m_msft)

g_cl <- GOOG$GOOG.Close
g_cl
g_cl <- zoo(g_cl[:2],order.by = g_cl[:1])

Goog <-read_excel("Google.xlsx")

g_cl <- dplyr::mutate(g_cl, Cl1=zoo::lag(g_cl$GOOG.Close))
lead(g_cl)

lag(g_cl,1)

g_cl2 <-  c(lag(g_cl,),na.rm = TRUE)

d <-data.frame(g_cl=g_cl,g_cl1=lag(g_cl,1), g_cl2=lag(g_cl,2))

m_goog <- lm()

# 11 -2.651401e-07
# 13 HC4m
# 15 0.69612
# 16 4.45404e-06
# 17 0.6273489 (0.6134221 - не верный, хотя верный)
# 18 20.0399
# 19 - 2 Microsoft

install.packages("igraph")
library("igraph")

v1 <- c(1,5,11,33,45)
v1
v2 <- c("hello","world")
v2
v3 <- c(TRUE,TRUE,FALSE)
v3
v4 <- c(v1,v2,v3,"boo")
v4 # Combining different types of elements in one vector 
#will coerce the elements to the least restrictive type
v <- rep(1:10, each=2) # Repeat each element from 1 to 10 twice 
v
v5 <- seq(10,20,2) # sequence: numbers between 10 and 20, in jumps of 2
v5
v6 <- rep(1,5) # 1,1,1,1,1
length(v6) # the length of a vector
v1 + v5
v1 <- 1:5
v2 <- rep(1,5)

v1 + v2
v1+1 # Add 1 to each element
v1*2 # Multiply each element by 2
sum(v1)
mean(v1)
sd(v1)
cor(v1,v1*5)

Logical operations:
  
  v1>2 # Each element is compared to 2, returns logical vector
v1==v2 # Are corresponding elements equivalent, returns logical vector.
v1!=v2 # Are corresponding elements *not* equivalent? Same as !(v1==v2)
(v1>2) | (v2>0) # | is the boolean OR, returns a vector.
(v1>2) & (v2>0)

m <- rep(1,20) # вектор из 20 элементов, все из которых 1
dim(m) <- c(5,4) # массив 5x4 
m

m <- matrix(data=1, nrow=5, ncol=4)
m

m <- cbind(1:5, 5:1, 5:9) # массив из 3х колонок (colomns) 1-ая от 1 до 5, 2-ая от 5 до 1, 3-я от 5 до 9
m <- rbind(1:5, 5:1, 5:9) # массиф из 3х строк (rows) 1-ая от 1 до 5, 2-ая от 5 до 1, 3-я от 5 до 9

m <-  matrix(1:10,10,10)

Выбор элементов матрицы:
  
  m[2,3] # из матрица m взять значение из 2-ой строки, 3-его столбца 
m[2,] # вся вторая строка матрицы m как вектор
m[,2] # весь второй столбец матрицы m как вектор
m[1:2,4:6] # строки 1 и 2, столбцы 4,5 и 6
m[-1,] # все строки за исключением первой

m[1,]==m[,1] # are elements in row 1 equivalent to corresponding elements from column 1
m>3 # a logical matrix: TRUE for m elements >3, FALSE otherwise:
m[m>3] # selects only TRUE elements - that is ones grater than 3:

t(m) # транспонировать
m <- t(m) # положить в m транспонированный m
m %*% t(m) # перемножение матриц
m * m # 

a <- array(data = 1:18, dim=c(3,3,2)) # 3d with dimensions 3x3x2
a <- array (1:18, c(3,3,2))

11 <- list(boo=v1, foo=v2, moo=v3, zoo="Animals!") # A list with four components

VRU<-read.csv(('soziv_8_dep.csv'),header=TRUE,sep=",")
describe(VRU)
glimpse(VRU)
head(VRU)

VRU8<-read.csv(('rollcall_8skl.csv'),header=TRUE,sep=",")
VRU9 <-read.csv(('rollcall_9skl.csv'),header=TRUE,sep=",")
ident8 <- read.csv(('identifiers_8skl.csv'),header=TRUE,sep=",")

library("tidyverse")
str(VRU8)
glimpse(VRU8.3)

VRU8.2 <- dplyr::select(VRU8, -id_mp, -fraction_id, -fraction_name)

VRU8.3 <- dplyr::select(VRU8, -id_mp, -fraction_id)
str(VRU8.3)
describe(VRU8.3)

cn <- colnames(VRU8.3)
cn
cn2=as.vector(cn)
cn2
cn3 <- cn2[cn2 != "full_name"]
cn3
cn4 <- cn3[cn3 != "fraction_name"]
cn4
print(cn4)

_______

describe(VRU8.3)

VRU8.3$X52363.2008

VRU8.3.2 <- mutate(VRU8.3, lapply(VRU8.3[,c(3:24105,24105)]) = case_when(
  lapply(VRU8.3[,c(3:24105,24105)]) == "1.Yes" ~ 1,
  lapply(VRU8.3[,c(3:24105,24105)]) == "2.No" ~ -1,
  lapply(VRU8.3[,c(3:24105,24105)]) == "4.Did not vote" ~ 0,
  lapply(VRU8.3[,c(3:24105,24105)]) == "0.Absent" ~ -0.5, 
  lapply(VRU8.3[,c(3:24105,24105)]) == "3.Abstain" ~ -0.25,
  TRUE ~ NA_real_
))

VRU8.3.2 <- mutate(VRU8.3, X52363.2008 = case_when(
  X52363.2008 == "1.Yes" ~ 1,
  X52363.2008 == "2.No" ~ -1,
  X52363.2008 == "4.Did not vote" ~ 0,
  X52363.2008 == "0.Absent" ~ -0.5, 
  X52363.2008 == "3.Abstain" ~ -0.25,
  TRUE ~ NA_real_
))

VRU8.3[ , 3:24105 ]

VRU8.3.2 <- mutate(VRU8.3, VRU8.3[ , 3:24105 ] = case_when(
  VRU8.3[ , 3:24105 ] == "1.Yes" ~ 1,
  VRU8.3[ , 3:24105 ] == "2.No" ~ -1,
  VRU8.3[ , 3:24105 ] == "4.Did not vote" ~ 0,
  VRU8.3[ , 3:24105 ] == "0.Absent" ~ -0.5, 
  VRU8.3[ , 3:24105 ] == "3.Abstain" ~ -0.25,
  TRUE ~ NA_real_
))

#Верный вариант перекодирования

VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "1.Yes" ] <- 1
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "2.No" ] <- -1
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "4.Did not vote" ] <- 0
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "0.Absent" ] <- -0.5
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "3.Abstain" ] <- -0.25
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "" ] <- 0

lapply(VRU8.3[,c(3:24105,24105)], as.numeric)
VRU8.3 <- mutate_each(VRU8.3, "numeric")

#верное решение перекодирования as.numeric
VRU8.4 <- VRU8.3 %>% mutate_at(c(3:24105), as.numeric)

#перекодировали название партий в более короткие и в скобках

VRU8.4[ , 2 ][ VRU8.4[ , 2 ] == "Фракція ПАРТІЇ \"БЛОК ПЕТРА ПОРОШЕНКА\"" ] <- "(БПП)"
VRU8.4[ , 2 ][ VRU8.4[ , 2 ] == "Фракція політичної партії \"Всеукраїнське об’єднання \"Батьківщина\"" ] <- "(Батьковщина)"
VRU8.4[ , 2 ][ VRU8.4[ , 2 ] == "Фракція  Політичної партії \"НАРОДНИЙ ФРОНТ\"" ] <- "(НФ)"
VRU8.4[ , 2 ][ VRU8.4[ , 2 ] == "Група \"Воля народу\"" ] <- "(Воля народа)"
VRU8.4[ , 2 ][ VRU8.4[ , 2 ] == "Група \"Партія \"Відродження\"" ] <- "(Возрождение)"
VRU8.4[ , 2 ][ VRU8.4[ , 2 ] == "Фракція Радикальної партії Олега Ляшка" ] <- "(РП О.Л.)"
VRU8.4[ , 2 ][ VRU8.4[ , 2 ] == "Фракція Політичної партії \"Об’єднання \"САМОПОМІЧ\"" ] <- "(Сам.пом)"
VRU8.4[ , 2 ][ VRU8.4[ , 2 ] == "Фракція Політичної партії \"Опозиційний блок\"" ] <- "(Оп.Бл)"
VRU8.4[ , 2 ][ VRU8.4[ , 2 ] == "Позафракційні" ] <- "(внефракционный)"

#Объединим full_name fraction_name

class(VRU8.4)
VRU8.5 <- as_tibble(VRU8.4)
class(VRU8.5)

library("tidyr")
VRU8.6 <- unite(VRU8.5, MP, c(full_name, fraction_name), remove=FALSE)

help("unite")

VRU8.7 <- dplyr::select(VRU8.6, -full_name, -fraction_name)

# первая колонка с именами PM в названия строк
library("textshape")
VRU8.8 <-column_to_rownames(VRU8.7)
# или
VRU8.8 <-column_to_rownames(VRU8.7, 'MP')

k_means_VRU8 <- kmeans(VRU8.8, centers = 4)
attributes(k_means_VRU8)
k_means_VRU8$size

# Визуализация результатов

fviz_cluster(object = k_means_VRU8, data = VRU8.8, labelsize = 3, geom = c( ,"text"),
             ellipse.type = "norm")

help("fviz_cluster")

# k=6
k_means_VRU8_k6 <- kmeans(VRU8.8, centers = 6)
fviz_cluster(object = k_means_VRU8_k6, data = VRU8.8, labelsize = 3,
             ellipse.type = "norm")

#k=3
k_means_VRU8_k3 <- kmeans(VRU8.8, centers = 3)
fviz_cluster(object = k_means_VRU8_k3, data = VRU8.8, labelsize = 3,
             ellipse.type = "convex")

# Формальный тест на то, сколько кластеров нужно

library("factoextra")

# Elbow method или метод локтя
g1 <- fviz_nbclust(VRU8.8, kmeans, method = 'wss') +
  labs(subtitle = 'Elbow method')

g1

# Silhouette method или метод силуэта
g2 <- fviz_nbclust(VRU8.8, kmeans, method = 'silhouette') +
  labs(subtitle = 'Silhouette method')

g2

g3 <- fviz_nbclust(VRU8.8, kmeans, method = 'gap_stat') +
  labs(subtitle = 'Gap statistic method')

g3

devtools::install_github('thomasp85/patchwork')
library("patchwork")

# Функция для замены числовых значений в датафрейме

df.Rep <- function(.data_Frame, .search_Columns, .search_Value, .sub_Value){
  .data_Frame[, .search_Columns] <- ifelse(.data_Frame[, .search_Columns]==.search_Value,.sub_Value/.search_Value,1) * .data_Frame[, .search_Columns]
  return(.data_Frame)
}

df.Rep(VRU8.3, 3:24105, "1.Yes", 1)

help(lapply)

VRU8.3.2

VRU8.3.2 <- dplyr::select(VRU8.3, -full_name, -fraction_name)

VRU8.3.2 <- mutate(VRU8.3.2, Predilection_1 = case_when(
  Predilection_1 == 1 ~ 0.6,
  Predilection_1 == 2 ~ 1.2,
  Predilection_1 == 3 ~ 2.5,
  Predilection_1 == 4 ~ 1.8,
  TRUE ~ NA_real_
))


for(fname in c("file01.csv","file02.csv")) {
  temp <- read.csv(fname)
  all_data <- rbind(all_data,temp)
}

for(сolumns_name in cn4) {
  ifelse(grepl(colnames(VRU8.3))cn4),
  VRU8.3.2 <- read.csv(fname)
  all_data <- rbind(all_data,temp)
}




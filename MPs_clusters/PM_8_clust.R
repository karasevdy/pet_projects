# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK

library("dplyr") # манипуляции с данными
devtools::install_github("dgrtwo/broom")
library("broom") # манипуляции
library("ggplot2") # графики
library("rio") # открываются файлы разных форматов
library("stringr") # разбивка номинальных переменных
library("factoextra") # визуализации kmeans, pca
library("dendextend") # визуализация дендрограмм
library("tidyverse") # манипуляции с данными
library("tidyr") # манипуляции с данными (объединение номинальных переменных)
library("textshape") # vманипуляции с данными (превращение первой колонки в название строк)
devtools::install_github('thomasp85/patchwork')
library("patchwork") # размещение графиков рядом
library("ggpubr")
library("stringr")


# Открываем csv с результатами голосования и кодировочной книгой
VRU8<-read.csv(('rollcall_8skl.csv'),header=TRUE,sep=",")
ident8 <- read.csv(('identifiers_8skl.csv'),header=TRUE,sep=",")


str(VRU8)


# Удаляем ненужные переменные (столбцы): id депутата и id фракции
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

# Перекодируем значения всех переменных (столбцов с 3 по 24105) с результатами голосования
# Можно поиграть с "весами" - эффект будет, например Absent = -0,25 как и Abstrain = -0,25

VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "1.Yes" ] <- 1
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "2.No" ] <- -1
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "4.Did not vote" ] <- 0
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "0.Absent" ] <- -0.5
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "3.Abstain" ] <- -0.25
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "" ] <- 0

# Делаем все переменные с результатами голосования количественными (сохраняя структуру таблицы)

VRU8.4 <- VRU8.3 %>% mutate_at(c(3:24105), as.numeric)

# Перекодируем название партий/фракций в более короткие и в скобках, чтобы лучше читались на графике

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
# Разделим переменную (столбец) с ФИО депутата на 3, чтобы затем сделать "Фамилия ИО", чтобы лучше читался график 
VRU8.8 <- VRU8.7 %>% separate(full_name, c("surname", "name", "mid_name"), " ")


VRU8.8 <- dplyr::mutate(VRU8.8, name = substring(name,1,1))
VRU8.8 <- dplyr::mutate(VRU8.8, mid_name = substring(mid_name,1,1))

# Объединяем "Фамилия ИО" + "(Партия)" и удалим лишние колонки с промежутоными результатами объединения
VRU8.8 <- as_tibble(VRU8.8)
VRU8.8 <- unite(VRU8.8, IO, c(name, mid_name),sep = "", remove=FALSE)
VRU8.8 <- unite(VRU8.8, sur_IO, c(surname, IO), sep = "_", remove=FALSE)
VRU8.8 <- unite(VRU8.8, MP, c(sur_IO, fraction_name), sep = "_", remove=FALSE)

VRU8.8 <- dplyr::select(VRU8.8, -sur_IO, -fraction_name, -surname, -IO, -name, -mid_name)

# Первый столбец (переменную MP) превращаем в название строе
VRU8.8 <-column_to_rownames(VRU8.8)
# или
VRU8.8 <-column_to_rownames(VRU8.8, 'MP')

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




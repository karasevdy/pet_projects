
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
library(ggpubr)

# Открываем csv с результатами голосования и кодировочной книгой
VRU8<-read.csv(('rollcall_8skl.csv'),header=TRUE,sep=",")
Ident8 <- read.csv(('identifiers_8skl.csv'),header=TRUE,sep=",")

# Удаляем ненужные переменные (столбцы): id депутата и id фракции
VRU8.3 <- dplyr::select(VRU8, -id_mp, -fraction_id)

# Перекодируем значения всех переменных (столбцов с 3 по 24105) с результатами голосования
# Можно поиграть с "весами" - эффект будет, например Absent = -0,25 как и Abstrain = -0,25
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "1.Yes" ] <- 1
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "2.No" ] <- -1
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "4.Did not vote" ] <- 0
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "0.Absent" ] <- -0.5
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "3.Abstain" ] <- -0.25
VRU8.3[ , 3:24105 ][ VRU8.3[ , 3:24105 ] == "" ] <- 0

# Делаем все переменные с результатами голосования количественными (сохраняя структуру таблицы)
VRU8.3 <- VRU8.3 %>% mutate_at(c(3:24105), as.numeric)

# Перекодируем название партий/фракций в более короткие и в скобках, чтобы лучше читались на графике
VRU8.3[ , 2 ][ VRU8.3[ , 2 ] == "Фракція ПАРТІЇ \"БЛОК ПЕТРА ПОРОШЕНКА\"" ] <- "(БПП)"
VRU8.3[ , 2 ][ VRU8.3[ , 2 ] == "Фракція політичної партії \"Всеукраїнське об’єднання \"Батьківщина\"" ] <- "(Бтк)"
VRU8.3[ , 2 ][ VRU8.3[ , 2 ] == "Фракція  Політичної партії \"НАРОДНИЙ ФРОНТ\"" ] <- "(НФ)"
VRU8.3[ , 2 ][ VRU8.3[ , 2 ] == "Група \"Воля народу\"" ] <- "(ВлНр)"
VRU8.3[ , 2 ][ VRU8.3[ , 2 ] == "Група \"Партія \"Відродження\"" ] <- "(Взрж)"
VRU8.3[ , 2 ][ VRU8.3[ , 2 ] == "Фракція Радикальної партії Олега Ляшка" ] <- "(РПОЛ)"
VRU8.3[ , 2 ][ VRU8.3[ , 2 ] == "Фракція Політичної партії \"Об’єднання \"САМОПОМІЧ\"" ] <- "(СмПм)"
VRU8.3[ , 2 ][ VRU8.3[ , 2 ] == "Фракція Політичної партії \"Опозиційний блок\"" ] <- "(ОпБл)"
VRU8.3[ , 2 ][ VRU8.3[ , 2 ] == "Позафракційні" ] <- "(_)"

# Разделим переменную (столбец) с ФИО депутата на 3, чтобы затем сделать "Фамилия ИО", чтобы лучше читался график 
VRU8.3 <- VRU8.3 %>% separate(full_name, c("surname", "name", "mid_name"), " ")
library("stringr")

VRU8.3 <- dplyr::mutate(VRU8.3, name = substring(name,1,1))
VRU8.3 <- dplyr::mutate(VRU8.3, mid_name = substring(mid_name,1,1))

# Объединяем "Фамилия ИО" + "(Партия)" и удалим лишние колонки с промежутоными результатами объединения
VRU8.3 <- as_tibble(VRU8.3)
VRU8.3 <- unite(VRU8.3, IO, c(name, mid_name),sep = "", remove=FALSE)
VRU8.3 <- unite(VRU8.3, sur_IO, c(surname, IO), sep = "_", remove=FALSE)
VRU8.3 <- unite(VRU8.3, MP, c(sur_IO, fraction_name), sep = "_", remove=FALSE)

VRU8.3 <- dplyr::select(VRU8.3, -sur_IO, -fraction_name, -surname, -IO, -name, -mid_name)

# Первый столбец (переменную MP) превращаем в название строе
VRU8.3 <-column_to_rownames(VRU8.3)
# или
VRU8.3 <-column_to_rownames(VRU8.3, 'MP')

# Выделяем кластеры методом k-средних и визуализируем результаты


k_means_VRU8_k3 <- kmeans(VRU8.3, centers = 3)
k_means_VRU8_k2 <- kmeans(VRU8.3, centers = 2)
k_means_VRU8_k8 <- kmeans(VRU8.3, centers = 8)
k_means_VRU8_k4 <- kmeans(VRU8.3, centers = 4)
k_means_VRU8_k5 <- kmeans(VRU8.3, centers = 5)
k_means_VRU8_k7 <- kmeans(VRU8.3, centers = 7)

#уменьшаем размеры подписей, т.к. их слишком много (labelsize = 3)
install.packages("scales")
install.packages("broom", type="binary")

k3 <- fviz_cluster(object = k_means_VRU8_k3, data = VRU8.3, geom = c("point"), main = "k = 3",
                   ellipse.type = "convex", ggtheme = theme_bw(), show.clust.cent = FALSE)

k2 <- fviz_cluster(object = k_means_VRU8_k2, data = VRU8.3, main = "k = 2",
                   xlab = NULL,
                   ylab = NULL, geom = c("point"),
                   ellipse.type = "convex", ggtheme = theme_bw(), show.clust.cent = FALSE,
                   palette = c("#0098DE", "#FF311F"))

k8 <- fviz_cluster(object = k_means_VRU8_k8, data = VRU8.3, geom = c("point"), main = "k = 8",
                   ellipse.type = "convex", ggtheme = theme_bw(), show.clust.cent = FALSE)

# Проверяем верно ли раскрасили по фамилиям

k5 <- fviz_cluster(object = k_means_VRU8_k5, data = VRU8.3, geom = c("text"), main = "k = 5",
                   ellipse.type = "convex", ggtheme = theme_bw(), show.clust.cent = TRUE,
                   palette = c("#FF7B00", "#FF4545", "#0098DE", "#17CBFC", "#9424FF"),
                   labelsize = 3)

tiff("k5.tiff", units="in", width=20, height=20, res=300)
fviz_cluster(object = k_means_VRU8_k5, data = VRU8.3, geom = c("text"), main = "k = 5",
             ellipse.type = "convex", ggtheme = theme_bw(), show.clust.cent = TRUE,
             palette = c("#FF7B00", "#FF4545", "#0098DE", "#17CBFC", "#9424FF"),
             labelsize = 3)
dev.off()

fviz_cluster(object = k_means_VRU8_k7, data = VRU8.3, geom = c("text"), main = "k = 7",
             ellipse.type = "convex", ggtheme = theme_bw(), show.clust.cent = TRUE,
             palette = c("#FF7B00", "#17CBFC", "#38D435", "#FCFF17", "#0098DE", "#9424FF", "#FF4545"),
             labelsize = 3)

# красный - #FF4545
# оранжевый #FF7B00
# желтый -  #FCFF17
# зеленый - #38D435
# голубой - #17CBFC
# синий -   #0098DE
# фиолетовый #9424FF 

help("fviz_cluster")



k2 <- fviz_cluster(object = k_means_VRU8_k2, data = VRU8.3, main = "k = 2",
                   xlab = NULL,
                   ylab = NULL, geom = c("point"),
                   ellipse.type = "convex", ggtheme = theme_bw(), show.clust.cent = FALSE,
                   palette = c("#0098DE", "#FF4545"))

k3 <- fviz_cluster(object = k_means_VRU8_k3, data = VRU8.3, geom = c("point"), main = "k = 3",
                   ellipse.type = "convex", ggtheme = theme_bw(), show.clust.cent = FALSE,
                   palette = c("#FF4545", "#9424FF", "#0098DE"))

k4 <- fviz_cluster(object = k_means_VRU8_k4, data = VRU8.3, geom = c("point"), main = "k = 4",
                   ellipse.type = "convex", ggtheme = theme_bw(), show.clust.cent = FALSE,
                   palette = c("#FF4545", "#0098DE", "#FF7B00", "#9424FF"))

k8 <- fviz_cluster(object = k_means_VRU8_k8, data = VRU8.3, geom = c("point"), main = "k = 8",
                   ellipse.type = "convex", ggtheme = theme_bw(), show.clust.cent = FALSE,
                   palette = c("#FF7B00", "#17CBFC", "#38D435", "#9888BD", "#FCFF17", "#0098DE", "#9424FF", "#FF4545"))

Clust <- (k2 + k3)/(k4 + k8)

tiff("Clust_8_2_3_4_", units="in", width=12, height=10, res=200)
Clust
dev.off()

# Формальный тест на то, сколько кластеров методом k-средних нужно

library("factoextra")

# (1) Elbow method или метод локтя
g1 <- fviz_nbclust(VRU8.3, kmeans, method = 'wss') +
  labs(subtitle = 'Elbow method')
g1

# (2) Silhouette method или метод силуэта
g2 <- fviz_nbclust(VRU8.3, kmeans, method = 'silhouette') +
  labs(subtitle = 'Silhouette method')

g2

# 
g3 <- fviz_nbclust(VRU8.3, kmeans, method = 'gap_stat') +
  labs(subtitle = 'Gap statistic method')

g3

gg <- (g1 + g2)/g3

tiff("gg", units="in", width=12, height=10, res=200)
gg
dev.off()

# Иерархическая кластеризация и визуализация расстояний 

VRU8_dist <- dist(VRU8.3, method = 'euclidian')

tiff("matrix_euclidian.tiff", units="in", width=50, height=30, res=500)
fviz_dist(VRU8_dist)
dev.off()

# Дендрограмма

VRU8_hcl <- hcut(VRU8.3, k = 5)
fviz_dend(VRU8_hcl,
          cex = 0.15, # размер подписи
          color_labels_by_k = TRUE, # цвет подписей по группам
          palette = c("#FF7B00", "#FF4545", "#0098DE", "#17CBFC", "#9424FF")) # подбор цветов для кластеров 

# Экспорт графика с большим разрешением и вытягиванием по ширине (width)
tiff("dndr_VRU8_5.tiff", units="in", width=25, height=16, res=500)
fviz_dend(VRU8_hcl,
          cex = 0.23, # размер подписи
          color_labels_by_k = TRUE, # цвет подписей по группам
          palette = c("#FF7B00", "#FF4545", "#0098DE", "#17CBFC", "#9424FF"))
dev.off()

help ("fviz_dend")

VRU8_hcl8 <- hcut(VRU8.3, k = 8) 
fviz_dend(VRU8_hcl8, cex = 0.15, color_labels_by_k = TRUE, type = c("circular"))

tiff("circ_dendr_VRU8_k=8.tiff", units="in", width=10, height=10, res=700)
fviz_dend(VRU8_hcl8, cex = 0.15, color_labels_by_k = TRUE, type = c("circular"))
dev.off()

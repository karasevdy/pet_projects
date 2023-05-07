
# Кластерный анализ результатов голосования депутатов парламента
## Описание проекта
Неформальная структура парламента была выявлена методом кластерного анализа (иерархического и методом k-средних) результатов поименного голосования депутатов всех сессий данного созыва (более 24 тыс. законопроектов) в R-Studio. Оптимальное количество кластеров, выделяемых методом k-средних было установлено методом локтя (Elbow method), силуэта (Silhouette method) и GAP-анализа (Gap statistic method). Также была рассчитана матрица евклидова расстояния между депутатами и визуализированы результаты иерархической кластеризации.

## Описание данных
База данных [Ukrainian legislative networks dataset (U-LeNet)](https://www.kaggle.com/datasets/oleksastepaniuk/ukrainian-parliament-voting) была скачена с платформы Kaggle
<br><br>
Результаты проекта представлены на конференции [Kazandigitalweek](https://kazandigitalweek.com/ru/site)
## Используемые библиотеки
*dplyr, broom, ggplot2, rio, stringr, factoextra, dendextend, tidyverse, tidyr, textshape, patchwork*

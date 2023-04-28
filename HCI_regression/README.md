# Социальные расходы и индекс человеческого капитала в развивающихся странах
## Описание проекта
С помощью регрессионного анализа панельных данных по 20 развивающимся странам за 7 периодов (2000-2019) было доказано, что разные приоритеты в социальных расходах оказывают статистически значимое влияние на динамику компонентов индекса человеческого капитала. В ряд регрессионных моделей со случайными и фиксированными эффектами добавлялась дами-переменная, фиксирующая различия в приоритетах социальных расходах стран. Лучшая модель выбиралась по итогам статистических  тестов: Дарбина-Ву-Хаусмана, Бройша—Пагана, Вальда. Статистические гипотезы проверялись на статистически значимых коэффициентах уравнений лучшей модели. 
<br><br> Пример гипотезы: развивающиеся страны приоритезирующие расходы на первичную и вторичную ступень образования получают более высокие результаты в терминах соответствующих компонент индекса человеческого капитала (доля учащихся на первичной и вторичной ступени образования от тех, кто по возрасту должен учиться на этих ступенях(TNER); гармонизированные результаты международных образовательных тестов TIMSS, PISA, PIRLS)
## Описание данных
| **Переменные**  | **Источник**           |
| :-------------- | :--------------------- |
| Total net enrolment rate (TNER) by level of education (in %)
Repetition rate by level of education (in %)
School life expectancy by level of education (in years)
Initial government funding per student by level of education (in constant PPP$)
Household funding per student by level of education (in constant PPP$)| [UNESCO Institute for Statistics](http://data.uis.unesco.org)|
|||
|Harmonized Learning Outcome (HLO) on reading at primary school (for both sexes)
Harmonized Learning Outcome (HLO) on math at secondary school (for both sexes)
Harmonized Learning Outcome (HLO) on science at secondary school (for both sexes)|Harmonized Learning Outcomes (HLO) Database(Patrinos, Angrist, 2018)|
|||
|GDP (in constant PPP$)
Population, total
Urbanization rate (%) |[World Bank World Development Indicators database](https://data.worldbank.org/indicator)|
|||
|Government schemes and compulsory contributory health care financing schemes (in constant PPP$ per capita)
Compulsory contributory health insurance schemes (in constant PPP$ per capita)
Voluntary health insurance schemes (in constant PPP$ per capita)
Household out-of-pocket payment (in constant PPP$ per capita)|[WHO Global Health Expenditure Database](https://apps.who.int/nha/database)|
|||
|Mortality rates for 15–60-year-olds|[UNDP](https://data.unicef.org/resources/resource-topic/malnutrition/)|
Under-5 mortality rate (per 1000 birth)|[UNICEF WHO WBG](https://population.un.org/wpp/)| 
Children under-5 stunting rate (%)|[UN IGME](https://childmortality.org)|
 

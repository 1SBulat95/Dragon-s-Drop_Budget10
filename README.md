Перед запуском программы необходимо скачать все файлы (форматов .xlsx и .R). 
Далее запустить файл arima_budget_1.R/arima_budget_2.R/arima_budget_3.R с помощью программы RStudio. 
Код выводится в скрипте программы в левом верхнем окошке. 
Программа выполняется нажатием комбинации клавиш "CTRL + ENTER" на каждой строке кода. 
Промежуточные результаты выполнения отображаются в консоле программы в левом нижнем окошке. 
Графики выводятся в правом нижнем окошке. Окружение (данные и загружаемые базы) находятся в правом верхнем окошке.
Комментарии обозначены символом "#".

МОДЕЛИ:

1. arima_budget_1.R:
Модель авторегрессии и скользящего среднего (без определения типа процесса) доходов консолидированного бюджета субъекта РФ:
 - файл Excel с исходными данными;
 - файл R программы RStudio.
Точность прогноза: 73%.

2. arima_budget_2.R:
Модель авторегрессии и скользящего среднего (с определением типа процесса и интегрированием) доходов консолидированного бюджета субъекта РФ:
- файл R программы RStudio.
Точность прогноза: 99,69%.

3. arima_budget_3.R:
Модель авторегрессии и скользящего среднего (с включением экзогенной переменной) доходов консолидированного бюджета субъекта РФ:
- файл Excel с исходными данными (доходы, расходы);
- файл R программы RStudio.
Точность прогноза: 98,84%.


# создаем вектор и присваиваем это значение переменной s
# In R, the c() function returns a vector (a one dimensional array).
# "c" - предполагаю, кортеж
# В R присваивание описывается стрелкой "<-"
s<-c(1,2,3,4)

# demo() - показывает возможности (демонстрация) какого-либо пакета/функции
# demo(graphics, package = "graphics", ask = TRUE)
# help() - помощь по команде
# str() - переводит в строку

# установка пакета
install.packages("zoo")
# пакет нежно подключать. Подключать скачанные пакеты нужно через команду 
# library(...)
library(zoo)
# пакеты привязаны к проекту (скачиваются в рамках проекта, а не глобально)

install.packages("readxl")
library(readxl)

D<-read_excel("demo.xlsx")
View(D)

F<-read.csv("https://raw.githubusercontent.com/junaart/ForStudents/master/R/Lesson_2/demo.csv", sep=";")
View(F)

# вектор последовательно идущих чисел от 1 до 1000
s<-c(1:1000)

# Итерация с единицы
s[1]
length(s)
s[3:10]
s[c(5:500, 800, 900)]
# выбор по условиям (фильтр)
s[s>700]

# mean() - средне-арифметическое
s[s>mean(s)]
# median() - медиана
s[s<=median(s) & s>=mean(s) - 1]

# samole() - осуществляет случайный выбор из вектора c (первого аргумента) в количестве N = второй аргумент
#, если N > len(c), то необходимо поставить replace = TRUE - разрешает повторение. 
# Если не поставить replace = TRUE в случае N > len(c), то будет ошибка
y <- sample(c(1:100), 1000, replace=TRUE)

sqrt((sum((y-mean(y))^2))/(length(y)-1))

# логические сравнения - как обычно, только "И" == &, "ИЛИ" == |, остаток от деления = "%%"

#сумма векторов
s+y

#
ls()
#

#summary(s) - статистика по объекту
summary(s)

# Больший спектр функций в этом файле https://github.com/junaart/ForStudents/blob/master/KIS/Lab_1/Laboratory_work.ipynb

# названия функций распределения имеют вид r*тип распределения*
d <- runif(20, 6, 99) # генерация чисел от a до b
d <- rnorm(20, 50, 3)

# вывод и синтаксис - C-подобный, но его можно упрощать (убирать скобки). 
# Операторы в одной строке разделяются точкой с запятой

# Цикл. Другие виды - аналогичны C https://github.com/junaart/ForStudents/blob/master/KIS/Lab_1/Laboratory_work.ipynb
i <- 1
while (i <= 20) {
  print(i);
  i++;
}

# задание функции
f<-function(X) {
  if (n <= 2) {
    if (n >= 0) return(1) else return(0)
  } else {
    return {
      return(Recall(n-1)+Recall(n-2))
    }
  }
}

## Создать два вектора - состоит из 1000 вещественных чисел из диапазона от 1 до 1000000, вектор
## состоит из 500 целых чисел: 1,2,3,...,500
a<-sample(c(1:1000000), 1000, replace=TRUE)
b<-c(1:500)
## получить медианное значение элементов вектора x
mean(a)
## получить разницу между средним арифметическим и средним гармоническим элементов вектора у
median(a) - length(b)/sum(1/b)

srHarmonic<-function(v) {
  return<- length(v)/sum(1/v)
}

mean(a)-srHarmonic(b)

## отобрать из вектора x все элементы, которые принимают значения из диапазона
a[a>1 & a < 50000]
## отобрать из вектора y только четные элементы
a[a %% 2 == 0]
## сохранить объекты x,y.

###########################

# В комиссии из 5 человек 4 члена принимают независимо друг от друга правильное решение с вероятностью 0.9, 
# а пятый для принятия решения бросает монету. 
# Окончательное решенние принимается большинством голосов. 
# Кто с большей вероятностью принимает правильное решение: 
# комиссия или один человек из комиссии?

# return<-VALUE - засовывает VALUE в return, и только потом это значение возвращается (после отрабатывания функции)
# return(VALUE) - мгновенно возвращает VALUE, работа функции прерывается
genWithProbability<-function(prob) 
{
  val<-runif(1,0,1)
  if (val > prob) {
    return<-0
  } else {
    return<-1
  }
}

prob<-0.9

totalVoicesRes <- 0
singleVoiceRes <- 0

for (i in c(1:1000)) {
  x <-c(genWithProbability(prob),
        genWithProbability(prob),
        genWithProbability(prob), 
        genWithProbability(prob), 
        genWithProbability(0.5))
  
  totalVoices<-sum(x)
  if (totalVoices > 2) {
    totalVoicesRes <- totalVoicesRes + 1
  }
  j<-runif(1, 1, 5)
  
  if (x[j] == 1) {
    singleVoiceRes <- singleVoiceRes + 1
  }
  
}

totalVoicesRes
singleVoiceRes

##########
Variant<-1
set.seed(Variant) 
X1<-sample(c(1:20),5)
X2<-sample(c(3:100),5)
X3<-sample(c(0:40),5)
pp1<-runif(5)
p1<-pp1/sum(pp1)
pp2<-runif(5)
p2<-pp2/sum(pp2)
pp3<-runif(5)
p3<-pp3/sum(pp3)

View(data.frame(X1, X2, X3, p1, p2, p3))

# Заданы три независимые целочисленные неотрицательные случайные величины X1, X2, X3, 
# необходимо найти:
#   математическое ожидание, 
#   дисперсию, 
#   среднее квадратическое отклонение,
#   коэффициент вариации 
# случайной величины X1+X2+X3 численно и теоретически 
# (из определения и с использованием производящей функции).

# P1 = 0.42655337*x1^4 + 0.09681138*x1^7 +  0.29739176*x1^1 +  0.05729715*x1^2 +  0.12194633*x1^13 
# P2 = 0.193838971*x2^89 +  0.006722284*x2^45 +   0.191968357*x2^16 +   0.436606645*x2^84 +   0.170863743*x2^61
# P3 = 0.18621917*x3^32 + 0.23160185*x3^20 +  0.19064643*x3^39 +  0.07193262*x3^9 +  0.31959993*x3^6

########
Px1<-""; Px2<-""; Px3<-""

for (i in (1:5)) {
  Px1<-paste(Px1, "+",p1[i],"*z^",X1[i])
  Px2<-paste(Px2, "+",p2[i],"*z^",X2[i])
  Px3<-paste(Px3, "+",p3[i],"*z^",X3[i])
}

# производная n-го порядка
DD <- function(expr, name, order = 1) {
  if(order < 1) stop("'order' must be >= 1")
  if(order == 1) return(D(expr, name))
  else return(DD(D(expr, name), name, order - 1))
}

Px_temp <-paste("(",Px1,")*(",Px2,")*(",Px3,")")
# string -> expression 
Px_ <- parse(text=Px_temp)
Px <- function(z) {eval(Px_)}

DPx_ <- D(Px_, 'z')
DDPx_ <- DD(Px_, 'z', 2)

# Мат. ожидание
Mx <- function(z) {eval(DPx_)}
# дисперсия
Dx <- function(z) {eval(DDPx_) + eval(DPx_) - eval(DPx_) ^ 2}
# среднеквадратичное отклонение
MSD <- function(z) {sqrt(Dx(z))}
# коэффициент вариации
COV <- function(z) {MSD(z) * 100 / Mx(z)}

Px(1)
Mx(1)
Dx(1)
MSD(1)
COV(1)

#########
#Экспериментальная часть

N <- 100000
xx1 <- c()
xx2 <- c()
xx3 <- c()
for (i in c(1:N)) {
  f<-runif(1)
  p_left <- 0
  p_right <- p1[1]
  for (j in c(1:length(p1))) {
    if ((f > p_left) & (f < p_right))
      xx1 <- c(xx1, X1[j])
    if (j != length(p1)) {
      p_left <- p_right
      p_right <- p_right + p1[j + 1]
    }
  }
  
  f<-runif(1)
  p_left <- 0
  p_right <- p2[1]
  for (j in c(1:length(p2))) {
    if ((f > p_left) & (f < p_right))
      xx2 <- c(xx2, X2[j])
    if (j != length(p2)) {
      p_left <- p_right
      p_right <- p_right + p2[j + 1]
    }
  }
  
  f<-runif(1)
  p_left <- 0
  p_right <- p3[1]
  for (j in c(1:length(p3))) {
    if ((f > p_left) & (f < p_right))
      xx3 <- c(xx3, X3[j])
    if (j != length(p3)) {
      p_left <- p_right
      p_right <- p_right + p3[j + 1]
    }
  }
}

print(mean(xx1 + xx2 + xx3))
print((sd(xx1 + xx2 + xx3)) ^ 2)

#########



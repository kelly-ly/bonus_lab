# linreg RC Class Object

#' linreg RC Class Object
#' @import ggplot2
#' @param formula formula of regression model
#' @param data data of regression model
#' @examples
#' data(iris)
#' l1 <- linreg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data = iris)
#' l1$print()
#' @export linreg
#' @exportClass linreg

linreg<-setRefClass("linreg",fields=list(formula='formula',data='data.frame',lambda="numeric",fitted_value='matrix',data_name = "character",ridge_reg_coe='matrix'),methods=list(
  initialize = function(formula,data,lambda){
    formula<<-formula
    data<<-data
    lambda<<-lambda
    data_name <<- deparse(substitute(data))
    calculate_ridge_reg_coe()
    calculate_the_fitted_value()
  },
  calculate_the_fitted_value=function(){
    fitted_value<<-get_matrix_x()%*%ridge_reg_coe
  },
  get_matrix_y=function(){
    y_name<-all.vars(formula)[1]
    y<-data[y_name]
    return (as.matrix(y))
  },
  get_matrix_x=function(){
    fm<-formula(formula)
    fm<-lm(fm,data)
    x<-model.matrix(fm)

    return (as.matrix(x))
  },
  print = function(){
    cat("linreg(formula = ", format(formula), ", data = ",data_name,")\n", sep = "")
    argnames <- sys.call()
    print.table(t(ridge_reg_coe[,1]))
  },
  calculate_ridge_reg_coe=function(){
    x<-get_matrix_x()

    y<-get_matrix_y()
    
    ngene = nrow(x)
    nmotif = ncol(x)
    QR = qr(crossprod(x) + diag(nrow(x)*lambda,ncol(x),ncol(x)))
    result = qr.coef(QR,crossprod(x,y))
    # result <- solve(t(X) %*% X + diag(lambda, ncol(X)) , t(X) %*% y)
    # I <- diag(1,nmotif,nmotif)
    # result <- solve(crossprod(X)+lambda*I,crossprod(X,y))
    
    b_ridge <- solve(t(x) %*% x + (lambda * diag(dim(x)[2])), t(x) %*% y)
    ridge_reg_coe<<-b_ridge
    #ridge_reg_coe<<-rownames("Coefficients:")
    
  },
  predict=function(){
    return(fitted_value)
  },
  coef=function(){
    return(ridge_reg_coe[,1])
  }
))
  
  
  
  
l1<-linreg$new(Petal.Length~Species,data=iris,lambda=0)
l1$print()
unname(l1$coef()[-1])
library(MASS)
b<-lm.ridge(Petal.Length~Species,data=iris,lambda=0)
unname(b$coef/b$scales)
a<-l1$coef()
unname(a)

library("nycflights13")
data("airports")
data("flights")
library(dplyr)
flight_info<-dplyr::select(flights,dep_delay,arr_delay,flight,origin,dest)
airport_info<-dplyr::select(airports,name=faa,lat,lon)
join_arr_data<-dplyr::left_join(flight_info,airport_info,by=c("dest"="name"))
group_arr_data<-dplyr::group_by(join_arr_data,dest)
d<-summarise(group_arr_data,delay=mean(arr_delay,na.rm = TRUE))
result<-dplyr::left_join(d,airport_info,by=c("dest"="name"))
library(ggplot2)
world_map <- map_data("world")
ggplot(world_map, aes(x = lon, y = lat))+geom_point(data = result,aes(x=lon,y=lat), color = "red", size = 4) +geom_text(data = result, aes(label = paste("  ", as.character(round(delay,0)), sep="")), angle = 30, hjust = 0, color = "black")

library(caret)
library(mlbench)
data("BostonHousing")
train<-caret::create

#Yandex Test

#------------------- Постановка задачи -------------------#
##Загрузка данных
###LiveInternet: количество посетителей с разными браузерами | среднесуточные значения по месяцам
broid <- matrix(data=paste0("id=", c(1:60)), nrow=10, ncol=6)
yandex <- data.frame("Date"=seq(as.Date("2015-01-01"), length=31, by="1 month")-1)
for(i in 1:6){
  yandex <- cbind(yandex, read.table(paste0("http://www.liveinternet.ru/stat/ru/browsers.csv?slice=ru;", paste(broid[,i], collapse = ';'), 
                                            ";period=month;graph=csv"), header = T, sep = ";", encoding = "UTF-8", stringsAsFactors = F)[,-1])
}

##Основные браузеры
mainbro <- data.frame(round(apply(round(yandex[nrow(yandex),-1]/rowSums(yandex[nrow(yandex),-1]), 3), 1, sort)*100, 1))
names(mainbro) <- "Percent"
mainbro$Browser <- row.names(mainbro)
mainbro <- mainbro[mainbro$Percent >= 1,]
mainbro <- mainbro[order(-mainbro$Percent),]
row.names(mainbro) <- 1:nrow(mainbro)
mainbro

##Основные браузеры (>= 5%)
topbrowsers <- data.frame("Date"=yandex$Date, "Chrome"=yandex$Google.Chrome, "SafariMob"=yandex$Mobile.Safari, "Yandex"=yandex$Яндекс.Браузер, 
                          "Other" = apply(yandex[ , !(names(yandex) %in% c('Date', 'Google.Chrome', 'Mobile.Safari', 'Яндекс.Браузер'))], 1, sum))

##Автокорреляционная и частная автокорреляционная функции (ACF и PACF)
library("forecast")
for (i in 2:5){
  x <- ts(topbrowsers[,i]/1000000, start=c(2014, 12), frequency=12)
  tsdisplay(x, lwd=3, main=names(topbrowsers[i]))
}

##Сезонность
for (i in 2:5){
  x <- ts(topbrowsers[,i]/1000000, start=c(2014, 12), frequency=12)
  plot(decompose(x))
}


#------------------- Результат -------------------#
##Прогнозирование
results$Date <- data.frame("Date"=seq(as.Date("2017-08-01"), length=18, by="1 month")-1)
library("forecast")
for (i in 2:5){
  x <- ts(topbrowsers[,i]/1000000, start=c(2014, 12), frequency=12)
  sarima <- Arima(x, order = c(1,1,0), seasonal = list(order=c(1,1,0), period=12), method="ML")
  fsarima <- forecast(sarima, h=18, level=95)
  DF <- data.frame(fsarima$lower, fsarima$mean, fsarima$upper, stringsAsFactors = F)
  names(DF) <- c(paste0('lower', names(topbrowsers[i])), paste0('mean', names(topbrowsers[i])), paste0('upper', names(topbrowsers[i])))
  results <- cbind(results, DF)
  plot(fsarima, main = names(topbrowsers[i]))
}

##Проценты для интервального прогноза

results$plowerChrome <- round(results$lowerChrome/(results$lowerChrome+results$upperSafariMob+results$upperYandex+results$upperOther)*100, 1)
results$pmeanChrome <- round(results$meanChrome/(results$meanChrome+results$meanSafariMob+results$meanYandex+results$meanOther)*100, 1)
results$pupperChrome <- round(results$upperChrome/(results$upperChrome+results$lowerSafariMob+results$lowerYandex+results$lowerOther)*100, 1)

results$plowerSafariMob <- round(results$lowerSafariMob/(results$upperChrome+results$lowerSafariMob+results$upperYandex+results$upperOther)*100, 1)
results$pmeanSafariMob <- round(results$meanSafariMob/(results$meanChrome+results$meanSafariMob+results$meanYandex+results$meanOther)*100, 1)
results$pupperSafariMob <- round(results$upperSafariMob/(results$lowerChrome+results$upperSafariMob+results$lowerYandex+results$lowerOther)*100, 1)

results$plowerYandex <- round(results$lowerYandex/(results$upperChrome+results$upperSafariMob+results$lowerYandex+results$upperOther)*100, 1)
results$pmeanYandex <- round(results$meanYandex/(results$meanChrome+results$meanSafariMob+results$meanYandex+results$meanOther)*100, 1)
results$pupperYandex <- round(results$upperYandex/(results$lowerChrome+results$lowerSafariMob+results$upperYandex+results$lowerOther)*100, 1)

##Графики долей для точечных и интервальных прогнозов

###Chrome
plot(results$Date, results$pmeanChrome, col="#3366cc", pch=20, lwd=4, main="Chrome", ylab="Visitors, %", xlab="", xaxt = "n", 
     ylim=c(min(results$plowerChrome), max(results$pupperChrome)))
lines(results$Date, results$plowerChrome, col="#dc3912", lwd=1, lty=2)
lines(results$Date, results$pupperChrome, col="#109618", lwd=1, lty=2)
abline(v = as.Date("2018-01-15"))
legend("topleft", legend=c("Upper","Forecast","Lower"), title="95% level", fill=c("#109618","#3366cc","#dc3912"))
axis(1, at=results$Date, labels=format(results$Date, "%b%Y"), las=2)
text(results$Date, results$pmeanChrome, results$pmeanChrome, cex=0.6, pos=1, offset=0.5)
###SafariMob
plot(results$Date, results$pmeanSafariMob, col="#3366cc", pch=20, lwd=4, main="SafariMob", ylab="Visitors, %", xlab="", xaxt = "n", 
     ylim=c(min(results$plowerSafariMob), max(results$pupperSafariMob)))
lines(results$Date, results$plowerSafariMob, col="#dc3912", lwd=1, lty=2)
lines(results$Date, results$pupperSafariMob, col="#109618", lwd=1, lty=2)
abline(v = as.Date("2018-01-15"))
legend("topleft", legend=c("Upper","Forecast","Lower"), title="95% level", fill=c("#109618","#3366cc","#dc3912"))
axis(1, at=results$Date, labels=format(results$Date, "%b%Y"), las=2)
text(results$Date, results$pmeanSafariMob, results$pmeanSafariMob, cex=0.6, pos=1, offset=0.5)
###Yandex
plot(results$Date, results$pmeanYandex, col="#3366cc", pch=20, lwd=4, main="Yandex", ylab="Visitors, %", xlab="", xaxt = "n", 
     ylim=c(min(results$plowerYandex), max(results$pupperYandex)))
lines(results$Date, results$plowerYandex, col="#dc3912", lwd=1, lty=2)
lines(results$Date, results$pupperYandex, col="#109618", lwd=1, lty=2)
abline(v = as.Date("2018-01-15"))
legend("topleft", legend=c("Upper","Forecast","Lower"), title="95% level", fill=c("#109618","#3366cc","#dc3912"))
axis(1, at=results$Date, labels=format(results$Date, "%b%Y"), las=2)
text(results$Date, results$pmeanYandex, results$pmeanYandex, cex=0.6, pos=1, offset=0.5)

##Таблица результатов для 2018 года

###Chrome
View(results[results$Date >= '2018-01-01',c('Date', 'plowerChrome', 'pmeanChrome', 'pupperChrome')])
###SafariMob
View(results[results$Date >= '2018-01-01',c('Date', 'plowerSafariMob', 'pmeanSafariMob', 'pupperSafariMob')])
###Yandex
View(results[results$Date >= '2018-01-01',c('Date', 'plowerYandex', 'pmeanYandex', 'pupperYandex')])
#--------------------------------------#

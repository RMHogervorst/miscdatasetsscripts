sessie_info<-function() {
        ##dit scriptje schrijft een sessieinformatie bestandje naar logs
        sink(paste( "text/logs/sessioninfo", Sys.Date(), '.txt',  sep="") )
        sessionInfo()
        date()
        sink()       
}

eigenschappen_dataset<- function(df) {
if (is.data.frame(df) == TRUE){
        info <- as.data.frame(matrix(0, ncol = 0, nrow = length(df)))
        info$variable_name <- names(df)
        info$class <-lapply(df, class)
        info$type<-lapply(df, typeof)
        info$N_of_missing<- lapply(df, function(x) sum(is.na(x)))
        info$unique<-lapply(df, function(x) length(unique(x)))
        info<-cbind(info, min=apply (df, 2, function(x) which(x == min(x, na.rm = T))[1]))
        info<-cbind(info, max=apply (df, 2, function(x) which(x == max(x, na.rm = T))[1]))
        info<-cbind(info, mode= apply(test,2, function(x) {
                ux <- unique(x)
                ux[which.max(tabulate(match(x, ux)))]
        }) )
        info<-cbind(info, median=apply (df, 2, function(x) which(x == median(x, na.rm = T))[1]))
        return(info)
}  else {
        print("not a data frame!")
        }
}
#aan te passen met
#Voor numerieke variabelen N, M, SD, range
#Voor categorische variabelen N, freq + percentage per categorie
#als date, laagste en hoogste
#als character vaak voorkomenste
# als factor alle factoren?
#



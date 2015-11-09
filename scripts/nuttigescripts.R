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
        return(info)
}  else {
        print("not a data frame!")
        }
}
#aan te passen met
#Voor numerieke variabelen N, M, SD, range
#Voor categorische variabelen N, freq + percentage per categorie



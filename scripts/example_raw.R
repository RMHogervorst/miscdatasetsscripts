#  This function creates a dataset with 7 variables and random missings and weird values
#  
# You can use this created dataset for demonstrations of recoding. 
#
#create a raw data dataset
example_raw<-function(n_rows = 50, seed) {
        set.seed(seed)# reproducability is key
        M<- round(n_rows/10)+1 # ~ number of variables that will be replaced with NAs  (Missings)
        
        #creation of variables
        integ<-sample(1:n_rows, n_rows)
        integ2<-sample(3:n_rows, n_rows, replace = T)
        integ3<-sample(18:120, n_rows, replace = T)
        let1<- sample(letters, n_rows, replace = T)
        let2<-sample(LETTERS, n_rows, replace = T)
        let3<-sample(state.name, n_rows, replace = T)
        bla<- round(rnorm(n_rows, 27, 6), 2)
        
        
        # replacing random parts with NAs or weird values. 
        #thanks to:http://paleocave.sciencesortof.com/2014/07/insert-random-nas-in-a-vector-in-r/
        integ2[integ2 == 4] <- 99
        integ3[which(integ3 %in% sample(integ3, M))] <- 999
        integ3[which(integ3 %in% sample(integ3, M))] <- NA
        let1[which(let1 %in% sample(let1, M))] <- NA
        let2[let2 == "A"] <-"darth vader"
        let3[which(let3 %in% sample(let3, M, replace = T))]<-"Quebec"
        bla[which(bla %in% sample(bla, M, replace =T))] <-NA
        
        #creation of dataset
        df<-data.frame(integ, let1,integ2,let2, let3, bla,integ3 ) #put into dataframe
        return(df)
}

#example_raw(6, 345)
#
#  integ3 could be age from 18 to 120
#  I have made bla a normal distribution of BMI (2002)
#  The other variables are meaningless, state is of course a US state, except voor Quebec, which is Canadian
#  
# 

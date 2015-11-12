# IMDB R FUNCTIONS
# Creator: R. Hogervorst   works with R version 3.2.2 
# licence: MIT licence
# 2015 - 11 - 12
#
#  I wanted to automatically make data frames from imdb information.
#  I used the omdb api from : http://www.omdbapi.com
#  These functions need the jsonlite package.
# 
#  imdbSeries takes two arguments: a series name and (optional) seasons.
#  f.i. :  DS<-imdbSeries("House MD", 6:8) 
#  will download all info about season 6 till 8 from the series House MD
#  and put the result into a dataframe DS
#  
#  enrichIMDB takes 1 argument, the dataframe that contains a variable called imdbID. 
#  It uses this variable to download the runtime, director, writer, actors, plot and imdb votes. 
#  The end result is then a data frame. 
#  f.i.: using the DS dataset from the previous function:
#  DS2<- enrichIMDB(DS) gives all information about every episode from House MD seasons 6-8 and puts that in a new dataframe.
# 
#   NOTES: enrichIMDB only uses unique imdbIDs, the next step would be to combine the two datasets
#     for instance with the left_join command ( by = "imdbID" )from the excellent dplyr package.
imdbSeries<-function(seriesname, seasons = 1) {
        library(jsonlite)
        df<-data.frame(Title = character(0), Released = character(0), 
                       Episode = character(0), imdbRating = character(0), 
                       imdbID = character(0), Season =numeric(0))  #creates empty dataframe
        for( i in seasons) {
                link<-gsub(pattern = " ", replacement = "%20", x=(paste("http://www.omdbapi.com/?t=",seriesname,"&Season=",i, sep = "")))
                hold<-fromJSON(link)  # link with spaces replaced
                dftemp<-hold$Episodes #using only the Episodes part
                dftemp$Season <-i     # adding variable season
                df<- rbind(df, dftemp)# combining
                #loops through seasons and combines the seasons into 1 dataframe.
        }
        #assigning right classes.
        df$Released<-as.Date(df$Released)
        df$Episode<- as.numeric(df$Episode)
        df$imdbRating<- as.numeric(df$imdbRating)
        return(df)
}


enrichIMDB<- function(df){
        library(jsonlite)
        #  read all unique  id's
        IDs<- unique(df$imdbID)
        #issue with rbind that breaks the column names, forces me to create a useless row
        imdbID <- "t103" 
        runtime<- "asdf"
        director<- "asdf"
        writer<- "asdf"
        actors<- "asdf"
        plot<-"adsf"
        votes<-"456"
        dataframe<-data.frame(imdbID, runtime,director, writer,actors, plot,votes, stringsAsFactors = F)
        #  loop through ids and add information into a row and adding it to dataframe. 
        for(i in IDs) {
                link <- paste("http://www.omdbapi.com/?i=", i ,"&plot=full&r=json", sep = "")
                hold<-fromJSON(link)
                newrow<- c( i, hold$Runtime, hold$Director, hold$Writer, hold$Actors, hold$Plot, hold$imdbVotes)
                dataframe<-rbind(dataframe, newrow)
        }
        dataframe = dataframe[-1,] #issue with rbind forces me to remove the useless row
        dataframe$votes<-as.numeric(dataframe$votes) # votes are numeric
        return(dataframe)
}

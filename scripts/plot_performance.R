#loopje.
# execute functie, delete dataset
# tel hoelang duurde
# stop in dataframe
# plot tijd tegen aantal. 
# 
library(microbenchmark)
#
library(ggplot2)
#

tm<-microbenchmark(
        example_raw(20000, 1245),
        example_raw(30000, 1245),
        example_raw(40000, 1245),
        example_raw(50000, 1245),
        example_raw(60000, 1245),
        example_raw(70000, 1245),
        example_raw(80000, 1245),
        example_raw(90000, 1245),
        example_raw(100000, 1245),
        example_raw(110000, 1245),
        times = 100
)
autoplot(tm)

sales_file <- 'checkout_sales_last_hour__big_query__2022-02-22T19_45_09.595349Z.txt'

sales <- read.csv(sales_file, as.is = TRUE)
dim(sales)
head(sales)
summary(sales)

# replace missing values with zero
sales[is.na(sales)] <- 0

# backup data
sales_backup <- sales

# turn first column into index
rownames(sales) <- sales[,1]
sales <- sales[,-1]

# look at dataset
sales

# choose colors for plot 
cols <- c('blueviolet', 'deeppink', 'forestgreen', 'blue', 'chocolate')

# plot all curves and see if something stands out
plot(NULL, type = 'n', ylim = c(min(sales), max(sales)),
     xlim = c(0, nrow(sales)), xlab = 'Time', ylab ='POS sales per hour',
     main = 'CloudWalk sales')
for (column in names(sales)){
  index <- which(names(sales) == column)
  lines(1:nrow(sales), sales[,column], lwd = 2, col = cols[index])
}
legend('topleft', names(sales), fill = cols, cex = .8)

# look at sums
apply(sales, 2, sum)

# look at averages
apply(sales, 2, mean)

#' The dataset has the number of POS (credit card machine) sales per hour over
#' different time periods. Comparing different columns, we observe a general
#' trend in checkout activity. Sales are lowest in the morning between 1 and 6
#' am, when they start rising and peak between 10 am and 3 pm. 
#' 
#' Although there aren't any noticeable anomalies in the data, sales from 
#' `today` and `yesterday` show different trends in the middle of the day when
#' compared to historical data. In the data from yesterday, we see two troughs
#' at noon and at 2 pm. There was also an unusual peak at 1 pm. However, looking
#' at today's data, 1 pm corresponded to a trough in sales. It's important to
#' keep in mind, though, that small variations in sales volume are expected
#' throughout any given day. Since all curves have a similar shape, similar 
#' rough trends are observed at any of the given time periods. 

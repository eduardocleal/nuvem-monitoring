transactions_file <- 'transacoes_reversed_minuto_a_minuto_fis_cw__desfazimentos__2022-02-22T21_16_12.064248Z.txt'

transactions <- read.csv(transactions_file, as.is = TRUE)
dim(transactions)
summary(transactions)
head(transactions)

# save backup
transactions_backup <- transactions

timestamps <- unique(transactions$f0_)
statuses <- c('denied', 'failed', 'reversed')

percentages <- data.frame(matrix(0, ncol = length(statuses)))
names(percentages) <- statuses

for (minute in timestamps){
  trim <- transactions[transactions$f0_ == minute,]
  this_minute <- NULL
  for (i in 1:length(statuses)){
    if (statuses[i] %in% trim$status){
      this_minute[i] <- trim$f1_[trim$status == statuses[i]] / sum(trim$f1_)
    } else{
      this_minute[i] <- 0
    }
  }
  percentages <- rbind(percentages, this_minute)
}

percentages <- percentages[-1,] # remove first row
rownames(percentages) <- timestamps

averages <- apply(percentages, 2, mean)
sds <- apply(percentages, 2, sd)
bounds <- averages + sds

# plot data
x.axis <- 1:nrow(percentages)
plot(NULL, type = 'n', xlim = c(0, x.axis[length(x.axis)]), ylim = c(0, 1),
     xlab = 'Time', ylab = 'Fraction', xaxt = 'n', las = 1,
     main = 'Transaction status monitoring')
lines(x.axis, percentages$denied)
lines(x.axis, percentages$failed, col = 'red')
lines(x.axis, percentages$reversed, col = 'purple')
legend('topright', legend = statuses, fill = c('black', 'red', 'purple'))
abline(h = bounds, col = c('black', 'red', 'purple'), lwd = 2, lty = 2)

# new data (for next minute)
new_data <- transactions[transactions$f0_ == sample(timestamps, 1),]
new_minute <- NULL
for (i in 1:length(statuses)){
  if (statuses[i] %in% new_data$status){
    new_minute[i] <- new_data$f1_[new_data$status == statuses[i]] / 
      sum(new_data$f1_)
  } else{
    new_minute[i] <- 0
  }
}

alert <- new_minute > bounds
if(alert[1]) print(paste0('Alert: denied transations above normal at ',
                          unique(new_data$f0_)))
if(alert[2]) print(paste0('Alert: failed transations above normal at ',
                          unique(new_data$f0_)))
if(alert[3]) print(paste0('Alert: reversed transations above normal at ',
                          unique(new_data$f0_)))

# update percetantages matrix and recalculate bounds
# percentages <- rbind(percentages, new_minute)
# timestamps <- c(timestamps, unique(new_data$f0_))
# rownames(percentages) <- timestamps
# averages <- apply(percentages, 2, mean)
# sds <- apply(percentages, 2, sd)
# bounds <- averages + sds

for (i in 1:3) print(mean(percentages[,i] > bounds[i]))
# emulation of money management for small business, optimisation of interest rate earned


require(timeDate)
require(lubridate)
require(ggplot2)

source ("my_random.r")

# constants
start_date <- as.Date("2018-01-01") 
end_date <- as.Date("2018-12-31")
days <-as.integer(end_date-start_date+1)
# number of simulations
nsim <- 50

#Calendar - setting working and non-working days, have it in a variable provides better performance
work_day <- logical(days)
curr_date <- start_date-1
holidays <- as.Date(holidayZURICH(year(start_date)))



#weekend and holidays marking
for (i in 1:days){
        curr_date <-  curr_date +1
        if (weekdays(curr_date) %in% c("Saturday", "Sunday") | curr_date %in% holidays ){
                work_day[i] <- FALSE
        }
        else {work_day[i] <- TRUE}
} 
# number of days when transactions are possible
bank_days <-length(work_day[work_day])

# revenue and expenses 
annual_revenue <- 1000000
annual_expenses <-600000

checking_interest <- 0
saving_interest <- 0.025

initial_balance_checking <- 10000
initial_balance_saving <-100000


# how far payments deviate from average
deviation_factor_deb <-6
deviation_factor_cred <- 2



# financial manager default behavior
check_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
min_check_balance <- 1000
# number of checks per week then randomly select week day
transfers_week <- 1
check_week_day <- sample(check_days, transfers_week, replace=F)

rate <-50
time_intervention <- 0.25




# AI behaviou
# checks balance every day and move the money around
mon_min_check_balance <-1000



#calculated parameters


average_debit_value <- annual_expenses/bank_days
average_credit_value <- annual_revenue/bank_days
dev_credit <- average_credit_value*deviation_factor_cred
dev_debit <-  average_debit_value*deviation_factor_deb

#minimal and maximal transaction value and st.deviation




# initialisation part 1 - whole simulation

balance_sheet <- data.frame(date=as.Date(character()), checking=integer(), saving=integer(), day_debit=integer()
                            ,day_credit=integer(),interest=integer())
balance_sheet[1,] <- NA
balance_sheet$date[1] <- start_date
balance_sheet$checking[1] <- initial_balance_checking
balance_sheet$saving[1] <- initial_balance_saving
balance_sheet$day_debit[1] <- 0
balance_sheet$day_credit[1] <-0
balance_sheet$interest[1] <-0

agg_balance <-data.frame()
# simulation starts, outer circle, number of simulations

for (j in 1:nsim)
{   
# annual parameters initiation human manager
balance_sheet <- data.frame(date=as.Date(character()), checking=numeric(), saving=numeric(), day_debit=numeric()
                                    ,day_credit=numeric(),interest=numeric())
balance_sheet[1,] <- NA
balance_sheet$date[1] <- start_date
balance_sheet$checking[1] <- initial_balance_checking
balance_sheet$saving[1] <- initial_balance_saving
balance_sheet$day_debit[1] <- 0
balance_sheet$day_credit[1] <-0
balance_sheet$interest[1] <-0
interventions <-0

# annual parameters initiation AI (monkey)
mon_balance_sheet <- data.frame(date=as.Date(character()), checking=numeric(), saving=numeric(), day_debit=numeric()
                            ,day_credit=numeric(),interest=numeric())
mon_balance_sheet[1,] <- NA
mon_balance_sheet$date[1] <- start_date
mon_balance_sheet$checking[1] <- initial_balance_checking
mon_balance_sheet$saving[1] <- initial_balance_saving
mon_balance_sheet$day_debit[1] <- 0
mon_balance_sheet$day_credit[1] <-0
mon_balance_sheet$interest[1] <-0
mon_interventions <-0


#generating debit and credits pool 

# Normal distrubution
#credits <-abs(rnorm(days, mean=average_credit_value, dev_credit))
#debits <- abs(rnorm(days, mean=average_debit_value, dev_debit))

# My own random fuction used, in a separate file



credits <- randtrans(annual_revenue, bank_days, deviation_factor_cred)
debits <- randtrans(annual_expenses, bank_days, deviation_factor_deb)


current_date <- start_date
work_day_ind <- 0 
# annual simulation  


for (i in 2:days){
        
current_date <- current_date+1  
deficit <- 0
mon_deficit <-0


# print(current_date)


# human manager
balance_sheet[nrow(balance_sheet)+1,] <- NA
balance_sheet$date[i] <- current_date
# monkey AI
mon_balance_sheet[nrow(mon_balance_sheet)+1,] <- NA
mon_balance_sheet$date[i] <- current_date
# for work days we have debit and credit transactions from pre-generated pool, for non-work we calculate the interest only
if (work_day[i]) 
        {
                work_day_ind <- work_day_ind + 1
        if (!is.na(debits[work_day_ind])){
                deb_day_sum <- debits[work_day_ind]}
            else {deb_day_sum <-0}
        if (!is.na(credits[work_day_ind])){
                cred_day_sum <- credits[work_day_ind]}
            else {cred_day_sum <-0}
        }
else    {
        deb_day_sum=0
        cred_day_sum=0
}

        #human manager
        interest_this_day_sav <- balance_sheet$saving[i-1]*saving_interest/365
        interest_this_day_check <- balance_sheet$checking[i-1]*checking_interest/365
        #monkey AI
        mon_interest_this_day_sav <- mon_balance_sheet$saving[i-1]*saving_interest/365
        mon_interest_this_day_check <- mon_balance_sheet$checking[i-1]*checking_interest/365
        
        # human exception check - not enough money on check account to pay all bills, take the deficit from check
        # manager adds deficit with buffer to keep check balance to min level
                if (balance_sheet$checking[i-1] < deb_day_sum-cred_day_sum-interest_this_day_check)
                        {
                        deficit <- ceiling(deb_day_sum-(balance_sheet$checking[i-1]+cred_day_sum+interest_this_day_check))
                        if (deficit<min_check_balance){deficit <- min_check_balance}
                        interventions <- interventions+1
                        # print (current_date)
                        # print ("Not enough money on checking. Taking amount below from saving:")
                        # print (deficit)
                
                        }
        
        # monkey_ai exception check - not enough money on check account to pay all bills, take the deficit from check
        # ai adds deficit with buffer to keep check balance to min level
        if (mon_balance_sheet$checking[i-1] < deb_day_sum-cred_day_sum-mon_interest_this_day_check)
        {
                mon_deficit <- ceiling(deb_day_sum-(mon_balance_sheet$checking[i-1]+cred_day_sum+mon_interest_this_day_check))
                if (mon_deficit<mon_min_check_balance){mon_deficit <- mon_min_check_balance}
                
                mon_interventions <- mon_interventions+1
                # print (current_date)
                # print ("Not enough money on checking. Taking amount below from saving:")
                # print (deficit)
                
        }
        
        #human manager end of day calculations
        balance_sheet$checking[i] <- balance_sheet$checking[i-1]-deb_day_sum+cred_day_sum+interest_this_day_check+deficit
        balance_sheet$saving[i] <- balance_sheet$saving[i-1]+interest_this_day_sav - deficit
        balance_sheet$interest[i] <- balance_sheet$interest[i-1]+interest_this_day_sav+interest_this_day_check
        balance_sheet$day_debit[i] <- deb_day_sum
        balance_sheet$day_credit[i] <- cred_day_sum
        
        #Monkey AI manager end of day calculations
        mon_balance_sheet$checking[i] <- mon_balance_sheet$checking[i-1]-deb_day_sum+cred_day_sum+mon_interest_this_day_check+mon_deficit
        mon_balance_sheet$saving[i] <- mon_balance_sheet$saving[i-1]+mon_interest_this_day_sav-mon_deficit
        mon_balance_sheet$interest[i] <- mon_balance_sheet$interest[i-1]+mon_interest_this_day_sav+mon_interest_this_day_check
        mon_balance_sheet$day_debit[i] <- deb_day_sum
        mon_balance_sheet$day_credit[i] <- cred_day_sum
        
        # debug print
        # print(balance_sheet[i,])       
# manual financial manager action, moving money around
if ( weekdays(current_date)%in% check_week_day & 
     work_day[i])
        { 
        if (balance_sheet$checking[i]> min_check_balance)
                {
                        transfer <- balance_sheet$checking[i]-min_check_balance
                        balance_sheet$checking[i] <- min_check_balance
                        balance_sheet$saving[i] <- balance_sheet$saving[i]+ transfer
                }
        else
                {
                        transfer <- min_check_balance-balance_sheet$checking[i]
                        balance_sheet$checking[i] <- balance_sheet$checking[i]+transfer
                        balance_sheet$saving[i] <- balance_sheet$saving[i]-transfer
                        
                }
        }
# AI (monkey) manager action, moving money around every day
        if (mon_balance_sheet$checking[i]> mon_min_check_balance)
                {
                        transfer <- mon_balance_sheet$checking[i]-mon_min_check_balance
                        mon_balance_sheet$checking[i] <- mon_min_check_balance
                        mon_balance_sheet$saving[i] <- mon_balance_sheet$saving[i]+ transfer
                }
                else
                {
                        transfer <- mon_min_check_balance-mon_balance_sheet$checking[i]
                        mon_balance_sheet$checking[i] <- mon_balance_sheet$checking[i]+transfer
                        mon_balance_sheet$saving[i] <- mon_balance_sheet$saving[i]-transfer
                        
                }
        
        
        if (balance_sheet$checking[i] <0) {stop("Negative balance")}
         
}
        print (c("Run", j, "completed"))
        # human result
        newrow<- cbind(balance_sheet[days,], interventions)
        #monkey result
        mon_newrow<- cbind(mon_balance_sheet[days,], mon_interventions)
        # clean up and prepare for merge
        names(mon_newrow) <- sub("checking", "mon_checking", names(mon_newrow))
        names(mon_newrow) <- sub("saving", "mon_saving", names(mon_newrow))
        names(mon_newrow) <- sub("interest", "mon_interest", names(mon_newrow))
        mon_newrow$date <- NULL
        mon_newrow$day_debit <- NULL
        mon_newrow$day_credit <- NULL
        newrow$day_debit <- NULL
        newrow$day_credit <- NULL
        newrow <- cbind(newrow, mon_newrow)
        newrow$date <- NULL
        
        newrow$monkey_gain <- newrow$mon_interest - newrow$interest
        newrow$int_cost <- newrow$interventions * time_intervention * rate
        
        row.names(newrow)<-j
        
        agg_balance<-rbind(agg_balance, newrow)
        #print (agg_balance[j,])
        
}

# Display the summary 

av_results <- sapply(agg_balance[c(3,7,9,10)], mean)
names(av_results) <- c("Interest human", "Interest monkey", "Monkey gain", "Labour cost")
barplot(av_results, main="Interest earned", ylab="$",sub="Human vs Robo-Monkey", col=c("darkblue","red", "green"))
print(av_results)


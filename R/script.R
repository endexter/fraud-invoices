####################################################################################
#
# PROJECT: FRAUD INVOICES/DECK
#
# PURPOSE: Create figures for fraud losses based off of Radius invoices
# DATE:    2018-09-05
# AUTHOR:  Eve Dexter
#
# NOTES:   - Data was aggregated from invoices sent to Aspiration from Radius containing
#            Aspiration's portion of financial responsibility of fraud/charge-off losses
#          - This differs from Aspiration's database numbers for charge-off costs or 
#            due to the fact that Aspiration is (mostly) only responsible for external
#            charge-offs (not NSF). Though there are times when Radius and Aspiration
#            have split costs for various charge-off types
#
####################################################################################



####################################################################################
# SET UP
####################################################################################


#----------#
# Packages #
#----------#

library(data.table)
library(ggplot2)
library(xtable)
library(scales)
library(gridExtra)

#-----------------------#
# Set Working Directory #
#-----------------------#

setwd("/Users/edexter/projects/fraud-invoices")
list.files()

#---------------#
# Query Dataset #
#---------------#
# Note: Redshift table was created via a csv 
#       combining all the monthly invoices

# Fraud invoices
df <- redshift("select * from public.ued_fraud_invoices_20180905;")
df <- data.table(df)
df

# Portfolio counts
portfolio <- redshift("select * from public.ued_monthly_portfolio_20180904;")
portfolio <- data.table(portfolio)
portfolio


#---------------#
# Data Cleaning #
#---------------#

# Convert date column to date object
str(df)
df[, date := as.Date(date)]
str(df)

# Convert date column to date object
str(portfolio)
portfolio[, date := as.Date(account_month)]
str(portfolio)

# Subset to just data prior to 2018-06-01
df <- df[date < "2018-06-01"]


# Merge portfolio counts into df
merge(df, portfolio, by = "date", all.x = TRUE)

#----------------------#
# Set Up Plot Defaults #
#----------------------#

# get min and max dates for x-axis
min_date <- min(df[, date])
min_date
max_date <- max(df[, date])
max_date

# get Aspiration colors
my_colors <- c("#1165bf", "#119fbf", "#37bf72", "#f2ac16", "#d94514", "#b6babf")


####################################################################################
# DATA ANALYSIS
####################################################################################


#---------#
# SUMMARY #
#---------#

# total count of fraud
df[ , .N ]

# total cost of fraud
df[ , sum( amount ) ]


#------------------------------#
# PLOT 1: MONTHLY FRAUD COUNTS #
#------------------------------#

# get counts by month
df1 <- df[ , .( count = .N ), by = date ]

# plot
p <- ggplot( data = df1, 
             aes( x = date, y = count) ) +
       geom_bar( stat = "identity", fill = my_colors[1] ) + 
       scale_x_date( limits = c( min_date, max_date ),
                     labels = date_format( "%b %Y" ),
                     date_breaks = "1 month" ) +
       scale_y_continuous( limits = c( 0, 200 ) ) + 
       labs( title = "Monthly Fraud Counts", 
             x = "Charge-Off / Termination (Month)"
             y = "Count" ) +
       theme( legend.position="none", 
              axis.text.x = element_text( angle = 45, hjust = 1 ),
              plot.title = element_text( hjust = 0.5 ) ) 
p

# save plot
ggsave( filename = "plots/01_monthly_fraud_counts.png", 
        plot = p, 
        width = 8, 
        height = 4 )
list.files( "plots" )


#------------------------------------------------------#
# PLOT 2: RATE OF FRADULENT ACCOUNTS BY PORTFOLIO SIZE #
#------------------------------------------------------#

# get fraud counts/portfolio counts by month
# recall: df1 <- df[, .(count = .N), by = date]
df2 <- merge( df1,
              portfolio, 
              by = "date", 
              all.x = TRUE )
              
df2[, fraud_rate := count/account_count][]

# plot
p <- ggplot( data = df2, 
             aes( x = date, y = fraud_rate ) ) +
	   geom_bar( stat = "identity", fill = my_colors[1] ) + 
       scale_x_date( limits = c( min_date, max_date ), 
                     labels = date_format( "%b %Y" ), 
                     date_breaks = "1 month" ) +
       labs( title = "Rate of Fraudulent Accounts by Portfolio Size", 
             x = "Charge-Off / Termination (Month)",
             y = "Count of Fraudulent Accounts / Portfolio Count" ) +
       theme( legend.position="none", 
              axis.text.x = element_text( angle = 45, hjust = 1 ),
              plot.title = element_text( hjust = 0.5 ) ) 
p

# save plot
ggsave( filename = "plots/02_rate_of_fraud_accounts_by_portfolio.png", 
        plot = p, 
        width = 8, 
        height = 4 )
list.files( "plots" )


#----------------------------#
# PLOT 3: MONTHLY FRAUD LOSS #
#----------------------------#

# get cost by month
df3 <- df[ , .( amount = sum( amount ) ), by = date ]

# plot
p <- ggplot( data = df3, 
             aes( x = date, y = amount ) ) +
	   geom_bar( stat = "identity", 
	             fill = my_colors[ 1 ] ) + 
       scale_x_date( limits = c( min_date, max_date ),
                     labels = date_format( "%b %Y" ),
                     date_breaks = "1 month") +
       labs( title = "Monthly Fraud Loss", 
             x = "Charge-Off / Termination (Month)",
             y = "Total Loss ($)" ) +
       theme( legend.position="none", 
              axis.text.x = element_text( angle = 45, hjust = 1 ),
              plot.title = element_text( hjust = 0.5 ) ) 
p

# save plot
ggsave( filename = "plots/03_monthly_fraud_loss.png", 
        plot = p, 
        width = 8, 
        height = 4 )
list.files( "plots" )


#-----------------------------------------------#
# PLOT 3B: MONTHLY FRAUD LOSS PROJECTED SAVINGS #
#-----------------------------------------------#

# Using 2018 data provided by Ops team
prevention <- data.table(
                data.frame( "date" = as.Date( c( "2018-01-01", "2018-02-01", "2018-03-01", 
                                                "2018-04-01", "2018-05-01" )),
                            "fraud_loss" = c( 97214.00, 26838.25, 117788.00, 68239.00, 58441.00 ),
                            "prevented_loss" = c( 53582.00, 0.00, 31957.00, 31376.00, 18046.00)))

# merge savings with df
prevention2 <- merge( df[ , .( total_amount = sum( amount ) ), by = date ], 
                      prevention,
                      by = "date", 
                      all.x = TRUE )

# clean data
prevention2[ is.na( prevented_loss ), prevented_loss := 0][ ]
prevention2[ , fraud_loss := ifelse( is.na( fraud_loss ), total_amount, fraud_loss ) ][ ]

# convert to long form
prevention3 <- melt(prevention2, measure.vars = c("fraud_loss", "prevented_loss"),
               variable.name = "prevented_loss", value.name = "amount")

# relevel prevented column for betting graphing
prevention3[ , Prevented  := factor( prevented_loss, 
                                     levels = c( "prevented_loss", "fraud_loss" ), 
                                     labels = c( "Yes", "No" ) ) ]


p <- ggplot( data = prevention3, 
             aes( x = date, y = amount, fill = Prevented ) ) +
       geom_bar( stat = "identity" ) + 
       scale_fill_manual( values = my_colors[ c( 3, 1 ) ] ) +
       scale_x_date( limits = c( min_date, max_date ),
                     labels = date_format( "%b %Y" ),
                     date_breaks = "1 month" ) +
       labs( title = "Monthly Fraud Loss With Two Day Hold Prevention", 
             x = "Charge-Off / Termination (Month)",
             y = "Total Loss ($)" ) +
       theme( legend.position=c( .1, .8 ), 
              axis.text.x = element_text( angle = 45, hjust = 1 ),
              plot.title = element_text( hjust = 0.5 ) ) 
p

# save plot
ggsave( filename = "plots/03B_monthly_fraud_loss_with_prevented.png", 
        plot = p, 
        width = 8, 
        height = 4 )
list.files( "plots" )


#----------------------------------------------#
# PLOT 4: RATE OF FRAUD LOSS BY PORTFOLIO SIZE #
#----------------------------------------------#

# get fraud cost/portfolio counts by month
# recall: df3 <- df[, .(amount = sum(amount)), by = date]
df4 <- merge( df3, 
              portfolio, 
              by = "date", 
              all.x = TRUE )
df4[ , loss_rate := amount / account_count ][ ]

# mean fraud loss per portfolio account
df4[ , mean( loss_rate ) ]

# plot
p <- ggplot( data = df4, 
             aes( x = date, y = loss_rate ) ) +
	   geom_bar( stat = "identity", 
	             fill = my_colors[ 1 ] ) + 
       scale_x_date( limits = c( min_date, max_date ),
                     labels = date_format( "%b %Y" ),
                     date_breaks = "1 month" ) +
       labs( title = "Rate of Fraud Loss by Portfolio Size", 
             y = "Charge-Off Cost ($) / Total Portfolio Count", 
             x = "Charge-Off / Termination (Month)" ) +
       theme( legend.position="none", 
              axis.text.x = element_text( angle = 45, hjust = 1 ),
              plot.title = element_text( hjust = 0.5 ) ) 
p

# save plot
ggsave( filename = "plots/04_rate_of_fraud_loss_by_portfolio.png", 
        plot = p, 
        width = 8, 
        height = 4 )
list.files( "plots" )


#------------------------#
# PLOT 5: FRAUD SEVERITY #
#------------------------#
# Note: Severity = (fraud $) / (# fraud accounts)

# get fraud severity by month
df5 <- df[ , .( amount = sum( amount ), count = .N ), by = date ]
df5[ , loss_rate := amount / count ][ ]

# average loss rate
df5[ , mean( loss_rate ) ]

# average loss rate without outliers
ol <- boxplot( df[ , amount ], plot = FALSE)$out
df[ , outlier := ifelse( amount %in% ol, 1, 0 ) ]
df[ outlier == 0, .( amount = sum( amount ), count = .N ), by = date ][ , 
    .( loss_rate = amount / count ) ][ , mean( loss_rate ) ]

# plot
p <- ggplot( data = df5, 
             aes( x = date, y = loss_rate ) ) +
	   geom_bar( stat = "identity", 
	             fill = my_colors[ 1 ] ) + 
       scale_x_date( limits = c( min_date, max_date ),
                     labels = date_format( "%b %Y" ),
                     date_breaks = "1 month" ) +
       labs( title = "Fraud Severity", 
             x = "Charge-Off / Termination (Month)",
             y = "Charge-Off Cost ($) / Count Charged Off" ) +
       theme( legend.position="none", 
              axis.text.x = element_text( angle = 45, hjust = 1 ),
              plot.title = element_text( hjust = 0.5 ) ) 
p

# save plot
ggsave( filename = "plots/05_fraud_severity.png", 
        plot = p, 
        width = 8, 
        height = 4)
list.files( "plots" )



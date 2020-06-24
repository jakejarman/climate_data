#### NOT RELEVANT because Hemda can get the data off the NIWA site herself



#### Importing the VCN data from the NIWA website using the Github script####
## https://github.com/niwa/data-mashup/blob/master/r_script.r
library(tidyverse)


# httr library to use HTTP and basic auth. You can install it with
# install.packages('httr')
library(httr)

# You should probably use command line parameters, dotEnv files,
# or another approach, instead of hard-coding values in your
# script.
USERNAME <- "jjarman"
PASSWORD <- "Hawera4672"
URL <- "https://data.niwa.co.nz:443/api/data/products/1/21645/159396797/1980-06-01T00:00:00+1200/1981-06-01T23:00:00+1200"

# Request data from NIWAData
req <- GET(URL, authenticate(USERNAME, PASSWORD, type = "basic"))
stop_for_status(req)

# Successful requests will return HTTP status code 200
stopifnot(req$status_code == 200)

# Get the response JSON
data <- content(req)$data

# You can retrieve the attributes about the dataset,
analysis_time <- data$analysisTime
measure_name <- data$measureName
name <- data$name
unit_symbol <- data$unitSymbol
# and also the values
values.list <- data$values

# Plot the values
values.df <- do.call(rbind, lapply(values.list, data.frame, stringsAsFactors=FALSE))

values_to_plot <- data.frame(X=as.POSIXct(rownames(values.df), format="%Y-%m-%dT%H:%M:%OS"), Y=values.df$X)

title <- name
x_label <- ""
y_label <- paste0("Value in ", unit_symbol)

#plot(values_to_plot, type="n", main=title, xlab=x_label, ylab=y_label)
plot(values_to_plot$X, values_to_plot$Y, main=title, type="n", xaxt="n", xlab=x_label, ylab=y_label)
grid(nx=NA, ny=NULL)
abline(v=axis.POSIXct(1,at=values_to_plot$X,labels=format(values_to_plot$X,"%d/%m"),las=1),col = "lightgray", lty = "dotted", lwd = par("lwd"))
#axis.POSIXct(1,at=values_to_plot$X,labels=format(values_to_plot$X,"%d/%m"),las=1)
lines(values_to_plot, col="blue")
values_to_plot

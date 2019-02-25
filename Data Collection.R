# ------------------------------------------------------------------------
# ---- Packages ----------------------------------------------------------
# ------------------------------------------------------------------------

require(data.table)
require(foreach)

# ------------------------------------------------------------------------
# ---- Functions ---------------------------------------------------------
# ------------------------------------------------------------------------

types = function(dat)
{
	dat = data.frame(dat)
  
	Column = sapply(1:NCOL(dat), function(i)
	(
		colnames(dat)[i]
    ))
  
	Data_Type = sapply(1:NCOL(dat), function(i)
    (
		class(dat[,i])
    ))
  
	results = data.frame(cbind(Column, Data_Type))	
	results
}

# ------------------------------------------------------------------------
# ---- Import Data -------------------------------------------------------
# ------------------------------------------------------------------------

# Data Sources:
	# Presidential Election Results by State by Year
	# http://www.presidency.ucsb.edu/showelection.php?year=1912
		# 1912 - 2016
		
	# Electoral College Size by State by Year
	# https://en.wikipedia.org/wiki/Electoral_College_(United_States)
		# Chronological table
		
	# National Voter Turnout by Year
	# http://www.electproject.org/national-1789-present
	# http://www.electproject.org/2016g

	# Wealth & Income Data by Country by Year
	# http://wid.world/data/
	
# if the above data sources don't produce valuable results then use the following ones
# these are only useful since 1986 (due to Voter Turnout by Demographic by Year)
	
	# Voter Turnout by State by Year
	# http://www.electproject.org/home/voter-turnout/voter-turnout-data
	
	# Voter Turnout by Demographic by Year
	# http://www.electproject.org/home/voter-turnout/demographics
	
	# Congressional bills and resolutions by final status by year
	# https://www.govtrack.us/congress/bills/statistics
	
	# Presidential Job Approval Time Series
	# http://www.presidency.ucsb.edu/data/popularity.php?pres=32&sort=time&direct=DESC&Submit=DISPLAY

# import the Presidential Election Results by State by Year and the Electoral College Size by State by Year

getwd()

dat1912 = read.csv("1912.csv")
dat1916 = read.csv("1916.csv")
dat1920 = read.csv("1920.csv")
dat1924 = read.csv("1924.csv")
dat1928 = read.csv("1928.csv")
dat1932 = read.csv("1932.csv")
dat1936 = read.csv("1936.csv")
dat1940 = read.csv("1940.csv")
dat1944 = read.csv("1944.csv")
dat1948 = read.csv("1948.csv")
dat1952 = read.csv("1952.csv")
dat1956 = read.csv("1956.csv")
dat1960 = read.csv("1960.csv")
dat1964 = read.csv("1964.csv")
dat1968 = read.csv("1968.csv")
dat1972 = read.csv("1972.csv")
dat1976 = read.csv("1976.csv")
dat1980 = read.csv("1980.csv")
dat1984 = read.csv("1984.csv")
dat1988 = read.csv("1988.csv")
dat1992 = read.csv("1992.csv")
dat1996 = read.csv("1996.csv")
dat2000 = read.csv("2000.csv")
dat2004 = read.csv("2004.csv")
dat2008 = read.csv("2008.csv")
dat2012 = read.csv("2012.csv")
dat2016 = read.csv("2016.csv")

# combine all these data tables into a list

states = list(yr1912 = dat1912, yr1916 = dat1916, yr1920 = dat1920, yr1924 = dat1924, 
			yr1928 = dat1928, yr1932 = dat1932, yr1936 = dat1936, yr1940 = dat1940, 
			yr1944 = dat1944, yr1948 = dat1948, yr1952 = dat1952, yr1956 = dat1956, 
			yr1960 = dat1960, yr1964 = dat1964, yr1968 = dat1968, yr1972 = dat1972, 
			yr1976 = dat1976, yr1980 = dat1980, yr1984 = dat1984, yr1988 = dat1988, 
			yr1992 = dat1992, yr1996 = dat1996, yr2000 = dat2000, yr2004 = dat2004, 
			yr2008 = dat2008, yr2012 = dat2012, yr2016 = dat2016)

# combine states into one data table

states = rbindlist(states, idcol = TRUE)
states[, "Year" := as.integer(substr(.id, 3, 6))]
states[, .id := NULL]

setcolorder(states, c(1, 7, 2:6))

# remove uneeded data

rm(dat1912, dat1916, dat1920, dat1924, 
	dat1928, dat1932, dat1936, dat1940, 
	dat1944, dat1948, dat1952, dat1956, 
	dat1960, dat1964, dat1968, dat1972, 
	dat1976, dat1980, dat1984, dat1988, 
	dat1992, dat1996, dat2000, dat2004, 
	dat2008, dat2012, dat2016)
	
# import electorial college size by state over time
	
EV = data.table(read.csv("EV.csv"))

# join states and EV together

setkey(states, Year, State)
setkey(EV, Year, State)

states = data.table(states[EV])
setcolorder(states, c(1:3, 8, 4:7))

rm(EV)

# update the data types of state

types(states)

states[, TotalVotes := as.character(TotalVotes)]
states[, DemocratVotes := as.character(DemocratVotes)]
states[, RepublicanVotes := as.character(RepublicanVotes)]

states[, Year := as.numeric(Year)]
states[, TotalVotes := as.numeric(gsub(",", "", TotalVotes))]
states[, TotalEV := as.numeric(TotalEV)]
states[, DemocratVotes := as.numeric(gsub(",", "", DemocratVotes))]
states[, RepublicanVotes := as.numeric(gsub(",", "", RepublicanVotes))]
states[is.na(states)] = 0

types(states)

# compute how many votes non democrat/republican canidate(s) earned per state per year

states[, OtherVotes := TotalVotes - DemocratVotes - RepublicanVotes]
states[, OtherEV := TotalEV - DemocratEV - RepublicanEV]

setcolorder(states, c(2, 1, 3:NCOL(states)))

# import the National Turnout by Year and Wealth and Income Data by Year data

turnout = data.table(read.csv("Turnout.csv"))
WID = data.table(read.csv("WID.csv"))

turnout[, Year := as.numeric(Year)]
WID[, Year := as.numeric(Year)]

# compute which party won the presidency each election year

winner = data.table(states[,.(DemocratWon = as.numeric(sum(DemocratEV) > sum(RepublicanEV))), by = Year])

# put all tables into a list

tables = list("states" = states, "turnout" = turnout, "WID" = WID, "winner" = winner)

rm(states, turnout, WID, winner)

# export tables to csv file

foreach(i = 1:length(tables)) %do%
{
	write.table(tables[[i]], 
				file = paste0(names(tables)[i], ".csv"), 
				sep = ", ",
				row.names = FALSE,
				quote = FALSE)
}


















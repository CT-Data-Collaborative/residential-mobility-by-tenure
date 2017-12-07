library(dplyr)
library(datapkg)
library(data.table)
library(stringr)
library(reshape2)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Residential Mobility by Tenure
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
col_names_file <- dir(path_to_raw, recursive=T, pattern = "column")

#Read in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])
towns <- as.data.table(towns)

dataset <- data.table()
for(file in list.files(raw_location, pattern = "ACS_1.*")) {
    # parse year out of file name
    year <- str_split(file, fixed("_"), 3)
    year <- 2000 + as.numeric(year[[1]][2])
    year <- paste(year-4, year, sep = "-")
    
    # this table is used to only select required columns
    # and rename all the columns something we can work with
    newNames <- read.csv(paste0(path_to_raw, "/", col_names_file), stringsAsFactors = FALSE)
    
    # read data
    # Grab only columns we want
    raw <- fread(
        file.path(getwd(), "raw", file),
        skip = 1L,
        select = newNames$id,
        na.strings = c("(x)", "-", "**"),
        colClasses = c(
            rep_len("character", 3),
            rep_len("numeric", 560)
        )
    )
    
    setnames(raw, newNames$name)
    remove(newNames)
    
    # cull any observations for "County subdivisions not defined _____"
    raw <- raw[
        substring(FIPS, 6) != "00000"
        ,]
    
    # reshape data
    raw <- melt(
        raw,
        id.vars = c("FIPS"),
        variable.name = "Variable",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
    )
    
    
    raw[,c("Variable", "Mobility", "Tenure") := do.call(Map, c(f = c, str_split(Variable, fixed(";"))))]
    
    # Cast values as numeric
    raw[, Value := as.numeric(Value)]
    
    # continue to reshape so we can do our calculations
    estimates <- raw[Variable == "Estimate"]
    estimates[, `:=`(
        Estimate = Value,
        Value = NULL,
        Variable = NULL
    )]
    setkey(estimates, FIPS, Mobility, Tenure)
    
    moes <- raw[Variable == "Margins of Error"]
    moes[, `:=`(
        `Margins of Error` = Value,
        Value = NULL,
        Variable = NULL
    )]
    setkey(moes, FIPS, Mobility, Tenure)
    
    numbers <- estimates[moes]

    # cleanup
    remove(estimates, moes)
    
    # pull out subgroup totals as they are our denominators
    totals <- numbers[Mobility == "Total", c(1,3:5), with = F]
    numbers <- numbers[Mobility != "Total"]
    
    setkey(totals, FIPS, Tenure)
    setkey(numbers, FIPS, Tenure)
    
    numbers <- numbers[totals]
    
    # create function for calculating MOE for derived numeric estimates
    moe.multiply <- function(est.1, moe.1, est.2, moe.2) {
        radicand <- (est.1^2 * moe.2^2) + (est.2^2 * moe.1^2)
        
        return(sqrt(radicand))
    }
    
    # "i" prefix specifies totals
    numbers[, `:=`(
        Estimate = (Estimate/100) * `i.Estimate`,
        `Margins of Error` = moe.multiply((Estimate/100), `Margins of Error`, (i.Estimate/100), `i.Margins of Error`),
        `i.Estimate` = NULL,
        `i.Margins of Error` = NULL
    )]
    
    # reshape again so that we have one value column, one variable column
    numbers <- melt(
        numbers,
        measure.vars = c("Estimate", "Margins of Error"),
        variable.name = "Variable",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
    )
    
    numbers$`Measure Type` <- "Number"
    
    # take corresponding percent measure from raw data
    raw <- raw[Mobility != "Total"]
    raw$`Measure Type` <- "Percent"
    
    # put numeric and percentage observations together
    raw <- rbind(raw[Mobility != "Total"], numbers)
    
    # round values appropriately, add year value
    raw[
            `Measure Type` == "Percent",
            Value := round(Value, 2)
        ][
            `Measure Type` == "Number",
            Value := round(Value, 0)
        ][
            ,
            Year := year
        ]
    
    # join town names by fips
    #towns <- fread(file.path(getOption("common_path"), "Geography", "town_fips.csv"))
    setkey(towns, FIPS)
    setkey(raw, FIPS)
    
    raw <- towns[raw]
    #remove(towns)
    
    #reorder columns
    setcolorder(raw, c(1,2,8,6,5,7,3,4))
    
    dataset <- rbind(dataset, raw)
}

# Change Variable name
dataset[
        Variable == "Estimate",
        Variable := "Residential Mobility"
    ]

#Sort columns
dataset <- dataset %>% 
  arrange(Town, Year, `Tenure`, `Measure Type`, desc(Variable))

# write to file - new NA value will be "-6666" for values that are incalculable or inestimable
# for now using suppression value "-9999"
write.table(
    dataset,
    file.path(getwd(), "data", "mobility_by_tenure_2016.csv"),
    sep = ",",
    row.names = F,
    na = "-6666"
)
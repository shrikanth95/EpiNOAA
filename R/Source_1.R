#' This function retrieves daily averages for the specified county codes or the
#' state. Start and end dates are optional inputs.
#' @param county_FIPS_codes An integer array of county FIPS codes.
#' @param measurements Four measurements currently available:
#' Temperature (Average, Minimum, Maximum) and Precipitation. Defaults to
#' c("Temp_avg","Temp_min", "Temp_max", "Precipitation").
#' @param coverage Shows the fraction of data that is not missing. Defaults to 1.
#' @param start_date Indicates the start date
#' @param end_date Indicates the end date
#' @return Basic summary statistics.
#'  @examples
#' get_daily_averages(37001,c("Temp_avg","Temp_min", "Temp_max", "Precipitation"))
#' get_daily_averages(37001,c("Temp_avg","Temp_min", "Temp_max", "Precipitation"), start_date = "2018-09-21", end_date = "2019-09-21")
#' @importFrom magrittr "%>%"
#' @export

get_daily_FIPS_averages <- function(county_FIPS_codes, measurements,dailyTemperatures,
                                    coverage = 1.0, start_date = NULL,
                                    end_date= NULL) {
  is.date <- function(x) inherits(x, 'Date')
  # county_FIPS_codes = 37001
  if((nargs()<3)&(is.null(start_date))&(is.null(end_date))){
    stop("\n atleast three arguments with county FIPS codes, measurements, and the original is needed. ")
  }
  else if((length(county_FIPS_codes)==0)){
    stop('\n Atleast one FIPS code is needed.')
  }
  load(file = "../data/EpiMeta_county.RData") # loads MetaEpiNOAA_county
  relevant_counties <- MetaEpiNOAA_county %>% dplyr::filter(FIPS_corrected %in% county_FIPS_codes)
  if(!is.null(start_date)){
    if((length(start_date)>1)) stop("\n multiple start dates not currently supported.")
    if ((all(is.date(start_date)))){ # check validity of dates
      stop("\n All dates in start_date argument should be in a standard unambiguous format.")
    }

    start_date = as.Date(start_date)
    if(any(start_date<relevant_counties$Start_Date))
      warning("WARNING: Some counties do not have data at the specified start date. Check available start dates in EpiNOAA::generate_data_dictionary()")
    relevant_counties = relevant_counties %>% dplyr::mutate(Start_Date = dplyr::case_when(start_date>Start_Date~start_date,
                                                                            T~Start_Date))
  }
  if(!is.null(end_date)){
    if((length(end_date)>1)) stop("\n multiple end dates not currently supported.")
    if ((all(is.date(end_date)))){ # check validity of dates
      stop("\n All dates in end_date argument should be in a standard unambiguous format.")
    }

    end_date = as.Date(end_date)
    if(any(end_date>relevant_counties$End_Date))
      warning("WARNING: Some counties do not have data at the specified end date. Check available start dates in EpiNOAA::generate_data_dictionary()")
    relevant_counties = relevant_counties %>% dplyr::mutate(End_Date = dplyr::case_when(end_date<End_Date~end_date,
                                                                          T~Start_Date))
  }
  if(!all(measurements %in% c("Temp_avg","Temp_min", "Temp_max", "Precipitation"))){
    check = paste(measurements[!(measurements %in% c("Temp_avg","Temp_min", "Temp_max", "Precipitation"))], sep = ", ")
    stop(paste("\n Measurements ",check," are not valid",sep =""))
  }

  if(!all(county_FIPS_codes %in% MetaEpiNOAA_county$FIPS_corrected)){
    check_FIPS = county_FIPS_codes[!(county_FIPS_codes %in% MetaEpiNOAA_county$FIPS_corrected)]
    if(length(check_FIPS)<11){
      check = paste(check_FIPS, collapse  = ", ")
      stop(paste("\n Some County FIPS codes are not valid. Check the FIPS codes: ",check, sep = ""))
    }
    else{
      check = paste(check_FIPS[1:10], collapse  = ", ")
      stop(paste("EpiNOAA ERROR: Some County FIPS codes are not valid. The first ten of the invalid codes are: ",check, sep = ""))
    }
  }
  if((!is.numeric(coverage))|(length(coverage)!=1)) stop("\n coverage should be a scalar between zero and one.")
  else if((coverage<0)|(coverage>1)){
    stop("\n Coverage should be a numeric between zero and one.")
  }


  callSummary = data.frame(county_FIPS_codes = county_FIPS_codes,
                           start_dates = start_date,
                           end_dates = end_date)

  # use this for single file
  # MetaEpiNOAA_county
  # county_interest = dailyTemperatures %>% dplyr::filter(FIPS_corrected %in% county_FIPS_codes)

    dailyTemperaturesWindow = dailyTemperatures %>%
      dplyr::select(Date, Year, Month, Day, State, County_Name,
                    FIPS_corrected,dplyr::all_of(measurements))%>%
      dplyr::filter(Date >= start_date,
                    FIPS_corrected %in% relevant_counties$FIPS_corrected,
                    Date <= end_date)
    # implement test case with available  county code, start Date, end date, and measurements
    # load said data dictionary
    return(dailyTemperaturesWindow)
}

#' This function retrieves metadata for the raw ARC dataset.
#' @param daily_temperatures A dataframe from the function get_daily_FIPS_averages().
#' @return A list with two elements county and state level with available
#' meta-data, start and end dates and coverage for each measurement.
#'  @examples
#' get_daily_averages(daily_temperatures)
#' @importFrom magrittr "%>%"
#' @export


get_data_dictionary <- function(daily_temperatures){
  is.date <- function(x) inherits(x, 'Date')
  if((nargs()<1)){
    stop("\n atleast one arguments with daily_temperatures is needed. ")
  }
  if(nrow(daily_temperatures)==0){
    stop('\n Data will need atleast one row.')
  }
  else if(all(colnames(daily_temperatures)!=c("Date", "Year", "Month", "Day",
                                              "State","County_Name",
                                              "FIPS_corrected","Temp_avg",
                                              "Temp_max","Temp_min",
                                              "Precipitation"))){
    stop("\n Current implementation only supports dataframe from the get_daily_FIPS_averages() function.")
  }

  # check = dailyTemperatures[!complete.cases(dailyTemperatures),]
  MetaEpiNOAA_county = dailyTemperatures %>%
    dplyr::group_by(State, County_Name, FIPS_corrected)%>%
    dplyr::summarize(Start_Date = min(Date),
                     End_Date = max(Date),
                     Expected_Samples = as.numeric((1+difftime(End_Date, Start_Date,units = "days"))),
                     Temp_avg = sum(!(Temp_avg==-999.99))/Expected_Samples,
                     Temp_max = sum(!(Temp_max==-999.99))/Expected_Samples,
                     Temp_min = sum(!(Temp_min==-999.99))/Expected_Samples,
                     Precipitation = sum(!(Precipitation==-999))/Expected_Samples)
  MetaEpiNOAA_state = dailyTemperatures %>%
    dplyr::group_by(State)%>%
    dplyr::summarize(Start_Date = min(Date),
                     End_Date = max(Date),
                     Counties = length(unique(County_Name)),
                     Expected_Samples = as.numeric(Counties*(1+difftime(End_Date, Start_Date,units = "days"))),
                     Temp_avg = sum(!(Temp_avg==-999.99))/Expected_Samples,
                     Temp_max = sum(!(Temp_max==-999.99))/Expected_Samples,
                     Temp_min = sum(!(Temp_min==-999.99))/Expected_Samples,
                     Precipitation = sum(!(Precipitation==-999))/Expected_Samples)
  return(list(County_Level = MetaEpiNOAA_county, State_level = MetaEpiNOAA_state))
}
#' This function retrieves historic daily averages for a given date.
#' @param daily_temperatures A dataframe from the function get_daily_FIPS_averages().
#' @param historic_date A date for which the
#' @return A dataframe containing
#'  @examples
#' get_historic_data(daily_temperatures, "2019-09-21")
#' @importFrom magrittr "%>%"
#' @export


get_historic_data <- function(daily_temperatures, historic_date){
  # dailyTemperatures = read.csv(file = "E:/SecureData/CTL/R package/ARC3years.csv",
  #                              header =F,
  #                              col.names =  c("Date", "Year", "Month", "Day",
  #                                             "State","County_Name",
  #                                             "FIPS_corrected","Temp_avg",
  #                                             "Temp_max","Temp_min",
  #                                             "Precipitation"))
  is.date <- function(x) inherits(x, 'Date')
  if((nargs()<2)){
    stop("\n atleast two arguments with data and histroic date is needed. ")
  }
  if(nrow(daily_temperatures)==0){
    stop('EpiNOAA ERROR: data will need atleast one row.')
  }
  else if(all(colnames(daily_temperatures)!=c("Date", "Year", "Month", "Day",
                                          "State","County_Name",
                                          "FIPS_corrected","Temp_avg",
                                          "Temp_max","Temp_min",
                                          "Precipitation"))){
    stop("\n Current implementation only supports dataframe from the get_daily_FIPS_averages() function.")
  }
  daily_temperatures$Date <- as.Date(daily_temperatures$Date)
  if(!is.null(historic_date)){
    if((length(historic_date)>1)) stop("\n Multiple start dates not currently supported.")
    historic_date = as.Date(historic_date)
    if (!(all(is.date(historic_date)))){ # check validity of dates
      stop("\n All dates in historic_date argument should be in a standard unambiguous format.")
    }
  }
  else{stop("Check input sequence.")}
  if (!(all(is.date(daily_temperatures$Date)))){ # check validity of dates in the dataframe.
    stop("\n All dates in daily_temperatures argument should be in a standard unambiguous format.")
  }


  historicData = daily_temperatures %>% filter(Date == historic_date)

  return(historicData)
}



#' This function retrieves daily averages for the specified county codes or the
#' state. Start and end dates are optional inputs.
#' @param county_FIPS_codes An integer array of county FIPS codes.
#' @param path Relative path to the folder where the retrieved data will be
#' stored. Defaults to a folder named EpiNOAA_call. TODO
#' @param measurements Four measurements currently available:
#' Temperature (Average, Minimum, Maximum) and Precipitation. Defaults to
#' c("Temp_avg","Temp_min", "Temp_max", "Precipitation").
#' @param coverage Shows the fraction of data that is not missing. Defaults to 1.
#' @param plot_directory Relative path to the folder where the time series plot
#' of the daily data will be saved. Defaults to a folder named EpiNOAA_plots. TODO
#' @return Basic summary statistics.
#'  @examples
#' get_daily_averages(37001,c("Temp_avg","Temp_min", "Temp_max", "Precipitation"))
#' get_daily_averages(37001,c("Temp_avg","Temp_min", "Temp_max", "Precipitation"), start_date = "2018-09-21", end_date = "2019-09-21")
#' @importFrom magrittr "%>%"
#' @export

get_daily_FIPS_averages <- function(county_FIPS_codes, measurements, path=NULL,
                                    coverage = 1.0, plot_directory = NULL,
                                    start_date = NULL, end_date= NULL,
                                    verbose = NULL) {
  # county_FIPS_codes = 37001
  if((nargs()<2)&(is.null(path))&(is.null(plot_directory))&(is.null(start_date))&(is.null(end_date))&(is.null(verbose))){
    stop("\n EpiNOAA ERROR: atleast two arguments with county FIPS codes and measurements is needed. ")
  }
  else if((length(county_FIPS_codes)==0)){
    stop('EpiNOAA ERROR: Atleast one FIPS code is needed.')
  }
  load(file = "E:/SecureData/CTL/R package/EpiMeta.RData") # loads MetaEpiNOAA_county
  relevant_counties <- MetaEpiNOAA_county %>% dplyr::filter(FIPS_corrected %in% county_FIPS_codes)
  if(!is.null(start_date)){
    if((length(start_date)>1)) stop("\n EpiNOAA ERROR: multiple start dates not currently supported.")
    if ((all(lubridate::is.Date(start_date)))){ # check validity of dates
      stop("\n EpiNOAA ERROR: All dates in start_date argument should be in a standard unambiguous format.")
    }

    start_date = as.Date(start_date)
    if(any(start_date<relevant_counties$Start_Date))
      warning("EpiNOAA WARNING: Some counties do not have data at the specified start date. Check available start dates in EpiNOAA::generate_data_dictionary()")
    relevant_counties = relevant_counties %>% mutate(Start_Date = case_when(start_date>Start_Date~start_date,
                                                                            T~Start_Date))
  }
  if(!is.null(end_date)){
    if((length(end_date)>1)) stop("\n EpiNOAA ERROR: multiple end dates not currently supported.")
    if ((all(lubridate::is.Date(end_date)))){ # check validity of dates
      stop("\n EpiNOAA ERROR: All dates in end_date argument should be in a standard unambiguous format.")
    }

    end_date = as.Date(end_date)
    if(any(end_date>relevant_counties$End_Date))
      warning("EpiNOAA WARNING: Some counties do not have data at the specified end date. Check available start dates in EpiNOAA::generate_data_dictionary()")
    relevant_counties = relevant_counties %>% mutate(End_Date = case_when(end_date<End_Date~end_date,
                                                                          T~Start_Date))
  }
  if(!all(measurements %in% c("Temp_avg","Temp_min", "Temp_max", "Precipitation"))){
    check = paste(measurements[!(measurements %in% c("Temp_avg","Temp_min", "Temp_max", "Precipitation"))], sep = ", ")
    stop(paste("EpiNOAA ERROR: Measurements ",check," are not valid",sep =""))
  }


  load(file = "E:/SecureData/CTL/R package/dailyTemperatures.RData")

  if(!all(county_FIPS_codes %in% MetaEpiNOAA_county$FIPS_corrected)){
    check_FIPS = county_FIPS_codes[!(county_FIPS_codes %in% MetaEpiNOAA_county$FIPS_corrected)]
    if(length(check_FIPS)<11){
      check = paste(check_FIPS, collapse  = ", ")
      stop(paste("EpiNOAA ERROR: Some County FIPS codes are not valid. Check the FIPS codes: ",check, sep = ""))
    }
    else{
      check = paste(check_FIPS[1:10], collapse  = ", ")
      stop(paste("EpiNOAA ERROR: Some County FIPS codes are not valid. The first ten of the invalid codes are: ",check, sep = ""))
    }
  }
  if((!is.numeric(coverage))|(length(coverage)!=1)) stop("\n EpiNOAA ERROR: coverage should be a scalar between zero and one.")
  else if((coverage<0)|(coverage>1)){
    stop("\n EpiNOAA ERROR: Coverage should be a numeric between zero and one.")
  }


  callSummary = data.frame(county_FIPS_codes = county_FIPS_codes,
                           start_dates = start_date,
                           end_dates = end_date)

  # MetaEpiNOAA_county
  # county_interest = dailyTemperatures %>% dplyr::filter(FIPS_corrected %in% county_FIPS_codes)
  countyList = list()
  for(county in 1:nrow(relevant_counties)){
    countyList[[county]] = dailyTemperatures %>% dplyr::filter(Date >= relevant_counties$Start_Date[county],
                                                  FIPS_corrected %in% relevant_counties$FIPS_corrected[county],
                                                  Date <= relevant_counties$End_Date[county])

  }
  rawData <- do.call(rbind, countyList)
  # implement test case with available  county code, start Date, end date, and measurements
  # load said data dictionary
  return(rawData)

}

#' This function retrieves daily averages for the specified county codes or the
#' state. Start and end dates are optional inputs.
#' @param input No input required
#' @return A list with two elements county and state level with available
#' meta-data, start and end dates and coverage for each measurement.
#'  @examples
#' get_daily_averages()
#' @importFrom magrittr "%>%"
#' @export


get_data_dictionary <- function(){
  dailyTemperatures = read.csv(file = "E:/SecureData/CTL/R package/ARC3years.csv",
                               header =F,
                               col.names =  c("Date", "Year", "Month", "Day",
                                              "State","County_Name",
                                              "FIPS_corrected","Temp_avg",
                                              "Temp_max","Temp_min",
                                              "Precipitation"))
  dailyTemperatures$Date <- as.Date(with(dailyTemperatures, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
  # check = dailyTemperatures[!complete.cases(dailyTemperatures),]
  MetaEpiNOAA_county = dailyTemperatures %>%
    dplyr::group_by(State, County_Name, FIPS_corrected)%>%
    dplyr::summarize(Start_Date = min(Date),
                     End_Date = max(Date),
                     Expected_Samples = as.numeric((1+difftime(End_Date, Start_Date,units = "days"))),
                     Temp_avg = sum(!is.na(Temp_avg))/Expected_Samples,
                     Temp_max = sum(!is.na(Temp_max))/Expected_Samples,
                     Temp_min = sum(!is.na(Temp_min))/Expected_Samples,
                     Precipitation = sum(!is.na(Precipitation))/Expected_Samples)
  MetaEpiNOAA_state = dailyTemperatures %>%
    dplyr::group_by(State)%>%
    dplyr::summarize(Start_Date = min(Date),
                     End_Date = max(Date),
                     Counties = length(unique(County_Name)),
                     Expected_Samples = as.numeric(Counties*(1+difftime(End_Date, Start_Date,units = "days"))),
                     Temp_avg = sum(!is.na(Temp_avg))/Expected_Samples,
                     Temp_max = sum(!is.na(Temp_max))/Expected_Samples,
                     Temp_min = sum(!is.na(Temp_min))/Expected_Samples,
                     Precipitation = sum(!is.na(Precipitation))/Expected_Samples)
  return(list(County_Level = MetaEpiNOAA_county, State_level = MetaEpiNOAA_state))
}

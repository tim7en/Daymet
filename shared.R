getYear <- function (x) {
  Y <- format(as.Date(x, format="%Y-%m-%d"),"%Y")
  return (Y)
}

getNcdf4 <- function (date1,date2,Var, N, W, E, S){
  
  Y <- getYear (date1)
  
  url <- paste0(
    'https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/1328/',
    Y,
    '/daymet_v3_',
    Var,
    '_',
    Y,
    '_na.nc4?var=lat&var=lon&var=',
    Var,
    '&north=',
    N,
    '&west=',
    W,
    '&east=',
    E,
    '&south=',
    S,
    '&disableProjSubset=on&horizStride=1&time_start=',
    date1,
    'T12%3A00%3A00Z&time_end=',
    date2,
    'T12%3A00%3A00Z&timeStride=1&accept=netcdf'
  )
  return(url)
}
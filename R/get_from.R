#' Get first or last date for which there are data stored on disk
#'
#' Get first or last date for which there are data stored on disk
#' 
#' Data should be stored as follows:  There should be a directory under \code{base_dir} named
#' the same as \code{Symbol} that contains a separate data file for each day.  The
#' data file names should begin with "\%Y.\%m.\%d".  ("2011.07.05.SPY.RData", for example).
#'
#' @param Symbol name of instrument
#' @param base_dir directory that has subdirectories for all of your Symbols (where each subdirectory contains data files named ).
#' @param maxDays if there are no data for \code{Symbol} stored on disk, get_from and get_to will return today's date minus maxDays.
#' @return character string of format "\%Y-\%m-\%d" representing the first date for which there is
#' data stored in \code{base_dir}.  If there are no data for \code{Symbol} then the date returned
#' will be today's date minus \code{maxDays}
#' @aliases get_from get_to
#' @author Garrett See
#' @seealso \code{\link[FinancialInstrument]{saveSymbols.days}} to save data the way this function expects it to be saved.
#' @export
#' @rdname get_from
#' @examples
#' get_from("SPY")
#' @export
#' @rdname get_from
get_from <-
function(Symbol, base_dir='/mnt/W/TRADES', maxDays=365) {
    if (!Symbol %in% list.files(base_dir,all.files=TRUE)) { 
        warning(paste("Cannot find directory ", Symbol, 
                " in ", base_dir, ". Returning Sys.Date() - maxDays \n", 
                Sys.Date() - maxDays,sep=""))      
        return (Sys.Date() - maxDays)
    }
    if (substr(base_dir,nchar(base_dir),nchar(base_dir)) != "/") base_dir <- paste(base_dir, "/",sep="")
    firstfile <- list.files(paste(base_dir,Symbol,sep=""))[1]
    paste(strsplit(firstfile, "\\.")[[1]][1:3],collapse="-")
}

#' @export
#' @rdname get_from
get_to <-
function(Symbol, base_dir='/mnt/W/TRADES', maxDays=365) {
    if (!Symbol %in% list.files(base_dir,all.files=TRUE)) {
      warning(paste("Cannot find directory ", Symbol, 
                " in ", base_dir, ". Returning Sys.Date() - maxDays ", 
                Sys.Date() - maxDays,sep=""))      
      return (Sys.Date() - maxDays)
    }
    if (substr(base_dir,nchar(base_dir),nchar(base_dir)) != "/") base_dir <- paste(base_dir, "/",sep="")
    lastfile <- last(list.files(paste(base_dir,Symbol,sep="")))    
    return(paste(strsplit(lastfile, "\\.")[[1]][1:3],collapse="-"))
}

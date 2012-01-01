#TODO: add match to all functions
#.onLoad <- function(lib, pkg) {
#    if(!exists('.instrument'))
#        .instrument <<- new.env(hash=TRUE)
#}



#' instrument class constructor.
#' 
#' instrument class constructor. Slightly modified version of
#' FinancialInstrument function by same name
#' 
#' This function masks the instrument function from FinancialInstrument.  The
#' main difference is thatthis version allows a blank instrument template to be
#' created (but usually not stored)It is not necessary to define a
#' \code{currency}, or to provide \code{primary_id}, or \code{multiplier}. If
#' no primary_id is provided, the \code{assign_i} argument will be overridden,
#' and the instrument will not be stored.
#' 
#' Currently everything in the twsInstrument package will function properly
#' with the original (FinancialInstrument) version. However, it can be
#' inhibiting.
#' 
#' For example, knowing only the conId, you could get all of the details of a
#' tradeable security from Interactive Brokers.  Therefore, it would be nice to
#' store conIds in instrument slots even before we know anything else about the
#' instrument.  With the original instrument function you cannot do this.  In
#' addition to having to know the primary_id, multiplier and currency of the
#' instrument, you would have also have to have previously defined the
#' instrument's currency.
#' 
#' Most function do not attempt to create an instrument without fulfilling the
#' prerequisites. The exception is \code{define_stocks}.  If it is called with
#' \code{currency=""} or \code{currency=NULL} it will create an instrument
#' without a currency, and then request that IB fill it in. This is useful if
#' you are defining several stocks that are not all denominated in the same
#' currency.
#' 
#' (Below is pasted from original instrument function) In \dots{} you may pass
#' any other arbitrary instrument fields that will be used to create 'custom'
#' fields.  S3 classes in are basically lists with a class attribute. We use
#' this to our advantage to allow us to set arbitrary fields.
#' 
#' \code{identifiers} should be a named list to specify other identifiers
#' beyond the \code{primary_id}. Please note that whenever possible, these
#' should still be unique.  Perhaps Bloomberg, Reuters-X.RIC, CUSIP, etc.  The
#' code will return the first (and only the first) match that it finds,
#' starting with the primary_id, and then searching all instruments in the list
#' alphabetically by primary_id.  This is robust enough if you take some care,
#' though a more robust patch would be welcomed.
#' 
#' If \code{primary_id} is given, it will be coerced within reason to a valid
#' variable name by using \code{\link{make.names}}. We also remove any leading
#' '1' digit (a simple workaround to account for issues with the Reuters API).
#' Please use some care to choose your primary identifiers so that R won't
#' complain. If you have better regular expression code, we'd be happy to
#' include it.
#' 
#' Identifiers will also try to be discovered as regular named arguments passed
#' in via \code{...}.  We currently match any of the following:
#' \code{"CUSIP","SEDOL","ISIN","OSI","Bloomberg","Reuters","X.RIC","CQG","TT","Yahoo","Google"}
#' Others mat be specified using a named list of identifiers, as described
#' above.
#' 
#' \code{assign_i} will use \code{\link{assign}} to place the constructed
#' instrument class object into the \code{.instrument} environment.  Most of
#' the special type-specific constructors will use \code{assign_i=TRUE}
#' internally.  Calling with \code{assign_i=FALSE}, or not specifying it, or
#' not specifying \code{primary_id} will return an object and will \emph{not}
#' store it.  Use this option ether to wrap calls to \code{instrument} prior to
#' further processing (and presumably assignment) or to test your parameters
#' before assignment.
#' 
#' @param primary_id string describing the unique ID for the instrument
#' @param \dots any other passthru parameters including \sQuote{underlying_id}
#' for derivatives: the identifier of the instrument that this one is derived
#' from.
#' @param currency string describing the currency ID of an object of type
#' \code{\link{currency}}
#' @param multiplier numeric multiplier to apply to the price in the instrument
#' currency to get to notional value
#' @param tick_size the tick increment of the instrument price in it's trading
#' venue, as numeric quantity (e.g. 1/8 is .125)
#' @param identifiers named list of any other identifiers that should also be
#' stored for this instrument
#' @param type instrument type to be appended to the class definition,
#' typically not set by user
#' @param assign_i TRUE/FALSE if TRUE, assign the instrument to the .instrument
#' environment, default FALSE
#' @note This function was written by the authors of FinancialInstrument, and
#' has only been slightly modified by me.  Furthermore, this help page is
#' copied almost verbatim from the instrument help page in FinancialInstrument.
#' @export
instrument.tws <-
function (primary_id="", ..., currency="", multiplier="", tick_size = NULL, 
    identifiers = NULL, type = NULL, assign_i = FALSE) 
{
    if (is.null(primary_id) || primary_id=="") {
        assign_i = FALSE
        warning('No primary_id. Instrument will not be assigned')
#        stop("you must specify a primary_id for the instrument")
    } else {
        if (substr(primary_id, 1, 1) == 1) 
        primary_id <- substr(primary_id, 2, nchar(primary_id))
        primary_id <- make.names(primary_id)
    }
    
#    if (!is.currency(currency)) 
#        stop("currency ", currency, " must be an object of type 'currency'")
    if (!hasArg(identifiers) || is.null(identifiers)) 
        identifiers = list()
    if (!is.list(identifiers)) {
        warning("identifiers", identifiers, "do not appear to be a named list")
    }
    arg <- list(...)
    if (is.list(arg[["..."]])) {
        if (length(arg) == 1) 
            arg <- arg[["..."]]
        else {
            targ <- arg[["..."]]
            arg[["..."]] <- NULL
            arg <- c(arg, targ)
        }
    }
    ident_str <- c("X.RIC", "RIC", "CUSIP", "SEDOL", "OSI", "Bloomberg", 
        "Reuters", "ISIN", "CQG", "TT", "Yahoo", "Google")
    for (i_s in ident_str) {
        if (any(grepl(i_s, names(arg), ignore.case = TRUE))) {
            pos <- first(grep(i_s, names(arg), ignore.case = TRUE))
            identifiers <- c(identifiers, arg[[pos]])
            names(identifiers)[length(identifiers)] <- names(arg)[pos]
            arg[[pos]] <- NULL
        }
    }
#    if (!is.numeric(multiplier) | length(multiplier) > 1) 
#        stop("multiplier must be a single number")
    if (!is.null(tick_size) && (!is.numeric(tick_size) | length(tick_size) > 
        1)) 
        stop("tick_size must be NULL or a single number")
    if (is.null(type)) 
        tclass = "instrument"
    else tclass = c(type, "instrument")
    tmpinstr <- list(primary_id = primary_id, currency = currency, 
        multiplier = multiplier, tick_size = tick_size, identifiers = identifiers, 
        type = type)
    if (length(arg) >= 1) {
        tmpinstr <- c(tmpinstr, arg)
    }
    class(tmpinstr) <- tclass
    if (assign_i) 
        assign(primary_id, tmpinstr, envir = as.environment(FinancialInstrument:::.instrument))
    else return(tmpinstr)
}


#stock <-
#function (primary_id, currency = NULL, multiplier = 1, tick_size = 0.01, 
#    identifiers = NULL, ...) 
#{
#    stock_temp = instrument(primary_id = primary_id, currency = currency, 
#        multiplier = multiplier, tick_size = tick_size, identifiers = identifiers, 
#        ..., type = "stock", assign_i = TRUE)
#}

#option <-
#function (primary_id, currency, multiplier, tick_size = NULL, 
#    identifiers = NULL, ..., underlying_id = NULL) 
#{
#    option_temp = instrument(primary_id = primary_id, currency = currency, 
#        multiplier = multiplier, tick_size = tick_size, identifiers = identifiers, 
#        ..., type = "option")
#    if (is.null(underlying_id)) {
#        warning("underlying_id should only be NULL for cash-settled options")
#    }
#    else {
#        if (!exists(underlying_id, where = .instrument, inherits = TRUE)) 
#            warning("underlying_id not found")
#    }
#    option_temp = instrument(primary_id = primary_id, currency = currency, 
#        multiplier = multiplier, tick_size = tick_size, identifiers = identifiers, 
#        ..., type = "option", underlying_id = underlying_id, 
#        assign_i = TRUE)
#}

#future <-
#function (primary_id, currency, multiplier, tick_size = NULL, 
#    identifiers = NULL, ..., underlying_id = NULL) 
#{
#    if (is.null(underlying_id)) {
#        warning("underlying_id should only be NULL for cash-settled futures")
#    }
#    else {
#        if (!exists(underlying_id, where = .instrument, inherits = TRUE)) 
#            warning("underlying_id not found")
#    }
#    future_temp = instrument(primary_id = primary_id, currency = currency, 
#        multiplier = multiplier, tick_size = tick_size, identifiers = identifiers, 
#        ..., type = "future", underlying_id = underlying_id, 
#        assign_i = TRUE)
#}

#currency <-
#function (primary_id, currency = NULL, multiplier = 1, identifiers = NULL, 
#    ...) 
#{
#    currency_temp <- list(primary_id = primary_id, currency = primary_id, 
#        multiplier = 1, tick_size = 0.01, identifiers = identifiers, type='currency')
#    currency_temp <- c(currency_temp, list(...))
#    class(currency_temp) <- c("currency", "instrument")
#    assign(primary_id, currency_temp, envir = as.environment(.instrument))
#}











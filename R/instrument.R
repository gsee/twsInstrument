#TODO: add match to all functions
.onLoad <- function(lib, pkg) {
    if(!exists('.instrument'))
        .instrument <<- new.env(hash=TRUE)
}

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
        assign(primary_id, tmpinstr, envir = as.environment(.instrument))
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











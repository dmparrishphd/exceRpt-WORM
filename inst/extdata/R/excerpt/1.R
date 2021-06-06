excerpt <- function (
        description ,
        i = NULL ,
        encoding = eval ( formals ( file ) $ encoding ) ,
        method = eval ( formals ( file ) $ method ) ) {
    stopifnot (
        is.character ( description ) ,
        length ( description ) == 1 )
    if ( ! is.null ( i ) ) stopifnot (
        is.numeric ( i ) ,
        all ( is.finite ( i ) ) ,
        all ( 0 < i ) )
    readLines. <- function ( con , n = -1L ) readLines (
        con = con ,
        n = n ,
        ok = n < 0 ,
        skipNul = TRUE )
    con <- file (
        description = description ,
        open = "rt" ,
        encoding = encoding ,
        method = method )
    LINES <- tryCatch (
        expr =
            if ( is.null ( i ) ) {
                readLines. ( con )
            } else {
                readLines. ( con , n = max ( i ) ) [ i ] } ,
        error = function ( e ) e )
    close ( con )
    if ( ! is.character ( LINES ) ) stop ( LINES )
    LINES }

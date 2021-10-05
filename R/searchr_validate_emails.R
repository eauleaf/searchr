#' Check if an email looks mis-typed
#'
#' Checks for common email issues, but email conformity is hard. See
#' tradeoffs discussion for regexs:  https://www.regular-expressions.info/email.html
#'
#' @param email_vec character vector of email addresses
#'
#' @return a bool vector where T means okay, and FALSE means probably a mis-typed email
#'
#' @export
#' @examples schr_validate_emails(c("Sun100@yahoo.com","RICHY5000@MSN.COM","ab?c@mac.com"))
schr_validate_emails <- function(email_vec){

  email_vec %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_detect("^(?=[a-z0-9@._%+-]{6,254}$)[a-z0-9._%+-]{1,64}@(?:[a-z0-9-]{1,63}\\.){1,8}[a-z]{2,63}$")

}


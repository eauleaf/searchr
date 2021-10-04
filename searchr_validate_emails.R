#' check if an email looks mis-typed
#' (tradeoffs discussion:  https://www.regular-expressions.info/email.html)
#'
#' @param email_vec character vector of email addresses
#'
#' @return a bool vector where T means okay, and FALSE means probably a mis-typed email
#'
#' @examples \code{email_vec <- c("Sun100@yahoo.com","RICHY5000@MSN.COM","ab?c@mac.com")
#'                 schr_validate_emails(email_vec)}
schr_validate_emails <- function(email_vec){
  
  email_vec %>% 
    str_trim() %>% 
    str_to_lower() %>% 
    str_detect("^(?=[a-z0-9@._%+-]{6,254}$)[a-z0-9._%+-]{1,64}@(?:[a-z0-9-]{1,63}\\.){1,8}[a-z]{2,63}$")
  
}


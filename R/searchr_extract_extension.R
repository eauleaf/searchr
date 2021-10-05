#' Gets email or phone extension
#'
#' Returns extension from email or phone number when given a separation character like "@"
#'
#' @param emails vector of emails or phone numbers
#' @param ext_separator regex separator characters denoting email or phone number extension e.g. ext_separator = "ext", "#", "@"
#'
#' @return email or phone number extensions as char vec
#' @export
#'
#' @examples schr_extract_extension(c("Sun100@yahoo.com","RICHY5000@MSN.COM","ab?c@mac.com"), '@')

schr_extract_extension <- function(emails, ext_separator){
  (stringr::str_extract(emails, pattern = stringr::str_glue(ext_separator,".+"))) %>%
    stringr::str_replace(pattern = ext_separator, replacement = "") %>%
    stringr::str_squish() %>%
    tidyr::replace_na(replace = "")
}


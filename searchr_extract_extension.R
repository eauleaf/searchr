#' pull extension out of the phone number given the preceeding extension pattern
#'
#' @param emails vector of emails or phone numbers
#' @param ext_separator separator characters denoting email or phone number extension e.g. ext_separator = "ext", ext_separator = "#", "@"
#'
#' @return email or phone number extensions as char vec
#' @export
#'
#' @examples  \code{
#' schr_extract_extension(c("Sun100@yahoo.com","RICHY5000@MSN.COM","ab?c@mac.com"), '@')
#' }
schr_extract_extension <- function(emails, ext_pattern){
  (stringr::str_extract(emails, pattern = stringr::str_glue(ext_pattern,".+"))) %>%
    stringr::str_replace(pattern = ext_pattern, replacement = "") %>%
    stringr::str_squish() %>% 
    tidyr::replace_na(replace = "")
}


#' remove extension from email or phone number
#'
#' @param emails vector of emails or phone numbers
#' @param ext_separator separator characters denoting phone number extension e.g. ext_separator = "ext", ext_separator = '#', '@'
#'
#' @return email or phone number extensions as char vec
#' @export
#'
#' @examples schr_remove_extension(c("Sun100@yahoo.com","RICHY5000@MSN.COM","ab?c@mac.com"), '@')
schr_remove_extension <- function(emails, ext_pattern) {
  
  emails <- tidyr::replace_na(emails, replace = "") %>% stringr::str_squish()
  ext <- stringr::str_extract(emails, pattern = stringr::str_glue(ext_pattern,".+")) %>% 
    tidyr::replace_na(., replace = "")
  emails <- stringr::str_sub(emails, 1, nchar(emails)-nchar(ext))
  
  return(emails)
  
}


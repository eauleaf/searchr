#' Remove email or phone extension
#'
#' Removes extension from email or phone number when given a separation character like "@"
#'
#' @param emails vector of emails or phone numbers
#' @param ext_separator regex separator characters denoting email or phone number extension e.g. ext_separator = "ext", ext_separator = '#', '@'
#'
#' @return email or phone number extensions as char vec
#'
#' @export
#' @examples schr_remove_extension(c("Sun100@yahoo.com","RICHY5000@MSN.COM","ab?c@mac.com"), '@')
schr_remove_extension <- function(emails, ext_separator) {

  emails <- tidyr::replace_na(emails, replace = "") %>% stringr::str_squish()
  ext <- stringr::str_extract(emails, pattern = stringr::str_glue(ext_separator,".+")) %>%
    tidyr::replace_na(., replace = "")
  emails <- stringr::str_sub(emails, 1, nchar(emails)-nchar(ext))

  return(emails)

}


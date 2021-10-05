#' Standardizes names, emails, and phone numbers
#'
#' Performs simple preparation of email, customer name, and phone data by standardizing
#' input data for US locale and removing common problems specific to those data types.
#' Run prior to calculating combinatorial matching
#'
#' @param x a vector containing email, phone data, customer or business names
#' @param type input data type of c('email', 'phone', 'name')
#' @return a vector containing standardized email, phone data, customer or business names
#' @export
#'
#' @examples
#' schr_clean_pii(c("Sun100@yahoo.com","RICHY5000@MSN.COM","ab?c@mac.com"), 'email')
#' schr_clean_pii('12345678910', 'phone')
schr_clean_pii <- function(x, type = 'name') {

  type <- match.arg(type, c('email', 'phone', 'name'))


  if(type == 'email') {


    out <- x %>%
      # TODO: add more email cleaning logic, i.e. forbidden characters in certain locations
      gsub("[ ]", "", .) %>% # remove all spaces
      stringr::str_to_lower()


  } else if (type == 'name'){


    out <- x %>%
      stringr::str_to_lower() %>%
      stringr::str_remove_all(pattern = "[-,|./`'\\_{}()# ]") %>% # remove any special characters
      stringr::str_remove_all("\\b(ltd|l\\.?l?\\.?[cp]|inc|incorporated|corp|corporation|co|company|na|none)\\.?\\b") %>%
      stringr::str_replace_all(" & | and ", replacement = "&") %>%
      # str_remove_all(pattern = "\\b\\w\\b| ") %>%   # remove middle initials or stand-alone characters
      stringr::str_squish()


  } else if (type == 'phone'){

    out <- x %>%
      stringr::str_remove_all('[^\\d]') %>%
      stringr::str_trim() %>%
      tibble::tibble(phone = .) %>%
      dplyr::mutate(
        phone = dplyr::case_when(
          ((nchar(phone) == 11) & (substr(phone, 1, 1) == '1'))  == T ~ substr(phone, 2, 11),
          ((nchar(phone) == 12) & (substr(phone, 1, 2) == '01')) == T ~ substr(phone, 3, 12),
          TRUE ~ phone),
        #  remove obvious non-phone numbers (i.e. 8888888888, 1234567890)
        phone = stringr::str_replace(phone,'^(.)\\1+$|0123456789|1234567890',''),
        # keep only numbers with reasonable character counts
        phone = dplyr::if_else(nchar(phone) %in% c(7,9,10), phone, "")
        # phone = na_if(phone, '')
      ) %>%
      unlist()


  }

  return(out)

}

#' Tables character counts
#'
#' Counts and tables characters in a vector of strings ordered from most to least
#' frequent
#'
#' @param char_vec vector of character strings to table with character counts
#' @return tabled and aligned character count matrix
#' @export
#' @examples schr_tbl_chars(c(" Amazon.com Inc ", " amazon inc"))
schr_tbl_chars <- function(char_vec){

  char_vec %>%
    stringr::str_split('|') %>%
    purrr::map(~table(.)[-1]) %>%
    purrr::map(rbind) %>%
    purrr::map(tibble::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::bind_cols(char_count = rowSums(.,na.rm = T),.) %>%
    dplyr::mutate(dplyr::across(.fns = ~as.integer(tidyr::replace_na(.,0)))) %>%
    dplyr::select(order(colSums(.),decreasing = T))

}

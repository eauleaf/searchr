#' counts characters in a vector of strings
#'
#' @param char_vec vector of character strings to table with character counts
#' @return tabled and aligned character count matrix
#' 
#' @examples \code{schr_tbl_chars(c(" Amazon.com Inc ", " amazon inc"))}
schr_tbl_chars <- function(char_vec){
  
  char_vec %>% 
    str_split('|') %>% 
    map(~table(.)[-1]) %>% 
    map(rbind) %>% 
    map(as_tibble) %>% 
    bind_rows() %>% 
    bind_cols(char_count = rowSums(.,na.rm = T),.) %>% 
    mutate(across(.fns = ~as.integer(replace_na(.,0)))) %>% 
    select(order(colSums(.),decreasing = T))
  
}

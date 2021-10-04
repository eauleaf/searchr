#' matches any combination of letters from A against any combination of letters in B within specified tolerances. Function is case-sensitive.
#' 
#' @param match_this tabled characters; primary input with single row of a table created by /code{table_characters()}
#' @param against_these tabled characters; various possible names to match the first argument against, characters also must be tabled through /code{table_characters()}
#' @param a_not_in_b_tolerance number of non-required characters from your /code{match_this} input to find in matches, i.e. how many characters from A are not required to be in B e.g. "tori" has no characters that "tory girl" does not also have; any tolerance setting would match these; however, "tori" has one character that "tory" does not have, so matching these would require at least a tolerance of 1. Combinatorially, "tori" is a perfect match with "tior", "riot", etc, which means an /code{a_not_in_b_tolerance} of zero would find these matches
#' @param b_not_in_a_tolerance number of additional-non-matching characters (like wildcards) to allow in B that are not in A, e.g. "tori" differs from "torii" by b_not_in_a_tolerance of 1, and from "victoria" by b_not_in_a_tolerance of 4. Matching those names therefore requires at least those values.
#' 
#' @examples \code{
#' schr_match_combinations(match_this = schr_table_chars("eau leaf"), 
#'                         against_these = schr_table_chars(c("eauleaf", "leafy", "eu leaf", "fael eau")))
#'                         }
schr_match_combinations <- function(match_this    = schr_table_chars(...), 
                                    against_these = schr_table_chars(c(...)), 
                                    a_not_in_b_tolerance = 1, 
                                    b_not_in_a_tolerance = 1) {
  
  
  # # do not allow tolerance to be larger than the length of the matching since that will match all from set B to A
  # if(match_this$char_count <= a_not_in_b_tolerance) {
  #   message(glue::glue("Reduce a_not_in_b_tolerance to be {match_this$char_count - 1} characters or fewer. 
  #                    This current setting does not require the matching set to include any characters from your input (i.e. match_this)."))
  #   return(NULL)
  # }
  
  # # return anything that exists in A, but not in B
  # # if you have more unmatchable character names in A than your a_not_in_b_tolerance 
  # # allows, you cannot find a match and therefore return NULL
  a_not_in_b_names <- setdiff(names(match_this), names(against_these))
  # if(a_not_in_b_tolerance < length(a_not_in_b_names)) {
  #   message("No match possible. You may want to increase the a_not_in_b_tolerance parameter")
  #   return(NULL)
  # }
  
  
  # get remaining sets
  b_not_in_a_names     <- setdiff(names(against_these),names(match_this))
  a_b_intersect_names  <- intersect(names(against_these),names(match_this)) %>% setdiff('char_count')
  
  # remove impossible matches due to total char lengths
  filtered_df <- 
    against_these %>% 
    dplyr::mutate(locn = row_number(), .before = 1) %>% 
    dplyr::filter(char_count >= (match_this$char_count - a_not_in_b_tolerance),
                  char_count <= (match_this$char_count - length(a_not_in_b_names) + b_not_in_a_tolerance)
    )
  
  # remove impossible matches due to summed set char lengths
  filtered_df <- filtered_df %>% 
    dplyr::filter(rowSums(filtered_df[b_not_in_a_names]) <= b_not_in_a_tolerance)
  
  # filter(across(.cols = a_b_intersect_names, .fns = ~(. >= match_this[a_b_intersect_names]))) 
  if(nrow(filtered_df) == 0) return(filtered_df)
  
  # save remaining row location id's for later
  filtered_locns <- filtered_df %>% select(locn)
  
  
  # create a tibble of repeated "match_this" rows to mimics the rowcount of filtered_df
  empty_df <- filtered_df %>% dplyr::bind_rows(match_this) %>% slice(0)
  
  filtered_df <- bind_rows(empty_df,filtered_df) %>% 
    dplyr::select(-locn,-char_count) %>% 
    dplyr::mutate(across(.fns = ~as.integer(tidyr::replace_na(.,0))))
  match_this <- dplyr::bind_rows(empty_df,match_this) %>% 
    select(-locn,-char_count) %>% 
    dplyr::mutate(across(.fns = ~as.integer(tidyr::replace_na(.,0)))) %>% 
    dplyr::bind_rows(replicate(nrow(filtered_df)-1, ., simplify = FALSE))
  
  
  # positive means a not in b (match_this not in against_this)
  dist_calcs <- (match_this - filtered_df) %>% tibble::as_tibble()
  dist_calcs_pos <- 
    dist_calcs %>% 
    dplyr::mutate(dplyr::across(.fns = ~if_else(.>0, ., as.integer(0)))) %>% 
    rowSums()
  dist_calcs_neg <- 
    dist_calcs %>% 
    dplyr::mutate(dplyr::across(.fns = ~if_else(.<0, ., as.integer(0)))) %>% 
    rowSums()
  
  # put index and results back together
  a_b_distances <- tibble::tibble(filtered_locns, a_not_in_b = dist_calcs_pos, b_not_in_a = abs(dist_calcs_neg))
  
  a_b_distances %>% 
    filter(a_not_in_b <= a_not_in_b_tolerance,
           b_not_in_a <= b_not_in_a_tolerance) %>% 
    arrange(sum(a_not_in_b, b_not_in_a), a_not_in_b, b_not_in_a)
  
}

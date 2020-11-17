
# main functions ----------------------------------------------------------

multi_get_friends <- function(u, token_list) {
  
  user_info <- lookup_users(u, token = sample(token_list, 1)[[1]])
  fc <- user_info$friends_count
  message("<<", user_info$screen_name, ">> is following ", scales::comma(fc), " users ")
  
  if (user_info$protected) stop(call. = FALSE, "The account is protected, we can't get followers.")
  
  num_queries <- ceiling(fc / 5000)
  rl <- rate_limit(token_list, "get_friends")
  rl <- validate_rate_limit(rl, "get_friends", token_list)
  
  index <- get_available_token_index(rl)
  
  # Case 0: User doesn't have any friends
  
  if (fc == 0) return(tibble(from = character(0), to = character(0))) 
  
  # Case 1: Less than 5,000 friends, only call is needed
  
  if (fc <= 5e3) {
    
    friends <- get_friends(u, token = token_list[[index]])
    
  } else {
    
    # Case 2: Many calls are needed
    
    output <- vector("list", length = num_queries)
    output[[1]] <- get_friends(u, token = token_list[[index]])
    
    for (i in 2:length(output)) {
      
      rl <- validate_rate_limit(rl, "get_friends", token_list)
      index <- get_available_token_index(rl)
      output[[i]] <- get_friends(u, token = token_list[[index]], page = next_cursor(output[[i - 1]]))
      
    }
    
    friends <- bind_rows(output) %>% 
      distinct()
    
  }
  
  attr(friends, "next_cursor") <- NULL
  
  friends %>% 
    rename(from = user, to = user_id) %>% 
    mutate(from = user_info$user_id)
  
}

multi_get_timeline <- function(u, n, token_list, home = FALSE) {
  
  message(u)
  rl <- rate_limit(token_list, "get_timeline")
  rl <- validate_rate_limit(rl, "get_timeline", token_list)
  
  index <- get_available_token_index(rl)
  
  # Case 0: User doesn't have any posts
  
  # what to do?
  
  # Should we allow to get all the timeline??? If so, mimic previous function
    
  tl <- get_timeline(u, n = n, home = home, token = token_list[[index]])

  return(tl)
  
}

# multi_lookup_users <- function() {
#   
#   
# }


# helpers -----------------------------------------------------------------

validate_rate_limit <- function(rl, q, token_list) {
  
  if (is_empty(rl)) {
    message("Waiting for rate limiting update")
    Sys.sleep(60)
    rl <- rate_limit(token_list, query = q)
    validate_rate_limit(rl, q, token_list) # recursion!
    
  }
  
  if (all(rl$remaining == 0)) {
    
    message("Waiting for token reset in ", round(min(rl$reset), 1), " minutes")
    Sys.sleep(min(as.numeric(rl$reset_at - Sys.time(), units = "secs")) + 5)
    rl <- rate_limit(token_list, query = q)
    validate_rate_limit(rl, q, token_list) # recursion!
    
  }
  
  rl
  
}

get_available_token_index <- function(rl) {
  
  env <- rlang::caller_env()
  available_token <- rl$remaining > 0
  index <- which(available_token)[[1]]
  env$rl[index, ]$remaining <- rl[index, ]$remaining - 1  # this modifies the rl obj in the parent frame
  return(index)
  
}




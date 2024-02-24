#data extraction functions

remove_ici_drugs_from_string <- function(string_to_clean, ici_drugs){
  if(is.na(string_to_clean)) NA_character_
  else gsub("^\\++|\\+$", "", stringr::str_remove_all(string_to_clean, paste0(ici_drugs, collapse = '|')))
}

paste_non_ici_drug_to_string <- function(string_to_clean, string_to_paste, ici_drugs){

  cleaned_string <- remove_ici_drugs_from_string(string_to_clean, ici_drugs)

  if(is.na(cleaned_string)) return(string_to_paste)
  if(is.na(string_to_paste)) cleaned_string
  if(nchar(cleaned_string) == 0) return(string_to_paste)
  else paste(string_to_paste, cleaned_string, sep = "_")
}

format_entry <- function(string_to_format, patterns_to_remove, add_underscore = FALSE){
  reg_remove <- stringr::str_remove(string_to_format, paste0(patterns_to_remove, collapse = '|') )

  if(add_underscore) tolower(gsub(" ", "_", reg_remove)) #remove white spaces
  else reg_remove
}

if(
  getRversion() %>%
  magrittr::is_weakly_greater_than("2.15.1"))
{
  utils::globalVariables(c("."))
}

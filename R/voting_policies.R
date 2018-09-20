#' Отримайте датафрейм із сукупністю політик до яких підв'язане певне голосування.
#'
#' @param id Вектор, який містить щонайменше один id голосування.
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org.
#'
#' @return Датафрейм із 10 змінних vote_id, vote_name, date, bill_number,policy_id, policy_name, description, provisional, vote, strong.


#'
#' @export
#'

voting_policies <- function (id = 4896, key)

{
   voting <- data.frame()

   for (i in id) {

      request <- httr::GET(url = "https://rada4you.org/",
                           path = paste0("api/v1/divisions//", i, ".json"),
                           query = list(key = key))

      if (httr::status_code(request) != 200) {
         message(paste("Помилка. Код відповіді сервера: "),
                 httr::status_code(request))
      }

      else {

         response <- httr::content(request, as = "text")
         json <- jsonlite::fromJSON(response)

         null <- function(x) {
            if (is.null(x))
               return("NA")
            return(x)

         }

         votes <- data.frame(
                           vote_id = null(json$id),
                           vote_name = null(json$name),
                           date = null(json$date),
                           bill_number = null(paste0("№", json$bills$official_id)),
                           policy_id = null(json$policy_divisions$policy$id),
                           policy_name = null(json$policy_divisions$policy$name),
                           description = null(json$policy_divisions$policy$description),
                           provisional = null(json$policy_divisions$policy$provisional),
                           vote = null(json$policy_divisions$vote),
                           strong = null(json$policy_divisions$strong),

                                                               stringsAsFactors = F)

         voting <- rbind.data.frame(voting, votes)
         voting$date <- as.Date(voting$date)

      }
   }

   return(voting)

}

#' Отримайте короткий опис голосування
#'
#' @param id Вектор, який містить щонайменше один id голосування
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org
#'
#' @return Датафрейм складається з 14 змінних: vote_id, vote_name, date, time, aye_votes, no_votes, possible_turnout, rebellions, edited, summary, bill_id, bill_number, bill_title, bill_url
#'
#' @export

voting_info <- function (id = 4896, key)

{
   voting <- data.frame()

   for (i in id) {
      request <- httr::GET(url = "https://rada4you.org/",
                           path = paste0("api/v1/divisions//", i, ".json"),
                           query = list(key = key))

      if (httr::status_code(request) != 200) {
         stop(message(paste("Помилка. Код відповіді сервера: "),
                      httr::status_code(request)))

      }
      else {

         response <- httr::content(request, as = "text")
         json <- jsonlite::fromJSON(response)

         print(id)

         null <- function(x) {
            if (is.null(x))
               return("NA")
            return(x)

         }
         votes <- data.frame(
                           vote_id = json$id,
                           vote_name = json$name,
                           date = json$date,
                           time = json$clock_time,
                           aye_votes = json$aye_votes,
                           no_votes = json$no_votes,
                           possible_turnout = json$possible_turnout,
                           rebellions = json$rebellions,
                           edited = null(json$edited),
                           summary = null(json$summary),
                           bill_id = null(json$bills$id),
                           bill_number = null(paste0("№", json$bills$official_id)),
                           bill_title = null(json$bills$title),
                           bill_url = null(json$bills$url),

                                                      stringsAsFactors = F)

         voting <- rbind.data.frame(voting, votes)
         voting$date <- as.Date(voting$date)

      }
   }

   return(voting)

}

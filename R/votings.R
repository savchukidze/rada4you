#' Отримайте датафрейм з усіма голосуваннями за обраний період.
#'
#' @param start_date Перший день обраного періоду.
#'
#' @param end_date Останній день обраного періоду.
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org
#'
#' @return Датафрейм, що складається з дев'яти стовбців: vote_id, vote_name, date, time, aye_votes, no_votes, possible_turnout, rebellions, edited
#'
#' @export
#'

votings <- function (start_date, end_date, key)

{

   dates = seq(as.Date(start_date), as.Date(end_date), by = 1)

   all_votes <- data.frame()

   for (i in as.list(dates)) {

      request <- httr::GET(url = "https://rada4you.org/",
                           path = paste0("/api/v1/divisions.json"),
                           query = list(key = key, end_date = i, start_date = i, house = "rada"))

      if (httr::status_code(request) != 200) {
         message(paste("Помилка. Код відповіді сервера: "),
                 httr::status_code(request))
      }

      else {

         response <- httr::content(request, as = "text")
         json <- jsonlite::fromJSON(response)

         print(i)

         votes <- data.frame(
                           vote_id = json$id,
                           vote_name = json$name,
                           date = json$date,
                           time = json$clock_time,
                           aye_votes = json$aye_votes,
                           no_votes = json$no_votes,
                           possible_turnout = json$possible_turnout,
                           rebellions = json$rebellions,
                           edited = json$edited,

                                                   stringsAsFactors = F)

         all_votes <- rbind.data.frame(all_votes, votes)
      }
   }

   return(all_votes)

}

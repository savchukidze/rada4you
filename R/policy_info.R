#' Отримайте короткий опис політики
#'
#' @param id Вектор, який містить щонайменше один id політики
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org
#'
#' @return Датафрейм, що складається з одинадцятьох змінних для кожної політики: policy_id, policy_name, description, provisional, vote_id, vote_name, date, time, aye_votes, no_votes, rebellions.
#'
#' @export

policy_info <- function (id = 1, key)

{
   details <- data.frame()

   for (i in id) {
      request <- httr::GET(url = "https://rada4you.org/", path = paste0("api/v1/policies/", i, ".json"),
                           query = list(key = key))

      if (httr::status_code(request) != 200) {
         stop(message(paste("Помилка. Код відповіді сервера: "),
                      httr::status_code(request)))

      }
      else {

         response <- httr::content(request, as = "text")
         json <- jsonlite::fromJSON(response)

         null <- function(x) {
            if (is.null(x))
               return("NA")
            return(x)
            
            }
         print(json$id)
         
         detail <- data.frame(
                        policy_id = null(json$id),
                        policy_name = null(json$name),
                        description = null(json$description),
                        provisional = null(json$provisional),
                        vote_id = null(json$policy_divisions$division$id),
                        vote_name = null(json$policy_divisions$division$name),
                        date = null(json$policy_divisions$division$date),
                        time = null(json$policy_divisions$division$clock_time),
                        aye_votes = null(json$policy_divisions$division$aye_votes),
                        no_votes = null(json$policy_divisions$division$no_votes),
                        rebellions = null(json$policy_divisions$division$rebellions),

                                                   stringsAsFactors = F)

         details <- rbind.data.frame(details, detail)
         details$date <- as.Date(details$date)

         Sys.sleep(1)
      }
   }

   return(details)

}

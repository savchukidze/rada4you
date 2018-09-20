#' Отримайте перелік всіх доступних політик
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org
#'
#' @return Датафрейм складається з чотирьох змінних: policy_id, policy_name, description, provisional.
#'
#' @export

policies <- function (key)

{
   request <- httr::GET(url = "https://rada4you.org/api/v1/policies.json",
                        query = list(key = key))

   if (httr::status_code(request) != 200) {
      stop(message(paste("Помилка. Код відповіді сервера: "),
                   httr::status_code(request)))

   }
   else {

      response <- httr::content(request, as = "text")
      json <- jsonlite::fromJSON(response)

      names(json) <- c("policy_id", "policy_name", "description", "provisional")
   }

   return(json)

}

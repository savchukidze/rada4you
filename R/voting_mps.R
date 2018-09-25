#' Отримайте датафрейм із голосуванням кожного депутата щодо певного голосування.
#'
#' @param id Вектор, який містить id щонайменше одне id голосування
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org
#'
#' @return Датафрейм складається з десяти стовбців: vote_id, vote_name, bill_id, bill_number, bill_title, date, mp_name, party, electorate, vote
#'
#' @export

voting_mps <- function (id = 4896, key)
   
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
      
      else
         
      {
         response <- httr::content(request, as = "text")
         json <- jsonlite::fromJSON(response)
         
         print(json$id)
         
       #if null
         null <- function(x) {
            if (is.null(x))
               return("NA")
            return(x)
            
         }
         
         votes <- data.frame(
            vote_id = json$id,
            vote_name = json$name,
            bill_id = null(unique(json$bills$id)),
            bill_number = null(unique(paste0("№", json$bills$official_id))),
            bill_title = null(unique(json$bills$title)),
            date = json$date, mp_id = json$votes$member$person$id,
            mp_name = paste(json$votes$member$first_name, json$votes$member$last_name),
            party = json$votes$member$party,
            electorate = json$votes$member$electorate,
            vote = json$votes$vote,
            
                        stringsAsFactors = F)
         
         voting <- rbind.data.frame(voting, votes)
         voting$date <- as.Date(voting$date)
         
      }
      
      library(magrittr)
      library(stringr)
      
      voting$party <- voting$party %>%
         str_replace_all(pattern = "Фракція Політичної партії \"НАРОДНИЙ ФРОНТ\"",
                         replacement = "Народний фронт") %>%
         str_replace_all(pattern = "Фракція ПАРТІЇ \"БЛОК ПЕТРА ПОРОШЕНКА\"",
                         replacement = "Блок Петра Порошенка") %>%
         str_replace_all(pattern = "Фракція політичної партії \"Всеукраїнське об'єднання \"Батьківщина\" у Верховній Раді України",
                         replacement = "ВО \"Батьківщина\"") %>%
         str_replace_all(pattern = "Фракція Радикальної партії Олега Ляшка",
                         replacement = "Радикальна партія Олега Ляшка") %>%
         str_replace_all(pattern = "Група \"Воля народу\"",
                         replacement = "група \"Воля народу\"") %>%
         str_replace_all(pattern = "Фракція Політичної партії \"Об'єднання \"САМОПОМІЧ\"",
                         replacement = "Об’єднання \"Самопоміч\"") %>%
         str_replace_all(pattern = "Група \"Партія \"Відродження\"",
                         replacement = "група \"Відродження\"") %>%
         str_replace_all(pattern = "Фракція Політичної партії \"Опозиційний блок\" у Верховній Раді України восьмого скликання",
                         replacement = "Опозиційний блок")
      
      Sys.sleep(1)
      
      detach(package:magrittr)
      detach(package:stringr)
      
   }
   
   return(voting)
   
}

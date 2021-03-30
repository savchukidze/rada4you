#' Отримайте перелік усіх народних депутатів.
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org
#'
#' @return Датафрейм cкладається з чотирьох змінних: mp_id, mp_name, party, electorate
#'
#' @export

mps <- function (key)

{
   request <- httr::GET(url = "https://rada4you.org/api/v1/people.json",
                        query = list(key = key))

   if (httr::status_code(request) != 200) {
      stop(message(paste("Помилка. Код відповіді сервера: "),
                   httr::status_code(request)))

   }

   else {

      response <- httr::content(request, as = "text")
      json <- jsonlite::fromJSON(response, flatten = TRUE)

      names(json) <- c("id", "member_id", "electorate", "house",
                       "party", "name_first", "name_last")

      people <- data.frame(mp_id = json$id,
                           mp_name = paste(json$name_first, json$name_last),
                           party = json$party,
                           electorate = json$electorate,

                                                stringsAsFactors = F)

      library(magrittr)
      library(stringr)

      people$party <- people$party %>%
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

   #9скликання
   people$party <- people$party %>%
   str_replace_all(pattern = "депутатська група За майбутнє", 
                   replacement = 'За майбутнє') %>% 
   str_replace_all(pattern = "фракція Батьківщина", 
                   replacement = 'Батьківщина') %>% 
   str_replace_all(pattern = "фракція Голос", 
                   replacement = 'Голос') %>% 
   str_replace_all(pattern = "фракція Європейська солідарність", 
                   replacement = 'ЄС') %>% 
   str_replace_all(pattern = "фракція Опозиційна платформа - За життя", 
                   replacement = 'ОПЗЖ') %>% 
   str_replace_all(pattern = "фракція Слуга народу", 
                   replacement = 'Слуга народу') %>%
   str_replace_all(pattern = "депутатська група ДОВІРА", 
                            replacement = 'Довіра')
      
      
      detach(package:magrittr)
      detach(package:stringr)

   }

   return(people)

}



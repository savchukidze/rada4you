#' Отримайте датафрейм із деталізованою інформацією про кожного народного депутата.
#'
#' @param id Вектор, який містить щонайменше один id депутата.
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org
#'
#' @return Датафрейм cкладається з семи змінних для кожного депутата: mp_id, mp_name, party, electorate, rebellions, votes_attended, votes_possible&
#' @export

mps_info <- function (id, key)

{
   depbioall <- data.frame()

   for (i in id) {
      request <- httr::GET(url = "https://rada4you.org/", path = paste0("api/v1/people/", i, ".json"),
                           query = list(key = key))

      if (httr::status_code(request) != 200) {
         stop(message(paste("Помилка. Код відповіді сервера: "),
                      httr::status_code(request)))
      }

      else {

         response <- httr::content(request, as = "text")
         json <- jsonlite::fromJSON(response)

         print(paste(json$latest_member$name$first, json$latest_member$name$last))

         depbio <- data.frame(

                           mp_id = json$id,
                           mp_name = paste(json$latest_member$name$first, json$latest_member$name$last),
                           party = json$latest_member$party,
                           electorate = json$latest_member$electorate,
                           rebellions = json$rebellions,
                           votes_attended = json$votes_attended,
                           votes_possible = json$votes_possible,

                                                stringsAsFactors = F)


         depbioall <- rbind.data.frame(depbioall, depbio)

         library(magrittr)
         library(stringr)

         depbioall$party <- depbioall$party %>%
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
         depbioall$party <- depbioall$party %>%
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
                   replacement = 'Слуга народу')
         Sys.sleep(1)

         detach(package:magrittr)
         detach(package:stringr)
      }

   }

   return(depbioall)

}

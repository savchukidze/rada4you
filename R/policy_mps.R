#' Отримайте датафрейм із рівнем підтримки політики кожним з нардепів.
#'
#' @param id Вектор, який містить містить щонайменше один id політики
#'
#' @param key Ваш персональний ключ API, отриманий на rada4you.org
#'
#' @return Датафрейм із рівнем підтримки політики кожним з нардепів, що складається з десяти змінних для кожної політики:  policy_id, policy_name, description, provisional, mp_id, mp_name, party, electorate, agreement, voted.
#'
#' @export

policy_mps <- function (id = 1, key)

{
   details <- data.frame()

   for (i in id) {

      request <- httr::GET(url = "https://rada4you.org/",
                           path = paste0("api/v1/policies/", i, ".json"),
                           query = list(key = key))

      if (httr::status_code(request) != 200) {
         stop(message(paste("Помилка. Код відповіді сервера: "),
                      httr::status_code(request)))
      }

      else {
         response <- httr::content(request, as = "text")
         json <- jsonlite::fromJSON(response)

         detail <- data.frame(
                           policy_id = json$id,
                           policy_name = json$name,
                           description = json$description,
                           provisional = json$provisional,
                           mp_id = json$people_comparisons$person$id,
                           mp_name = paste(json$people_comparisons$person$latest_member$name$first, json$people_comparisons$person$latest_member$name$last),
                           party = json$people_comparisons$person$latest_member$party,
                           electorate = json$people_comparisons$person$latest_member$electorate,
                           agreement = json$people_comparisons$agreement,
                           voted = json$people_comparisons$voted,

                                                stringsAsFactors = F)

         details <- rbind.data.frame(details, detail)
         details$agreement <- as.numeric(details$agreement)

      library(magrittr)
      library(stringr)

      details$party <- details$party %>%
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

  }


  return(details)

}

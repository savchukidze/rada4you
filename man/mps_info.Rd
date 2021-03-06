% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mps_info.R
\name{mps_info}
\alias{mps_info}
\title{Отримайте датафрейм із деталізованою інформацією про кожного народного депутата.}
\usage{
mps_info(id, key)
}
\arguments{
\item{id}{Вектор, який містить щонайменше один id депутата.}

\item{key}{Ваш персональний ключ API, отриманий на rada4you.org}
}
\value{
Датафрейм cкладається з семи змінних для кожного депутата: mp_id, mp_name, party, electorate, rebellions, votes_attended, votes_possible&
}
\description{
Отримайте датафрейм із деталізованою інформацією про кожного народного депутата.
}

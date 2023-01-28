


packageKey <- Sys.getenv("packageKey")
comments <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                          cluster = "Cluster0",
                          database = "sample_mflix",
                          collection = "comments",
                          apiKey = packageKey)



resp <- findOne(comments, filter = '"filter": { "$and": [{"name": "Jojen Reed"}, {"date": {"$date": "1996-09-17T15:42:05Z"}} ] }') |>
  httr::content()

resp <- find(comments, filter = '"filter": {"date": {"$date": "1996-09-17T15:42:05Z"}}') |>
  httr::content()

resp <- find(comments, filter = '"filter": {"date": {"$gte": { "$date": "2017-09-01T00:00:00.000+00:00" }}}')


andClauses <- mongAnd(c('{"name": "Jojen Reed"}', '{"date": {"$date": "1996-09-17T15:42:05Z"}}'))
orClauses <- mongOr(c('{"name": "Jojen Reed"}', '{"date": {"$gte": { "$date": "2017-09-01T00:00:00.000+00:00" }}}'))

resp <- find(comments, filter = sprintf('"filter": %s', orClauses))


resp <- find(comments, filter = sprintf('"filter": {"_id": %s}', mongOid("5a9427658b0beebeb697bfd6")))


resp <- find(comments, filter = sprintf('"filter": %s', mongEq("name", "Jojen Reed") ))

date <- "2017-09-01"
isoTime <- "T00:00:00.000+00:00"
isoDateTime <- paste0(date, isoTime)
isoDateTime

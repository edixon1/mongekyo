


testKey <- Sys.getenv("mongekyoTestKey")
devtools::load_all()



comments <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                                      cluster = "Cluster0",
                                      database = "sample_mflix",
                                      collection = "comments",
                                      apiKey = testKey)

sampleAdmin <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                             cluster = "Cluster0",
                             database = "sample_admin",
                             collection = "project_data",
                             apiKey = testKey)


weatherCol <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                            cluster = "Cluster0",
                            database = "sample_weatherdata",
                            collection = "data",
                            apiKey = testKey)


irisCollection <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                      cluster = "Cluster0",
                      database = "mongekyoTesting",
                      collection = "iris",
                      apiKey = testKey)


#------------------------Insert-----------------

remIris <- findOne(irisCollection, mongFilter("{}")) |>
  httr::content()

remIris <- remIris$document

myIris <- dplyr::mutate(iris, Source.Id = paste0(dplyr::row_number(), "_iris_LocalId")) |>
  dplyr::select(Source.Id, dplyr::everything())

irisDocs <- asDocument(myIris)

resp <- insertMany(irisCollection, documents = sprintf('"documents": %s', irisDocs))




resp <- insertOne(irisCollection, document = sprintf('"document": %s', irisDocs))

asDocument(myIris)

resp <- findOne(irisCollection, filter = '"filter" : {}')



#----------------------delete--------------------

delFilter <- mongOid("63d931a1fc52907cf72eaa33") |>
  mongEq("_id") |>
  mongFilter()

resp <- deleteOne(irisCollection, delFilter)


delFilter <- mongEq("virginica", "Species") |>
  mongFilter()

resp <- deleteMany(irisCollection, delFilter)

deleteMany(irisCollection, mongFilter("{}"))


#-----------------------Comments-------------------------------


comments <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                          cluster = "Cluster0",
                          database = "sample_mflix",
                          collection = "comments",
                          apiKey = testKey)




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



filter <- mongOid("5a9427658b0beebeb697bfd6") |>
  mongEq("_id") |>
  mongFilter()


update <- mongDateTime(Sys.time()) |>
  mongEq("date") |>
  mongSet() |>
  mongUpdate()


resp <- updateOne(comments, filter = filter,
          update = update
          )



#----------------------Sample Admin------------------------

sampleAdmin <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                            cluster = "Cluster0",
                            database = "sample_admin",
                            collection = "project_data",
                            apiKey = testKey)

filter <- sprintf('"filter": %s', mongLt(5000, "PNTE"))

resp <- find(collection, filter)



update <- mongEq(5500, "PNTE") |>
  mongSet() |>
  mongUpdate()

filter <- mongLte(5500, "PNTE") |>
  mongFilter()

resp <- updateMany(sampleAdmin, filter = filter, update = update)

resp <- updateOne(sampleAdmin, filter = filter, update = update)

resp <- findOne(sampleAdmin, filter = filter)

testJson

resp <- insertOne(sampleAdmin, sprintf('"document" : %s', testJson))



#---------------------- WeatherData----------------


"2017-09-01"
x <- "1997-01-26T00:00:00.000+0000"



timeStr <- as.POSIXlt(Sys.time(), "America/Metlakatla") |>
  strftime("%Y-%m-%dT%H:%M:%S%z")



timeStr


filter <- mongAnd(
  c(
    mongDateTime(as.Date("1997-01-26")) |>
      mongGte() |>
      mongEq("date"),
    mongDateTime(as.Date("1998-01-26")) |>
      mongLte() |>
      mongEq("date")
  )

) |>
  mongFilter()


filter <- mongDateTime(as.Date("1997-01-26")) |>
  mongGte() |>
  mongEq("date") |>
  mongFilter()

resp <- find(comments, filter = filter)



weatherCol <- getCollection(url = "https://data.mongodb-api.com/app/data-nmzks/endpoint/data/beta",
                            cluster = "Cluster0",
                            database = "sample_weatherdata",
                            collection = "data",
                            apiKey = testKey)



filter <- mongEq("_id", mongOid("5553a998e4b02cf715119412"))

filter <- mongEq("")



resp <- find(weatherCol, filter = sprintf('"filter": {"_id": %s}', mongOid("5553a998e4b02cf715119412")))




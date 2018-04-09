# library(datapackage.r)
# package_path = '~/medsl/data-management/datapackage.json'
# pkg = Package.load(package_path)
# pkg$valid
# jsonlite::toJSON(pkg$descriptor, pretty = TRUE)
# pkg$resources[[1]]$headers
# jsonlite::toJSON(
#   pkg$getResource("2016-wy-precinct")$read(keyed = TRUE)
#   ,auto_unbox = FALSE,pretty = TRUE)
#
# resource = Resource.load()
# inferred = 
# resource$descriptor$schema = resource$infer()
# resource$commit()
# resource$valid
# resource$errors
# head(jsonlite::toJSON(inferred, pretty = TRUE))

# devtools::install_github("frictionlessdata/tableschema-r")
# library(tableschema.r)
#
# csv_path = '~/medsl/2016-precinct-data/data/WY/final/2016-wy-precinct.csv'
# package_path = '~/medsl/data-management/datapackage.json'
# s = Schema.load(package_path)
# jsonlite::fromJSON(package_path)
# def = Table.load(csv_path, schema = s)
# def.
# tbl = def$value()

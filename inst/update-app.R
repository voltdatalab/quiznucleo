# deploy app
rsconnect::setAccountInfo(
  name = 'nucleojor',
  token = Sys.getenv('SHINYAPPS_TOKEN'),
  secret = Sys.getenv('SHINYAPPS_SECRET')
)

rsconnect::deployApp('inst/app', appName = "quinznucleo")

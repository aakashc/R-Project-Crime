rm(list=ls())

runApp()

library(shinyapps)
shinyapps::setAccountInfo(name="aakash", token="7DA671E6E39024F976D1506E0CAACDCC", secret="TxIA4r9W+4OwL3jvg4tjjw5U5o+JexpQMKCp1OmG")
deployApp()
# redeploy
shinyapps::configureApp('shiny', size="large")
# delete app
shinyapps::terminateApp('shiny')
shinyapps::applications()

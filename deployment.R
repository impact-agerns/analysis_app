
library(rsconnect)

path <- getwd();path

rsconnect::setAccountInfo(name='impact-initiatives', 
                          token='DBCDABF269BFDA5D0AF76C4D147C739A', 
                          secret='qjbjtckDyiY6HZNgwl/beR3yS8EU7szYozehS5Fd')


rsconnect::deployApp(account='impact-initiatives',paste0(path), appTitle = "ki_analysis_app")

# rsconnect::configureApp(account='impact-initiatives',appName = "ki_analysis_app",size="xxlarge", appDir = paste0(path))
# rsconnect::configureApp(account='impact-initiatives',appName = "reach-ssd-hsm-dashboard",size="xxlarge", appDir = paste0(path))

rsconnect::showLogs(paste0(path))

#remove an app
# rsconnect::appDelete(account = 'impact-initiatives', appName = "geuki_analysis_app")

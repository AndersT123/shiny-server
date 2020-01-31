# shiny-server
Every directory in this repo is a shiny-app.

All apps in master are supposed to be runable using shiny-server, use branches for development of new features.

The structure is based of [this how-to guide](https://deanattali.com/2015/05/09/setup-rstudio-shiny-server-digital-ocean/).

This repository is serving shiny-apps located on a server. It is the shiny-server directory in the /srv/shiny-server/ file on the machine running the shiny-server. The intention is that while on the machine that are serving the app all that is needed to put a new app in production is a `git pull`. Be aware that if `sudo git pull` is needed the ownership of the directories on the server may be change to root which will cause read/write permission issues for the shiny app, as they are run as the user shiny.

If `sudo git pull` is needed remember to check the ownership of the pulled directory. As references to change them back:
    ## make anders and shiny owner of shinyserver dir
    sudo groupadd shiny-apps
    sudo usermod -aG shiny-apps anders
    sudo usermod -aG shiny-apps shiny
    cd /srv/shiny-server
    sudo chown -R anders:shiny-apps .
    sudo chmod g+w .
    sudo chmod g+s .


* emtest: Testing the app that will replace the dropbox/excel based app.
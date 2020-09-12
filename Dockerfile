# Using Rocker/shiny-verse as base

FROM rocker/shiny-verse:4.0.0

# Update the image

RUN apt-get update 

# RStudio authentication                            
CMD ["/bin/bash"] 

# Install required packages
RUN Rscript -e "install.packages(c('lavaan', 'shinydashboard', 'shinydashboardPlus',\
'shinyalert', 'shinyWidgets', 'shinyjs', 'sROC', 'ggthemes', 'plotly', 'DT'))"

# Copy app
RUN mkdir /root/nhl_lv
COPY nhl_lv /root/nhl_lv

COPY nhl_lv/Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

RUN sudo chown -R shiny:shiny /root/nhl_lv

CMD ["R", "-e", "shiny::runApp('/root/nhl_lv', port = 3838, host = '0.0.0.0')"]


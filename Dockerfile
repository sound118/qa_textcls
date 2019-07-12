FROM openanalytics/r-base

MAINTAINER Jason Yang "jason.jian.yang@amway.com"

# This is in accordance to : https://www.digitalocean.com/community/tutorials/how-to-install-java-with-apt-get-on-ubuntu-16-04
RUN apt-get update && \
	apt-get install -y openjdk-8-jdk && \
	apt-get install -y ant && \
	apt-get clean && \
	rm -rf /var/lib/apt/lists/* && \
	rm -rf /var/cache/oracle-jdk8-installer;
	
# Fix certificate issues, found as of 
# https://bugs.launchpad.net/ubuntu/+source/ca-certificates-java/+bug/983302
RUN apt-get update && \
	apt-get install -y ca-certificates-java && \
	apt-get clean && \
	update-ca-certificates -f && \
	rm -rf /var/lib/apt/lists/* && \
	rm -rf /var/cache/oracle-jdk8-installer;

# Setup JAVA_HOME, this is useful for docker commandline
ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/

RUN R CMD javareconf

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 

# system library dependency for Chinese text classification app
RUN apt-get update && apt-get install -y \
    libxml2-dev 

# basic shiny functionality
RUN R -e "install.packages(c('shiny','rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of Chinese text classification app
RUN R -e "install.packages(c('rJava','xlsx','readxl','readr','jiebaR','stringr','fastrtext') , repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/voc
COPY voc /root/voc

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/voc')"]

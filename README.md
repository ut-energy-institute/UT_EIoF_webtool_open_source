# UT EIoF webtool

This repo holds the code for the UT-Austin Energy Institutes Energy Infrastructure of the Future web tool. 

This repo holds one version of the code in the the 'rplumber-app' directory.



# NOTES DECEMBER 2020 on issues with creating new Docker images 

++++
TWO: plumber version updates … 
++++
It seems the latest version of plumber.r package moved to 1.0.0 on Sept 14, 2020.  I’m not sure if this coincides with our problems (when was the last images you made that worked?):
https://cran.r-project.org/web/packages/plumber/index.html.  There are images that we created after 9/14/20 that worked, but seemingly only until 9/22/20 … so maybe there was some lag before the new plumber package was made available via CRAN for automatic downloading. 

Before, there was version 0.4.6 that I think started working as of June 5, 2018 (see https://cran.r-project.org/ and search for the “plumber” package of archived packages and you get a heading on the webpage of this: “Index of /src/contrib/Archive/plumber” that shows the date of the package.


IMPORTANT NOTE: I have plumber 0.4.6 on my computer (maybe you do too).


++++
THREE:  MAYBE we can just change the directory listed in the docker file … 
++++

Maybe we just need to change the following line:
CMD ["/usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R"]

To this line:
CMD ["/usr/local/lib/R/site-library/plumber/plumber/04-mean-sum/plumber.R"]


… Because a lot of stuff I see on the web discussions now is listing this following directory (perhaps it stays with “04-mean-sum” or moves to “03-mean-sum” or similar):
/usr/local/lib/R/site-library/plumber/plumber/04-mean-sum/plumber.R
Maybe plumber v1.0.0 changed this default directory name.  For instance, this site (https://www.rplumber.io/articles/hosting.html) notes of this directory:
“/usr/local/lib/R/site-library/plumber/plumber/03-mean-sum/plumber.R is the path inside of the Docker container to the Plumber file you want to host. You’ll note that you do not need plumber installed on your host machine for this to work, nor does the path /usr/local/... need to exist on your host machine. This references the path inside of the docker container where the R file you want to plumb() can be found. This mean-sum path is the default path that the image uses if you don’t specify one yourself.”


!!!!!! HOWEVER IF THIS WORKED … then we are still at the mercy of future updates to the plumber package, and we’d still likely need to find a way to install a fixed version of rplumber. !!!!!


++++
FOUR:  setting a fixed plumber version to install in the Dockerfile
++++

See here (https://github.com/rstudio/plumber/blob/master/Dockerfile) that a “latest” version of how to call plumber in a Dockerfile has some different syntax and maybe this is causing us our issue (but this is referencing “rstudio” and not “R” so we need to look into this

==============
ARG PLUMBER_REF=master
RUN Rscript -e "remotes::install_github('rstudio/plumber@${PLUMBER_REF}')"

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(rev(commandArgs())[1]); pr$run(host='0.0.0.0', port=8000, swagger=TRUE)"]

==============


This discussion (https://community.rstudio.com/t/latest-rstudio-plumber-docker-container-outdated/86785/4) shows how you can hard-code the version of plumber to install (from downloading), in this case Docker 1.0.0:
==============
you can use this Docker file to build a docker image with plumber 1.0.0 version.
Replace

ARG PLUMBER_REF=master

by

ARG PLUMBER_REF=v1.0.0
==============

So, I suppose we COULD FIRST try to do the following from our Docker file … 

CURRENTLY WE HAVE:
=============
RUN install2.r plumber

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000)"]
CMD ["/usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R"]
=============

MAYBE WE COULD CHANGE TO INSTALL VERSION 0.4.6 (I don’t know if we still need the line with the command “RUN install2.r plumber”):
=============
ARG PLUMBER_REF=V0.4.6
RUN Rscript -e "remotes::install_github('rstudio/plumber@${PLUMBER_REF}')"

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000)"]
CMD ["/usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R"]
=============



++++
FIVE:  Install plumber from source file (tar.gz) ???
++++
I think this site has what we need to do: https://chtc.cs.wisc.edu/docker-build.shtml .   For a Dockerfile, it discusses installing a package from source file, then using install2.r like we already have in our Dockerfile.  You store the source code in the same directory as the Dockerfile.

From the example for installing the “JAGS” package, from source file “JAGS-4.3.0.tar.gz”, I’ve modified it to think about plumber:
================
# COPY the plumber source code into the image under /tmp
COPY plumber_0.4.6.tar.gz /tmp

# RUN a series of commands to unpack the plumber source, compile it, and install it
RUN cd /tmp \
 && tar -xzf plumber_0.4.6.tar.gz \
 && cd plumber_0.4.6 \
 && ./configure \
 && make \
 && make install

# install the R package plumber
RUN install2.r --error plumber
================



!!!! PREVIOUS NOTES #1: !!!!
This site discusses using the “install2.r” to install a package (perhaps plumber) from its source file:
https://rdrr.io/cran/littler/src/inst/examples/install2.r 

It seems that we could maybe have the source file package for the “plumber” package in the default directory of our local computer (or can still put into “R_packages_static/” directory and install it directly using the install2.r command like this:
install2.r \".\"                                  # install package in current directory

so use
install2.r \“.\plumber_0.4.6.tar.gz”
or
install2.r \“plumber_0.4.6.tar.gz”

OR we can copy the way we are already installing R packages that does not use the install2.r command (like “RUN install2.r plumber”):
RUN R -e "install.packages(c('plumber_0.4.6.tar.gz’) , repos = NULL, type='source')"
… Or if we want version 1.0.0 …
RUN R -e "install.packages(c('plumber_1.0.0.tar.gz’) , repos = NULL, type='source')"


!!!! PREVIOUS NOTES #2: !!!!
ANOTEHR PLACE TO LOOK (but I think not as good)
This site discusses using the “install2.r” to install a package (perhaps plumber) from its source file:
https://cran.r-project.org/web/packages/littler/vignettes/littler-examples.html#install2.r-with-cmdline-parsing


The above is referencing “rstudio” so perhaps we have to learn how to change the line below, but MAYBE WE COULD CHANGE TO INSTALL VERSION 0.4.6 (I don’t know if we still need the line with the command “RUN install2.r plumber”) with something like this:
=============
ARG PLUMBER_REF=V0.4.6
RUN Rscript -e "remotes::install_github('rstudio/plumber@${PLUMBER_REF}')"

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000)"]
CMD ["/usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R"]
=============





# Docker instructions (when code was on TACC):

list containers: docker container ls

kill containers: docker kill $(docker ps -q) 95c29847d25d

(Carey to Josh): I played with the "dev" version and could not use the command above. I think to kill a container you just need this:

docker kill container_name

... where "container_name" is the weird assigned names like "loving_torvalds". I guess you can do it by "CONTAINER ID" which is per the above command (I think), but I couldn't get it to work.

## production version:

rebuild containers: docker build --tag=test .

run containers: docker run -p 8000:8000 -it test

## dev version:

rebuild containers: docker build --tag=dev .

run containers: docker run -p 8005:8000 -it dev


# Local call to app:

curl -X GET "http://129.114.6.122:8000/eiof?region_id=2&r_sh_e=.5&ldv_e=50&ng_percent=0&geothermal_percent=10&nuclear_percent=10&petroleum_percent=0&hydro_percent=10&biomass_percent=10&wind_percent=20&CSP_percent=0&PV_percent=20&coal_percent=10" -H "accept: application/json"

For "test" (confirmed) website, as of March 12, 2020 it includes "r_sh_ng" as an input:

curl -X GET "http://129.114.6.122:8000/eiof?region_id=6&r_sh_e=50&r_sh_ng=50&ldv_e=50&ng_percent=49&geothermal_percent=0&nuclear_percent=0&petroleum_percent=0&hydro_percent=0&biomass_percent=0&wind_percent=0&CSP_percent=0&PV_percent=0&coal_percent=50" -H "accept: application/json"


For "development" website

curl -X GET "http://129.114.6.122:8005/eiof?region_id=2&r_sh_e=.5&ldv_e=50&ng_percent=0&geothermal_percent=10&nuclear_percent=10&petroleum_percent=0&hydro_percent=10&biomass_percent=10&wind_percent=20&CSP_percent=0&PV_percent=20&coal_percent=10" -H "accept: application/json"


Now adding "r_sh_ng" as necessary input:

curl -X GET "http://129.114.6.122:8005/eiof?region_id=2&r_sh_e=50&r_sh_ng=50&ldv_e=50&ng_percent=0&geothermal_percent=10&nuclear_percent=10&petroleum_percent=0&hydro_percent=10&biomass_percent=10&wind_percent=20&CSP_percent=0&PV_percent=20&coal_percent=10" -H "accept: application/json"

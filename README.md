# UT EIoF webtool

This repo holds the code for the UT-Austin Energy Institutes Energy Infrastructure of the Future web tool. 

This repo holds two versions of the code, the 'rplumber-app' which is the production version and 'rplumber-app-dev' which is the development version of the app.






# Docker instructions:

list containers: docker container ls

kill containers: docker kill $(docker ps -q) 95c29847d25d

rebuild containers: docker build --tag=test .

run containers: docker run -p 8000:8000 -it test

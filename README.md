# UT EIoF webtool

This repo holds the code for the UT-Austin Energy Institutes Energy Infrastructure of the Future web tool. 

This repo holds two versions of the code, the 'rplumber-app' which is the production version and 'rplumber-app-dev' which is the development version of the app.






# Docker instructions:

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

For "development" website
curl -X GET "http://129.114.6.122:8005/eiof?region_id=2&r_sh_e=.5&ldv_e=50&ng_percent=0&geothermal_percent=10&nuclear_percent=10&petroleum_percent=0&hydro_percent=10&biomass_percent=10&wind_percent=20&CSP_percent=0&PV_percent=20&coal_percent=10" -H "accept: application/json"

Now adding "r_sh_ng" as necessary input:
curl -X GET "http://129.114.6.122:8005/eiof?region_id=2&r_sh_e=50&r_sh_ng=50&ldv_e=50&ng_percent=0&geothermal_percent=10&nuclear_percent=10&petroleum_percent=0&hydro_percent=10&biomass_percent=10&wind_percent=20&CSP_percent=0&PV_percent=20&coal_percent=10" -H "accept: application/json"

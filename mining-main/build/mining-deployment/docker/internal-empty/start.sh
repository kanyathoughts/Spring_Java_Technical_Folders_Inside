#! /bin/bash
FindLicense () {
	echo "locating License file"

	for file in $(find $innowake_license_activation -name innowake.lic)
	do
		echo License file found at location=$file
		echo "License_location=$file" >> docker/.env
		cp $file docker/
		return 0
	done
	echo License File not found at innowake_license_activation - $innowake_license_activation

	for file in $(find $innowake_license_activation_location -name innowake.lic)
	do
		echo License file found at location=$file
		echo "License_location=$file" >> docker/.env
		cp $file docker/
		return 0
	done
	echo License File not found at innowake_license_activation_location - $innowake_license_activation_location

	if [[ -f "$HOME/innowake.lic" ]]; then
		echo License file found at location=$HOME/innowake.lic
		echo "License_location=$HOME/innowake.lic" >> docker/.env
		cp $file docker/
		return 0;
	fi
	echo License File not found at USER folder - $HOME
	
	if [[ -f "$TMPDIR/innowake.lic" ]]; then
		echo License file found at location=$TMPDIR/innowake.lic
		echo "License_location=$TMPDIR/innowake.lic" >> docker/.env
		cp $file docker/
		return 0;
	fi
	echo License File not found at TEMP folder - $TMPDIR
	echo License File not found at any location
}
FindLicense

obtain_check_ports () {
	echo "checking if ports are available"
	#use find_open_port.sh script to find nearest usable port
	apiserver_port_check=$(./helper/find_open_port.sh $apiserver_port)
	orient_db_studio_port_check=$(./helper/find_open_port.sh $orient_db_studio_port)
	orient_db_connection_port_check=$(./helper/find_open_port.sh $orient_db_connection_port)
	keycloak_port_check=$(./helper/find_open_port.sh $keycloak_port)
}

# Taken from:
#   https://unix.stackexchange.com/questions/443751/run-entire-bash-script-as-root-or-use-sudo-on-the-commands-that-need-it
# Detects if script is not running as root
# This check is required because if you don't run it as root, you won't be able to connect to the Docker daemon
if [ "$UID" != "0" ]; then
   echo "This script needs to be run with root privileges because the Docker daemon requires them. (See https://docs.docker.com/engine/security/)"
   # $0 is how the script was invoked
   if whereis sudo &>/dev/null; then
     echo "Please type the sudo password for the user $USER"
     exec sudo $0 $1
     exit
   else
     echo "Sudo not found. You will need to run this script as root."
     exit
   fi 
fi


echo ""
echo "Making sure that shell files are executable."
echo ""
chmod -R +x .

#obtain ports as variables as defined in docker/.env
. ./helper/obtain_default_ports.sh

#if there is no orientdb container, potential client data in a potential postgres container can't have matching data in an orientdb container
#-> potentially existing postgres container gets removed
available_containers=$(docker ps -a  | grep "dockerized-mining_orientdb_1")
echo $available_containers
if [[ -z "$available_containers" ]]; then
	echo no dockerized-mining_orientdb_1 container exists. Removing potentially existing dockerized-mining_postgres_1 container to avoid non matching client data.
	docker rm dockerized-mining_postgres_1
fi


#check whether images are available. If yes -> Up them. If no or reinitialize flag set -> build images.
images=$(exec docker images --format "{{.Repository}}")
echo "Available images: $images"
echo "$1"
if [[ $1 != "--reinitialize" ]] \
&& [[ $images==*"mining_orientdb"* ]] \
&& [[ $images==*"mining_apiserver"* ]] \
&& [[ $images==*"postgres"* ]] \
&& [[ $images==*"mining_keycloak"* ]]; then
	echo "found required images"
	echo ""
	obtain_check_ports

	missing_port=false

	if [ "$apiserver_port_check" != "$apiserver_port" ]
	then
		echo "port of mining api server is in use. Make sure" $apiserver_port "is available and rerun the script!"
		missing_port=true
	fi
	if [ "$orient_db_studio_port_check" != "$orient_db_studio_port" ]
	then
		echo "port of orientdb studio is in use. Make sure" $orient_db_studio_port "is available and rerun the script!"
		missing_port=true
	fi
	if [ "$orient_db_connection_port_check" != "$orient_db_connection_port" ]
	then
		echo "port of orientdb studio is in use. Make sure" $orient_db_connection_port "is available and rerun the script!"
		missing_port=true
	fi
	if [ "$keycloak_port_check" != "$keycloak_port" ]
	then
		echo "port of keycloak is in use. Make sure" $keycloak_port "is available and rerun the script!"
		missing_port=true
	fi
	if [ $missing_port = true ]
	then 
		echo "Exiting the script because port is not available. Make sure missing port is available and rerun the script!"
		exit 
	fi

	echo " Starting Keycloak, OrientDB and API-Server."
	echo " Execute stop.sh to stop."
	echo ""

	echo "apiserver port:" $apiserver_port
	echo "orientdb studio port:" $orient_db_studio_port
	echo "orientdb connection port:" $orient_db_connection_port
	echo "keycloak port:" $keycloak_port
	echo ""
	echo "Execute \"start.sh --reinitialize\" to force reinitialization (f.e. if you want to reset your orientdb)"
	echo ""	
	sleep 3s
	sudo docker-compose -f docker/docker-compose.yml -p "dockerized-mining" up
else
	echo "Couldn't find required images. Initializing dockerized mining..."
	echo ""
	echo " WARNING:"
	echo " This script is intended to create/recreate all dockerized-mining images."
	echo " It will DELETE ALL DATA in dockerized-mining containers that is not stored in external volumes!"
	echo ""
	echo " If you are running this script for the first time on this computer then it is safe to execute it."
	echo ""

	read -p "Are you sure that you want to run this script? (y|n) " -r

	if [[ ! $REPLY =~ ^[Yy]$ ]]
	then
		exit
	fi

	echo ""
	echo "  Making sure that dockerized-mining is down."
	echo ""
	docker-compose -f docker/docker-compose.yml -p "dockerized-mining" down	

	obtain_check_ports

	#insert ports into .env file
	echo "orient_db_studio_port=$orient_db_studio_port_check" > docker/.env
	echo "orient_db_connection_port=$orient_db_connection_port_check" >> docker/.env
	echo "keycloak_port=$keycloak_port_check" >> docker/.env
	echo "apiserver_port=$apiserver_port_check" >> docker/.env

	echo ""
	echo " Making sure that all images exist and that they are up to date."
	echo ""
	docker build -f docker/apiserver.docker -t mining_apiserver ..
	docker build -f docker/orientdb.docker -t mining_orientdb ..
	docker build -f docker/keycloak.docker -t mining_keycloak ..

	echo ""
	echo "Removing potentially existing postgres container"
	echo ""
	sudo docker rm dockerized-mining_postgres_1

	echo ""
	echo " Starting Keycloak, OrientDB and API-Server."
	echo " Press CTRL-C to stop."
	echo ""
	echo "apiserver port:" $apiserver_port_check
	echo "orientdb studio port:" $orient_db_studio_port_check
	echo "orientdb connection port:" $orient_db_connection_port_check
	echo "keycloak port:" $keycloak_port_check
	sleep 3s
	sudo docker-compose -f docker/docker-compose.yml -p "dockerized-mining" up --force-recreate
	
fi
#!/bin/bash

#reads in the docker/.env file, extracts the ports and sets variables for the ports accordingly

filename='docker/.env'
n=1
while read line; do
	name=$(echo $line| cut -d'=' -f 1)
	port=$(echo $line | tr -dc '0-9')
	if [ "$name" == "apiserver_port" ]
	then
		apiserver_port=$port
	elif [ "$name" == "keycloak_port" ]
	then
		keycloak_port=$port
	elif [ "$name" == "orient_db_connection_port" ]
	then
		orient_db_connection_port=$port
	elif [ "$name" == "orient_db_studio_port" ]
	then
		orient_db_studio_port=$port
	fi
	n=$((n+1))
done < $filename

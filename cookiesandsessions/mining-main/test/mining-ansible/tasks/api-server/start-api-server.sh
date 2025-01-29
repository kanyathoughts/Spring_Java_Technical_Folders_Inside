#!/bin/bash

echo -n "Starting api-server ... "
# Starts the actual api-server by using the explicit provided application-default.yml instead of the one contained in the jar
nohup java -jar ./mining-api-server.jar -Dspring.config.location=file:/api-server/
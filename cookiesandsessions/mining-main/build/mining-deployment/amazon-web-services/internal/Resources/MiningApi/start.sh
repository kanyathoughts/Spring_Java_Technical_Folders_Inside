#!/bin/sh
cd /home/ec2-user/server/
java -Dlogging.config=log4j.xml -jar mining-api-server-dist-21.4.0-alpha-202105290426-2.jar --spring.config.location=application-default.yml
#! /bin/sh

KEYCLOAK_IP=$(ping -c1 keycloak | grep  -Eo '([0-9]{1,3}\.){3}[0-9]{1,3}' | head -1)
echo "KEYCLOAK_IP=$KEYCLOAK_IP"
APISERVER_IP=$(ping -c1 apiserver | grep  -Eo '([0-9]{1,3}\.){3}[0-9]{1,3}' | head -1)
echo "APISERVER_IP=$APISERVER_IP"

sysctl -w net.ipv4.conf.all.route_localnet=1
iptables-legacy -t nat -I OUTPUT -p tcp -d 127.0.0.1 --dport $KEYCLOAK_PORT -j DNAT --to-destination $KEYCLOAK_IP:8080
iptables-legacy -t nat -I POSTROUTING -p tcp -d $KEYCLOAK_IP --dport 8080 -j SNAT --to-source $APISERVER_IP
iptables-legacy --table nat -L

echo "Waiting for OrientDB to start..."
wget --retry-connrefused --tries=30 orientdb:2480

echo "Waiting for Keycloak to start..."
wget --retry-connrefused --tries=30 keycloak:8080

echo "Starting api-server ..."
nohup java --add-opens java.base/java.lang=ALL-UNNAMED --add-opens java.base/java.util=ALL-UNNAMED -jar -Dserver.port=8080 -Djava.awt.headless=true -Dmining.cookieId=DISABLED -Dspring.datasource.url=jdbc:orient:remote:orientdb:2424/mining -Dpostgres.datasource.jdbc-url=jdbc:postgresql://postgres_api_server:5432/mining ./mining-api-server-dist.jar --spring.profiles.active=authorized-access --keycloak.auth-server-url=http://localhost:$KEYCLOAK_PORT

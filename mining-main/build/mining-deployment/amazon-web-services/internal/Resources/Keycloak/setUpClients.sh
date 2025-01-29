echo $1

./keycloak-15.1.1/bin/kcadm.sh config credentials --server http://localhost:8080/auth --realm master --user admin --password Worx2000
./keycloak-15.1.1/bin/kcadm.sh create clients -r mining -s id=backend -s clientId=backend -s directAccessGrantsEnabled=true -s publicClient=true -s redirectUris+="$1/*" -s webOrigins='["*"]' -s adminUrl="$1/" -s baseUrl="$1/" -s enabled=true
./keycloak-15.1.1/bin/kcadm.sh create clients -r mining -s id=web -s clientId=web -s directAccessGrantsEnabled=true -s publicClient=true -s redirectUris+="$1/*" -s webOrigins='["*"]' -s base
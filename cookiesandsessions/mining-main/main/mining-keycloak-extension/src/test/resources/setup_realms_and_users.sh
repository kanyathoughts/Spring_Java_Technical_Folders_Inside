#! /bin/sh

 export PATH=$PATH:$KEYCLOAK_HOME/bin;

if /opt/keycloak/bin/kcadm.sh config credentials --server http://localhost:$KEYCLOAK_PORT --realm master --user $KEYCLOAK_USER --password $KEYCLOAK_PASSWORD
then
	createRealmClient=$(/opt/keycloak/bin/kcadm.sh get clients --realm master -q clientId=realm-creator 2> /dev/null)
	if [ "$createRealmClient" == "[ ]" ];
	then
		echo "Creating master 'realm-creator' client";
		/opt/keycloak/bin/kcadm.sh create clients --realm master \
			-s clientId=realm-creator \
			-s directAccessGrantsEnabled=true \
			-s redirectUris+="*" \
			-s webOrigins='[]' \
			-s publicClient=false \
			-s serviceAccountsEnabled=true \
			-s authorizationServicesEnabled=true \
			-s secret=$CLIENT_SECRET \
			-s enabled=true;
	else
		echo "Master 'realm-creator' client already exists";
	fi
	
	/opt/keycloak/bin/kcadm.sh add-roles --uusername service-account-realm-creator --rolename create-realm --realm master
	
	echo "Create realm-creator client done!";
else
	echo "logging into keycloak failed!"
fi

#! /bin/sh

export PATH=$PATH:/opt/keycloak/bin;

function create_mining_realm() {
    if kcadm.sh get realms/mining &> /dev/null;
    then
        echo "Mining realm already exists";
    else
        echo "Creating mining realm";
        /opt/keycloak/bin/kcadm.sh create realms -s realm=mining -s id=mining -s enabled=true \
            -s loginTheme=innowake-theme \
            -s displayNameHtml=${API_SERVER_URL};
        kcadm.sh update realms/master -s loginTheme=innowake-theme;
    fi

    backendClient=$(kcadm.sh get clients -r mining -q clientId=backend 2> /dev/null)
    if [ "$backendClient" == "[ ]" ]
    then
        echo "Creating mining 'backend' client";
        kcadm.sh create clients -r mining \
            -s clientId=backend \
            -s directAccessGrantsEnabled=true \
            -s publicClient=true \
            -s redirectUris+="${API_SERVER_URL}/*" \
            -s webOrigins='["*"]' \
            -s adminUrl=$API_SERVER_URL \
            -s baseUrl=$API_SERVER_URL \
            -s enabled=true;
    else
        echo "Mining 'backend' client already exists";
    fi

    webClient=$(kcadm.sh get clients -r mining -q clientId=web 2> /dev/null)
    if [ "$webClient" == "[ ]" ]
    then
        echo "Creating mining 'web' client"
        kcadm.sh create clients -r mining \
            -s clientId=web \
            -s directAccessGrantsEnabled=true \
            -s publicClient=true \
            -s redirectUris+="${API_SERVER_URL}/*" \
            -s redirectUris+="http://localhost:4200/*" \
            -s webOrigins='["*"]' \
            -s baseUrl="${API_SERVER_URL}/" \
            -s enabled=true;
    else
        echo "Mining 'web' client already exists"
    fi

    eclipseClient=$(kcadm.sh get clients -r mining -q clientId=eclipse 2> /dev/null)
    if [ "$eclipseClient" == "[ ]" ];
    then
        echo "Creating mining 'eclipse' client";
        kcadm.sh create clients -r mining \
            -s clientId=eclipse \
            -s directAccessGrantsEnabled=true \
            -s publicClient=true \
            -s redirectUris+="urn:ietf:wg:oauth:2.0:oob" \
            -s redirectUris+="http://localhost" \
            -s webOrigins='[]' \
            -s enabled=true;
    else
        echo "Mining 'eclipse' client already exists";
    fi

    adminUser=$(kcadm.sh get users -r mining -q username=$MINING_ADMIN_USER 2> /dev/null)
    if [ "$adminUser" == "[ ]" ];
    then
        echo "Creating mining admin user '$MINING_ADMIN_USER'";
        kcadm.sh create users -r mining -s username=$MINING_ADMIN_USER -s enabled=true;
        kcadm.sh set-password -r mining --username $MINING_ADMIN_USER --new-password $MINING_ADMIN_PASSWORD;
        kcadm.sh create roles -r mining -s name=admin;
        kcadm.sh add-roles --uusername $MINING_ADMIN_USER --rolename admin -r mining;
    else
        echo "Mining admin user '$MINING_ADMIN_USER' already exists";
    fi

    echo "Create mining realm done!";
}

function wait_for_keycloak() {
    echo "Waiting for Keycloak server to start";
    # ping is not installed in the docker image
    # so we try to login in order to figure outh whether the server has started yet
    until kcadm.sh config credentials --server http://localhost:8080 --realm master --user $KEYCLOAK_ADMIN --password $KEYCLOAK_ADMIN_PASSWORD;
    do
        sleep 1;
    done;
    create_mining_realm;
}

wait_for_keycloak &

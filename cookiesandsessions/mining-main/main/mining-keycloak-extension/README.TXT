# How to install the extension

## Deploy module to keycloak:

Run the following command:

.\jboss-cli.bat --command="module add --name=innowake.products.mining.mining-keycloak-extension --resources=fully\qualified\path\mining-keycloak-extension-99.9.99-TRUNK-MINING-SNAPSHOT.jar --dependencies=org.keycloak.keycloak-core,org.keycloak.keycloak-services,org.keycloak.keycloak-model-jpa,org.keycloak.keycloak-server-spi,org.keycloak.keycloak-server-spi-private,javax.ws.rs.api,javax.persistence.api,org.hibernate,org.javassist,org.liquibase"
./jboss-cli.sh --command="module add --name=innowake.products.mining.mining-keycloak-extension --resources=/Users/pamosby/Data/maven-repos/innowake/innowake/products/mining/mining-keycloak-extension/99.9.99-TRUNK-MINING-SNAPSHOT/mining-keycloak-extension-99.9.99-TRUNK-MINING-SNAPSHOT.jar --dependencies=org.keycloak.keycloak-core,org.keycloak.keycloak-services,org.keycloak.keycloak-model-jpa,org.keycloak.keycloak-server-spi,org.keycloak.keycloak-server-spi-private,javax.ws.rs.api,javax.persistence.api,org.hibernate,org.javassist,org.liquibase"


Add the the mining-keycloak-extension provider by changing

        <subsystem xmlns="urn:jboss:domain:keycloak-server:1.1">
            <web-context>auth</web-context>
            <providers>
                <provider>classpath:${jboss.home.dir}/providers/*</provider>
            </providers>
            
to

        <subsystem xmlns="urn:jboss:domain:keycloak-server:1.1">
            <web-context>auth</web-context>
            <providers>
                <provider>classpath:${jboss.home.dir}/providers/*</provider>
                <provider>module:innowake.products.mining.mining-keycloak-extension</provider>
            </providers>
            
in the KEYCLOAK-INSTALLATION-DIRECTORY/standalone/configuration/standalone.xml.


## Remove module (i.e. required for re-deployment of new version):

.\jboss-cli.bat --command="module remove --name=innowake.products.mining.mining-keycloak-extension"
./jboss-cli.sh --command="module remove --name=innowake.products.mining.mining-keycloak-extension"



# How to use the extension

REST calls:
1) Get access token:
	POST http://localhost:8180/auth/realms/mining/protocol/openid-connect/token
	grant_type: password
	username: first
	password: first
	client_id: backend
	
2) Get all Users for Client 1:
	GET http://localhost:8180/auth/realms/mining/admin/clients/1/members
	
3) Get all Users for Project 1 with pagination:
	GET http://localhost:8180/auth/realms/mining/admin/clients/1/projects/1/members?page=0&size=2
	
4) Create new User and add to Project:
	POST http://localhost:8180/auth/realms/mining/admin/clients/1/projects/1/members
	Body Parameter: Member to be created
	
5) Delete Client Roles for Client 2:
	DELETE http://localhost:8180/auth/realms/mining/admin/clients/2
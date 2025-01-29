@Grab('com.jcraft:jsch:0.1.54')

import com.jcraft.jsch.Session

/**
 * Constants for RemoteCommandExec
 */
class RemComExConstants {
    static final int WAIT = 45
}

/**
 * Creates an instance of the Utility class and returns it.
 * @return Utility class object
 */
private static Utility getUtil(){
    return new Utility()
}

/**
 * Connects remotely to the OrientDB instance and properly configures it.
 * @param key Private key used to connect to server
 * @param miningHome Local home directory of mining build
 * @param map Map containing information about AWS environment
 * @throws Exception
 */
static void startOrientDbServer(String key, String miningHome, def map) throws Exception {
    InstanceConnector inst = new InstanceConnector()
    Session session = inst.createSession(map.orientDb.ip, key, inst.getLinuxInstOS())

    inst.runCommand(session, "mv ./orientdb ./orientdb-old", false) /* move old mining build */
    inst.runScpCommand(session, "${miningHome}/mining/orientdb/orientdb-mining-${map.miningVersion}.zip", ".", false)
    /* upload new mining build */
    inst.runCommand(session, "unzip -q orientdb-mining-${map.miningVersion}.zip -d ./orientdb", false)
    inst.runCommand(session, "sed -i 's/2480-2480/8080-8080/g' ./orientdb/config/orientdb-server-config.xml", false)
    inst.runCommand(session, "chmod 744 ./orientdb/bin/server.sh", false)
    inst.runCommand(session, "sudo systemctl start orientDbStart.service", false) /* start service */

    getUtil().snooze("started OrientDB server. waiting ${RemComExConstants.WAIT} seconds...", RemComExConstants.WAIT)

    inst.runCommand(session, "systemctl status orientDbStart.service", false)
    inst.runCommand(session, "cp ./orientdb-old/bin/DemoDez18.export.gz ./orientdb/bin", false)
    /* add demo project to new database */
    inst.runCommand(session, "chmod 744 ./orientdb/bin/console.sh", false)
    inst.runCommand(session, "./orientdb/bin/console.sh \"CONNECT remote:localhost/mining root Worx2000;IMPORT DATABASE ./orientdb/bin/DemoDez18.export.gz\"", true)
}

/**
 * Connects remotely to the Keycloak instance and properly configures it.
 * @param key Private key used to connect to server
 * @param miningHome Local home directory of mining build
 * @param map Map containing information about AWS environment
 * @throws Exception
 */
static void startKeycloakServer(String key, String miningHome, def map) throws Exception {
    InstanceConnector inst = new InstanceConnector()
    Session session = inst.createSession(map.keycloak.ip, key, inst.getLinuxInstOS())

    inst.runCommand(session, "rm -rf ./keycloak-15.1.1/standalone/deployments/mining-keycloak-*", false)
    inst.runScpCommand(session, "${miningHome}/mining/keycloak/mining-keycloak-extension-${map.miningVersion}.jar", "./keycloak-15.1.1/standalone/deployments/", false)
    inst.runScpCommand(session, "${miningHome}/mining/keycloak/mining-keycloak-theme-${map.miningVersion}.jar", "./keycloak-15.1.1/standalone/deployments/", false)
    /* remove old keycloak version and then upload new version */
    inst.runCommand(session, "sudo systemctl start keycloak.service", false)

    getUtil().snooze("started Keycloak server. waiting ${RemComExConstants.WAIT} seconds...", RemComExConstants.WAIT)

    inst.runCommand(session, "systemctl status keycloak.service", true)
}

/**
 * Connects remotely to the Windows api server instance and starts it.
 * @param key Private key used to connect to server
 * @param miningHome Local home directory of mining build
 * @param map Map containing information about AWS environment
 * @param tempPass Password of instance that was generated at launch
 * @return Updated instance password
 * @throws Exception
 */
static String startWindowsApiServer(String key, String miningHome, def map, String tempPass) throws Exception {
    InstanceConnector inst = new InstanceConnector()
    Session session = inst.createSession(map.windowsApi.ip, key, inst.getWindowsInstOS())

    inst.runScpCommand(session, "${miningHome}/mining/server/mining-api-server-dist-${map.miningVersion}.jar", "server", false)
    /* upload mining build */

    String newPass = getUtil().replaceSpecialCharacters(tempPass)
    /* we must remove any special characters so that our batch file can handle the password */
    if (newPass != tempPass) {
        inst.runCommand(session, "net user Administrator '${newPass}'", false)
        /* set new password on instance - only run if password change */
    }
    if (map.keycloak.ip == "") {
        inst.runCommand(session, ".\\server\\install-service.ps1 '${newPass}' 'mining-api-server-dist-${map.miningVersion}.jar' '${map.orientDb.ip}'", false)
    } else {
        inst.runCommand(session, ".\\server\\install-service.ps1 '${newPass}' 'mining-api-server-dist-${map.miningVersion}.jar' '${map.orientDb.ip}' '${map.keycloak.ip}'", false)
    } /* set up mining api service */

    getUtil().snooze("installed Windows Api server. waiting ${RemComExConstants.WAIT} seconds...", RemComExConstants.WAIT)

    inst.runCommand(session, ".\\server\\MiningApi.exe start MiningApi", false) /* start service */

    getUtil().snooze("started Windows Api server. waiting ${RemComExConstants.WAIT} seconds...", RemComExConstants.WAIT)

    inst.runCommand(session, "Get-Process -Name MiningApi", true)
    return newPass /* return password so we can record it */
}

/**
 * Connects remotely to the OrientDB instance to upload the api-server build, which is mounted via EFS.
 * @param key Private key used to connect to server
 * @param miningHome Local home directory of mining build
 * @param prefix Prefix name to create a directory for
 * @param map Map containing information about AWS environment
 * @return true if operation succeeds
 * @throws Exception
 */
static boolean uploadLinuxApiServer(String key, String miningHome, String prefix, def map) throws Exception {
    InstanceConnector inst = new InstanceConnector()
    Session session = inst.createSession(map.orientDb.ip, key, inst.getLinuxInstOS())

    inst.runCommand(session, "mkdir /mnt/efs/server/${prefix}", false) /* create new directory for environment */
    inst.runCommand(session, "unzip -q /mnt/efs/server/mining-config.zip -d /mnt/efs/server/${prefix}/", false)

    inst.runScpCommand(session, "${miningHome}/mining/server/mining-api-server-dist-${map.miningVersion}.jar",
            "/mnt/efs/server/${prefix}/", false) /* upload mining build */
    inst.runCommand(session, "echo 'uploaded jar'", true)
    return true
}

/**
 * Connects remotely to the OrientDB instance to configure the api-server, which is mounted via EFS.
 * @param key Private key used to connect to server
 * @param clusterTag Name of Api server cluster
 * @param Prefix name of environment
 * @param map Map containing information about AWS environment
 * @throws Exception
 */
static void configureLinuxApiServer(String key, String clusterTag, String prefix, def map) throws Exception {
    InstanceConnector inst = new InstanceConnector()
    Session session = inst.createSession(map.orientDb.ip, key, inst.getLinuxInstOS())

    if (map.keycloak.ip == "") { /* configure Spring Boot yamls according to whether keycloak is set */
        inst.runCommand(session, "sed -i 's/tagValue:.*/tagValue: ${clusterTag}/g' /mnt/efs/server/${prefix}/application-default.yml", false)
        inst.runCommand(session, "sed -i 's/jdbc:orient:remote:.*\\/mining/jdbc:orient:remote:${map.orientDb.ip}:2424" +
                "\\/mining/g' /mnt/efs/server/${prefix}/application-default.yml", false)
        inst.runCommand(session, "sed -i 's/java .*/java -Dlogging.config=log4j.xml -jar mining-api-server-dist-${map.miningVersion}.jar " +
                "--spring.config.location=application-default.yml/g' /mnt/efs/server/${prefix}/start.sh", false)
    } else {
        inst.runCommand(session, "sed -i 's/tagValue:.*/tagValue: ${clusterTag}/g' /mnt/efs/server/${prefix}/application-default-keycloak.yml", false)
        inst.runCommand(session, "sed -i 's/jdbc:orient:remote:.*\\/mining/jdbc:orient:remote:${map.orientDb.ip}:2424\\/mining/g' " +
                "/mnt/efs/server/${prefix}/application-default-keycloak.yml", false)
        inst.runCommand(session, "sed -i 's/auth-server-url: http:\\/\\/.*\\/auth\\//auth-server-url: http:\\/\\/${map.keycloak.ip}:8080" +
                "\\/auth\\//g' /mnt/efs/server/${prefix}/application-default-keycloak.yml", false)
        inst.runCommand(session, "sed -i 's/java .*/java -Dlogging.config=log4j.xml -jar mining-api-server-dist-${map.miningVersion}.jar " +
                "--spring.config.location=application-default-keycloak.yml/g' /mnt/efs/server/${prefix}/start.sh", false)
    }
    inst.runCommand(session, "sed -i 's/cd .*/cd \\/home\\/ec2-user\\/server\\/${prefix}\\//g' /mnt/efs/server/${prefix}/start.sh", true)
}

/**
 * Remotely connects to the Eclipse instance and sets up the development environment.
 * @param key Private key used to connect to server
 * @param map Map containing information about AWS environment
 * @throws Exception
 */
static void configureEclipseServer(String key, def map) throws Exception {
    InstanceConnector inst = new InstanceConnector()
    Session session = inst.createSession(map.windowsEclipse.ip, key, inst.getWindowsInstOS())

    File eclipse = new File(getUtil().provideItem("Please provide the path of the windows eclipse zip:", "Found Eclipse zip."
            , getUtil().getManFile(), map.miningVersion, "Eclipse version must be ${map.miningVersion}"))
    /* we save 'eclipse' as a file since we need to do some other manipulation other than an upload */
    String license = getUtil().provideItem("Please provide the path to the innoWake license to be used in the Eclipse workspace:"
            , "Found innoWake license.", getUtil().getManFile(), ".lic", "License must be of format '.lic'")
    String project = getUtil().provideItem("Please provide the path of a project to be tested in the Eclipse workspace:", "Found project.", getUtil().getOptFile(), "", "")
    /* prompt the user for needed components */

    inst.runScpCommand(session, eclipse.getAbsolutePath(), ".", false)
    inst.runCommand(session, ".\\server\\unzip.ps1 \"C:\\Users\\Administrator\\${eclipse.getName()}\" \"C:\\Users\\Administrator\\.\"", false)
    inst.runCommand(session, ".\\server\\set-shortcut.ps1 \"C:\\Users\\Administrator\\eclipse\\eclipse.exe\" \".\\Desktop\\eclipse.lnk\"", false)
    /* upload eclipse zip, unzip it, and then set a desktop shortcut for the IDE */

    if (project != "") {
        inst.runScpCommand(session, project, "C:\\Users\\Administrator\\projects\\", false)
        inst.runCommand(session, ".\\server\\unzip.ps1 \"C:\\Users\\Administrator\\projects\\$project\" \"C:\\Users\\Administrator\\projects\\\"", false)
        /* upload project (if given) and unzip it */
    }

    inst.runScpCommand(session, license, ".", true) /* upload innoWake license */
}

/**
 * Adds the proper clients to the keycloak server.
 * @param key Private key used to connect to server
 * @param map Map containing information about AWS environment
 * @throws Exception
 */
static void configureKeycloakClients(String key, def map) throws Exception {
    InstanceConnector inst = new InstanceConnector()
    Session session = inst.createSession(map.keycloak.ip, key, inst.getLinuxInstOS())
    String serverUrl

    if (map.windowsApi.ip != "") {
        serverUrl = "http://${map.windowsApi.ip}:8080"
    } else {
        String url = map.loadBalancer.url
        serverUrl = "${url.toLowerCase()}" /* we convert to lowercase or else we will receive invalid redirect uri errors for keycloak */
    } /* based on what api-server was provisioned, we set the url accordingly */

    inst.runCommand(session, "bash setupClients.sh '$serverUrl'", true)
}

/**
 * Connects remotely to an api server instance and starts it.
 * @param instanceIp Ip of api server to connect to
 * @param Prefix name of environment
 * @throws Exception
 */
static void startLinuxApiServer(String key, String prefix, def ip) throws Exception {
    InstanceConnector inst = new InstanceConnector()
    Session session = inst.createSession(ip, key, inst.getLinuxInstOS())

    inst.runCommand(session, "sudo sed -i 's/ExecStart=.*/ExecStart=\\/home\\/ec2-user\\/server\\/${prefix}\\/start.sh/g' /etc/systemd/system/miningApiStart.service", false)
    /* update service to point to environment's start.sh file */

    inst.runCommand(session, "chmod 744 ./server/${prefix}/start.sh", false)
    inst.runCommand(session, "sudo systemctl start miningApiStart.service", false)

    getUtil().snooze("Started Api server on instance: $ip - waiting ${RemComExConstants.WAIT} seconds...", RemComExConstants.WAIT)

    inst.runCommand(session, "sudo systemctl status miningApiStart.service", true)
    /* start api server on each instance */
}

/**
 * Remotely connects to the orientDb server to remove files stores in the EFS mount.
 * @param Prefix name of environment which was used to create a directory
 * @throws Exception
 */
static boolean cleanUpEfs(String key, String prefix, def map) throws Exception {
    InstanceConnector inst = new InstanceConnector()
    Session session = inst.createSession(map.orientDb.ip, key, inst.getLinuxInstOS())
    inst.runCommand(session, "rm -r /mnt/efs/server/${prefix}/", true)
    return false
    /* we return false since the EFS is no longer configured and we need to record this action */
}

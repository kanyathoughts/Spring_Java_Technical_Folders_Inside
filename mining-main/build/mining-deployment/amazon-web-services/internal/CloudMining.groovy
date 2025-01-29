@Grab('software.amazon.awssdk:ec2:2.16.57')
@Grab('software.amazon.awssdk:auth:2.16.57')
@Grab('software.amazon.awssdk:sts:2.16.57')
@Grab('software.amazon.awssdk:autoscaling:2.16.57')
@Grab('software.amazon.awssdk:elasticloadbalancingv2:2.16.57')
@Grab('org.apache.commons:commons-lang3:3.11')
@Grab('info.picocli:picocli-groovy:4.2.0')
@Grab('com.jcraft:jsch:0.1.54')
@Grab('org.slf4j:slf4j-log4j12:1.7.30')
@GrabConfig(systemClassLoader = true)

import software.amazon.awssdk.services.ec2.Ec2Client
import software.amazon.awssdk.services.ec2.model.*
import software.amazon.awssdk.services.sts.StsClient
import software.amazon.awssdk.services.sts.model.GetCallerIdentityResponse
import software.amazon.awssdk.services.autoscaling.AutoScalingClient
import software.amazon.awssdk.services.autoscaling.model.*
import software.amazon.awssdk.services.elasticloadbalancingv2.ElasticLoadBalancingV2Client
import software.amazon.awssdk.services.elasticloadbalancingv2.model.*
import software.amazon.awssdk.auth.credentials.ProfileCredentialsProvider
import software.amazon.awssdk.core.client.builder.*

import groovy.cli.picocli.*
import org.apache.commons.lang3.RandomStringUtils

/**
 * Main method for CloudMining. Initializes a ScriptCli class which handles rest of script logic.
 * @param args String[] passed in during runtime
 */
static void main(args) {
    ScriptCli commandLineInterface = new ScriptCli()
    commandLineInterface.parse(args)
}

/**
 * ScriptCli creates a custom cli builder that is used to parse the needed parameters to start and stop the AWS environment.
 * Parameters are stored as static variables to be referenced by other classes.
 */
class ScriptCli {
    private CliBuilder cli
    private Utility util = new Utility()

    private static final String START_INFO = "Parameters required for start: \n--private-key\n--mining-version\n--prefix-name"
    private static final String STOP_INFO = "Parameters required for stop:\n--private-key\n--prefix-name"
    private static final String HELP_INFO = "For more information run command -h"

    /**
     * Constructor for ScriptCli. Initializes the CliBuilder object and generates the available parameters.
     */
    ScriptCli() {
        cli = new CliBuilder(name: 'aws-mining-start-stop', stopAtNonOption: false)
        cli.with {
            r(type: boolean, longOpt: 'run', 'Start the AWS environment')
            s(type: boolean, longOpt: 'stop', 'Stop the AWS environment')
            d(type: boolean, longOpt: 'debug', 'Prevents automatic cleanup from failure when provisioning.')
            p(type: String, longOpt: 'private-key', 'Private key used to access Ec2 instances')
            m(type: String, longOpt: 'mining-home', defaultValue: ".", 'Directory where mining project is located')
            v(type: String, longOpt: 'mining-version', 'Mining version')
            k(type: boolean, longOpt: 'with-keycloak', 'Should the script spin up a Keycloak server')
            c(type: int, longOpt: 'cluster-size', 'How many Api servers to provision. Range (1-4). Set to 1 if not specified')
            n(type: String, longOpt: 'prefix-name', 'The prefix used to identify provisioned Ec2 instances')
            i(type: boolean, longOpt: 'custom-instances', 'Should custom instance sizes be requested')
            w(type: boolean, longOpt: 'windows-api-server', 'Should a Windows instance be used for the api-server (auto-scaling disabled for windows)')
            e(type: boolean, longOpt: 'eclipse-instance', 'Should a Windows instance be used to run an Eclipse workspace')
            h(type: boolean, longOpt: 'help', required: false, 'Show usage information')
        }
    }

    /**
     * Parses the arguments passed in and determines which actions the script should take.
     * @param args List of parameters to be parsed
     */
    void parse(def args) {
        def options = cli.parse(args)
        options || System.exit(1) /* if an unknown option exists we exit */

        if (options.h) {
            cli.usage()
            println "$START_INFO\n-------\n$STOP_INFO"
            System.exit(0)
        }

        AwsResources.miningHome = options.m /* use default value if not set explicitly */
        AwsResources.withKeycloak = (options.k ? true : false)
        AwsResources.customInstanceTypes = (options.i ? true : false)
        AwsResources.windowsApiServer = (options.w ? true : false)
        AwsResources.eclipseInstance = (options.e ? true : false)
        AwsResources.debugMode = (options.d ? true : false)
        AwsResources.prefixName = (options.n ? options.n : null)
        AwsResources.privateKey = (options.p ? options.p : null)
        AwsResources.resources.miningVersion = (options.v ? options.v : null) /* set parameters if they exist */

        if (options.c == 0 || options.c) {
            if (AwsResources.windowsApiServer) {
                println "error: you cannot specify a cluster size when using a Windows Mining Api server\n$HELP_INFO"
                System.exit(1)
            } else if (options.c < 1 || options.c > 4) {
                println "error: cluster-size must be within range of 1-4\n$HELP_INFO"
                System.exit(1)
            } else {
                AwsResources.clusterSize = options.c
            }
        } else {
            AwsResources.clusterSize = 1 /* set default size */
        }

        if (options.r) {
            if (new File(AwsResources.RESOURCE_FILE_NAME).exists()) {
                println "error: AWS environment is already provisioned. Please clean up your environment before creating a new one.\n$HELP_INFO"
                System.exit(1)
            } else if (AwsResources.privateKey == null && AwsResources.resources.miningVersion == null && AwsResources.prefixName == null) {
                println "error: Missing required parameter for start. $START_INFO\n$HELP_INFO"
                System.exit(1)
            } else {
                AmazonClient aws = new AmazonClient()
                util.printAsciiTitle()
                aws.createAWSEnvironment()
            } /* if required params are set create AWS environment */
        }

        if (options.s) {
            if (!new File(AwsResources.RESOURCE_FILE_NAME).exists()) {
                println "error: No resource file found. Please create an environment before attempting to clean up.\n$HELP_INFO"
                System.exit(1)
            } else if (AwsResources.privateKey == null && AwsResources.prefixName == null) {
                println "error: Missing required parameter for stop. $STOP_INFO\n$HELP_INFO"
                System.exit(1)
            } else {
                AmazonClient aws = new AmazonClient()
                util.printAsciiTitle()
                aws.cleanUpAWSEnvironment(AmazonClient.CleanState.MANUAL)
            }
        }
    }

}

/**
 * AwsResources holds relevant information for the start and stop of the mining environment. It also manages the reading/writing of
 * information that should persist after the script executes.
 */
class AwsResources {
    /* final params that are consistent for every execution */
    static final String SNS = "arn:aws:sns:eu-central-1:068757897087:PD_api-server-autoscaling"
    static final String ORIENTDB_LAUNCH_TEMPLATE = "lt-0a94328de7c218ddb"
    static final String KEYCLOAK_LAUNCH_TEMPLATE = "lt-0b74661d77d845e3c"
    static final String API_SERVER_LAUNCH_TEMPLATE = "lt-02eba189bfa64fb51"
    static final String API_SERVER_WINDOWS_LAUNCH_TEMPLATE = "lt-036dbe4b1479e142e"
    static final String ECLIPSE_LAUNCH_TEMPLATE = "lt-04277e70f196446eb"
    static final String VPC = "vpc-06156d7d217b75c39"
    static final String ROUTE_TABLE = "rtb-0b92e99b46040b59a"
    static final String ROLE_NAME = "Mining-TestingRole"
    static final String CLIENT_PROFILE = "AssumeRole"
    static final String RESOURCE_FILE_NAME = "aws-mining-resources.json"
    static final String ORIENTDB_BASE = "-pd-orientDb"
    static final String KEYCLOAK_BASE = "-pd-keycloak"
    static final String API_SERVER_BASE = "-pd-api-server-autoscaling"
    static final String API_SERVER_CLUSTER_BASE = "-pd-autoscaling-cluster"
    static final String API_SERVER_WINDOWS_BASE = "-pd-api-server-windows"
    static final String ECLIPSE_BASE = "-pd-eclipse-workspace"
    static final String ASG_NAME_BASE = "-PD_Api-Server"
    static final String TARGET_GROUP_BASE = "-PD-Api-Server-Target"
    static final String ALB_NAME_BASE = "-PD-Api-Server-LB"
    static final String SUBNET_BASE = "-PD-MINING-subnet"
    static final String AZ_1 = "eu-central-1a"
    static final String AZ_2 = "eu-central-1b"
    static final String IPV4_CIDR = "10.195.135."
    static final String ALB_SUPPORT_SUBNET_BASE = "PD-ALB-SUPPORT-"

    /* parameters that are gotten from ScriptCli */
    static String privateKey, miningHome, prefixName
    static int clusterSize
    static boolean withKeycloak, customInstanceTypes, windowsApiServer, eclipseInstance, debugMode

    /* resourceMap used to store relevant dynamic information that is used for start and stop */
    static def resources = [
            keycloak      : [
                    ip      : "",
                    id      : "",
                    template: ""],
            orientDb      : [
                    ip           : "",
                    id           : "",
                    template     : "",
                    efsConfigured: false],
            windowsApi    : [
                    ip      : "",
                    id      : "",
                    template: "",
                    password: ""
            ],
            windowsEclipse: [
                    ip      : "",
                    id      : "",
                    template: "",
                    password: ""
            ],
            apiAutoScaling: [
                    servers    : "",
                    template   : "",
                    autoscaling: false,
                    groupName  : ""
            ],
            loadBalancer  : [
                    targetGroupArn: "",
                    listenerArn   : "",
                    albArn        : "",
                    url           : ""
            ],
            subnets       : [
                    "1": "",
                    "2": ""
            ],
            miningVersion : "" /* mining version gotten from ScriptCli */
    ]

    /**
     * Enum used to specify the server associated with a certain action.
     */
    static enum ServerType {
        ORIENTDB, KEYCLOAK, API_SERVER, API_SERVER_WINDOWS, ECLIPSE, NOT_FOUND
    }

    /**
     * Static void method that saves information about a provisioned Ec2 instance to a map.
     * @param server enum object that identifies which server's information is being sent
     * @param ip Instance privateIp address
     * @param id Instance id
     * @param name Instance name
     */
    static void setServerInformation(ServerType server, String ip, String id) {
        if (server == ServerType.ORIENTDB) {
            resources.orientDb.ip = ip
            resources.orientDb.id = id
        } else if (server == ServerType.KEYCLOAK) {
            resources.keycloak.ip = ip
            resources.keycloak.id = id
        } else if (server == ServerType.API_SERVER_WINDOWS) {
            resources.windowsApi.ip = ip
            resources.windowsApi.id = id
        } else if (server == ServerType.API_SERVER) {
            println "$server not compatible for data storage with this method. No information was saved"
        } else if (server == ServerType.ECLIPSE) {
            resources.windowsEclipse.ip = ip
            resources.windowsEclipse.id = id
        } else {
            println "Unable to catalogue server:\nip: $ip\nid: $id\ntype: $server"
        }
    }

    /**
     * Static void method that saves information about a created launch template to a map.
     * @param server enum object that identifies which server's information is being sent.
     * @param launchTemplateVersion Created launch template version
     */
    static void setTemplateInformation(ServerType server, String launchTemplateVersion) {
        if (server == ServerType.ORIENTDB) {
            resources.orientDb.template = launchTemplateVersion
        } else if (server == ServerType.KEYCLOAK) {
            resources.keycloak.template = launchTemplateVersion
        } else if (server == ServerType.API_SERVER) {
            resources.apiAutoScaling.template = launchTemplateVersion
        } else if (server == ServerType.API_SERVER_WINDOWS) {
            resources.windowsApi.template = launchTemplateVersion
        } else if (server == ServerType.ECLIPSE) {
            resources.windowsEclipse.template = launchTemplateVersion
        } else {
            println "Unable to catalogue template:\n$launchTemplateVersion"
        }
    }

}

/**
 * AmazonClient handles the provisioning and clean up of all needed AWS resources.
 */
class AmazonClient {
    private StsClient sts
    private Ec2Client ec2
    private AutoScalingClient asc
    private ElasticLoadBalancingV2Client elbV2Client
    /* client classes are used to send commands to a specific AWS service */

    private LocalCommandExec local = new LocalCommandExec()
    private RemoteCommandExec remote = new RemoteCommandExec()
    private Utility util = new Utility()
    private AwsUtility awsUtil = new AwsUtility()
    /* classes imported from other Groovy scripts */

    static enum CleanState {
        MANUAL, ON_FAILURE
    }

    static enum CidrFound {
        NEVER, TRUE
    }

    /**
     * Constructor for AmazonClient
     */
    AmazonClient() {
        sts = StsClient.builder().credentialsProvider(ProfileCredentialsProvider.create(AwsResources.CLIENT_PROFILE)).build()
        /* for credentials, we specify a profile that references a profile in our AWS CLI config */
        ec2 = Ec2Client.builder().credentialsProvider(ProfileCredentialsProvider.create(AwsResources.CLIENT_PROFILE)).httpClient(awsUtil.trustAllCertificates()).build()
        asc = AutoScalingClient.builder().credentialsProvider(ProfileCredentialsProvider.create(AwsResources.CLIENT_PROFILE)).httpClient(awsUtil.trustAllCertificates()).build()
        elbV2Client = ElasticLoadBalancingV2Client.builder().credentialsProvider(ProfileCredentialsProvider.create(AwsResources.CLIENT_PROFILE)).httpClient(awsUtil.trustAllCertificates()).build()
    }

    /**
     * Calls methods that will create each piece of AWS environment. If any stage fails, the environment will be cleaned up.
     */
    void createAWSEnvironment() {
        println "Creating AWS environment... "
        try {
            checkForCorrectRole(sts)
            setUpSubnets() /* all resources are created in the order they are required */
            util.printStage("STARTING ORIENTDB")
            startEc2Instance(AwsResources.ServerType.ORIENTDB, AwsResources.ORIENTDB_LAUNCH_TEMPLATE)
            remote.startOrientDbServer(AwsResources.privateKey, AwsResources.miningHome, AwsResources.resources)
            if (AwsResources.withKeycloak) {
                util.printStage("STARTING KEYCLOAK")
                startEc2Instance(AwsResources.ServerType.KEYCLOAK, AwsResources.KEYCLOAK_LAUNCH_TEMPLATE)
                remote.startKeycloakServer(AwsResources.privateKey, AwsResources.miningHome, AwsResources.resources)
            }
            if (AwsResources.windowsApiServer) {
                util.printStage("STARTING WINDOWS MINING API")
                startEc2Instance(AwsResources.ServerType.API_SERVER_WINDOWS, AwsResources.API_SERVER_WINDOWS_LAUNCH_TEMPLATE)
                util.snooze("Waiting 4 minutes for instance password to be ready...", 240)
                String tempPassword = local.getWindowsPassword(AwsResources.CLIENT_PROFILE, AwsResources.resources.windowsApi.id, AwsResources.privateKey)
                AwsResources.resources.windowsApi.password = remote.startWindowsApiServer(AwsResources.privateKey, AwsResources.miningHome, AwsResources.resources, tempPassword)
            } else {
                util.printStage("STARTING MINING API AUTO-SCALING GROUP")
                AwsResources.resources.orientDb.efsConfigured = remote.uploadLinuxApiServer(AwsResources.privateKey, AwsResources.miningHome, AwsResources.prefixName, AwsResources.resources)
                createTargetGroup()
                createLoadBalancer()
                createListener()
                remote.configureLinuxApiServer(AwsResources.privateKey, "${AwsResources.prefixName}${AwsResources.API_SERVER_CLUSTER_BASE}", AwsResources.prefixName, AwsResources.resources)
                createApiServerGroup()
                scaleApiServerGroup()
            }
            if (AwsResources.eclipseInstance) {
                util.printStage("PROVISIONING ECLIPSE WORKSPACE")
                startEc2Instance(AwsResources.ServerType.ECLIPSE, AwsResources.ECLIPSE_LAUNCH_TEMPLATE)
                remote.configureEclipseServer(AwsResources.privateKey, AwsResources.resources)
                AwsResources.resources.windowsEclipse.password = local.getWindowsPassword(AwsResources.CLIENT_PROFILE, AwsResources.resources.windowsEclipse.id, AwsResources.privateKey)
            }
            if (AwsResources.withKeycloak) {
                util.printStage("CONFIGURING KEYCLOAK")
                remote.configureKeycloakClients(AwsResources.privateKey, AwsResources.resources)
            }
            util.writeJsonFile(AwsResources.resources, AwsResources.RESOURCE_FILE_NAME)
            /* as a final step we write the Json file to save environment info */
            println "AWS environment successfully created!"
            util.printJson(AwsResources.resources)
        } catch (MismatchedRoleException e) {
            e.printStackTrace()
            /* if this exceptions is caught we don't need to clean up AWS environment because it has not been provisioned */
        } catch (Exception e) {
            println "An exception (${e}) has been caught, moving to clean up AWS environment if it exists"
            println e.getMessage()
            e.printStackTrace()
            cleanUpAWSEnvironment(CleanState.ON_FAILURE)
        }
    }

    /**
     * Handles the decision making regarding how to clean up resources depending on manual or on-failure cleanup.
     * @param state enum used to determine if clean up state is manual or due to failure
     */
    void cleanUpAWSEnvironment(CleanState state) {
        println "Cleaning up AWS environment..."
        if (AwsResources.debugMode) {
            println "Debug mode active. No resources will be deleted. Please manually clean up resources when done."
            util.writeJsonFile(AwsResources.resources, AwsResources.RESOURCE_FILE_NAME)
        } else {
            try {
                if (state == CleanState.MANUAL) {
                    AwsResources.resources = util.readJson(new File(AwsResources.RESOURCE_FILE_NAME))
                    /* read in data from json file so script knows what to terminate */
                    checkForCorrectRole(sts)
                    terminateResources()
                } else {
                    /* in case of failure, role has already been checked */
                    /* data in resourceMap is to be populated before this is called */
                    util.snooze("Due to clean up caused by failure, waiting 45 seconds before attempting cleaning.", 45)
                    terminateResources()
                }
                println "AWS environment successfully terminated!"
                util.deleteFile(AwsResources.RESOURCE_FILE_NAME)
                /* we can safely delete json once cleanup is successful */
            } catch (MismatchedRoleException e) {
                e.printStackTrace()
                /* if this exceptions is caught we don't need to write JSON file because it was not edited */
            } catch (Exception e) {
                util.writeJsonFile(AwsResources.resources, AwsResources.RESOURCE_FILE_NAME)
                e.printStackTrace()
                println "An exception (${e}) has been caught. Failed to clean up AWS environment"
                println e.getMessage()
            }
        }
    }

/**
 * If resources exist in the resource map, they are terminated and then removed from the map.
 * @throws Exception
 */
    private void terminateResources() throws Exception {
        if (AwsResources.resources.orientDb.ip) {
            util.printStage("TERMINATING ORIENTDB")
            if (AwsResources.resources.orientDb.efsConfigured) {
                /* rm command results in failure if file doesn't exist, so we check that is has been configured */
                AwsResources.resources.orientDb.efsConfigured = remote.cleanUpEfs(AwsResources.privateKey, AwsResources.prefixName, AwsResources.resources)
            }
            terminateInstance(AwsResources.resources.orientDb.id)
            AwsResources.setServerInformation(AwsResources.ServerType.ORIENTDB, "", "")
            /* remove instance data from map once it has been terminated */
        }
        if (AwsResources.resources.orientDb.template) {
            println "Deleting OrientDb Launch Template"
            deleteLaunchTemplateVersions(AwsResources.ORIENTDB_LAUNCH_TEMPLATE, AwsResources.resources.orientDb.template)
            AwsResources.setTemplateInformation(AwsResources.ServerType.ORIENTDB, "")
        }
        if (AwsResources.resources.keycloak.ip) {
            util.printStage("TERMINATING KEYCLOAK")
            terminateInstance(AwsResources.resources.keycloak.id)
            AwsResources.setServerInformation(AwsResources.ServerType.KEYCLOAK, "", "")
        }
        if (AwsResources.resources.keycloak.template) {
            println "Deleting Keycloak Launch Template"
            deleteLaunchTemplateVersions(AwsResources.KEYCLOAK_LAUNCH_TEMPLATE, AwsResources.resources.keycloak.template)
            AwsResources.setTemplateInformation(AwsResources.ServerType.KEYCLOAK, "")
        }
        if (AwsResources.resources.windowsApi.ip) {
            util.printStage("TERMINATING WINDOWS MINING API")
            terminateInstance(AwsResources.resources.windowsApi.id)
            AwsResources.setServerInformation(AwsResources.ServerType.API_SERVER_WINDOWS, "", "")
            AwsResources.resources.windowsApi.password = ""
        }
        if (AwsResources.resources.windowsApi.template) {
            println "Deleting Windows Mining Api Launch Template"
            deleteLaunchTemplateVersions(AwsResources.API_SERVER_WINDOWS_LAUNCH_TEMPLATE, AwsResources.resources.windowsApi.template)
            AwsResources.setTemplateInformation(AwsResources.ServerType.API_SERVER_WINDOWS, "")
        }
        if (AwsResources.resources.windowsEclipse.ip) {
            util.printStage("TERMINATING ECLIPSE WORKSPACE")
            terminateInstance(AwsResources.resources.windowsEclipse.id)
            AwsResources.setServerInformation(AwsResources.ServerType.API_SERVER_WINDOWS, "", "")
            AwsResources.resources.windowsEclipse.password = ""
        }
        if (AwsResources.resources.windowsEclipse.template) {
            println "Deleting Eclipse Launch Template"
            deleteLaunchTemplateVersions(AwsResources.ECLIPSE_LAUNCH_TEMPLATE, AwsResources.resources.windowsEclipse.template)
            AwsResources.setTemplateInformation(AwsResources.ServerType.ECLIPSE, "")
        }
        if (AwsResources.resources.apiAutoScaling.autoscaling) {
            util.printStage("SCALING IN MINING API AUTO-SCALING GROUP")
            AwsResources.clusterSize = 0
            scaleApiServerGroup()
            util.snooze("Wait 15 seconds to make sure ASG has updated...", 15)
        }
        if (AwsResources.resources.apiAutoScaling.groupName) {
            deleteAutoScalingGroup()
        }
        if (AwsResources.resources.loadBalancer.listenerArn) {
            deleteListener()
        }
        if (AwsResources.resources.loadBalancer.url) {
            deleteLoadBalancer()
        }
        if (AwsResources.resources.loadBalancer.targetGroupArn) {
            deleteTargetGroup()
        }
        if (AwsResources.resources.apiAutoScaling.template) {
            println "Deleting Mining Api Auto Scaling Launch Template"
            deleteLaunchTemplateVersions(AwsResources.API_SERVER_LAUNCH_TEMPLATE, AwsResources.resources.apiAutoScaling.template)
            AwsResources.setTemplateInformation(AwsResources.ServerType.API_SERVER, "")
        }
        util.snooze("Waiting 1 minute before deleting subnets...", 60)
        deleteSubnets()
    }

    /**************************************
     * ---------STS CLIENT METHODS---------*
     **************************************/

    /**
     * Checks the that profile provided is using the correct role.
     * @param sts Sts client used to make request.
     * @throws MismatchedRoleException
     */
    private static void checkForCorrectRole(sts) throws MismatchedRoleException {
        println "Checking that client roles are properly set"
        GetCallerIdentityResponse response = sts.getCallerIdentity()

        println "The user id is ${response.userId()}"
        println "The ARN value is ${response.arn()}"

        if (!response.arn().contains(AwsResources.ROLE_NAME)) {
            throw new MismatchedRoleException("Profile does not have correct role set.\nShould contain: ${AwsResources.ROLE_NAME}\nIs: ${response.arn()}")
        }
    }

    /*********************************************
     * ---------(VPC) EC2 CLIENT METHODS---------*
     *********************************************/

    /**
     * This method handles control the subnet creation needed for the testing environment.
     * @throws Exception
     */
    private void setUpSubnets() throws Exception {
        AwsResources.resources.subnets."1" = createSubnet("${AwsResources.prefixName.toUpperCase()}${AwsResources.SUBNET_BASE}", AwsResources.AZ_1)
        associateSubnetRouting(AwsResources.resources.subnets."1")
        if (!AwsResources.windowsApiServer) { /* we don't need a second subnet when creating the windows api server */
            AwsResources.resources.subnets."2" = getAlbSubnet()
            associateSubnetRouting(AwsResources.resources.subnets."2")
        }
    }

    /**
     * Uses the Ec2Client to create a subnet. Only creates subnets with a /28 CIDR block.
     * @param name Name of the subnet to be created.
     * @param availabilityZone AZ for the subnet to be located in.
     * @return The id of the subnet.
     * @throws Exception
     */
    private String createSubnet(String name, String availabilityZone) throws Exception {
        Tag tag = Tag.builder().key("Name").value(name).build()
        TagSpecification tags = TagSpecification.builder().resourceType(ResourceType.SUBNET).tags(tag).build()
        CreateSubnetRequest createRequest = CreateSubnetRequest.builder().availabilityZone(availabilityZone)
                .cidrBlock(calculateAvailableCidrs()).vpcId(AwsResources.VPC).tagSpecifications(tags).build()
        String id = ec2.createSubnet(createRequest).subnet().subnetId()

        println "Created $id"
        return id
    }

    /**
     * Associates a given subnet with a route table.
     * @param subnetId Id of the subnet
     * @throws Exception
     */
    private void associateSubnetRouting(String subnetId) throws Exception {
        AssociateRouteTableRequest associateRequest = AssociateRouteTableRequest.builder().routeTableId(AwsResources.ROUTE_TABLE).subnetId(subnetId).build()
        ec2.associateRouteTable(associateRequest)
    }

    /**
     * Searches all of the active subnets to determine which allocation provides an available IP range. Calculation made for a /28 CIDR block.
     * @return The CIDR block determined to be available
     * @throws Exception
     */
    private String calculateAvailableCidrs() throws Exception {
        List<Subnet> subnets = ec2.describeSubnets().subnets()
        List<Range<Integer>> occupiedCidrs = new ArrayList<>()
        for (Subnet subnet : subnets) {
            if (subnet.cidrBlock().contains(AwsResources.IPV4_CIDR)) {
                Integer temp = Integer.valueOf(subnet.cidrBlock().replace(AwsResources.IPV4_CIDR, "").replaceAll("/.*\$", ""))
                occupiedCidrs.add(temp..temp + 16)
            } /* first we find all of the currently occupied ranges */
        }

        if (occupiedCidrs.isEmpty()) {
            return "${AwsResources.IPV4_CIDR}0/28"
        } else {
            println occupiedCidrs
            for (Range<Integer> allocation : occupiedCidrs) {
                Integer tempRight = allocation.getTo() + 16
                Integer tempLeft = allocation.getFrom() - 16
                /* get the two ranges to the left and right of allocation */

                CidrFound leftFound = CidrFound.NEVER
                CidrFound rightFound = CidrFound.NEVER
                /* check if the ranges are available */
                for (Range<Integer> allocation2 : occupiedCidrs) {
                    if (allocation2.contains(tempLeft) && allocation2.contains(tempRight)) {
                        leftFound = CidrFound.TRUE
                        rightFound = CidrFound.TRUE
                    } else if (allocation2.contains(tempLeft)) {
                        leftFound = CidrFound.TRUE
                    } else if (allocation2.contains(tempRight)) {
                        rightFound = CidrFound.TRUE
                    }
                }

                /* if either range is available (prioritizing lower range) we return the CIDR block to use */
                if (leftFound == CidrFound.NEVER && tempLeft >= 0) {
                    println "returning left $tempLeft"
                    return "${AwsResources.IPV4_CIDR}${tempLeft}/28"
                }
                if (rightFound == CidrFound.NEVER && tempRight < 128) {
                    println "returning right $tempRight"
                    return "${AwsResources.IPV4_CIDR}${tempRight}/28"
                }
            }
            throw new CidrOutOfBoundsException("Allocated ip range for ${AwsResources.IPV4_CIDR}0/25 has been exceeded. No available IPs found.")
        }
    }

    /**
     * Searches all of the active subnets to determine if there is an available subnet for our application load balancer. This is done to reduce the amount
     * of subnets created for each subsequent test environment.
     * @return The id of the created or reused subnet.
     * @throws Exception
     */
    private String getAlbSubnet() throws Exception {
        String availableSubnet = ""
        List<Subnet> subnets = ec2.describeSubnets().subnets()
        for (Subnet subnet : subnets) {
            if (subnet.cidrBlock().contains(AwsResources.IPV4_CIDR) && subnet.availabilityZone() == AwsResources.AZ_2 && subnet.availableIpAddressCount() > 9) {
                availableSubnet = subnet.subnetId()
            }
        }
        if (!availableSubnet.equalsIgnoreCase("")) {
            println "We are using the existing subnet ${availableSubnet} for this environment."
            return availableSubnet
        } else {
            return createSubnet("${AwsResources.ALB_SUPPORT_SUBNET_BASE}${RandomStringUtils.randomAlphanumeric(7)}", AwsResources.AZ_2)
        }
    }

    /**
     * Handles the deletion of the provisioned subnets.
     * @throws Exception
     */
    private void deleteSubnets() throws Exception {
        if (AwsResources.resources.subnets."1") {
            println "Deleting subnet with id: ${AwsResources.resources.subnets."1"}"
            ec2.deleteSubnet(DeleteSubnetRequest.builder().subnetId(AwsResources.resources.subnets."1").build())
            AwsResources.resources.subnets."1" = ""
        }
        if (AwsResources.resources.subnets."2") {
            if (isSubnetEmpty()) {
                /* we check to make sure all IPs are available on subnet 2 before deleting, since other environments can use this subnet */
                println "Deleting subnet with id: ${AwsResources.resources.subnets."2"}"
                ec2.deleteSubnet(DeleteSubnetRequest.builder().subnetId(AwsResources.resources.subnets."2").build())
                AwsResources.resources.subnets."2" = ""
            } else {
                println "Subnet with id ${AwsResources.resources.subnets."2"} is still in use by a different environment. Not deleting as part of this cleanup."
                AwsResources.resources.subnets."2" = ""
                /* we still clear the subnetId, since it is now the responsibility of the other environment's script to clean up the subnet */
            }
        }
    }

    /**
     * Checks to see if a subnet has any of its customer facing IP addresses available.
     * @param subnetId Id of subnet to be checked
     * @return True if all IPs are available
     */
    private boolean isSubnetEmpty(String subnetId) {
        List<Subnet> subnets = ec2.describeSubnets().subnets()
        boolean subnetEmpty = false
        for (Subnet subnet : subnets) {
            if (subnet.subnetId() == AwsResources.resources.subnets."2" && subnet.availableIpAddressCount() == 11) {
                subnetEmpty = true
            }
        }
        return subnetEmpty
    }

    /**************************************
     * ---------EC2 CLIENT METHODS---------*
     **************************************/

/**
 * Launches an Ec2 instance given the specified launchTemplate
 * @param server enum used to identify type of instance being created
 * @param launchTemplate Launch template used to create instance
 * @throws Exception
 */
    private void startEc2Instance(AwsResources.ServerType server, String launchTemplate) throws Exception {
        println "Provisioning Ec2 instance using launch template: $launchTemplate"
        def templateSpec = createLaunchTemplateSpecification(server, launchTemplate, false)
        LinkedHashMap<String, String> instanceMap = makeRunInstancesRequest(templateSpec)
        /* save information about created instance */
        AwsResources.setServerInformation(server, instanceMap.ip, instanceMap.id)
        waitForInstanceInitialization(instanceMap.id)
    }

/**
 * Make a request via Ec2 client to create an instance and returns relevant information.
 * @param templateSpec Object containing information about the launch template to be used
 * @return map of instance ip and instance id
 * @throws Exception
 */
    private LinkedHashMap<String, String> makeRunInstancesRequest(def templateSpec) throws Exception {
        RunInstancesRequest request = RunInstancesRequest.builder().launchTemplate(templateSpec).maxCount(1).minCount(1).build()
        RunInstancesResponse response = ec2.runInstances(request) /* make request to create instance */
        String instanceIp = response.instances().get(0).privateIpAddress()
        String instanceId = response.instances().get(0).instanceId()
        return [ip: instanceIp, id: instanceId]
    }

/**
 * Creates the launch template specification that is used to reference a specific launch template upon Ec2 instance creation.
 * @param server Enum identifying the server type associated with the launch template
 * @param launchTemplate The launch template to reference
 * @param forAutoScaling Is the launch template for an autoscaling group
 * @return LaunchTemplateSpecification object
 * @throws Exception
 */
    private def createLaunchTemplateSpecification(AwsResources.ServerType server, String launchTemplate, boolean forAutoScaling) throws Exception {
        String templateVersion = buildCustomLaunchTemplate(server, launchTemplate, forAutoScaling)
        /* create a new launch template version and return the version number */
        AwsResources.setTemplateInformation(server, templateVersion)
        if (forAutoScaling) {
            /* return a different LaunchTemplateSpecification object based on value of forAutoScaling */
            return software.amazon.awssdk.services.autoscaling.model
                    .LaunchTemplateSpecification.builder().launchTemplateId(launchTemplate).version(templateVersion).build()
        } else {
            return software.amazon.awssdk.services.ec2.model
                    .LaunchTemplateSpecification.builder().launchTemplateId(launchTemplate).version(templateVersion).build()
        }
    }

/**
 * Compiles the parameters needed and then creates a new launch template version.
 * @param server Enum to identify server of launch template
 * @param launchTemplate The launch template to create a new version for
 * @param forAutoScaling Is the template for the autoscaling group
 * @return Version number of newly created template
 * @throws Exception
 */
    private String buildCustomLaunchTemplate(AwsResources.ServerType server, String launchTemplate, boolean forAutoScaling) throws Exception {
        String defaultVersion = getDefaultTemplateVersion(launchTemplate) /* get default version of template */
        String defaultInstanceType = getDefaultTemplateInstanceType(launchTemplate, defaultVersion)
        /* get instance type from default version */
        String instanceType = (AwsResources.customInstanceTypes ?
                util.provideItem("Would you like to change the instance type for $server? Default is $defaultInstanceType.", "", util.getOptString(), "", "")
                : defaultInstanceType)
        /* choose new instance type if needed */
        List<LaunchTemplateTagSpecificationRequest> tagList = buildTags(server, forAutoScaling)
        LaunchTemplateInstanceNetworkInterfaceSpecificationRequest networkInterface = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest.builder()
                .groups("sg-0007b243027c9a386").subnetId(AwsResources.resources.subnets."1").deviceIndex(0).build()
        /* get the tags for the launch template */
        RequestLaunchTemplateData templateData = RequestLaunchTemplateData.builder().tagSpecifications(tagList).instanceType(instanceType)
                .networkInterfaces(networkInterface).build()

        CreateLaunchTemplateVersionRequest createRequest = CreateLaunchTemplateVersionRequest.builder().launchTemplateId(launchTemplate)
                .sourceVersion(defaultVersion).launchTemplateData(templateData).build()
        CreateLaunchTemplateVersionResponse createResponse = ec2.createLaunchTemplateVersion(createRequest)
        /* create launch template */
        return String.valueOf(createResponse.launchTemplateVersion().versionNumber())
    }

/**
 * Gets the default version number of the specified launch template.
 * @param launchTemplate The launch template to parses
 * @return Returns a String representing the launch template's default version
 * @throws Exception
 */
    private String getDefaultTemplateVersion(String launchTemplate) throws Exception {
        DescribeLaunchTemplatesRequest describeRequest = DescribeLaunchTemplatesRequest.builder().launchTemplateIds(launchTemplate).build()
        DescribeLaunchTemplatesResponse describeResponse = ec2.describeLaunchTemplates(describeRequest)

        return String.valueOf(describeResponse.launchTemplates().get(0).defaultVersionNumber())
    }

/**
 * Retrieves the instance type set in the default version of the specified template.
 * @param launchTemplate The launch template to access
 * @param defaultVersion The template version to parse
 * @return String representation of the instance type
 * @throws Exception
 */
    private String getDefaultTemplateInstanceType(String launchTemplate, String defaultVersion) throws Exception {
        DescribeLaunchTemplateVersionsRequest describeVersionRequest = DescribeLaunchTemplateVersionsRequest.builder().launchTemplateId(launchTemplate)
                .versions(defaultVersion).build()
        DescribeLaunchTemplateVersionsResponse describeVersionResponse = ec2.describeLaunchTemplateVersions(describeVersionRequest)

        return describeVersionResponse.launchTemplateVersions().get(0).launchTemplateData().instanceTypeAsString()
    }

/**
 * Generate the tags to be used for the new launch template version.
 * @param server Enum identifying the server type associated with the launch template
 * @param forAutoScaling Is the launch template for an autoscaling group
 * @return List of tags
 */
    private List<LaunchTemplateTagSpecificationRequest> buildTags(AwsResources.ServerType server, boolean forAutoScaling) {
        List<LaunchTemplateTagSpecificationRequest> tagList = new ArrayList<>()
        String baseName = (server == AwsResources.ServerType.ORIENTDB ? AwsResources.ORIENTDB_BASE
                : server == AwsResources.ServerType.KEYCLOAK ? AwsResources.KEYCLOAK_BASE
                : server == AwsResources.ServerType.API_SERVER ? AwsResources.API_SERVER_BASE
                : server == AwsResources.ServerType.API_SERVER_WINDOWS ? AwsResources.API_SERVER_WINDOWS_BASE
                : server == AwsResources.ServerType.ECLIPSE ? AwsResources.ECLIPSE_BASE : null)
        Tag name = Tag.builder().key("Name").value("${AwsResources.prefixName}${baseName}").build()
        tagList.add(LaunchTemplateTagSpecificationRequest.builder().resourceType("instance").tags(name).build())
        tagList.add(LaunchTemplateTagSpecificationRequest.builder().resourceType("volume").tags(name).build())
        if (forAutoScaling) { /* if for autoscaling create another tag */
            Tag autoScale = Tag.builder().key("api-server-cluster").value("${AwsResources.prefixName}${AwsResources.API_SERVER_CLUSTER_BASE}").build()
            tagList.add(LaunchTemplateTagSpecificationRequest.builder().resourceType("instance").tags(autoScale).build())
            tagList.add(LaunchTemplateTagSpecificationRequest.builder().resourceType("volume").tags(autoScale).build())
        }
        return tagList
    }

/**************************************
 * ---------ASG CLIENT METHODS---------*
 **************************************/

    /**
     * Creates an API Server group to be used by the environment.
     * @throws Exception
     */
    private void createApiServerGroup() throws Exception {
        def templateSpec =
                createLaunchTemplateSpecification(AwsResources.ServerType.API_SERVER, AwsResources.API_SERVER_LAUNCH_TEMPLATE, true)
        /* create launch template for the ASG */
        String asgName = "${AwsResources.prefixName}${AwsResources.ASG_NAME_BASE}"
        CreateAutoScalingGroupRequest createRequest = CreateAutoScalingGroupRequest.builder().autoScalingGroupName(asgName)
                .availabilityZones(AwsResources.AZ_1).desiredCapacity(0).maxSize(0).minSize(0).launchTemplate(templateSpec).newInstancesProtectedFromScaleIn(false)
                .vpcZoneIdentifier(AwsResources.resources.subnets."1").healthCheckGracePeriod(300).targetGroupARNs(AwsResources.resources.loadBalancer.targetGroupArn).build()
        asc.createAutoScalingGroup(createRequest)
        AwsResources.resources.apiAutoScaling.groupName = asgName /* set name if creation succeeds */
        asc.putNotificationConfiguration(PutNotificationConfigurationRequest.builder().autoScalingGroupName(AwsResources.resources.apiAutoScaling.groupName)
                .notificationTypes("autoscaling:EC2_INSTANCE_LAUNCH", "autoscaling:EC2_INSTANCE_LAUNCH_ERROR", "autoscaling:EC2_INSTANCE_TERMINATE"
                        , "autoscaling:EC2_INSTANCE_TERMINATE_ERROR").topicARN(AwsResources.SNS).build())
        /* link SNS topic to ASG */
    }

/**
 * Delete the specified auto scaling group and wait for confirmation of deletion.
 * @throws Exception
 */
    private void deleteAutoScalingGroup() throws Exception {
        boolean scalingActivity
        while (scalingActivity) {
            DescribeScalingActivitiesResponse response = asc.describeScalingActivities(DescribeScalingActivitiesRequest.builder()
                    .autoScalingGroupName(AwsResources.resources.apiAutoScaling.groupName).build())
            if (response.hasActivities()) {
                /* we check to make sure there are no in progress activities before attempting deletion */
                scalingActivity = false
                for (Activity act : response.activities()) {
                    switch (act.statusCode()) {
                        case ScalingActivityStatusCode.CANCELLED:
                            break
                        case ScalingActivityStatusCode.FAILED:
                            break
                        case ScalingActivityStatusCode.SUCCESSFUL:
                            break
                        default:
                            println "Activity ${act.description()} in progress: ${act.statusCode}"
                            scalingActivity = true
                    }
                }
                if (scalingActivity) {
                    util.snooze("Scaling activities detected for ${AwsResources.resources.apiAutoScaling.groupName}. Waiting 1 minute then polling again...", 60)
                }
            } else {
                scalingActivity = response.hasActivities()
            }
        }
        println("Deleting auto scaling group ${AwsResources.resources.apiAutoScaling.groupName}")
        asc.deleteAutoScalingGroup(DeleteAutoScalingGroupRequest.builder().autoScalingGroupName(AwsResources.resources.apiAutoScaling.groupName).build())
        println("Waiting for delete confirmation...")
        asc.waiter().waitUntilGroupNotExists(DescribeAutoScalingGroupsRequest.builder().autoScalingGroupNames(AwsResources.resources.apiAutoScaling.groupName).build())
        AwsResources.resources.apiAutoScaling.groupName = ""
    }

/**
 * Handles the decision making to scale the auto-scaling group up or down.
 * @throws Exception
 */
    private void scaleApiServerGroup() throws Exception {
        def apiInstances = getListOfAutoScalingInstances() /* get list of current instances in auto-scaling group */
        def initialInstanceCount = apiInstances.size()
        /* first describe the specified ASG to determine initial instance count */

        if (initialInstanceCount != AwsResources.clusterSize) {
            updateASGCapacity()
            apiInstances = getListOfAutoScalingInstances()
            /* update ASG capacity and get updated instance count */

            def apiServerMap = [:]

            if (initialInstanceCount < AwsResources.clusterSize) {
                println "Scaling up"
                apiServerMap = scaleUp(apiServerMap, apiInstances)
            } else if (initialInstanceCount > AwsResources.clusterSize) {
                println "Scaling down"
                apiServerMap = scaleDown(apiServerMap, apiInstances)
            }

            if (!apiServerMap.isEmpty()) {
                /* save contents if instances still exist */
                AwsResources.resources.apiAutoScaling.servers = apiServerMap
            }
        } else if (initialInstanceCount == 0 && AwsResources.clusterSize == 0) {
            println "Autoscaling Group size is already set to zero - no scale in required"
        } else {
            println "Autoscaling Group already at requested cluster size."
            AwsResources.resources.apiAutoScaling.autoscaling = true
            /* set to true since no changes were made, but ASG has capacity > 0 */
        }
    }

/**
 * Make a request to ASG to get count of instances.
 * @return List of currently active instances
 * @throws Exception
 */
    private def getListOfAutoScalingInstances() throws Exception {
        DescribeAutoScalingGroupsRequest describeAsgRequest = DescribeAutoScalingGroupsRequest.builder().autoScalingGroupNames(AwsResources.resources.apiAutoScaling.groupName).build()
        DescribeAutoScalingGroupsResponse asgDescribe = asc.describeAutoScalingGroups(describeAsgRequest)
        return asgDescribe.autoScalingGroups().get(0).instances()
    }

/**
 * Update the ASG capacity
 * @throws Exception
 */
    private def updateASGCapacity() throws Exception {
        UpdateAutoScalingGroupRequest updateRequest
        updateRequest = UpdateAutoScalingGroupRequest.builder().autoScalingGroupName(AwsResources.resources.apiAutoScaling.groupName)
                .desiredCapacity(AwsResources.clusterSize).minSize(AwsResources.clusterSize).maxSize(AwsResources.clusterSize).build()
        asc.updateAutoScalingGroup(updateRequest) /* update capacity of autoscaling group */

        util.snooze("Autoscaling group capacity updated to ${AwsResources.clusterSize}. Waiting for 1 minute for instance update...", 60)
        AwsResources.resources.apiAutoScaling.autoscaling = AwsResources.clusterSize != 0
        /* set to true if clusterSize != 0 */
    }

/**
 * Handles the scale up processes for the auto scaling group.
 * @param apiServerMap Map to hold information about the instances in the ASG
 * @param apiInstances Contains information about the instances in the ASG
 * @return Map
 */
    private def scaleUp(def apiServerMap, def apiInstances) throws Exception {
        apiInstances.each {
            instance ->
                String instanceId = instance.instanceId()
                waitForInstanceInitialization(instanceId)
                DescribeInstancesRequest describeInstRequest = DescribeInstancesRequest.builder().instanceIds(instanceId).build()
                DescribeInstancesResponse instDescribe = ec2.describeInstances(describeInstRequest)
                apiServerMap.put(instanceId, instDescribe.reservations().get(0).instances().get(0).privateIpAddress())
                /* for each instance, add its information to apiServerMap */
        }

        apiServerMap.each {
            id, ip ->
                remote.startLinuxApiServer(AwsResources.privateKey, AwsResources.prefixName, ip) /* start server */
                waitForTargetInService(id) /* wait for server to be in service for target group */
        }
        return apiServerMap
    }

/**
 * Monitors the ASG instances as they scale down.
 * @param apiServerMap Map to hold information about the instances in the ASG
 * @param apiInstances Contains information about the instances in the ASG
 * @return Map
 */
    private def scaleDown(def apiServerMap, def apiInstances) throws Exception {
        apiInstances.each {
            instance ->
                String instanceId = instance.instanceId()
                switch (instance.lifecycleState()) {
                    case "Terminating":
                        waitForTargetDeregister(instanceId)
                        waitForInstanceTermination(instanceId)
                        break
                    case "InService":
                        DescribeInstancesResponse scaleDownDescribe = ec2.describeInstances(DescribeInstancesRequest.builder()
                                .instanceIds(instanceId).build())
                        apiServerMap.put(instanceId, scaleDownDescribe.reservations().get(0).instances().get(0).privateIpAddress())
                        /* if instance designated as in service, add to apiServerMap */
                        break
                    default:
                        println "Unexpected ASG Instance state: ${instance.lifecycleState}"
                        break
                }
        }
        return apiServerMap
    }

    /*****************************************
     * ---------ELBV2 CLIENT METHODS---------*
     *****************************************/

    /**
     * Creates a target group that will perform health checks on port 8080.
     */
    private void createTargetGroup() {
        String targetGroupName = "${AwsResources.prefixName}${AwsResources.TARGET_GROUP_BASE}"
        CreateTargetGroupRequest createRequest = CreateTargetGroupRequest.builder().healthCheckEnabled(true).healthCheckIntervalSeconds(30).healthCheckPath("/")
                .port(8080).healthCheckProtocol(ProtocolEnum.HTTP).healthCheckTimeoutSeconds(5)
                .healthyThresholdCount(5).name(targetGroupName).protocol(ProtocolEnum.HTTP)
                .protocolVersion("HTTP1").unhealthyThresholdCount(2).vpcId(AwsResources.VPC).build()
        CreateTargetGroupResponse response = elbV2Client.createTargetGroup(createRequest)
        AwsResources.resources.loadBalancer.targetGroupArn = response.targetGroups().get(0).targetGroupArn()
    }

    /**
     * Deletes the target group that was created for this environment. 
     */
    private void deleteTargetGroup() {
        println "Deleting target group with ARN: ${AwsResources.resources.loadBalancer.targetGroupArn}"
        elbV2Client.deleteTargetGroup(DeleteTargetGroupRequest.builder().targetGroupArn(AwsResources.resources.loadBalancer.targetGroupArn).build())
        AwsResources.resources.loadBalancer.targetGroupArn = ""
    }

    /**
     * Creates a load balancer.
     */
    private void createLoadBalancer() {
        println "Creating load balancer with name ${AwsResources.prefixName}${AwsResources.ALB_NAME_BASE}"
        CreateLoadBalancerRequest createRequest = CreateLoadBalancerRequest.builder().ipAddressType(IpAddressType.IPV4)
                .name("${AwsResources.prefixName}${AwsResources.ALB_NAME_BASE}").scheme(LoadBalancerSchemeEnum.INTERNAL).securityGroups("sg-0007b243027c9a386")
                .subnets(AwsResources.resources.subnets."1", AwsResources.resources.subnets."2").type(LoadBalancerTypeEnum.APPLICATION).build()
        LoadBalancer alb = elbV2Client.createLoadBalancer(createRequest).loadBalancers().get(0)
        AwsResources.resources.loadBalancer.url = "http://${alb.dnsName()}:8080" /* url will be used for user access and keycloak client configuration */ 
        AwsResources.resources.loadBalancer.albArn = alb.loadBalancerArn()

        println "Waiting until load balancer ${alb.loadBalancerName()} exists..."
        elbV2Client.waiter().waitUntilLoadBalancerExists(software.amazon.awssdk.services.elasticloadbalancingv2.model.DescribeLoadBalancersRequest
                .builder().loadBalancerArns(AwsResources.resources.loadBalancer.albArn).build())
        println "Waiting until load balancer ${alb.loadBalancerName()} is available..."
        elbV2Client.waiter().waitUntilLoadBalancerAvailable(software.amazon.awssdk.services.elasticloadbalancingv2.model.DescribeLoadBalancersRequest
                .builder().loadBalancerArns(AwsResources.resources.loadBalancer.albArn).build())
    }

    private void deleteLoadBalancer() {
        println "Deleting load balancer with arn: ${AwsResources.resources.loadBalancer.albArn}"
        elbV2Client.deleteLoadBalancer(DeleteLoadBalancerRequest.builder().loadBalancerArn(AwsResources.resources.loadBalancer.albArn).build())

        println "Waiting for deletion confirmation..."
        elbV2Client.waiter().waitUntilLoadBalancersDeleted(software.amazon.awssdk.services.elasticloadbalancingv2.model.DescribeLoadBalancersRequest
                .builder().loadBalancerArns(AwsResources.resources.loadBalancer.albArn).build())

        AwsResources.resources.loadBalancer.albArn = ""
        AwsResources.resources.loadBalancer.url = ""
    }

    private void createListener() {
        println "Creating new listener for load balancer with ARN: ${AwsResources.resources.loadBalancer.albArn}"
        Action action = Action.builder().targetGroupArn(AwsResources.resources.loadBalancer.targetGroupArn).type(ActionTypeEnum.FORWARD).build()
        CreateListenerRequest createRequest = CreateListenerRequest.builder().loadBalancerArn(AwsResources.resources.loadBalancer.albArn).port(Integer.valueOf(8080))
                .protocol(ProtocolEnum.HTTP).defaultActions(action).build()
        CreateListenerResponse response = elbV2Client.createListener(createRequest)
        AwsResources.resources.loadBalancer.listenerArn = response.listeners().get(0).listenerArn()
    }

    private void deleteListener() {
        println "Deleting listener with ARN: ${AwsResources.resources.loadBalancer.listenerArn}"
        elbV2Client.deleteListener(DeleteListenerRequest.builder().listenerArn(AwsResources.resources.loadBalancer.listenerArn).build())
        AwsResources.resources.loadBalancer.listenerArn = ""
//        AwsResources.resources.loadBalancer.port = ""
    }

//private int getAvailableListenerPort(List<Listener> listeners) {
//    int highestPort = 0
//    for (Listener listener : listeners) {
//        if (listener.protocol() == ProtocolEnum.HTTP && listener.port() > highestPort) {
//            highestPort = listener.port()
//        }
//    }
//    return highestPort + 1
//}

//private List<Listener> describeBalancerListeners() {
//    return elbV2Client.describeListeners(DescribeListenersRequest.builder().loadBalancerArn(AwsResources.ALB_ARN).build()).listeners()
//}

/**
 * Waits for an instance status of ok for given instance
 * @param ec2 Ec2 client to make requests
 * @param instanceId id to identify instance
 * @throws Exception
 */
    private void waitForInstanceInitialization(String instanceId) throws Exception {
        println "Waiting for status of OK on instance: $instanceId"
        ec2.waiter().waitUntilInstanceStatusOk(DescribeInstanceStatusRequest.builder().instanceIds(instanceId).build())
    }

/**
 * Calls Ec2Client to wait for specified instance to be terminated.
 * @param instanceId Instance to wait for
 * @param ec2 Ec2Client to make requests
 * @throws Exception
 */
    private void waitForInstanceTermination(String instanceId) throws Exception {
        println "Waiting for termination status on instance: $instanceId"
        ec2.waiter().waitUntilInstanceTerminated(DescribeInstancesRequest.builder().instanceIds(instanceId).build())
    }

/**
 * Wait for an Ec2 instance to by in service for target group.
 * @param instanceId Instance to wait for
 * @throws Exception
 */
    private void waitForTargetInService(def instanceId) throws Exception {
        println "Waiting for target to be in service: $instanceId"
        elbV2Client.waiter().waitUntilTargetInService(DescribeTargetHealthRequest.builder()
                .targetGroupArn(AwsResources.resources.loadBalancer.targetGroupArn).targets(TargetDescription.builder().id(instanceId)
                .port(Integer.valueOf(8080)).build()).build())
        /* wait for instance to be in service for the specified target group */
    }

    private void waitForTargetDeregister(def instanceId) throws Exception {
        println "Waiting for target to be deregistered: $instanceId"
        elbV2Client.waiter().waitUntilTargetDeregistered(DescribeTargetHealthRequest.builder()
                .targetGroupArn(AwsResources.resources.loadBalancer.targetGroupArn).targets(TargetDescription.builder().id(instanceId)
                .port(Integer.valueOf(8080)).build()).build())
    }

/**
 * Calls Ec2Client to terminate instance.
 * @param instanceId Instance to be terminated
 * @param ec2 Ec2Client to make requests
 * @throws Exception
 */
    private void terminateInstance(String instanceId) throws Exception {
        println "Terminating instance $instanceId"
        ec2.terminateInstances(TerminateInstancesRequest.builder().instanceIds(instanceId).build())
        waitForInstanceTermination(instanceId)
    }

/**
 * Creates a request to delete the specified launch template version and executes the request.
 * @param launchTemplate Launch template to access
 * @param version Version of launch template to be deleted
 * @throws Exception
 */
    private void deleteLaunchTemplateVersions(String launchTemplate, String version) throws Exception {
        println "Deleting launch template version $version for $launchTemplate"
        DeleteLaunchTemplateVersionsRequest request = DeleteLaunchTemplateVersionsRequest.builder().launchTemplateId(launchTemplate).versions(version).build()
        ec2.deleteLaunchTemplateVersions(request)
    }

}


/**
 * Custom exception to identify when an exception occurs due to an incorrect AWS role being assigned.
 */
class MismatchedRoleException extends Exception {
    MismatchedRoleException(String err) {
        super(err)
    }
}

/**
 * Custom exception to identify when an exception occurs while executing a process via Amazon STS client object.
 */
class StsClientException extends Exception {
    StsClientException(String err) {
        super(err)
    }
}

class CidrOutOfBoundsException extends Exception {
    CidrOutOfBoundsException(String err) {
        super(err)
    }
}
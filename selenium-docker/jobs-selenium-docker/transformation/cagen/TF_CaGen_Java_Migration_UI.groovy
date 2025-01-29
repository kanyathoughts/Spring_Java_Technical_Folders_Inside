@Library('TestUtils') _

/**
 * Runs CAGen GUI migration.
 *
 * @param caGenBuildVersion The caGen/hotfix version to migrate the Gen GUI Application.
 * @param javaVersion The java version the migration will use
 */

nodeTF('Docker-host && Region-US') {
    timestamps {
        def dockerUtils = new DockerUtils()
        def gitUtils = new GitUtils()
        def mxVersionUtils = new MxVersionUtils()
        def miscUtils = new MiscUtils()
        def user = miscUtils.getUserID()
        def group = miscUtils.getGroupID()

        buildName "#${env.BUILD_ID} - ${caGenBuildVersion}"
        buildDescription "javaVersion=${javaVersion}"

        stage('setup') {
            deleteDir()
            docker.image(dockerUtils.pullJenkinsEnvironmentCaGenUiImage(javaVersion)).inside('-u=0 -v ' +
                    '/etc/ssl/certs/pd-nexus.deloitte.com.pem:/etc/ssl/certs/pd-nexus.deloitte.com.pem -v ' +
                    'root-jenkins-m2-repo:/var/m2_repo:rw') {
                withMaven(maven: 'Default', publisherStrategy: 'EXPLICIT') {
                    sh "$MVN_CMD dependency:copy -Dartifact=com.innowake.gen:javagen-dist-spring-boot:${caGenBuildVersion} -DoutputDirectory=."
                    sh "$MVN_CMD dependency:copy -Dartifact=org.jacoco:org.jacoco.agent:0.8.8 -DoutputDirectory=."
                }
                gitUtils.getLicenseFile(mxVersionUtils.getBranchForMxBuildVersion(caGenBuildVersion), pwd())
                sh "unzip -q ./org.jacoco.agent-0.8.8.jar -d ./org.jacoco.agent-0.8.8"
                try {
                    withEnv(["NODE_EXTRA_CA_CERTS=/etc/ssl/certs/pd-nexus.deloitte.com.pem"]) {
                        miscUtils.npmCliLogin()
                        ['NN DELTA LLOYD JRA DEV V5', 'WPDO2815 CONCAT', 'WPDO 2821 DATETEXT', 'WPDO 2827 FIND', 'WPDO 2832 LENGTH',
                         'WPDO 2843 NUMDATE', 'WPDO 2850 TEXTNUM', 'WPDO-2855 TRIM', 'WPDO 2856 UPPER', 'WPDO 2857 VERIFY',
                         'WPDO 2849 SUBSTR', 'WPDO 2851 TIMENUM', 'WPDO 2825 DAYS', 'WPDO 2826 DAYSTIMESTAMP', 'WPDO 2822 DATETIMESTAMP',
                         'WPDO-2836 MINUTE', 'WPDO-2838 MINUTETIMESTAMP', 'WPDO 2933 TEXTTONUMBER', 'WPDO 2835 MINUS',
                         'WPDO-2718 NE DROP DOWN', 'WPDO-2833 MICROSECOND', 'WPDO-2828 HOUR', 'WPDO-2775 TRIMLEADING', 'WPDO 2934 TRIMLEADTRAIL',
                         'WPDO-2829 HOURS', 'WPDO-2920 COLORREFRGB', 'SERVER FLOWS', 'WPDO-2770 NUMBERFORMAT', 'WPDO 3308', 'WPDO 2823 DAY',
                         'WPDO 2839 MONTH', 'WPDO 2845 NUMTIME', 'WPDO-3309 GETFUNCTIONERRORNUMBER', 'WPDO-3227 ENABLE DISABLE CMD',
                         'WPDO-2846 SECOND', 'WPDO 2830 HOURTIMESTAMP', 'WPDO-2858 YEAR', 'WPDO 3272 DAYTIMESTAMP', 'WPDO 2762 DATEFORMAT',
                         'WPDO 2841 MONTHTIMESTAMP', 'WPDO-2848 SECONDTIMESTAMP', 'GUI FLOWS W MNEMONIC', 'GUI MULTILINE',
                         'GUI VALID2', 'GUI SET USING', 'WPDO2731', 'WPDO 3015 CHECKBOX', 'WMEE-13529 DIRECT URL NAV',
                         'WPDO-3326 DISABLED TAB ORDER', 'WPDO-2751 FILTER', 'WPDO-2831 JULDATE', 'WMEE-13753 BAD INDEX LIST STMTS',
                         'WPDO-3228 INVOKE', 'WPDO-3313 MISSING FIELD PROPS', 'WPDO 3281', 'GUI MESSAGE BOX', 'WPDO-2860 YEARTIMESTAMP',
                         'WPDO 2759 ABSOLUTEVALUE', 'WPDO-2853 TIMETEXT', 'WPDO-3188 USE IMPORTEXPORT LIST', 'WPDO3218',
                         'WMEE-13605 SERVER GV LIST', 'NESTED SUBMENUS', 'GUI FLOWS 2', 'GUI LIST STATEMENTS', 'WPDO-2739 REFRESH',
                         'GUI OBJECTS', 'WMEE 13549', 'WPDO-2795 MAKE STMT FUNCTIONAL', 'WPDO-3303 TEST FOR REGRESSIONS',
                         'GUI SAVED LOCALS', 'WPDO-3314 FLOW W COMMAND', 'WPDO-3288 COMPLEX FOCUS EVENTS', 'WPDO-3224 E DROP DOWN',
                         'WPDO-3112 USE NO GV', 'WPDO-3067 WHILE', 'WPDO-3173 NUMERIC ATTRIBUTE', 'WPDO-2750 SORT',
                         'WPDO-3248 RD BTN OCC DSBLD BY', 'WPDO-2729 DROP DOWN LIST', 'WPDO-3266 RDBTN DISABLEDBY',
                         'WPDO-3267 LIST BOX DISABLEDBY', 'WPDO-3249 SIMPLE CHANGE EVENTS', 'STATUSB', 'WPDO27301',
                         'WPDO-3321 PUSH BUTTON BITMAP', 'WPDO2719', 'WPDO 2852 TIMESTAMP', 'GUI FLOWS',
                         'GUI MARK UNMARK', 'WMEE 14164', 'WPDO-3365 BUSSYS DFLT COLOR FONT', 'WPDO-3347 MAKE STMT VISUAL',
                         'WMEE 14494 DEFAULT BUTTON', 'WPDO-2765 FINDREPLACESTRING', 'EXIT STATE MESSAGE' , 'WMEE-14321 OLD MAPPING']
                                .each {
                                    modelName ->
                                        migrateAndArchive(modelName)
                                        cleanUp(user, group)
                                }
                    }
                } finally {
                    // If we abort the build while npm is installing packages, npm will continue to create files
                    // for a short time.
                    sleep 60
                    cleanUp(user, group)
                }
            }
        }
    }
}

/**
 * Cleans up artifacts from the workspace.
 *
 * @param user The effective user ID
 * @param group The effective group ID
 */
def cleanUp(String user, String group) {
    sh returnStdout: true, script: "chown -hR ${user}:${group} *"
    sh returnStdout: true, script: 'rm -rf *backend*'
    sh returnStdout: true, script: 'rm -rf *gui*'
}

/**
 * Migrates and archives a model.
 *
 * @param modelName The name of the model
 */
def migrateAndArchive(String modelName) {
    stage(modelName) {
        def additionalProperties = ''
        if (modelName in ['GUI FLOWS W MNEMONIC', 'GUI FLOWS 2', 'WPDO-3288 COMPLEX FOCUS EVENTS', 'GUI FLOWS']) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=Procedure_A'
        } else if (modelName in ['SERVER FLOWS', 'WMEE-13605 SERVER GV LIST', 'WPDO-3314 FLOW W COMMAND']) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=false --com.innowake.gen.initial-procedure=Client'
        } else if (modelName.equals('WMEE 14164')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=WINDOW'
        } else if (modelName in ['WPDO-3227 ENABLE DISABLE CMD', 'GUI MARK UNMARK']) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=Client'
        } else if (modelName in ['GUI MULTILINE', 'GUI VALID2', 'GUI SET USING']) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=Hello'
        } else if (modelName in ['WPDO2731', 'WPDO27301']) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=P1'
        } else if (modelName in ['STATUSB', 'EXIT STATE MESSAGE']) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=A'
        } else if (modelName.equals('NESTED SUBMENUS')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=Start'
        } else if (modelName.equals('WPDO3218')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=E2 ' +
                    '--com.innowake.gen.x-scaling-factor=1.00 --com.innowake.gen.y-scaling-factor=1.50'
        } else if (modelName.equals('WPDO 3015 CHECKBOX')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=Main'
        } else if (modelName.equals('WPDO-2855 TRIM')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=Trim'
        } else if (modelName.equals('WMEE-13529 DIRECT URL NAV')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true'
        } else if (modelName.equals('GUI MESSAGE BOX')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=MessageBoxPrimary'
        } else if (modelName.equals('WPDO2719')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=MainProcedure'
        } else if (modelName.equals('WMEE 14494 DEFAULT BUTTON')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=DEFAULTBUTTON'
        } else if (modelName.equals('WMEE 13549')) {
            additionalProperties = '--com.innowake.gen.x-scaling-factor=10.00 --com.innowake.gen.y-scaling-factor=5.50 ' +
                    '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=Starter'
        } else if (modelName.equals('GUI SAVED LOCALS')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=Saved_Locals'
        } else if (modelName.equals('NN DELTA LLOYD JRA DEV V5')) {
            additionalProperties = '--com.innowake.gen.include-window-procedures=false --com.innowake.gen.disable-db=false --com.innowake.gen.initial-procedure=AVB0J00C_LOGIN'
        } else {
            additionalProperties = '--com.innowake.gen.include-window-procedures=true --com.innowake.gen.initial-procedure=Starter'
        }
        catchError(message: "Encountered an error while trying to migrate and archive model ${modelName}.\nWe will skip this model, and continue.",
                buildResult: 'UNSTABLE', catchInterruptions: false, stageResult: 'UNSTABLE') {
            withCredentials([usernamePassword(credentialsId: 'CaGen-UI-JRA-DB-Credentials', passwordVariable: 'dbPassword', usernameVariable: 'dbUser')]) {
                sh "java -javaagent:${pwd()}/org.jacoco.agent-0.8.8/jacocoagent.jar -jar javagen-dist-spring-boot-${caGenBuildVersion}.jar \
				--com.innowake.gen.model-name='${modelName}' \
				--com.innowake.gen.destination-directory=. \
				--com.innowake.gen.preserve-symlinks=false \
				--com.innowake.gen.migrate-frontend=false \
				--com.innowake.gen.migrate-backend=true \
				--com.innowake.gen.migrate-gui=true \
				--com.innowake.gen.bootstrap-frontend=false \
				--com.innowake.gen.bootstrap-gui=true \
				--com.innowake.gen.split-app=true \
				--com.innowake.gen.backend-base-url=http://localhost:8080 \
				--com.innowake.gen.app-package=com.someclient.gen \
				--com.innowake.gen.database-ip=localhost \
				--com.innowake.gen.database-port=1521 \
				--com.innowake.gen.database-name=ORCLPDB1 \
				--com.innowake.gen.database-un=${dbUser} \
				--com.innowake.gen.database-pw=${dbPassword} \
				--com.innowake.gen.database-driver=oracle.jdbc.OracleDriver \
				--com.innowake.gen.database-url=jdbc:oracle:thin:@localhost:1521/ORCLPDB1 \
				--com.innowake.gen.database-type=oracle \
				--server.port=8080 \
				--spring.datasource.url='jdbc:sqlserver://10.195.131.109:1433;database=DBCSE' \
				--spring.datasource.username=DBCSEadmin \
				--spring.datasource.password=Innowake123! \
				--spring.datasource.driver-class-name=com.microsoft.sqlserver.jdbc.SQLServerDriver \
                --com.innowake.gen.use-dist-jar=true \
				--com.innowake.gen.version-id=${caGenBuildVersion} \
				${additionalProperties}"
            }
            renameAndArchive(modelName)
        }
    }
}

/**
 * Renames and archives the gui and backend directories.
 *
 * @param modelName The name of the model
 */
def renameAndArchive(String modelName) {
    def spUtils = new SharepointUtils()
    String modelNameWithUnderscores = modelName.replace(' ', '_').replace('-', '_')
    sh "mv gui ${modelNameWithUnderscores}_gui"
    sh "mv backend ${modelNameWithUnderscores}_backend"
    zip zipFile: "${modelNameWithUnderscores}_gui.zip", archive: false, dir: "${modelNameWithUnderscores}_gui"
    zip zipFile: "${modelNameWithUnderscores}_backend.zip", archive: false, dir: "${modelNameWithUnderscores}_backend"
    spUtils.uploadJobArtifact(caGenBuildVersion, "${modelNameWithUnderscores}_backend.zip")
    sh returnStatus: true, script: "mv ${pwd()}/jacoco.exec ${pwd()}/cagenUI-${modelNameWithUnderscores}-migration-jacoco.exec"
    archiveArtifacts allowEmptyArchive: true, artifacts: "**/cagenUI-${modelNameWithUnderscores}-migration-jacoco.exec"
}
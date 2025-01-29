/*
 * This job creates WordPress image with the same environment as the produktive documentation WordPress instance. 
 * Therefore, following parameters have to be specified.
 * 
 * @param javaVersion     The java version which is used for the docker environment.
 * @param executeOn       The node where to run the instance.
 */
@Library('TestUtils') _

node(executeOn) {
    def jobWorkspace = pwd()
    def dockerUtils = new DockerUtils()
	def miscUtils = new MiscUtils()
	def hostname = miscUtils.getHostname()
    def gitUtils = new GitUtils()

	buildName "WordPress Image"
    buildDescription "executeOn=${executeOn} javaVersion=${javaVersion}"

    stage('clean up') {
        deleteDir()
    }

    docker.image(dockerUtils.pullJenkinsEnvironmentImage(javaVersion)).inside {
        stage('checkout and copy') {
            dir('wp-docker') {
                gitUtils.checkoutGitProject("${jobWorkspace}/wp-documentation-utilities", gitUtils.getGitUrlDocumentationUtils(), 'master')

                sh 'mkdir themes'
                sh 'mkdir plugins'
                sh 'cp -r ../wp-documentation-utilities/phpscripts .'
                sh 'cp -r ../wp-documentation-utilities/uploads .'
                sh 'cp -r ../wp-documentation-utilities/themes/neve-child ./themes'
            }
            
        }
        
        stage('generate and fill plugins folder') {    
            dir('wp-docker/themes') {
                sh 'cp -r ../../wp-documentation-utilities/themes/neve-child .'
                sh 'curl $(curl -s https://wordpress.org/themes/neve/ | egrep -o -m 1 "https://downloads.wordpress.org/theme/[^\\"]+") -O'
                sh 'unzip \\*.zip'
                sh 'rm *.zip'
            }
        }
        
        stage('generate and fill themes folder') {
            dir('wp-docker/plugins') {
                sh 'curl $(curl -s https://wordpress.org/plugins/breadcrumb-navxt/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/clone-page-tree/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/cms-tree-page-view/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/enable-media-replace/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/html-editor-syntax-highlighter/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/luckywp-table-of-contents/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/media-library-assistant/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/wp-nested-pages/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/page-list/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/revisionary/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/relevanssi/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/search-regex/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/shortcodes-ultimate/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/custom-css-js/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/ultimate-addons-for-gutenberg/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/stream/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/syntax-highlighting-code-block/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/tablepress/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/updraftplus/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/user-role-editor/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/wp-2fa/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/wp-file-manager/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/wp-last-modified-info/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/wp-mail-smtp/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/wpdiscuz/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'curl $(curl -s https://wordpress.org/plugins/wpforms-lite/ | egrep -o -m 1 "https://downloads.wordpress.org/plugin/[^\\"]+") -O'
                sh 'unzip \\*.zip'
                sh 'rm *.zip'
            }
        }
    }

    stage('create Dockerfile') {
        dir('wp-docker') {
            sh '''cat << EOF > Dockerfile
FROM wordpress:latest

COPY /plugins /usr/src/wordpress/wp-content/plugins
COPY /themes /usr/src/wordpress/wp-content/themes
COPY /uploads /usr/src/wordpress/wp-content/uploads
COPY /phpscripts /usr/src/wordpress/wp-content/phpscripts
EOF'''
        }
    }

    stage('create and push image to repository') {
        dir('wp-docker') {
            sh 'docker build --no-cache . -t wordpress-test'

			docker.withRegistry(dockerUtils.getEuDockerRegistryUrl(), 'EU-Docker-Registry-User') {
	       	    docker.image('wordpress-test').push()
    	    }
        }
    }

    stage('clean up') {
        deleteDir()
        sh returnStatus: true, script: "docker image rm wordpress-test"
    }
}
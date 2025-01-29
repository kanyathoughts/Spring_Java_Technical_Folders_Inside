def lib = library("TestUtils@${branchName}")

/*
* Test for ProductDeliveryUtils. This also demonstrates how to use the methods.
*
* @param branchName Name of the branch where changes are stored, default: master
*/
timestamps {
    def productDeliveryUtils = lib.ProductDeliveryUtils.new()
    def mxVersionUtils = lib.MxVersionUtils.new()
    node('Docker-host') {
        def expectedUnattributedFiles = ["./ndt-mdd-mee/eclipse/innowake-product-plugins-<version>.zip",
                                        "./ndt-mdd-mee/server/ndt-services/ndt-services-natural-<version>.asc",
                                        "./ndt-mdd-mee/server/ndt-services/ndt-services-natural-run-client-<version>.asc",
                                        "./ndt-mdd-mee/server/ndt-services/ndt-services-natural-user-exits-<version>.asc",
                                        "./ca-gen/framework/framework-dist-<version>-javadoc.jar",
                                        "./ca-gen/framework/framework-dist-<version>.jar",
                                        "./ca-gen/gen-cobol-integration/gen-cobol-integration-dist-<version>-javadoc.jar",
                                        "./ca-gen/javagen/javagen-dist-<version>-javadoc.jar",
                                        "./ca-gen/javagen/javagen-dist-<version>.jar",
                                        "./ca-gen/ui-framework/ui-framework-dist-<version>.zip"]
        def expectedUnattributedFolders = ["./ndt-mdd-mee/eclipse/install-tool_<version>",
                                        "./sc/broker/java",
                                        "./sc/broker/natural",
                                        "./sc/control-center",
                                        "./sc/doc",
                                        "./sc/server",
                                        "./sc/workbench"]
        
        buildName "#${env.BUILD_ID} - ${branchName}"

        stage('Show check(...) usage') {
            def x = 3

            echo 'Start first Test (failing)'
            check(4, x, false)
            /* normaly you would use it like this: check(4,x) and since this is a failing test, it would turn the stage into yellow.
            * To prevent the unstable yellow, you can set an optional third parameter to false.
            * So in case of an error, it just prints the message without turining the stage into unstable.
            */

            echo 'Start second Test (failing)'
            check('hello', 3, false)

            echo 'Start third Test (passing)'
            check(3, 3)
        }

        stage('Test parseContent') {
            def fileContentAsStringValid = '''./ndt-mdd-mee/eclipse/install-tool_<version>
                    ./ndt-mdd-mee/eclipse/innowake-product-plugins-<version>.zip
                    ./ndt-mdd-mee/server/ndt-services/ndt-services-natural-<version>.asc
                    ./ndt-mdd-mee/server/ndt-services/ndt-services-natural-run-client-<version>.asc
                    ./ndt-mdd-mee/server/ndt-services/ndt-services-natural-user-exits-<version>.asc
                    ./sc/broker/java
                    ./sc/broker/natural
                    ./sc/control-center
                    ./sc/doc
                    ./sc/server
                    ./sc/workbench
                    
    ./ca-gen/framework/framework-dist-<version>-javadoc.jar

    ./ca-gen/framework/framework-dist-<version>.jar

    ./ca-gen/gen-cobol-integration/gen-cobol-integration-dist-<version>-javadoc.jar
    ./ca-gen/javagen/javagen-dist-<version>-javadoc.jar
        ./ca-gen/javagen/javagen-dist-<version>.jar
        ./ca-gen/ui-framework/ui-framework-dist-<version>.zip

    '''
            (parsedFilesList, parsedFoldersList) = productDeliveryUtils.parseContent(fileContentAsStringValid)

            echo 'Test of parseContent(...)'

            //check file list  
            check(10, parsedFilesList.size())
            for (int i = 0; i < expectedUnattributedFiles.size(); i++) {
                check(expectedUnattributedFiles[i], parsedFilesList[i])
            }

            //check folder list
            check(7, parsedFoldersList.size())
            for (int i = 0; i < expectedUnattributedFolders.size(); i++) {
                check(expectedUnattributedFolders[i], parsedFoldersList[i])
            }
        }

        stage('Test copy') {
            deleteDir() //clean up

            dir('rootDocuments') {
                sh 'echo Such line > rFile.txt'
                sh 'echo another > some.jar'
                dir('folder') {
                    sh 'echo a line > aFile.txt'
                    sh 'echo Hello World > bFile.txt'
                }
            }

            dir('foldfolder') {
                dir('subfolder') {
                    sh 'echo content in file > subFile.txt'
                }
                sh 'echo foldLeft > foldFile.h'
            }

            dir('folder') {
                sh 'echo dff1 > dFFile1.c'
                sh 'echo dff2 > dFFile2.c'

            }

            dir('targetLocation') {}

            def sPath = "${pwd()}"
            def tPath = "${pwd()}/targetLocation"

            def filesList = ['rootDocuments/rFile.txt', 'rootDocuments/some.jar']
            def foldersList = ['foldfolder', 'folder']

            echo 'Test of copy(...)'

            productDeliveryUtils.copy(filesList, foldersList, sPath, tPath)
            dir('targetLocation') {
                def itemCount = sh returnStdout: true, script: "ls"
                itemCount = itemCount.split('\n').findAll { !it.contains('@tmp') }
                check(3, itemCount.size())

                dir('folder') {
                    itemCount = sh returnStdout: true, script: "ls"
                    itemCount = itemCount.split('\n').findAll { !it.contains('@tmp') }

                    check(2, itemCount.size())
                    check(true, itemCount.contains('dFFile1.c'))
                    check(true, itemCount.contains('dFFile2.c'))

                }

                dir('foldfolder') {
                    itemCount = sh returnStdout: true, script: "ls"
                    itemCount = itemCount.split('\n').findAll { !it.contains('@tmp') }

                    check(2, itemCount.size())
                    check(true, itemCount.contains('foldFile.h'))
                    check(true, itemCount.contains('subfolder'))

                    dir('subfolder') {
                        itemCount = sh returnStdout: true, script: "ls"
                        itemCount = itemCount.split('\n').findAll { !it.contains('@tmp') }

                        check(1, itemCount.size())
                        check(true, itemCount.contains('subFile.txt'))
                    }
                }

                dir('rootDocuments') {
                    itemCount = sh returnStdout: true, script: "ls"
                    itemCount = itemCount.split('\n').findAll { !it.contains('@tmp') }

                    check(2, itemCount.size())
                    check(true, itemCount.contains('rFile.txt'))
                    check(true, itemCount.contains('some.jar'))

                }
            }
        }

        stage('Test attributeSourcePaths') {
            def mxBuildVersions = ['21.0.0', '18.0.0.03', '19.2.04']

            for (mxBuildVersion in mxBuildVersions) {
                (parsedFilesList, parsedFoldersList) = productDeliveryUtils.attributeSourcePaths(expectedUnattributedFiles, expectedUnattributedFolders, mxBuildVersion)

                echo "Test of attributeSourcePaths(...) in version ${mxBuildVersion}"

                //check if there is any path which contains <version>
                check(0, parsedFilesList.findAll { element -> element.contains('<version>') }.size())
                check(0, parsedFoldersList.findAll { element -> element.contains('<version>') }.size())

                check(10, parsedFilesList.size())
                //check if the version is contained in the lists
                for (item in parsedFilesList) {
                    //echo "mxBuildVersion: ${mxBuildVersion}\titem: ${item}"
                    check(true, item.contains(mxBuildVersion))
                }

                check(7, parsedFoldersList.size())
                for (int i = 0; i < parsedFoldersList.size(); i++) {
                    def item = parsedFoldersList[i]
                    //echo "mxBuildVersion: ${mxBuildVersion}\titem: ${item}"
                    if (i == 0) {
                        //only the first element had a <version> tag
                        check(true, item.contains(mxBuildVersion))
                    } else {
                        check(false, item.contains(mxBuildVersion))
                    }
                }
            }
        }

        stage('Test parseDocumentation') {
            def exampleXML = ''' <content>
        <data>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/18.0.0.99_NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/18.0.0.99_NDT-SNAPSHOT/</relativePath>
        <text>18.0.0.99_NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2020-02-07 13:24:54.308 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/18.0.1.99_NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/18.0.1.99_NDT-SNAPSHOT/</relativePath>
        <text>18.0.1.99_NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2020-06-25 09:34:36.89 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.1.99-NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/19.1.99-NDT-SNAPSHOT/</relativePath>
        <text>19.1.99-NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2021-05-07 12:18:30.741 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.2.99-NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/19.2.99-NDT-SNAPSHOT/</relativePath>
        <text>19.2.99-NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2020-05-18 14:33:49.848 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.3.99-NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/19.3.99-NDT-SNAPSHOT/</relativePath>
        <text>19.3.99-NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2020-07-21 13:23:49.228 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.4.99-NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/19.4.99-NDT-SNAPSHOT/</relativePath>
        <text>19.4.99-NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2020-09-09 14:12:40.182 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.0.99-NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/21.0.99-NDT-SNAPSHOT/</relativePath>
        <text>21.0.99-NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2021-02-12 16:01:32.864 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.1.99-NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/21.1.99-NDT-SNAPSHOT/</relativePath>
        <text>21.1.99-NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2021-05-17 15:45:08.60 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.2.99-NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/21.2.99-NDT-SNAPSHOT/</relativePath>
        <text>21.2.99-NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2021-05-12 09:20:09.825 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.3.99-NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/21.3.99-NDT-SNAPSHOT/</relativePath>
        <text>21.3.99-NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2021-05-12 09:35:36.920 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/99.9.99-TRUNK-NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/99.9.99-TRUNK-NDT-SNAPSHOT/</relativePath>
        <text>99.9.99-TRUNK-NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2021-05-07 12:18:36.80 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/99.9.9_TRUNK_NDT-SNAPSHOT/</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/99.9.9_TRUNK_NDT-SNAPSHOT/</relativePath>
        <text>99.9.9_TRUNK_NDT-SNAPSHOT</text>
        <leaf>false</leaf>
        <lastModified>2020-03-05 12:50:26.630 UTC</lastModified>
        <sizeOnDisk>-1</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml</relativePath>
        <text>maven-metadata.xml</text>
        <leaf>true</leaf>
        <lastModified>2021-06-15 08:20:41.56 UTC</lastModified>
        <sizeOnDisk>923</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.md5</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.md5</relativePath>
        <text>maven-metadata.xml.md5</text>
        <leaf>true</leaf>
        <lastModified>2021-06-15 08:20:41.56 UTC</lastModified>
        <sizeOnDisk>32</sizeOnDisk>
        </content-item>
        <content-item>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</relativePath>
        <text>maven-metadata.xml.sha1</text>
        <leaf>true</leaf>
        <lastModified>2021-06-15 08:20:41.71 UTC</lastModified>
        <sizeOnDisk>40</sizeOnDisk>
        </content-item>
        </data>
    </content>'''

            echo 'Test of parseDocumentation(...)'
            def contentItems = productDeliveryUtils.parseDocumentation(exampleXML)

            //in contentItems there should be as many elements as there are occurances of <content-item> in the XML file
            def contentItemCount = exampleXML.split('\n').collect { it.trim() }.findAll { it.contains('<content-item>') }.size()
            check(contentItemCount, contentItems.size())
            /*
            *  For every item assure its structure: 
            *    <resoureURI></resoureURI>
            *    <relativePath></relativePath>
            *    <text></text>
            *    <lastModified></lastModified>
            */
            for (item in contentItems) {
                check(4, item.size())

                check(true, item.get(0).startsWith('<resourceURI>'))
                check(true, item.get(0).endsWith('</resourceURI>'))

                check(true, item.get(1).startsWith('<relativePath>'))
                check(true, item.get(1).endsWith('</relativePath>'))

                check(true, item.get(2).startsWith('<text>'))
                check(true, item.get(2).endsWith('</text>'))

                check(true, item.get(3).startsWith('<lastModified>'))
                check(true, item.get(3).endsWith('</lastModified>'))
            }
        }

        stage('Test attributeDocumentation') {
            def contentItems = [
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/18.0.0.99_NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/18.0.0.99_NDT-SNAPSHOT/</relativePath>',
                    '<text>18.0.0.99_NDT-SNAPSHOT</text>',
                    '<lastModified>2020-02-07 13:24:54.308 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/18.0.1.99_NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/18.0.1.99_NDT-SNAPSHOT/</relativePath>',
                    '<text>18.0.1.99_NDT-SNAPSHOT</text>',
                    '<lastModified>2020-06-25 09:34:36.89 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.1.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.1.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.1.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-07 12:18:30.741 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.2.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.2.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.2.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2020-05-18 14:33:49.848 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.3.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.3.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.3.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2020-07-21 13:23:49.228 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.4.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.4.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.4.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2020-09-09 14:12:40.182 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.0.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.0.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.0.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-02-12 16:01:32.864 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.1.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.1.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.1.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-17 15:45:08.60 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.2.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.2.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.2.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-12 09:20:09.825 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.3.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.3.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.3.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-12 09:35:36.920 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/99.9.99-TRUNK-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/99.9.99-TRUNK-NDT-SNAPSHOT/</relativePath>',
                    '<text>99.9.99-TRUNK-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-07 12:18:36.80 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/99.9.9_TRUNK_NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/99.9.9_TRUNK_NDT-SNAPSHOT/</relativePath>',
                    '<text>99.9.9_TRUNK_NDT-SNAPSHOT</text>',
                    '<lastModified>2020-03-05 12:50:26.630 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml</relativePath>',
                    '<text>maven-metadata.xml</text>',
                    '<lastModified>2021-06-15 08:20:41.56 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.md5</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.md5</relativePath>',
                    '<text>maven-metadata.xml.md5</text>',
                    '<lastModified>2021-06-15 08:20:41.56 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</relativePath>',
                    '<text>maven-metadata.xml.sha1</text>',
                    '<lastModified>2021-06-15 08:20:41.71 UTC</lastModified>'
                    ]
            ]
            def parsedCustomerDocumentationFile = ['natclipse']

            echo 'Test of attributeDocumentation(...)'
        
            //<resourceURI> -> name and <lastModified> (== String) -> date (== SimpleDateFormat)
            parsedCustomerDocumentationFile.each { item ->
                contentItems = productDeliveryUtils.attributeDocumentation(contentItems, item)
                contentItems.each {
                    check(4, it.size())
                    //check invalidity
                    check(0, it.findAll { it.toString().contains('<resourceURI>') }.size())
                    check(0, it.findAll { it.toString().contains('</resourceURI>') }.size())
                    check(0, it.findAll { it.toString().contains('<lastModified>') }.size())
                    check(0, it.findAll { it.toString().contains('</lastModified>') }.size())

                    //check correct beginnings
                    check(item, it.get(0))

                    check(true, it.get(1).startsWith('<relativePath>'))
                    check(true, it.get(1).endsWith('</relativePath>'))

                    check(true, it.get(2).startsWith('<text>'))
                    check(true, it.get(2).endsWith('</text>'))

                    //date match. Warning: this also accpets dates like Mon Jun 99 29:59:59 NotUTC 9999 as valid which is wrong.
                    check(true, it.get(3) ==~ /^(Mon|Tue|Wed|Thu|Fri|Sat|Sun) (Jun|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dez) [0-9]{2,2} [0-2][0-9]:[0-5][0-9]:[0-5][0-9] [a-zA-Z]* [0-9]{4,4}/)
                }
            }
        }

        stage('Test dateSort') {
            def contentItems = [
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/18.0.0.99_NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/18.0.0.99_NDT-SNAPSHOT/</relativePath>',
                    '<text>18.0.0.99_NDT-SNAPSHOT</text>',
                    '<lastModified>2020-02-07 13:24:54.308 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/18.0.1.99_NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/18.0.1.99_NDT-SNAPSHOT/</relativePath>',
                    '<text>18.0.1.99_NDT-SNAPSHOT</text>',
                    '<lastModified>2020-06-25 09:34:36.89 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.1.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.1.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.1.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-07 12:18:30.741 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.2.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.2.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.2.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2020-05-18 14:33:49.848 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.3.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.3.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.3.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2020-07-21 13:23:49.228 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.4.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.4.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.4.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2020-09-09 14:12:40.182 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.0.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.0.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.0.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-02-12 16:01:32.864 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.1.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.1.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.1.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-17 15:45:08.60 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.2.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.2.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.2.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-12 09:20:09.825 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.3.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.3.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.3.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-12 09:35:36.920 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/99.9.99-TRUNK-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/99.9.99-TRUNK-NDT-SNAPSHOT/</relativePath>',
                    '<text>99.9.99-TRUNK-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-07 12:18:36.80 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/99.9.9_TRUNK_NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/99.9.9_TRUNK_NDT-SNAPSHOT/</relativePath>',
                    '<text>99.9.9_TRUNK_NDT-SNAPSHOT</text>',
                    '<lastModified>2020-03-05 12:50:26.630 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml</relativePath>',
                    '<text>maven-metadata.xml</text>',
                    '<lastModified>2021-06-15 08:20:41.56 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.md5</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.md5</relativePath>',
                    '<text>maven-metadata.xml.md5</text>',
                    '<lastModified>2021-06-15 08:20:41.56 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</relativePath>',
                    '<text>maven-metadata.xml.sha1</text>',
                    '<lastModified>2021-06-15 08:20:41.71 UTC</lastModified>'
                    ]
            ]

            expectedDates = ['Tue Jun 15 08:20:41 UTC 2021',
                            'Tue Jun 15 08:20:41 UTC 2021',
                            'Tue Jun 15 08:20:41 UTC 2021',
                            'Mon May 17 15:45:08 UTC 2021',
                            'Wed May 12 09:35:36 UTC 2021',
                            'Wed May 12 09:20:09 UTC 2021',
                            'Fri May 07 12:18:36 UTC 2021',
                            'Fri May 07 12:18:30 UTC 2021',
                            'Fri Feb 12 16:01:32 UTC 2021',
                            'Wed Sep 09 14:12:40 UTC 2020',
                            'Tue Jul 21 13:23:49 UTC 2020',
                            'Thu Jun 25 09:34:36 UTC 2020',
                            'Mon May 18 14:33:49 UTC 2020',
                            'Thu Mar 05 12:50:26 UTC 2020',
                            'Fri Feb 07 13:24:54 UTC 2020']

            def parsedCustomerDocumentationFile = ['natclipse']
            def attributedContentItems
            parsedCustomerDocumentationFile.each { item ->
                attributedContentItems = productDeliveryUtils.attributeDocumentation(contentItems, item)
            }

            echo 'Test of dateSort(). This requires a working attributeDocumentation(...)'

            attributedContentItems = productDeliveryUtils.dateSort(attributedContentItems)
            check(expectedDates.size(), attributedContentItems.size())

            //items should be sorted according to date from newest to oldest.
            for (int i = 0; i > attributedContentItems.size(); i++) {
                check(expectedDates.get(i), attributedContentItems.get(i))
            }
        }

        stage('Test parse-till-prepared-for-downloading') {
            //shows the usage with a trunk version
            def pureXML = '''<content>
            <data>
            <content-item>
            <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/18.0.1.99_SC-SNAPSHOT/</resourceURI>
            <relativePath>/innowake/documentation/user-manual_soa-connector/18.0.1.99_SC-SNAPSHOT/</relativePath>
            <text>18.0.1.99_SC-SNAPSHOT</text>
            <leaf>false</leaf>
            <lastModified>2020-06-25 09:40:18.291 UTC</lastModified>
            <sizeOnDisk>-1</sizeOnDisk>
            </content-item>
            <content-item>
            <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/19.1.99_SC-SNAPSHOT/</resourceURI>
            <relativePath>/innowake/documentation/user-manual_soa-connector/19.1.99_SC-SNAPSHOT/</relativePath>
            <text>19.1.99_SC-SNAPSHOT</text>
            <leaf>false</leaf>
            <lastModified>2021-05-07 12:18:39.701 UTC</lastModified>
            <sizeOnDisk>-1</sizeOnDisk>
            </content-item>
            <content-item>
            <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/19.2.99_SC-SNAPSHOT/</resourceURI>
            <relativePath>/innowake/documentation/user-manual_soa-connector/19.2.99_SC-SNAPSHOT/</relativePath>
            <text>19.2.99_SC-SNAPSHOT</text>
            <leaf>false</leaf>
            <lastModified>2020-05-18 14:41:38.982 UTC</lastModified>
            <sizeOnDisk>-1</sizeOnDisk>
            </content-item>
            <content-item>
            <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/19.4.99_SC-SNAPSHOT/</resourceURI>
            <relativePath>/innowake/documentation/user-manual_soa-connector/19.4.99_SC-SNAPSHOT/</relativePath>
            <text>19.4.99_SC-SNAPSHOT</text>
            <leaf>false</leaf>
            <lastModified>2020-09-09 14:15:06.902 UTC</lastModified>
            <sizeOnDisk>-1</sizeOnDisk>
            </content-item>
            <content-item>
            <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/21.0.99-SC-SNAPSHOT/</resourceURI>
            <relativePath>/innowake/documentation/user-manual_soa-connector/21.0.99-SC-SNAPSHOT/</relativePath>
            <text>21.0.99-SC-SNAPSHOT</text>
            <leaf>false</leaf>
            <lastModified>2021-02-12 16:02:04.208 UTC</lastModified>
            <sizeOnDisk>-1</sizeOnDisk>
            </content-item>
            <content-item>
            <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/21.0.99_SC-SNAPSHOT/</resourceURI>
            <relativePath>/innowake/documentation/user-manual_soa-connector/21.0.99_SC-SNAPSHOT/</relativePath>
            <text>21.0.99_SC-SNAPSHOT</text>
            <leaf>false</leaf>
            <lastModified>2020-12-17 13:52:03.865 UTC</lastModified>
            <sizeOnDisk>-1</sizeOnDisk>
            </content-item>
            </data>
        </content>'''

            echo 'Test parsing till before download and rename'

            def docuItem = 'soa-connector'
            def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion('21.0.0')
            def list = productDeliveryUtils.parseDocumentation(pureXML)
            check(6, list.size())

            echo "Before attributeDocumentation:  ${list}\n"
            list = productDeliveryUtils.attributeDocumentation(list, docuItem)
            check(6, list.size())

            echo "Before removeAttributedItems: ${list}\n"
            list = productDeliveryUtils.removeAttributedItems(list, mxVersion)

            echo "Before dateSort: ${list}\n"
            list = productDeliveryUtils.dateSort(list)

            echo "Before findItemWithVersion: ${list}\n"
            def toDocuItems = []
            toDocuItems = productDeliveryUtils.findItemWithVersion(list, toDocuItems, mxVersion)
            check(2, list.size())
            check(1, toDocuItems.size())
        }

        stage('Test getGroupAndArtifactName') {
            def fName = 'innowake-maxenso-runtime-swing-dist-19.0.0.1.jar'
            def fPath = '/nexus/innowake.maxenso'

            echo 'Test of getGroupAndArtifactName(...)'
            
            (fileName, fileVersion, fileType, groupID) = productDeliveryUtils.getGroupAndArtifactName(fName, fPath)
            check('innowake-maxenso-runtime-swing-dist', fileName)
            check('19.0.0.1', fileVersion)
            check('jar', fileType)
            check('innowake.maxenso',groupID)

            fName = 'innowake-runtime-swing-dist-21.0.0.jar'
            fPath = 'nexus/innowake.bundle'

            (fileName, fileVersion, fileType, groupID) = productDeliveryUtils.getGroupAndArtifactName(fName, fPath)
            check('innowake-runtime-swing-dist', fileName)
            check('21.0.0', fileVersion)
            check('jar', fileType)
            check('innowake.bundle',groupID)
        }

        stage('Test removeAttributedItems') {
            //requires a working attributeDocumentation(...)
            def expectedValues = [
                ['soa-connector',
                '<relativePath>/innowake/documentation/user-manual_soa-connector/21.0.99-SC-SNAPSHOT/</relativePath>',
                '<text>21.0.99-SC-SNAPSHOT</text>',
                'Fri Feb 12 16:02:04 UTC 2021'],
                ['soa-connector', 
                '<relativePath>/innowake/documentation/user-manual_soa-connector/21.0.99_SC-SNAPSHOT/</relativePath>', 
                '<text>21.0.99_SC-SNAPSHOT</text>', 
                'Thu Dec 17 13:52:03 UTC 2020'
                ]
            ]

            def list = [
                ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/18.0.1.99_SC-SNAPSHOT/</resourceURI>',
                '<relativePath>/innowake/documentation/user-manual_soa-connector/18.0.1.99_SC-SNAPSHOT/</relativePath>',
                '<text>18.0.1.99_SC-SNAPSHOT</text>',
                '<lastModified>2020-06-25 09:40:18.291 UTC</lastModified>'
                ],
                ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/19.1.99_SC-SNAPSHOT/</resourceURI>',
                '<relativePath>/innowake/documentation/user-manual_soa-connector/19.1.99_SC-SNAPSHOT/</relativePath>', 
                '<text>19.1.99_SC-SNAPSHOT</text>', 
                '<lastModified>2021-05-07 12:18:39.701 UTC</lastModified>'
                ], 
                ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/19.2.99_SC-SNAPSHOT/</resourceURI>', 
                '<relativePath>/innowake/documentation/user-manual_soa-connector/19.2.99_SC-SNAPSHOT/</relativePath>', 
                '<text>19.2.99_SC-SNAPSHOT</text>', 
                '<lastModified>2020-05-18 14:41:38.982 UTC</lastModified>'
                ], 
                ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/19.4.99_SC-SNAPSHOT/</resourceURI>', 
                '<relativePath>/innowake/documentation/user-manual_soa-connector/19.4.99_SC-SNAPSHOT/</relativePath>', 
                '<text>19.4.99_SC-SNAPSHOT</text>', 
                '<lastModified>2020-09-09 14:15:06.902 UTC</lastModified>'
                ], 
                ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/21.0.99-SC-SNAPSHOT/</resourceURI>', 
                '<relativePath>/innowake/documentation/user-manual_soa-connector/21.0.99-SC-SNAPSHOT/</relativePath>', 
                '<text>21.0.99-SC-SNAPSHOT</text>', 
                '<lastModified>2021-02-12 16:02:04.208 UTC</lastModified>'
                ], 
                ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_soa-connector/21.0.99_SC-SNAPSHOT/</resourceURI>', 
                '<relativePath>/innowake/documentation/user-manual_soa-connector/21.0.99_SC-SNAPSHOT/</relativePath>', 
                '<text>21.0.99_SC-SNAPSHOT</text>', 
                '<lastModified>2020-12-17 13:52:03.865 UTC</lastModified>'
                ]
            ]

            def docuItem = 'soa-connector'
            def mxVersion = mxVersionUtils.getMxVersionForMxBuildVersion('21.0.0')
            list = productDeliveryUtils.attributeDocumentation(list, docuItem)

            echo 'Test of removeAttributedVersion()'

            //there is no item in the given version but there are trunks, so we choose the trunks
            list = productDeliveryUtils.removeAttributedItems(list, mxVersion)
            echo "${list}"
            check(expectedValues.size(), list.size())
            for (int i = 0; i < expectedValues.size(); i++) {
                check(expectedValues.get(i).size(), list.get(i).size())

                for (int j = 0; j < expectedValues.get(i).size(); j++) {
                    check(true, expectedValues.get(i).get(j).toString().equals(list.get(i).get(j).toString()))
                }
            }
        }

        stage('Test findItemWithVersion') {
            def contentItems = [
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/18.0.0.99_NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/18.0.0.99_NDT-SNAPSHOT/</relativePath>',
                    '<text>18.0.0.99_NDT-SNAPSHOT</text>',
                    '<lastModified>2020-02-07 13:24:54.308 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/18.0.1.99_NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/18.0.1.99_NDT-SNAPSHOT/</relativePath>',
                    '<text>18.0.1.99_NDT-SNAPSHOT</text>',
                    '<lastModified>2020-06-25 09:34:36.89 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.1.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.1.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.1.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-07 12:18:30.741 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.2.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.2.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.2.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2020-05-18 14:33:49.848 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.3.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.3.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.3.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2020-07-21 13:23:49.228 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/19.4.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/19.4.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>19.4.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2020-09-09 14:12:40.182 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.0.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.0.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.0.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-02-12 16:01:32.864 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.1.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.1.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.1.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-17 15:45:08.60 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.2.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.2.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.2.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-12 09:20:09.825 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/21.3.99-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/21.3.99-NDT-SNAPSHOT/</relativePath>',
                    '<text>21.3.99-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-12 09:35:36.920 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/99.9.99-TRUNK-NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/99.9.99-TRUNK-NDT-SNAPSHOT/</relativePath>',
                    '<text>99.9.99-TRUNK-NDT-SNAPSHOT</text>',
                    '<lastModified>2021-05-07 12:18:36.80 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/99.9.9_TRUNK_NDT-SNAPSHOT/</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/99.9.9_TRUNK_NDT-SNAPSHOT/</relativePath>',
                    '<text>99.9.9_TRUNK_NDT-SNAPSHOT</text>',
                    '<lastModified>2020-03-05 12:50:26.630 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml</relativePath>',
                    '<text>maven-metadata.xml</text>',
                    '<lastModified>2021-06-15 08:20:41.56 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.md5</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.md5</relativePath>',
                    '<text>maven-metadata.xml.md5</text>',
                    '<lastModified>2021-06-15 08:20:41.56 UTC</lastModified>'
                    ],
                    ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</resourceURI>',
                    '<relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</relativePath>',
                    '<text>maven-metadata.xml.sha1</text>',
                    '<lastModified>2021-06-15 08:20:41.71 UTC</lastModified>'
                    ]
            ]
            def parsedCustomerDocumentationFile = ['natclipse']
            def attributedSortedContentItems
            parsedCustomerDocumentationFile.each { item ->
                attributedSortedContentItems = productDeliveryUtils.attributeDocumentation(contentItems, item)
            }
            attributedSortedContentItems = productDeliveryUtils.dateSort(attributedSortedContentItems)
            def mxVersions = [mxVersionUtils.getMxVersionForMxBuildVersion('21.0.0'), mxVersionUtils.getMxVersionForMxBuildVersion('18.0.0.03'), mxVersionUtils.getMxVersionForMxBuildVersion('19.2.04')]

            echo "Test of dateSort(). This requires a working attributeDocumentation(...) and dateSort(...)"

            def toDocuItems = []
            def oldSize = toDocuItems.size()
            mxVersions.each { mxVersion ->

                echo " Testing dateSort() with version ${mxVersion}"

                toDocuItems = productDeliveryUtils.findItemWithVersion(attributedSortedContentItems, toDocuItems, mxVersion)

                check(++oldSize, toDocuItems.size()) //only 1 new item should be added
                check(true, toDocuItems.get(toDocuItems.size() - 1).get(2).contains(mxVersion)) //assure correct version of new item
            }
        }

        stage('Test renameDocumentation') {
            /*
            * Since renameDocumentation(...) just moves the files to the specific new file format
            * it is more suitable to test the renameDocumentationRegExHelper() Method because 
            * this method creates the prefix for the new file format.
            */
            def namesToChange = ['Anwenderhandbuch_gui-importer-21.0.99-MDD-20210212.155716-4.pdf',
                                'user-manual_adaclipse-21.0.99-NDT-20210212.155945-5.pdf',
                                'user-manual_application-builder-21.0.99-MDD-20210212.155652-4.pdf',
                                'user-manual_cobolclipse-21.0.99-NDT-20210212.160103-5.pdf',
                                'user-manual_lifecycle-manager-21.0.99-NDT-20210212.160251-5.pdf',
                                'user-manual_natanalyzer-21.0.99-NDT-20210212.160024-5.pdf',
                                'user-manual_natclipse-21.0.99-NDT-20210212.160126-5.pdf',
                                'user-manual_soa-connector-21.0.99-SC-20210212.160156-3.pdf',
                                'user-manual_termx-21.0.99-SC-20210212.160003-4.pdf',
                                'user-manual_vaadin-21.0.99-MDD-20210212.155734-4.pdf',
                                'user-manual_vsam-21.0.99-MEE-20210212.155929-4.pdf']

            def expectedNames = ['Anwenderhandbuch_gui-importer-21.0.0.pdf',
                                'user-manual_adaclipse-21.0.0.pdf',
                                'user-manual_application-builder-21.0.0.pdf',
                                'user-manual_cobolclipse-21.0.0.pdf',
                                'user-manual_lifecycle-manager-21.0.0.pdf',
                                'user-manual_natanalyzer-21.0.0.pdf',
                                'user-manual_natclipse-21.0.0.pdf',
                                'user-manual_soa-connector-21.0.0.pdf',
                                'user-manual_termx-21.0.0.pdf',
                                'user-manual_vaadin-21.0.0.pdf',
                                'user-manual_vsam-21.0.0.pdf']

            echo "Test of renameDocumentation(...)"
            def mxBuildVersion = '21.0.0'

            for (int i = 0; i < namesToChange.size(); i++) {
                check(expectedNames.get(i), "${productDeliveryUtils.renameDocumentationRegExHelper(namesToChange.get(i))}${mxBuildVersion}.pdf")
            }
        }

        stage('Test isValidChild') {
            def validChild = ['<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</resourceURI>',
                            '<relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</relativePath>',
                            '<text>maven-metadata.xml.sha1</text>',
                            '<lastModified>2021-06-15 08:20:41.71 UTC</lastModified>'
            ]

            echo "Test of isValidChild(...)"

            check(true, productDeliveryUtils.isValidChild(validChild.get(0), validChild.get(1)))
            check(false, productDeliveryUtils.isValidChild(validChild.get(0), validChild.get(2)))
            check(false, productDeliveryUtils.isValidChild(validChild.get(0), validChild.get(3)))

            check(true, productDeliveryUtils.isValidChild(validChild.get(1), validChild.get(2)))
            check(false, productDeliveryUtils.isValidChild(validChild.get(1), validChild.get(3)))
            check(false, productDeliveryUtils.isValidChild(validChild.get(1), validChild.get(0)))

            check(true, productDeliveryUtils.isValidChild(validChild.get(2), validChild.get(3)))
            check(false, productDeliveryUtils.isValidChild(validChild.get(2), validChild.get(1)))
            check(false, productDeliveryUtils.isValidChild(validChild.get(2), validChild.get(2)))

            //<lastModified> is last entry and therefore can not have child items
            check(false, productDeliveryUtils.isValidChild(validChild.get(3), validChild.get(1)))
            check(false, productDeliveryUtils.isValidChild(validChild.get(3), validChild.get(2)))
            check(false, productDeliveryUtils.isValidChild(validChild.get(3), validChild.get(3)))
        }

        stage('Test isNewChild') {
        def xmlSnippet = '''<resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.md5</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.md5</relativePath>
        <text>maven-metadata.xml.md5</text>
        <lastModified>2021-06-15 08:20:41.56 UTC</lastModified>
        <resourceURI>http://triton.innowake.hq/nexus/service/local/repositories/snapshots-qm/content/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</resourceURI>
        <relativePath>/innowake/documentation/user-manual_natclipse/maven-metadata.xml.sha1</relativePath>
        <text>maven-metadata.xml.sha1</text>
        <lastModified>2021-06-15 08:20:41.71 UTC</lastModified>'''.split('\n').collect { it.trim() }

            echo "Test of isNewChild(...)"

            makeNewChildCounter = 0
            def old = null
            for (line in xmlSnippet) {
                if (old == null) {
                    old = line
                    continue
                } else {
                    if (productDeliveryUtils.isNewChild(old, line)) {
                        makeNewChildCounter++
                    }
                    old = line
                }
            }

            check(1, makeNewChildCounter)
        }

        stage('Test getFilePathAndFileName') {
            //BEWARE of file paths like ./myFile.txt since it is always expected that there is a folder in between like ./folder/myFile.txt
            //not for folders!
            def line = './myFile.txt'
            (fPath, fName) = productDeliveryUtils.getFilePathAndFileName(line)
            check('.', fPath)
            check('myFile.txt', fName)

            line = './root/myFile.txt'
            (fPath, fName) = productDeliveryUtils.getFilePathAndFileName(line)
            check('./root', fPath)
            check('myFile.txt', fName)

            line = './subfolder/and space folder/myFile.txt'
            (fPath, fName) = productDeliveryUtils.getFilePathAndFileName(line)
            check('./subfolder/and space folder', fPath)
            check('myFile.txt', fName)
        }
    }
}
/* We want to use the groovy power assertion instead of the 'normal' assert. 
* This is desireable because its output when failing the assertion is more meaningful than the normal one.
* Therefore every use of an assertion needs to be encapuslated by a @NonCPS method like this.
* !!This check does not terminate the pipeline process, it just turns stages into unstable with proper message!!
*/

@NonCPS
def check(expected, actual, boolean makeItUnstable = true) {
    checkHelp({assert expected == actual}, makeItUnstable)
}

@NonCPS
def checkHelp(Closure<?> assertion, boolean makeItUnstable) {
    try {
        assertion?.call()
    } catch (AssertionError aE) {
        if (makeItUnstable) {
            unstable "${aE}"
        } else {
            echo "${aE}"
        }
    }
}
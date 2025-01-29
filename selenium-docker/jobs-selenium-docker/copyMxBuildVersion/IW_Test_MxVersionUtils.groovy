def lib = library("TestUtils@${branchName}") // dynamically load library (does not work with @Library)

/**
 * Test of the MxVersionUtils
 *
 * @param branchName Name of the branch where changes are stored, default: master
 */
timestamps {
    def mxVersionUtils = lib.MxVersionUtils.new()
    node('OS-Linux') {
        buildName "#${env.BUILD_ID} - ${branchName}"

        deleteDir()      
        stage('test getMxVersionForMxBuildVersion') {
            def testValues = [
                'mx15_01', 'mx15_99',
                'mx15_IR_01', 'mx15_IR_99',
                'mx15_IR2_01', 'mx15_IR2_99',
                'mx15.1_01', 'mx15.1_99',
                'mx15.1_IR_01', 'mx15.1_IR_99',
                'mx15.1_IR2_01', 'mx15.1_IR2_99',
                'mx15.1_IR3_01', 'mx15.1_IR3_99',
                'mx16_01', 'mx16_99',
                'mx16_IR_01', 'mx16_IR_99',
                'mx16_IR2_01', 'mx16_IR2_99',
                'mx17_01', 'mx17_99', 'mx17_00_beta_20170102',
                'mx17_IR_01', 'mx17_IR_99', 'mx17_IR_00_beta_20170102',
                'mx17_IR2_01', 'mx17_IR2_99', 'mx17_IR2_00_beta_20170102',
                'mx17_IR3_01', 'mx17_IR3_99', 'mx17_IR3_00_beta_20170102',
                'mx17_IR4_01', 'mx17_IR4_99', 'mx17_IR4_00_beta_20180102',
                'mx17_IR5_01', 'mx17_IR5_99', 'mx17_IR5_00_beta_20180102',
                'mx18_01', 'mx18_99', 'mx18_00_beta_20180102',
                'mx18_IR_01', 'mx18_IR_99', 'mx18_IR_00_beta_20180102',
                'mx18_IR2_01', 'mx18_IR2_99', 'mx18_IR2_00_beta_20180102',
                'mx18_IR3_01', 'mx18_IR3_99', 'mx18_IR3_00_beta_20180102',
                'mx18_IR4_01', 'mx18_IR4_99', 'mx18_IR4_00_beta_20180102',
                '6.0.0.01', '6.0.0.99',
                '6.1.0.01', '6.1.0.99',
                '6.2.0.01', '6.2.0.99',
                '7.0.0.01', '7.0.0.99',
                '7.1.0.01', '7.1.0.99',
                '7.2.0.01', '7.2.0.99',
                '7.3.0.01', '7.3.0.99',
                '16.0.0.01', '16.0.0.99',
                '16.0.1.01', '16.0.1.99',
                '16.0.2.01', '16.0.2.99',
                '17.0.0.01', '17.0.0.99',
                '17.0.1.01', '17.0.1.99',
                '17.0.2.01', '17.0.2.99',
                '17.0.3.01', '17.0.3.99',
                '17.0.4.01', '17.0.4.99', '17.0.4.00_beta_201801152141',
                '17.0.5.01', '17.0.5.99', '17.0.5.00_beta_201801152141',
                '18.0.0.01', '18.0.0.99', '18.0.0.00_beta_201801152141',
                '18.0.1.01', '18.0.1.99', '18.0.1.00_beta_201801152141',
                '18.0.2.01', '18.0.2.99', '18.0.2.00_beta_201801152141',
                '18.0.3.01', '18.0.3.99', '18.0.3.00_beta_201801152141',
                '18.0.4.01', '18.0.4.99', '18.0.4.00_beta_201801152141',
                '19.0.0.01', '19.0.0.99', '19.0.0.00_beta_201801152141',
                '19.0.5.01', '19.0.5.99', '19.0.5.00_beta_201801152141',
                '19.1.01', '19.1.99', '19.1.00-beta-201912092026',
                '19.2.01', '19.2.99', '19.2.00-beta-201912092026',
                '19.3.01', '19.3.99', '19.3.00-beta-201912092026',
                '19.4.01', '19.4.99', '19.4.00-beta-201912092026',
                '21.0.01', '21.0.99', '21.0.00-beta-202001051505',
                '21.1.01', '21.1.99', '21.1.00-beta-202001051505',
                '21.2.01', '21.2.99', '21.2.00-beta-202001051505',
                '21.3.01', '21.3.99', '21.3.00-beta-202001051505',
                '21.4.01', '21.4.99', '21.4.00-beta-202001051505',
                '21.5.01', '21.5.99', '21.5.00-beta-202001051505',
                '21.6.01', '21.6.99', '21.6.00-beta-202001051505',
                '21.7.01', '21.7.99', '21.7.00-beta-202001051505',
                '22.0.01', '22.0.99', '22.0.00-beta-202001051505',
                '22.1.01', '22.1.99', '22.1.00-beta-202001051505',
                '22.2.01', '22.2.99', '22.2.0-beta-202208190313',
                '22.3.01', '22.3.99', '22.3.00-beta-202001051505',
                '19.5.00-alpha-202005121303', '21.5.0-alpha-202005121303',
                '23.0.01', '23.0.99', '23.0.0-beta-202208190313',
                '22.4.01', '22.4.99', '22.4.00-beta-202001051505',
                '23.1.01', '23.1.99', '23.1.0-beta-202208190313'
                
            ]
            def expectedValues = [
                'mx15', 'mx15',
                'mx15_IR', 'mx15_IR',
                'mx15_IR2', 'mx15_IR2',
                'mx15.1', 'mx15.1',
                'mx15.1_IR', 'mx15.1_IR',
                'mx15.1_IR2', 'mx15.1_IR2',
                'mx15.1_IR3', 'mx15.1_IR3',
                'mx16', 'mx16',
                'mx16_IR', 'mx16_IR',
                'mx16_IR2', 'mx16_IR2',
                'mx17', 'mx17', 'mx17',
                'mx17_IR', 'mx17_IR', 'mx17_IR',
                'mx17_IR2', 'mx17_IR2', 'mx17_IR2',
                'mx17_IR3', 'mx17_IR3', 'mx17_IR3',
                'mx17_IR4', 'mx17_IR4', 'mx17_IR4',
                'mx17_IR5', 'mx17_IR5', 'mx17_IR5',
                'mx18', 'mx18', 'mx18',
                'mx18_IR', 'mx18_IR', 'mx18_IR',
                'mx18_IR2', 'mx18_IR2', 'mx18_IR2',
                'mx18_IR3', 'mx18_IR3', 'mx18_IR3',
                'mx18_IR4', 'mx18_IR4', 'mx18_IR4',
                '6.0.0', '6.0.0',
                '6.1.0', '6.1.0',
                '6.2.0', '6.2.0',
                '7.0.0', '7.0.0',
                '7.1.0', '7.1.0',
                '7.2.0', '7.2.0',
                '7.3.0', '7.3.0',
                '16.0.0', '16.0.0',
                '16.0.1', '16.0.1',
                '16.0.2', '16.0.2',
                '17.0.0', '17.0.0',
                '17.0.1', '17.0.1',
                '17.0.2', '17.0.2',
                '17.0.3', '17.0.3',
                '17.0.4', '17.0.4', '17.0.4',
                '17.0.5', '17.0.5', '17.0.5',
                '18.0.0', '18.0.0', '18.0.0',
                '18.0.1', '18.0.1', '18.0.1',
                '18.0.2', '18.0.2', '18.0.2',
                '18.0.3', '18.0.3', '18.0.3',
                '18.0.4', '18.0.4', '18.0.4',
                '19.0.0', '19.0.0', '19.0.0',
                '19.0.5', '19.0.5', '19.0.5',
                '19.1', '19.1', '19.1',
                '19.2', '19.2', '19.2',
                '19.3', '19.3', '19.3',
                '19.4', '19.4', '19.4',
                '21.0', '21.0', '21.0',
                '21.1', '21.1', '21.1',
                '21.2', '21.2', '21.2',
                '21.3', '21.3', '21.3',
                '21.4', '21.4', '21.4',
                '21.5', '21.5', '21.5',
                '21.6', '21.6', '21.6',
                '21.7', '21.7', '21.7',
                '22.0', '22.0', '22.0',
                '22.1', '22.1', '22.1',
                '22.2', '22.2', '22.2',
                '22.3', '22.3', '22.3',
                '99.9', '99.9',
                '23.0', '23.0', '23.0',
                '22.4', '22.4', '22.4',
                '23.1', '23.1', '23.1',

            ]
            def testValue
            def expectedValue
            def actualValue

            echo 'Test of getMxVersionForMxBuildVersion()'
        
            /* Test with legal values */
            for (int i = 0; i < testValues.size(); i++) {
                testValue = testValues[i]
                expectedValue = expectedValues[i]
                actualValue = mxVersionUtils.getMxVersionForMxBuildVersion(testValue)
                echo "testValue: \"${testValue}\", expectedValue: \"${expectedValue}\", actualValue: \"${actualValue}\""
                if (! actualValue.toString().equals(expectedValue.toString())) {
                    error("actual value \"${actualValue}\" != expected value \"${expectedValue}\"")
                }
            }
        
            /* Test with an illegal value */
            def exceptionOccurred = true
            testValue = "mx9_999"
            try {
                actualValue = mxVersionUtils.getMxVersionForMxBuildVersion(testValue)
                exceptionOccurred = false
            } catch (ex) {
                echo "testValue: \"${testValue}\", results in an expected error: \"${ex}\""
            }
            if (! exceptionOccurred) {
                error("testValue: \"${testValue}\", should result in an error, but was mapped to actualValue: \"${actualValue}\"")
            }
        }

        stage('test normalizeMxBuildVersion') {
            def testValues = [
                'mx16_18', 'MX16_iR_18', 'mX16_Ir2_18',
                '6.0.0.01', '6.0.0.99',
                '7.3.0.01', '7.3.0.99',
                '16.0.0.01', '16.0.0.99',
                '17.0.0.01', '17.0.5.99', '17.0.5.00_beta_20180102',
                '18.0.0.01', '18.0.4.99', '18.0.4.00_beta_20180102',
                '19.0.0.01', '19.0.0.99', '19.1.01', '19.4.99', '19.4.00-beta-20180102',
                '19.1.00_alpha_20180102', '19.4.00-alpha-20180102'
                /* As this method is never used in our scripts it's not necessary to create new tests when branching a new version */
            ]
            def expectedValues = [
                'mx16_18', 'mx16_ir_18', 'mx16_ir2_18',
                '06.0.0.01', '06.0.0.99',
                '07.3.0.01', '07.3.0.99',
                '16.0.0.01', '16.0.0.99',
                '17.0.0.01', '17.0.5.99', '17.0.5.00_beta_20180102',
                '18.0.0.01', '18.0.4.99', '18.0.4.00_beta_20180102',
                '19.0.0.01', '19.0.0.99', '19.1.01', '19.4.99', '19.4.00-beta-20180102',
                '19.1.00_alpha_20180102', '19.4.00-alpha-20180102'
                /* As this method is never used in our scripts it's not necessary to create new tests when branching a new version */
            ]
            def testValue
            def expectedValue
            def actualValue
        
            echo 'Test of normalizeMxBuildVersion()'
        
            /* Test with legal values */
            for (int i = 0; i < testValues.size(); i++) {
                testValue = testValues[i]
                expectedValue = expectedValues[i]
                actualValue = mxVersionUtils.normalizeMxBuildVersion(testValue)
                echo "testValue: \"${testValue}\", expectedValue: \"${expectedValue}\", actualValue: \"${actualValue}\""
                if (! actualValue.equals(expectedValue)) {
                    error("actual value != expected value")
                }
            }
        }

        stage('test getEclipseZip') {
            def testValues = [
                '6.0.0.01', '6.2.0.01',
                '7.0.0.01', '7.3.0.01',
                '16.0.0.01', '16.0.2.01',
                '17.0.0.01', '17.0.5.01',
                '18.0.0.01', '18.0.1.01', 
                '18.0.2.01', '18.0.3.01', 
                '18.0.4.01', '18.0.5.01',
                '19.0.0.01', '19.1.01', '19.2.01', '19.3.01', '19.0.5.01',
                '19.4.01','21.0.01', '21.1.01', '21.2.01', '21.3.01', '21.4.01', '21.5.01','21.6.01','21.7.01', '22.0.01', 
                '22.1.01','19.5.00-alpha-202005121303', '21.5.0-alpha-202105121303', '22.3.0-alpha-202208091556', '22.2.0-beta-202208190313','22.3.01','23.0.01', '22.4.01','23.1.01'
            ]
            def expectedValues = [
                'eclipse-standard-kepler-SR2-linux-gtk-x86_64.tar.gz', 'eclipse-standard-kepler-SR2-linux-gtk-x86_64.tar.gz',
                'eclipse-standard-kepler-SR2-linux-gtk-x86_64.tar.gz', 'eclipse-standard-kepler-SR2-linux-gtk-x86_64.tar.gz',
                'eclipse-SDK-4.5.2-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.5.2-linux-gtk-x86_64.tar.gz',
                'eclipse-SDK-4.5.2-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.5.2-linux-gtk-x86_64.tar.gz',
                'eclipse-SDK-4.6.3-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.6.3-linux-gtk-x86_64.tar.gz',
                'eclipse-SDK-4.9-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.9-linux-gtk-x86_64.tar.gz',
                'eclipse-SDK-4.9-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.9-linux-gtk-x86_64.tar.gz',
                'eclipse-SDK-4.11-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.11-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.11-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.11-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.11-linux-gtk-x86_64.tar.gz',
                'eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.19-linux-gtk-x86_64.tar.gz',
                'eclipse-SDK-4.19-linux-gtk-x86_64.tar.gz' ,'eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz','eclipse-SDK-4.15-linux-gtk-x86_64.tar.gz','eclipse-SDK-4.24-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.19-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.24-linux-gtk-x86_64.tar.gz', 'eclipse-SDK-4.24-linux-gtk-x86_64.tar.gz','eclipse-SDK-4.24-linux-gtk-x86_64.tar.gz','eclipse-SDK-4.24-linux-gtk-x86_64.tar.gz'

            ]
                
            echo 'Test of getEclipseZip()'
        
            for (int i = 0; i < testValues.size(); i++) {
                testValue = testValues[i]
                expectedValue = expectedValues[i]
                actualValue = mxVersionUtils.getEclipseZip(testValue)
                echo "testValue: \"${testValue}\", expectedValue: \"${expectedValue}\", actualValue: \"${actualValue}\""
                if (! actualValue.equals(expectedValue)) {
                    error('actual value != expected value')
                }
            }
        }

        stage('test getVersionOnAboutDialog') {
            def testValues = [
                '7.2.0.01', '7.2.0.99',
                '16.0.0.01', '16.0.0.99',
                '16.0.1.01', '16.0.1.99',
                '16.0.2.01', '16.0.2.99',
                '17.0.0.01', '17.0.0.99', '17.0.0.00_beta_20170102',
                '17.0.1.01', '17.0.1.99', '17.0.1.00_beta_20170102',
                '17.0.2.01', '17.0.2.99', '17.0.2.00_beta_20170102',
                '17.0.3.01', '17.0.3.99', '17.0.3.00_beta_20170102',
                '17.0.4.01', '17.0.4.99', '17.0.4.00_beta_20180102',
                '17.0.5.01', '17.0.5.99', '17.0.5.00_beta_20180102',
                '18.0.0.01', '18.0.0.99', '18.0.0.00_beta_20180102',
                '18.0.1.01', '18.0.1.99', '18.0.1.00_beta_20180102',
                '18.0.2.01', '18.0.2.99', '18.0.2.00_beta_20180102',
                '18.0.3.01', '18.0.3.99', '18.0.3.00_beta_20180102',
                '18.0.4.01', '18.0.4.99', '18.0.4.00_beta_20180102',
                '19.0.0.01', '19.0.0.99', '19.0.0.00_beta_20180102',
                '19.0.5.01', '19.0.5.99', '19.0.5.00_beta_20180102',
                '19.1.01', '19.1.99', '19.1.00-beta-201912212035',
                '19.2.01', '19.2.99', '19.2.00-beta-201912212035',
                '19.3.01', '19.3.99', '19.3.00-beta-201912212035',
                '19.4.01', '19.4.99', '19.4.00-beta-201912212035',
                '21.0.01', '21.0.99', '21.0.00-beta-202001051505',
                '21.1.01', '21.1.99', '21.1.00-beta-202001051505',
                '21.2.01', '21.2.99', '21.2.00-beta-202001051505',
                '21.3.01', '21.3.99', '21.3.00-beta-202001051505',
                '21.4.01', '21.4.99', '21.4.00-beta-202001051505',
                '21.5.01', '21.5.99', '21.5.00-beta-202001051505',
                '21.6.01', '21.6.99', '21.6.00-beta-202001051505',
                '21.7.01', '21.7.99', '21.7.00-beta-202001051505',
                '22.0.01', '22.0.99', '22.0.00-beta-202001051505',
                '22.1.01', '22.1.99', '22.1.00-beta-202001051505',
                '22.3.01', '22.3.99', '22.3.00-beta-202001051505',
                '19.5.00-alpha-202005121303', '21.5.0-alpha-202105121303',
                '23.0.01', '23.0.99', '23.0.0-beta-202208190313',
                '22.4.01', '22.4.99', '22.4.00-beta-202001051505',
                '23.1.01', '23.1.99', '23.1.00-beta-202001051505'

                
            ]
            def expectedValues = [
                '15.1 IR2', '15.1 IR2',
                '16.0', '16.0',
                '16 IR', '16 IR',
                '16 IR2', '16 IR2',
                '17.0', '17.0', '17.0 beta_20170102',
                '17 IR', '17 IR', '17 IR beta_20170102',
                '17 IR2', '17 IR2', '17 IR2 beta_20170102',
                '17 IR3', '17 IR3', '17 IR3 beta_20170102',
                '17 IR4', '17 IR4', '17 IR4 beta_20180102',
                '17 IR5', '17 IR5', '17 IR5 beta_20180102',
                '18', '18', '18 beta_20180102',
                '18 IR', '18 IR', '18 IR beta_20180102',
                '18 IR2', '18 IR2', '18 IR2 beta_20180102',
                '18 IR3', '18 IR3', '18 IR3 beta_20180102',
                '18 IR4', '18 IR4', '18 IR4 beta_20180102',
                '19', '19', '19 beta_20180102',
                '19 IR5', '19 IR5', '19 IR5 beta_20180102',
                '19 IR', '19 IR', '19 IR beta-201912212035', 
                '19 IR2', '19 IR2', '19 IR2 beta-201912212035', 
                '19 IR3', '19 IR3', '19 IR3 beta-201912212035',
                '19 IR4', '19 IR4', '19 IR4 beta-201912212035', 
                '21', '21', '21 beta-202001051505',
                '21 IR', '21 IR', '21 IR beta-202001051505',
                '21 IR2', '21 IR2', '21 IR2 beta-202001051505',
                '21 IR3', '21 IR3', '21 IR3 beta-202001051505',
                '21 IR4', '21 IR4', '21 IR4 beta-202001051505',
                '21 IR5', '21 IR5', '21 IR5 beta-202001051505',
                '21 IR6', '21 IR6', '21 IR6 beta-202001051505',
                '21 IR7', '21 IR7', '21 IR7 beta-202001051505',
                '22', '22', '22 beta-202001051505',
                '22 IR', '22 IR', '22 IR beta-202001051505',
                '22 IR3', '22 IR3', '22 IR3 beta-202001051505',
                'TRUNK alpha-202005121303', 'TRUNK alpha-202105121303',
                '23', '23', '23 beta-202208190313',
                '22 IR4', '22 IR4', '22 IR4 beta-202001051505',
                '23 IR', '23 IR', '23 IR beta-202001051505'

            ]
            def testValue
            def expectedValue
            def actualValue
        
            echo 'Test of getVersionOnAboutDialog()'
        
            /* Test with legal values */
            for (int i = 0; i < testValues.size(); i++) {
                testValue = testValues[i]
                expectedValue = expectedValues[i]
                actualValue = mxVersionUtils.getVersionOnAboutDialog(testValue)
                echo "testValue: \"${testValue}\", expectedValue: \"${expectedValue}\", actualValue: \"${actualValue}\""
                if (! actualValue.toString().equals(expectedValue.toString())) {
                    error("actual value \"${actualValue}\" != expected value \"${expectedValue}\"")
                }
            }
        
            /* Test with an illegal value */
            def exceptionOccurred = true
            testValue = "1.2043"
            try {
                actualValue = mxVersionUtils.getVersionOnAboutDialog(testValue)
                exceptionOccurred = false
            } catch (ex) {
                echo "testValue: \"${testValue}\", results in an expected error: \"${ex}\""
            }
            if (! exceptionOccurred) {
                error("testValue: \"${testValue}\", should result in an error, but was mapped to actualValue \"${actualValue}\"")
            }
        }
        
        stage('test hasMaxensoInArtifactName') {
            def testValues = [
                '7.2.0.01', '7.2.0.99',
                '16.0.0.01', '16.0.0.99',
                '16.0.1.01', '16.0.1.99',
                '16.0.2.01', '16.0.2.99',
                '17.0.0.01', '17.0.0.99', '17.0.0.00_beta_20170102',
                '17.0.1.01', '17.0.1.99', '17.0.1.00_beta_20170102',
                '17.0.2.01', '17.0.2.99', '17.0.2.00_beta_20170102',
                '17.0.3.01', '17.0.3.99', '17.0.3.00_beta_20170102',
                '17.0.4.01', '17.0.4.99', '17.0.4.00_beta_20180102',
                '17.0.5.01', '17.0.5.99', '17.0.5.00_beta_20180102',
                '18.0.0.01', '18.0.0.99', '18.0.0.00_beta_20180102',
                '18.0.1.01', '18.0.1.99', '18.0.1.00_beta_20180102',
                '18.0.2.01', '18.0.2.99', '18.0.2.00_beta_20180102',
                '18.0.3.01', '18.0.3.99', '18.0.3.00_beta_20180102',
                '18.0.4.01', '18.0.4.99', '18.0.4.00_beta_20180102',
                '19.0.0.01', '19.0.0.99', '19.0.0.00_beta_20180102',
                '19.1.01', '19.1.99', '19.1.00-beta-201912212035',
                '19.2.01', '19.2.99', '19.2.00-beta-201912212035',
                '19.3.01', '19.3.99', '19.3.00-beta-201912212035',
                '19.4.01', '19.4.99', '19.4.00-beta-201912212035',
                '19.5.00-alpha-202005121303', '19.5.00-alpha-202011100904', '19.5.00-alpha-202101010904', '21.5.0-alpha-202102080433'
            ]
            def expectedValues = [
                'true', 'true',
                'true', 'true',
                'true', 'true',
                'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'true', 'true',
                'true', 'false', 'false', 'false'
            ]
            def testValue
            def expectedValue
            def actualValue
        
            echo 'Test of hasMaxensoInArtifactName()'
        
            /* Test with legal values */
            for (int i = 0; i < testValues.size(); i++) {
                testValue = testValues[i]
                expectedValue = expectedValues[i]
                actualValue = mxVersionUtils.hasMaxensoInArtifactName(testValue)
                echo "testValue: \"${testValue}\", expectedValue: \"${expectedValue}\", actualValue: \"${actualValue}\""
                if (! actualValue.toString().equals(expectedValue.toString())) {
                    error("actual value \"${actualValue}\" != expected value \"${expectedValue}\"")
                }
            }
        }
        
        stage('test removeLeadingZerosAtLastPosition') {
            def testValues = [
                '19.1.01', '19.1.99', '19.1.00-beta-201912212035',
                '19.2.01', '19.2.99', '19.2.00-beta-201912212035',
                '19.3.01', '19.3.99', '19.3.00-beta-201912212035',
                '19.4.01', '19.4.99', '19.4.00-beta-201912212035',
                '21.0.0', '21.0.99', '21.0.0-beta-201912212035',
                '21.1.0', '21.1.99', '21.1.0-beta-201912212035',
                '21.2.0', '21.2.99', '21.2.0-beta-201912212035',
                '21.3.0', '21.3.99', '21.3.0-beta-201912212035',
                '21.4.0', '21.4.99', '21.4.0-beta-201912212035',
                '19.5.0-alpha-202005121303', '21.5.0-alpha-202102080433'
            ]
            def expectedValues = [
                '19.1.1', '19.1.99', '19.1.0-beta-201912212035',
                '19.2.1', '19.2.99', '19.2.0-beta-201912212035',
                '19.3.1', '19.3.99', '19.3.0-beta-201912212035',
                '19.4.1', '19.4.99', '19.4.0-beta-201912212035',
                '21.0.0', '21.0.99', '21.0.0-beta-201912212035',
                '21.1.0', '21.1.99', '21.1.0-beta-201912212035',
                '21.2.0', '21.2.99', '21.2.0-beta-201912212035',
                '21.3.0', '21.3.99', '21.3.0-beta-201912212035',
                '21.4.0', '21.4.99', '21.4.0-beta-201912212035',
                '19.5.0-alpha-202005121303', '21.5.0-alpha-202102080433'
            ]
            def testValue
            def expectedValue
            def actualValue
        
            echo 'Test of removeLeadingZerosAtLastPosition()'
        
            /* Test with legal values */
            for (int i = 0; i < testValues.size(); i++) {
                testValue = testValues[i]
                expectedValue = expectedValues[i]
                actualValue = mxVersionUtils.removeLeadingZerosAtLastPosition(testValue)
                echo "testValue: \"${testValue}\", expectedValue: \"${expectedValue}\", actualValue: \"${actualValue}\""
                if (! actualValue.toString().equals(expectedValue.toString())) {
                    error("actual value \"${actualValue}\" != expected value \"${expectedValue}\"")
                }
            }
        }   
    }
}

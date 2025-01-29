@Library('TestUtils') _

/**
 * Test of the Utils methods for average calculating
 */
node {
    timestamps() {
        def perfUtils = new PerformanceUtils()
        
        deleteDir()
        stage('test get2DList()') {
            def listToTest = [
                'Id,Invocations,Own/µs,Own Min/µs,Own Avg/µs,Own Max/µs',
                'innowake.mee.ado.execsql',
                'innowake.mee.cics.api',
                'innowake.mee.cics.runtime.engine.run',
                'innowake.mee.exec.sql.execute-command',
                'innowake.mee.natural.runtime.controlflow',
                'mee.runtime.cobol.api',
                'mee.runtime.natural.api'
            ]
            def listToCompare = [
                ['innowake.mee.ado.execsql'],
                ['innowake.mee.cics.api'],
                ['innowake.mee.cics.runtime.engine.run'],
                ['innowake.mee.exec.sql.execute-command'],
                ['innowake.mee.natural.runtime.controlflow'],
                ['mee.runtime.cobol.api'],
                ['mee.runtime.natural.api']
            ]
           
            writeCSV file: 'test.csv', records: listToTest
            def actualValues = perfUtils.get2DList('test.csv')
            assert actualValues.toString().equals(listToCompare.toString())
            echo "Test passed"
        }
            
        stage('test compareSortedLoggerLists()') {
            def loggerList =  [
                ['innowake.mee.ado.execsql', '2651144', '3543580253', '0', '1336.62', '12970324'],
                ['innowake.mee.cics.api', '335720', '739492356', '0', '2202.71', '540267'],
                ['innowake.mee.cics.runtime.engine.run', '108', '33275526', '67110', '308106.72', '3534628'],
                ['innowake.mee.exec.sql.execute-command', '1283424', '311289164', '12', '242.55', '1444437'],
                ['innowake.mee.natural.runtime.controlflow', '11975118', '543628981', '0', '45.4', '1360703'],
                ['innowake.mee.natural.runtime.core.data.loop', '12451', '94798', '0', '7.61', '14972'],
                ['innowake.mee.natural.runtime.engine', '78', '1231897', '59', '15793.55', '82592'],
                ['mee.jcl.api.tso', '183', '576901', '2281', '3152.46', '23384'],
                ['mee.jcl.runner', '144', '18845948', '1', '130874.64', '1820711'],
                ['mee.runtime.cobol.api', '34855352', '955702315', '0', '27.42', '832900'],
                ['mee.runtime.natural.api', '1622785', '2860558', '0', '1.76', '47609'],
                ['total', '52736507', '6150578697', '0', '116.63', '12970324']
            ]
            def loggerListToCompare =  [
                ['innowake.mee.ado.execsql', '2651144', '3543580253', '0', '1336.62', '12970324'],
                ['innowake.mee.cics.api', '335720', '739492356', '0', '2202.71', '540267'],
                ['innowake.mee.cics.runtime.engine.run', '108', '33275526', '67110', '308106.72', '3534628'],
                ['innowake.mee.exec.sql.execute-command', '1283424', '311289164', '12', '242.55', '1444437'],
                ['innowake.mee.natural.runtime.controlflow', '11975118', '543628981', '0', '45.4', '1360703'],
                ['innowake.mee.natural.runtime.core.data.loop', '12451', '94798', '0', '7.61', '14972'],
                ['innowake.mee.natural.runtime.engine', '78', '1231897', '59', '15793.55', '82592'],               
                ['mee.runtime.natural.api', '1622785', '2860558', '0', '1.76', '47609'],
                ['total', '52736507', '6150578697', '0', '116.63', '12970324']
            ]
           
            // test with different loggers
            def message = 'Here the difference between logger lists.\nTest passed'
            if(perfUtils.compareSortedLoggerLists(loggerList, loggerListToCompare)) {
                message = 'No difference between logger lists'
                unstable message
            }
            echo message
            
            // test with the same logger in lists
            message = 'No difference between logger lists.\nTest passed'
            if(!perfUtils.compareSortedLoggerLists(loggerList, loggerList)) {
                message = 'Here the difference between logger lists.'
                unstable message
            }
            echo message
        }    
        
        stage('test calculateAverageOfSingleLogger()') {
            def logger2DList = [
                ['mee.runtime.ims.api', '27000005', '16992275', '0', '0', '25599'],
                ['mee.runtime.ims.api', '4500004', '10591158', '1', '2', '17985'],
                ['mee.runtime.ims.api', '41', '294380', '11', '7180', '259343']
            ]
            def expectedValues = ['mee.runtime.ims.api', 10500016 as Long, 9292604 as Long, 4 as Long, 2394 as double, 100975 as Long]
            def actualValues = perfUtils.calculateAverageOfSingleLogger(logger2DList)
                    
            // Positive test cases
            message = 'No difference between expected und calculated values.\nTest passed'
            for(int i = 0; i < expectedValues.size(); i++) {
                    if(actualValues[i] != expectedValues[i]) {
                        message = 'Here the difference between logger lists.\nTest failed'
                        echo "ActualValues: ${actualValues[i]}\nExpected values: ${expectedValues[i]}"
                    }
            } 
            assert message.contains('Test passed')            
        }
        
        stage('test calculateAverage') {
            def testValues = [
                [
                    ['innowake.mee.ado.execsql', '2651144', '3543580253', '0', '1336.62', '12970324'],
                    ['innowake.mee.cics.api', '335720', '739492356', '0', '2202.71', '540267'],
                    ['innowake.mee.cics.runtime.engine.run', '108', '33275526', '67110', '308106.72', '3534628'],
                    ['innowake.mee.exec.sql.execute-command', '1283424', '311289164', '12', '242.55', '1444437'],
                    ['innowake.mee.natural.runtime.controlflow', '11975118', '543628981', '0', '45.4', '1360703'],
                    ['innowake.mee.natural.runtime.core.data.loop', '12451', '94798', '0', '7.61', '14972'],
                    ['innowake.mee.natural.runtime.engine', '78', '1231897', '59', '15793.55', '82592'],
                    ['mee.jcl.api.tso', '183', '576901', '2281', '3152.46', '23384'],
                    ['mee.jcl.runner', '144', '18845948', '1', '130874.64', '1820711'],
                    ['mee.runtime.cobol.api', '34855352', '955702315', '0', '27.42', '832900'],
                    ['mee.runtime.natural.api', '1622785', '2860558', '0', '1.76', '47609'],
                    ['total', '52736507', '6150578697', '0', '116.63', '12970324']
                ],
                [
                    ['innowake.mee.ado.execsql', '2651144', '3041549849', '0', '1147.26', '13481777'],
                    ['innowake.mee.cics.api', '335720', '773152534', '0', '2302.97', '675459'],
                    ['innowake.mee.cics.runtime.engine.run', '108', '35265210', '73297', '326529.72', '3712763'],
                    ['innowake.mee.exec.sql.execute-command', '1283424', '272012303', '12', '211.94', '1653814'],
                    ['innowake.mee.natural.runtime.controlflow', '11975118', '560703309', '0', '46.82', '1403276'],
                    ['innowake.mee.natural.runtime.core.data.loop', '12451', '98073', '0', '7.88', '13231'],
                    ['innowake.mee.natural.runtime.engine', '78', '1185111', '59', '15193.73', '81987'],
                    ['mee.jcl.api.tso', '183', '571066', '2085', '3120.58', '27443'],
                    ['mee.jcl.runner', '144', '18924957', '1', '131423.31', '1962376'],
                    ['mee.runtime.cobol.api', '34855352', '986419392', '0', '28.3', '831797'],
                    ['mee.runtime.natural.api', '1622785', '2878412', '0', '1.77', '56019'],
                    ['total', '52736507', '5692760216', '0', '107.95', '13481777']
                ],
                [
                    ['innowake.mee.ado.execsql', '2651144', '3256620449', '0', '1228.38', '12981842'],
                    ['innowake.mee.cics.api', '335720', '777676534', '0', '2316.44', '577831'],
                    ['innowake.mee.cics.runtime.engine.run', '108', '34679916', '63851', '321110.33', '3570858'],
                    ['innowake.mee.exec.sql.execute-command', '1283424', '299564592', '13', '233.41', '1437292'],
                    ['innowake.mee.natural.runtime.controlflow', '11975118', '572148308', '0', '47.78', '1403542'],
                    ['innowake.mee.natural.runtime.core.data.loop', '12451', '83679', '0', '6.72', '13708'],
                    ['innowake.mee.natural.runtime.engine', '78', '1404885', '60', '18011.35', '194208'],
                    ['mee.jcl.api.tso', '183', '611918', '2240', '3343.81', '36413'],
                    ['mee.jcl.runner', '144', '19670508', '1', '136600.75', '1831547'],
                    ['mee.runtime.cobol.api', '34855352', '1002648620', '0', '28.77', '860233'],
                    ['mee.runtime.natural.api', '1622785', '2949022', '0', '1.82', '42935'],
                    ['total', '52736507', '5968058431', '0', '113.17', '12981842']
                ]
            ]
            def expectedValues = [
                ['innowake.mee.ado.execsql', '2651144',	'3280583517',	'0', '1237.42', '13144647'],
                ['innowake.mee.cics.api',	'335720', '763440474',	'0',	'2274.04',	'597852'],
                ['innowake.mee.cics.runtime.engine.run',	'108',	'34406884',	'68086',	'318582.26',	'3606083'],
                ['innowake.mee.exec.sql.execute-command',	'1283424',	'294288686',	'12',	'229.3',	'1511847'],
                ['innowake.mee.natural.runtime.controlflow',	'11975118',	'558826866',	'0',	'46.67',	'1389173'],
                ['innowake.mee.natural.runtime.core.data.loop',	'12451',	'92183',	'0',	'7.4',	'13970'],
                ['innowake.mee.natural.runtime.engine',	'78',	'1273964',	'59',	'16332.88',	'119595'],
                ['mee.jcl.api.tso',	'183',	'586628',	'2202',	'3205.62',	'29080'],
                ['mee.jcl.runner',	'144',	'19147137',	'1',	'132966.23',	'1871544'],
                ['mee.runtime.cobol.api',	'34855352',	'981590109',	'0',	'28.16',	'841643'],
                ['mee.runtime.natural.api',	'1622785',	'2895997',	'0',	'1.78',	'48854'],
                ['total',	'52736507',	'5937132448',	'0',	'112.58',	'13144647']
            ]

            //positive test case 
            def actualValues = perfUtils.calculateAverage(testValues)
            message = 'No difference between expected und calculated values.\nTest passed'
            for(int i = 0; i < expectedValues.size(); i++) {
                for(int j = 0; j < expectedValues[i].size(); j++) {
                    if(actualValues[i][j].toString() != expectedValues[i][j].toString()) {
                        message = 'Here the difference between logger lists.'
                        echo "AveragedLogRecords: ${actualValues[i]}\nExpected values: ${expectedValues[i]}"
                    }
                }
            } 
            assert message.contains('Test passed')

            // negative test case 
            expectedValues = [
                ['innowake.mee.ado.execsql', '32659',	'3280583517',	'0', '1237.42', '13144647'],
                ['innowake.mee.cics.api',	'335720', '763440474',	'0',	'2274.04',	'597852'],
                ['innowake.mee.cics.runtime.engine.run',	'108',	'34406884',	'68086',	'318582.26',	'3606083'],
                ['innowake.mee.exec.sql.execute-command',	'1283424',	'294288686',	'12',	'229.3',	'1511847'],
                ['innowake.mee.natural.runtime.controlflow',	'11975118',	'558826866',	'0',	'46.67',	'1389173'],
                ['innowake.mee.natural.runtime.core.data.loop',	'12451',	'92183',	'0',	'7.4',	'13970'],
                ['innowake.mee.natural.runtime.engine',	'78',	'1273964',	'59',	'16332.88',	'119595'],
                ['mee.jcl.api.tso',	'183',	'586628',	'2202',	'3205.62',	'29080'],
                ['mee.jcl.runner',	'144',	'19147137',	'1',	'132966.23',	'1871544'],
                ['mee.runtime.cobol.api',	'34855352',	'981590109',	'0',	'28.16',	'841643'],
                ['mee.runtime.natural.api',	'1622785',	'2895997',	'0',	'1.78',	'48854'],
                ['total',	'52736507',	'5937132448',	'0',	'112.58',	'13144647']
            ]
            
            actualValues = perfUtils.calculateAverage(testValues)
            message = 'No difference between expected und calculated values.\n Test failed'
            for(int i = 0; i < expectedValues.size(); i++) {
                for(int j = 0; j < expectedValues[i].size(); j++) {
                    if(actualValues[i][j].toString() != expectedValues[i][j].toString()) {
                        message = 'Here the difference between logger lists.\nTest passed'
                    }
                }
            } 
            assert message.contains('Test passed')
        }
    }
}

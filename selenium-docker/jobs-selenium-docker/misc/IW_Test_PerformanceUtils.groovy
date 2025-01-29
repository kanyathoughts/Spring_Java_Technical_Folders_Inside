def lib = library("TestUtils@${branchName}")

/**
* Test job for PerformanceUtils methods.
*
* @param branchName Name of the branch where changes are stored, default: master
*/
timestamps {
        def perfUtils = lib.PerformanceUtils.new()
        ['OS-Linux', 'OS-Windows'].each { labelExpression ->
                node(labelExpression) {

                        def profilingLogHeader = 'Id,Invocations,Own/µs,Own Min/µs,Own Avg/µs,Own Max/µs\r\n'
                        def PAGE_ID_OF_EMPTY_CONFLUENCE_PAGE = '166889918'
                        def initialChartTableRow =
                                ('           <tr>' +
                                        '                <th><strong>ID</strong></th>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.dataset.api</strong></td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.natural.runtime.controlflow</strong></td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>mee.jcl.runner</strong></td>' +
                                        '            </tr>').replaceAll('\\s+<', '<')
                        def chartTableRow0903 =
                                ('           <tr>' +
                                        '                <th><strong>ID</strong></th>' +
                                        '                <th>21.5a0903</th>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.dataset.api</strong></td>' +
                                        '                <td>27202151</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.natural.runtime.controlflow</strong></td>' +
                                        '                <td>27202152</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>mee.jcl.runner</strong></td>' +
                                        '                <td>27202153</td>' +
                                        '            </tr>').replaceAll('\\s+<', '<')
                        def chartTableRow0904 =
                                ('           <tr>' +
                                        '                <th><strong>ID</strong></th>' +
                                        '                <th>21.5a0904</th>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.dataset.api</strong></td>' +
                                        '                <td>27000005</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.natural.runtime.controlflow</strong></td>' +
                                        '                <td>4500004</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>mee.jcl.runner</strong></td>' +
                                        '                <td>41</td>' +
                                        '            </tr>').replaceAll('\\s+<', '<')
                        def chartTableRow0903And0904 =
                                ('           <tr>' +
                                        '                <th><strong>ID</strong></th>' +
                                        '                <th>21.5a0903</th>' +
                                        '                <th>21.5a0904</th>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.dataset.api</strong></td>' +
                                        '                <td>27202151</td>' +
                                        '                <td>27000005</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.natural.runtime.controlflow</strong></td>' +
                                        '                <td>27202152</td>' +
                                        '                <td>4500004</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>mee.jcl.runner</strong></td>' +
                                        '                <td>27202153</td>' +
                                        '                <td>41</td>' +
                                        '            </tr>').replaceAll('\\s+<', '<')
                        def chartTableRow0903And0904MinusControlflowValue0904 =
                                ('           <tr>' +
                                        '                <th><strong>ID</strong></th>' +
                                        '                <th>21.5a0903</th>' +
                                        '                <th>21.5a0904</th>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.dataset.api</strong></td>' +
                                        '                <td>27202151</td>' +
                                        '                <td>27000005</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.natural.runtime.controlflow</strong></td>' +
                                        '                <td>27202152</td>' +
                                        '                <td></td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>mee.jcl.runner</strong></td>' +
                                        '                <td>27202153</td>' +
                                        '                <td>41</td>' +
                                        '            </tr>').replaceAll('\\s+<', '<')
                        def chartTableRow0903And0904ExtraCategories0904 =
                                ('           <tr>' +
                                        '                <th><strong>ID</strong></th>' +
                                        '                <th>21.5a0903</th>' +
                                        '                <th>21.5a0904</th>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>a.extra.category</strong></td>' +
                                        '                <td></td>' +
                                        '                <td>0</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>b.extra.category</strong></td>' +
                                        '                <td></td>' +
                                        '                <td>0</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.dataset.api</strong></td>' +
                                        '                <td>27202151</td>' +
                                        '                <td>27000005</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.natural.runtime.controlflow</strong></td>' +
                                        '                <td>27202152</td>' +
                                        '                <td>4500004</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>j.extra.category</strong></td>' +
                                        '                <td></td>' +
                                        '                <td>0</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>k.extra.category</strong></td>' +
                                        '                <td></td>' +
                                        '                <td>0</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>mee.jcl.runner</strong></td>' +
                                        '                <td>27202153</td>' +
                                        '                <td>41</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>y.extra.category</strong></td>' +
                                        '                <td></td>' +
                                        '                <td>0</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>z.extra.category</strong></td>' +
                                        '                <td></td>' +
                                        '                <td>0</td>' +
                                        '            </tr>').replaceAll('\\s+<', '<')
                        def chartTableRow0905 =
                                ('           <tr>' +
                                        '                <th><strong>ID</strong></th>' +
                                        '                <th>21.5a0905</th>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.dataset.api</strong></td>' +
                                        '                <td>25599</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>innowake.mee.natural.runtime.controlflow</strong></td>' +
                                        '                <td>17985</td>' +
                                        '            </tr>' +
                                        '            <tr>' +
                                        '                <td><strong>mee.jcl.runner</strong></td>' +
                                        '                <td>259343</td>' +
                                        '            </tr>').replaceAll('\\s+<', '<')
                        def pageContent0903 =
                                ('<table class="wrapped">' +
                                        '    <colgroup>' +
                                        '        <col>' +
                                        '        <col>' +
                                        '        <col>' +
                                        '        <col>' +
                                        '        <col>' +
                                        '        <col>' +
                                        '        <col>' +
                                        '        <col>' +
                                        '    </colgroup>' +
                                        '    <tbody>' +
                                        '    <tr>' +
                                        '        <th><p><strong>Maxenso Build Version</strong></p></th>' +
                                        '        <th><p><strong>Id</strong></p></th>' +
                                        '        <th><p><strong>Invocations</strong></p></th>' +
                                        '        <th><p><strong>Own/&micro;s</strong></p></th>' +
                                        '        <th><p><strong>Own Min/&micro;s</strong></p></th>' +
                                        '        <th><p><strong>Own Avg/&micro;s</strong></p></th>' +
                                        '        <th><p><strong>Own Max/&micro;s</strong></p></th>' +
                                        '        <th><p><strong>Jenkins Build URL</strong></p></th>' +
                                        '    </tr>' +
                                        '    <tr>' +
                                        '        <td>21.5.0-alpha-20210903421</td>' +
                                        '        <td><p>innowake.mee.dataset.api</p>' +
                                        '            <p>innowake.mee.natural.runtime.controlflow</p>' +
                                        '            <p>mee.jcl.runner</p></td>' +
                                        '        <td><p>27202151</p>' +
                                        '            <p>27202152</p>' +
                                        '            <p>27202153</p></td>' +
                                        '        <td><p>22613805</p>' +
                                        '            <p>22613806</p>' +
                                        '            <p>22613807</p></td>' +
                                        '        <td><p>14312</p>' +
                                        '            <p>14313</p>' +
                                        '            <p>14314</p></td>' +
                                        '        <td><p>5294057</p>' +
                                        '            <p>5294058</p>' +
                                        '            <p>5294059</p></td>' +
                                        '        <td><p>786024</p>' +
                                        '            <p>786025</p>' +
                                        '            <p>786026</p></td>' +
                                        '       <td>' +
                                        '            <a href="http://qef-linux1.deloitte.com:8085/job/NMSLO_run/503">http://qef-linux1.deloitte.com:8085/job/NMSLO_run/503</a>' +
                                        '       </td>' +
                                        '    </tr>' +
                                        '    </tbody>' +
                                        '</table><p class="auto-cursor-target"><br></p>' +
                                        '<ac:structured-macro ac:name="chart" ac:schema-version="1" ac:macro-id="7d380b1b-1ca5-4341-884b-14f10921e45e">' +
                                        '        <table class="wrapped">' +
                                        '            <colgroup>' +
                                        '                <col>' +
                                        '                <col>' +
                                        '                <col>' +
                                        '            </colgroup>' +
                                        '            <tbody>' +
                                        chartTableRow0903 +
                                        '            </tbody>' +
                                        '        </table>' +
                                        '</ac:structured-macro>').replaceAll('\\s+<', '<')
                        def mxBuildVersion0903 = '21.5.0-alpha-20210903421'
                        def mxBuildVersion0904 = '21.5.0-alpha-20210904421'
                        def mxBuildVersion0905 = '21.5.0-alpha-20210905421'
                        def buildURL0904 = 'http://qef-linux1.deloitte.com:8085/job/NMSLO_run/504'
                        def buildURL0905 = 'http://qef-linux1.deloitte.com:8085/job/NMSLO_run/505'
                        def testPerfLogRecords = [
                                ['innowake.mee.dataset.api', 27000005, 16992275, 0, 0, 25599],
                                ['innowake.mee.natural.runtime.controlflow', 4500004, 10591158, 1, 2, 17985],
                                ['mee.jcl.runner', 41, 294380, 11, 7180, 259343]
                        ]
                        def testPerfLogRecordsMinusControlflowCategory = [
                                ['innowake.mee.dataset.api', 27000005, 16992275, 0, 0, 25599],
                                ['mee.jcl.runner', 41, 294380, 11, 7180, 259343]
                        ]
                        def testPerfLogRecordsUnsortedExtraCategory = [
                                ['innowake.mee.dataset.api', 27000005, 16992275, 0, 0, 25599],
                                ['innowake.mee.natural.runtime.controlflow', 4500004, 10591158, 1, 2, 17985],
                                ['mee.jcl.runner', 41, 294380, 11, 7180, 259343],
                                ['extra.category', 0, 0, 0, 0, 0]
                        ]
                        def testPerfLogRecordsExtraCategories = [
                                ['a.extra.category', 0, 0, 0, 0, 0],
                                ['b.extra.category', 0, 0, 0, 0, 0],
                                ['innowake.mee.dataset.api', 27000005, 16992275, 0, 0, 25599],
                                ['innowake.mee.natural.runtime.controlflow', 4500004, 10591158, 1, 2, 17985],
                                ['j.extra.category', 0, 0, 0, 0, 0],
                                ['k.extra.category', 0, 0, 0, 0, 0],
                                ['mee.jcl.runner', 41, 294380, 11, 7180, 259343],
                                ['y.extra.category', 0, 0, 0, 0, 0],
                                ['z.extra.category', 0, 0, 0, 0, 0],
                        ]
                        List categoriesInChart =
                                ['innowake.mee.dataset.api', 'innowake.mee.natural.runtime.controlflow', 'mee.jcl.runner']
                        List categoriesInChartMinusControlflowCategory = ['innowake.mee.dataset.api', 'mee.jcl.runner']

                        stage('test initialPageContent') {
                        deleteDir()
                        echo '1 Test for initialPageContent'
                        def actualPageContent = perfUtils.initialPageContent(testPerfLogRecords)
                        assert actualPageContent.contains(initialChartTableRow) :
                                "Expected to contain: ${initialChartTableRow}\n" +
                                        "But got: ${actualPageContent}\n"
                        echo 'PASSED: test 0'
                        }

                        stage('test trimHistoricalTableElementIfNeeded') {
                        echo '1 Test for trimHistoricalTableElementIfNeeded'
                        def actualPageContent = perfUtils.trimHistoricalTableElementIfNeeded(pageContent0903, 0)
                        assert !actualPageContent.contains(mxBuildVersion0903) :
                                "Expected to not contain: ${mxBuildVersion0903}\n" +
                                        "But actual full page content is: ${actualPageContent}\n"
                        echo 'PASSED: test 0'
                        }

                        stage('test addToHistoricalTableElement') {
                        echo '1 Test for addToHistoricalTableElement'
                        def actualPageContent = perfUtils.addToHistoricalTableElement(pageContent0903, testPerfLogRecords,
                                mxBuildVersion0904, buildURL0904)
                        assert actualPageContent.contains(mxBuildVersion0903) && actualPageContent.contains(mxBuildVersion0904) :
                                "Expected to contain: ${mxBuildVersion0903} and: ${mxBuildVersion0904}\n" +
                                        "But actual full page content is: ${actualPageContent}\n"
                        echo 'PASSED: test 0'
                        }

                        stage('test trimLineChartElementsIfNeeded') {
                        echo '1 Test for trimLineChartElementsIfNeeded'
                        def actualPageContent = perfUtils.trimLineChartElementsIfNeeded(pageContent0903, 0)
                        assert !actualPageContent.contains(chartTableRow0903) :
                                "Expected to not contain: ${chartTableRow0903}\n" +
                                        "But actual full page content is: ${actualPageContent}\n"
                        echo 'PASSED: test 0'
                        }

                        stage('test addToLineChartElements') {
                        echo '4 Test for addToLineChartElements'

                        // Positive test cases
                        def actualPageContent = perfUtils.addToLineChartElements(pageContent0903, testPerfLogRecords,
                                mxBuildVersion0904)
                        assert actualPageContent.contains(chartTableRow0903And0904) :
                                "Expected to contain: ${chartTableRow0903And0904}\n" +
                                        "But actual full page content is: ${actualPageContent}\n"
                        echo 'PASSED: test 0'

                        actualPageContent = perfUtils.addToLineChartElements(pageContent0903,
                                testPerfLogRecordsMinusControlflowCategory, mxBuildVersion0904)
                        assert actualPageContent.contains(chartTableRow0903And0904MinusControlflowValue0904) :
                                "Expected to contain: ${chartTableRow0903And0904MinusControlflowValue0904}\n" +
                                        "But actual full page content is: ${actualPageContent}\n"
                        echo 'PASSED: test 1'

                        actualPageContent = perfUtils.addToLineChartElements(pageContent0903,
                                testPerfLogRecordsExtraCategories, mxBuildVersion0904)
                        assert actualPageContent.contains(chartTableRow0903And0904ExtraCategories0904) :
                                "Expected to contain: ${chartTableRow0903And0904ExtraCategories0904}\n" +
                                        "But actual full page content is: ${actualPageContent}\n"
                        echo 'PASSED: test 2'

                        // Negative test case
                        try {
                                perfUtils.addToLineChartElements(pageContent0903, testPerfLogRecordsUnsortedExtraCategory,
                                        mxBuildVersion0904)
                        }
                        catch (Exception e) {
                                assert e.getMessage().contains('should be unique and in lexicographical order')
                                echo 'PASSED: test 3'
                        }
                        }

                        stage('test getCategoriesInLineChart') {
                        echo '1 Test for getCategoriesInLineChart'
                        String simplifiedChart =
                                '<ac:structured-macro ac:name="chart">' +
                                initialChartTableRow +
                                '</ac:structured-macro>'
                        def actualCategories = perfUtils.getCategoriesInLineChart(simplifiedChart, categoriesInChart)
                        assert categoriesInChart.equals(actualCategories) :
                                "Expected to equal: ${categoriesInChart}\n" +
                                        "But actual categories is: ${actualCategories}"
                        echo 'PASSED: test 0'
                        }

                        stage('test updatePageContent') {
                        echo '1 Test for updatePageContent'
                        def actualPageContent = perfUtils.updatePageContent(pageContent0903, testPerfLogRecords, mxBuildVersion0904,
                                buildURL0904, 1)
                        assert actualPageContent.contains(chartTableRow0904) && !actualPageContent.contains(chartTableRow0903And0904) :
                                "Expected to contain: ${chartTableRow0904}\n" +
                                        "And expected to not contain: ${chartTableRow0903And0904}\n" +
                                        "But actual full page content is: ${actualPageContent}\n"
                        echo 'PASSED: test 0'
                        }

                        stage('test reportToConfluence') {
                        echo "2 Tests for reportToConfluence"
                        // Positive test case.
                        def pageInfo = perfUtils.getConfluencePageJSON(PAGE_ID_OF_EMPTY_CONFLUENCE_PAGE)
                        def pageContent = perfUtils.getConfluencePageHTML(pageInfo)
                        assert pageContent.isEmpty()
                        def testText = 'Id,Invocations,Own/Âµs,Own Min/Âµs,Own Avg/Âµs,Own Max/Âµs\n' +
                                'innowake.mee.dataset.api,27000005,16992275,0,0,25599\n' +
                                'innowake.mee.natural.runtime.controlflow,4500004,10591158,1,2,17985\n' +
                                'mee.jcl.runner,41,294380,11,7180,259343'
                        def testCSVFileName = 'testReportToConfluence.csv'
                        writeFile file: testCSVFileName, text: testText
                        def actualPageContent = perfUtils.reportToConfluence(PAGE_ID_OF_EMPTY_CONFLUENCE_PAGE,
                                testCSVFileName, mxBuildVersion0905, buildURL0905, 1, true)
                        assert actualPageContent.contains(chartTableRow0905) :
                                "Expected to contain: ${chartTableRow0905}\n" +
                                        "But actual full page content is: ${actualPageContent}\n"

                        // Assert page is not empty.
                        // (We must obtain new pageInfo and pageContent objects each time in order to get the
                        // updates to the Confluence page.)
                        pageInfo = perfUtils.getConfluencePageJSON(PAGE_ID_OF_EMPTY_CONFLUENCE_PAGE)
                        pageContent = perfUtils.getConfluencePageHTML(pageInfo)
                        assert !pageContent.isEmpty()

                        // Clear page.
                        pageInfo = perfUtils.getConfluencePageJSON(PAGE_ID_OF_EMPTY_CONFLUENCE_PAGE)
                        perfUtils.updateConfluencePage('', pageInfo)

                        // Assert page is empty again.
                        pageInfo = perfUtils.getConfluencePageJSON(PAGE_ID_OF_EMPTY_CONFLUENCE_PAGE)
                        pageContent = perfUtils.getConfluencePageHTML(pageInfo)
                        assert pageContent.isEmpty()
                        echo 'PASSED: test 0'

                        // Negative test case
                        try {
                                perfUtils.reportToConfluence(PAGE_ID_OF_EMPTY_CONFLUENCE_PAGE,
                                        'doesNotExist.csv', mxBuildVersion0905, buildURL0905, 1, true)
                        } catch (Exception e) {
                                assert e.getMessage().contains('Encountered an error while trying to report performance')
                                echo 'PASSED: test 1'
                        }
                        }

                        stage('test numDaysInChart') {
                        echo 'Test numDaysInChart'
                        def actualNumDaysInChart = perfUtils.numDaysInChart(pageContent0903)
                        def expectedNumDaysInChart = 1
                        assert actualNumDaysInChart.equals(expectedNumDaysInChart) :
                                "Expected ${expectedNumDaysInChart}, but got ${actualNumDaysInChart}"
                        echo 'PASSED: test 0'
                        }

                        stage('test numDaysInTable') {
                        echo 'Test numDaysInTable'
                        def actualNumDaysInTable = perfUtils.numDaysInTable(pageContent0903)
                        def expectedNumDaysInTable = 1
                        assert actualNumDaysInTable.equals(expectedNumDaysInTable) :
                                "Expected ${expectedNumDaysInTable}, but got ${actualNumDaysInTable}"
                        echo 'PASSED: test 0'
                        }

                        stage('test confluencePageJSON and confluencePageHTML') {
                        def pageInfo = perfUtils.getConfluencePageJSON(PAGE_ID_OF_EMPTY_CONFLUENCE_PAGE)
                        assert pageInfo != null
                        def pageContent = perfUtils.getConfluencePageHTML(pageInfo)
                        assert pageContent.isEmpty()
                        }

                        stage('test createAndArchiveCSVs') {
                        final NUM_TESTS = 2
                        def durationsLogHeader = 'Date,Timestamp,Profile,Time elapsed/ms\r\n'

                        // Create a performance logger file for each test
                        def perfLogContents = [
                                '',
                                'any line that does not start with mee or innowake should not be included\r\n' +
                                        'mee.runtime.ims.api.dao                  27,000,005      16,992,275              2               1          25,599\r\n' +
                                        'mee.runtime.ims.api.handler              4,500,004      10,591,158               1               2          17,985\r\n' +
                                        'mee.runtime.cobol.api                    58,500,018     231,519,337              0               3          48,863\r\n' +
                                        'mee.runtime.ims.api.sql                          41         294,380              11           7,180         259,343\r\n' +
                                        'any lines besides profiling lines should not be included\r\n' +
                                        '2021-07-06 11:22:03 INFO  innowake.mee.natural.runtime.performance:78 - Time elapsed: [290004224 us, 290004 ms, 1 invocations]\r\n'
                        ]
                        for (int i = 0; i < NUM_TESTS; i++) {
                                writeFile file: 'test' + i + '.log', text: perfLogContents[i]
                        }

                        // Negative test case
                        echo "${NUM_TESTS} Test for createAndArchiveCSVs()"
                        try {
                                perfUtils.createAndArchiveCSVs('./test0.log', 'test0')
                        } catch (Exception e) {
                                assert e.getMessage().contains('Encountered an error while trying to create and archive CSVs')
                                assert e.getMessage().contains('This usually happens if the performance logger file is empty')
                                echo 'PASSED: test 0'
                        }

                        // Positive test case
                        def expectedValue =
                                [profilingLogHeader +
                                        'mee.runtime.ims.api.dao,27000005,16992275,2,1,25599\r\n' +
                                        'mee.runtime.ims.api.handler,4500004,10591158,1,2,17985\r\n' +
                                        'mee.runtime.cobol.api,58500018,231519337,0,3,48863\r\n' +
                                        'mee.runtime.ims.api.sql,41,294380,11,7180,259343\r\n',
                                profilingLogHeader +
                                        'mee.runtime.ims.api,31500009,27583433,1,0.88,25599\r\n' +
                                        'mee.runtime.cobol.api,58500018,231519337,0,3.96,48863\r\n' +
                                        'mee.runtime.ims.api,41,294380,11,7180.0,259343\r\n',
                                profilingLogHeader +
                                        'mee.runtime.cobol.api,58500018,231519337,0,3.96,48863\r\n' +
                                        'mee.runtime.ims.api,31500050,27877813,1,0.89,259343\r\n',
                                durationsLogHeader +
                                        '2021-07-06,11:22:03,innowake.mee.natural.runtime.performance:78,290004\r\n'
                                ]
                        perfUtils.createAndArchiveCSVs('./test1.log', 'test1')
                        def testLogsProfilingActual = readFile file: './test1_logs_profiling.csv'
                        def testLogsGroupedActual = readFile file: './test1_logs_grouped_profiling.csv'
                        def testLogsSummarizedActual = readFile file: './test1_logs_summarized_profiling.csv'
                        def testLogsDurationsActual = readFile file: './test1_logs_durations.csv'
                        if (testLogsProfilingActual != expectedValue[0]) {
                                error "actual value '${testLogsProfilingActual}' != expected value '${expectedValue[0]}'"
                        } else if (testLogsGroupedActual != expectedValue[1]) {
                                error "actual value '${testLogsGroupedActual}' != expected value '${expectedValue[1]}'"
                        } else if (testLogsSummarizedActual != expectedValue[2]) {
                                error "actual value '${testLogsSummarizedActual}' != expected value '${expectedValue[2]}'"
                        } else if (testLogsDurationsActual != expectedValue[3]) {
                                error "actual value '${testLogsDurationsActual}' != expected value '${expectedValue[3]}'"
                        } else {
                                echo 'PASSED: test 1'
                        }
                        }

                        stage('test combineLogs') {
                        // Negative test case
                        echo '2 Tests for combineLogs()'
                        try {
                                perfUtils.combineLogs([[]])
                        } catch (Exception e) {
                                assert e.getMessage().equals('The inner lists must have at least 6 elements,' +
                                        ' but instead have 0 elements')
                                echo 'PASSED: test 0'
                        }

                        // Positive test case
                        def testValue = [
                                ['mee.runtime.ims.api.dao', 27000005, 16992275, 0, 0, 25599],
                                ['mee.runtime.ims.api.handler', 4500004, 10591158, 1, 2, 17985],
                                ['mee.runtime.ims.api.sql', 41, 294380, 11, 7180, 259343]
                        ]
                        def expectedValue = ['mee.runtime.ims.api', 31500050, 27877813, 0, 0.89, 259343]
                        assert perfUtils.combineLogs(testValue).equals(expectedValue)
                        echo 'PASSED: test 1'
                        }

                        stage('test getCategory') {
                        // Negative test case
                        echo '3 Tests for getCategory()'
                        try {
                                perfUtils.getCategory(null)
                        } catch (Exception e) {
                                assert e.getMessage().equals('str is null')
                                echo 'PASSED: test 0'
                        }

                        // Positive test cases
                        assert perfUtils.getCategory('mee.runtime.ims.api.sql').equals('mee.runtime.ims.api')
                        // Not every mee or innowake package belongs to a category in getCategory's predefined list
                        // of categories. innowake.mee.dataset.api is one such package. When getCategory cannot find a
                        // category for an input string, it returns the input.
                        assert perfUtils.getCategory('innowake.mee.dataset.api').equals('innowake.mee.dataset.api')
                        echo 'PASSED: test 1'
                        assert perfUtils.getCategory('').equals('')
                        echo 'PASSED: test 2'
                        }

                        stage('test sortByFirst') {
                        // Negative test case
                        echo '3 Tests for sortByFirst()'
                        try {
                                perfUtils.sortByFirst([])
                        } catch (Exception e) {
                                assert e.getMessage().equals('List is empty')
                                echo 'PASSED: test 0'
                        }

                        // Positive test cases
                        def testValue = [
                                ['mee.runtime.cobol.api', 58500018, 231519337, 0, 3, 48863],
                                ['innowake.mee.dataset.api', 9200050, 15001780, 0, 1, 14031]
                        ]

                        def expectedValue = [
                                ['innowake.mee.dataset.api', 9200050, 15001780, 0, 1, 14031],
                                ['mee.runtime.cobol.api', 58500018, 231519337, 0, 3, 48863]
                        ]
                        assert perfUtils.sortByFirst(testValue).equals(expectedValue)
                        echo 'PASSED: test 1'
                        assert perfUtils.sortByFirst([[]]).equals([[]])
                        echo 'PASSED: test 2'
                        }
                }
        }
}        
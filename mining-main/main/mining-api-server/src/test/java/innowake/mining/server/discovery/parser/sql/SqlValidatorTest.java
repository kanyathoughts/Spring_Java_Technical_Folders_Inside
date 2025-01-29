/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.sql;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.SqlStatementType;

/**
 * JUnit tests for {@link SqlValidator}.
 */
@TestInstance(Lifecycle.PER_CLASS)
class SqlValidatorTest {

	public Stream<Arguments> testCases() {
		final String code1 = "  SELECT *" + System.lineSeparator() +
				"     FROM BQSFMIS1.TEMP_GL" + System.lineSeparator() +
				"     WHERE (CL_PPY_BALANCE <>0.00" + System.lineSeparator() +
				"       OR   CL_M01_BALANCE <>0.00" + System.lineSeparator() +
				"       OR   CL_CUM_BALANCE <>0.00)" + System.lineSeparator() +
				"       AND (CL_DEPARTMENT NOT IN('J01','J02','J03','J04','J05','J06'))" + System.lineSeparator() +
				"       AND (CL_TRANS_YY       IN('17','18'))" + System.lineSeparator() +
				"      WITH UR;";

		/* mining-quickstart: PWSE4201.job*/
		final String code2 = "WEK SE PWSE4201 BEG SELL PURGE:HDR/DTL DELETE";

		/* mining-quickstart: PWSE6010.job */
		final String code3 = "DATABASE DICTNAME=CORPDICT DBNAME=CORPDB" + System.lineSeparator() +
							 "    PROFILE RELEASE=6 EX=E PARMLIB=STANDARD TS=Y,Y,Y" + System.lineSeparator() +
							 "    PARAM=LIST" + System.lineSeparator() +
							 "    IN 8000 DB SS=VNSUBS01" + System.lineSeparator() +
							 "    PATHAA VN-SUPPLIER VN-SUPPLIER-ADDR" + System.lineSeparator() +
							 "    01OUT 300 D PS DD=SYS020" + System.lineSeparator() +
							 "    01410001 'CODE'" + System.lineSeparator() +
							 "    017 IF VNSPADD-CODE.1 NE ' ' TAKE" + System.lineSeparator() +
							 "    017 DROP";

		return Stream.of(
				Arguments.arguments("SQL 'SELECT FROM'", code1, SqlStatementType.SELECT),
				Arguments.arguments("Non-SQL 'DELETE'", code2, SqlStatementType.UNKNOWN),
				Arguments.arguments("Non-SQL 'DROP'", code3, SqlStatementType.UNKNOWN));
	}

	/**
	 * Parameterized test, executing the {@link SqlValidator#getSqlType(String)} and {@link SqlValidator#validate(String)} methods.
	 * 
	 * @param name the Display name
	 * @param code the code to test
	 * @param type the expected {@link SqlStatementType}
	 */
	@DisplayName("Code | Expected type")
	@ParameterizedTest(name = "{0} returns type {2}")
	@MethodSource("testCases")
	void test(final String name, final String code, final SqlStatementType type) {
		assertSame(type, SqlValidator.getSqlType(code));

		if (type == SqlStatementType.UNKNOWN) {
			assertFalse(SqlValidator.validate(code));
		} else {
			assertTrue(SqlValidator.validate(code));
		}
	}
}

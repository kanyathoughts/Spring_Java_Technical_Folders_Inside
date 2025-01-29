/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.vg;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JclContributorContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JobControl;
import innowake.mining.shared.model.StatementType;
import innowake.ndt.jcl.parser.api.IDDModelInternal;
import innowake.ndt.jcl.parser.model.StepExec;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class JclFtpContributorTest {

	private static final Path CONTROL_CARDS_PATH = Path.of("src/test/resources/controlcards");
	private static final String EXPECTED_FILE_EXT = ".expected";
	private static final boolean WRITE_EXPECTED = false;

	@Test
	void testAcceptForStepExecWithPgmFTP() {
		final JclFtpContributor contributor = new JclFtpContributor();
		final StepExec stepExec = Mockito.mock(StepExec.class);
		final Map<String, String> properties = Map.of("PGM", "FTP");
		when(stepExec.getProperties()).thenReturn(properties);
		assertTrue(contributor.accept(stepExec));
	}

	@Test
	void testAcceptForStepExecWithPgmNoFTP() {
		final JclFtpContributor contributor = new JclFtpContributor();
		final StepExec stepExec = Mockito.mock(StepExec.class);
		final Map<String, String> properties = Map.of("PGM", "FTP1");
		when(stepExec.getProperties()).thenReturn(properties);
		assertFalse(contributor.accept(stepExec));
	}

	@Test
	void testAcceptForStepExecWithNoPgm() {
		final JclFtpContributor contributor = new JclFtpContributor();
		final StepExec stepExec = Mockito.mock(StepExec.class);
		final Map<String, String> properties = Map.of("PROC", "FTP");
		when(stepExec.getProperties()).thenReturn(properties);
		assertFalse(contributor.accept(stepExec));
	}

	@Test
	void testContributeWithNoValidControlCard() {
		final JclFtpContributor contributor = new JclFtpContributor();
		final StepExec stepExec = Mockito.mock(StepExec.class);
		final JclContributorContext context = Mockito.mock(JclContributorContext.class);
		final DiscoveryBuilder.ModuleBuilder stepModule = Mockito.mock(DiscoveryBuilder.ModuleBuilder.class);
		final JobControl jobControl = Mockito.mock(JobControl.class);
		when(context.getJobControl()).thenReturn(jobControl);
		when(jobControl.getControlCardContent(stepExec, "SYSIN")).thenReturn(null);
		when(jobControl.getControlCardContent(stepExec, "INPUT")).thenReturn(null);

		contributor.contribute(context, stepExec, stepModule);
		Mockito.verify(stepModule, Mockito.atLeastOnce()).addError(any(), any(), any());
	}

	@Test
	void testContributeWithNoMatchingDDs() throws IOException {
		testContributor("CNT1.crd", Map.of());
	}

	@Test
	void testContributeWithNoServerDetails() throws IOException {
		testContributor("CNT2.crd", Map.of());
	}

	@Test
	void testContributeWithMatchingDDs() throws IOException {
		testContributor("CNT3.crd", Map.of("INP1", List.of("TEST.FILE1A"), "INP2", List.of("TEST.FILE2A, TEST.FILE2B"), "OUTPUT", List.of("TEST.OUTPUT")));
	}

	@SuppressWarnings("unchecked")
	private static String convertToCsv(final Map<String, Object> data) {
		final StringBuilder csv = new StringBuilder();
		csv.append("server,username,password,action,localFile,remoteFile,remoteDirectory\n");

		final String server = (String) data.get("server");
		final String user = (String) data.get("user");
		final String password = (String) data.get("password");

		final List<String> actions = Arrays.asList("get", "put", "append", "listCatalog");
		for (final String action : actions) {
			final List<Map<String, String>> actionList = (List<Map<String, String>>) data.get(action);
			if (actionList != null) {
				for (final Map<String, String> actionMap : actionList) {
					csv.append(server).append(",");
					csv.append(user).append(",");
					csv.append(password).append(",");
					csv.append(action).append(",");
					csv.append(actionMap.get("localFile")).append(",");
					csv.append(actionMap.get("remoteFile")).append(",");
					csv.append(actionMap.get("remoteDirectory")).append("\n");
				}
			}
		}
		return csv.toString();
	}

	private static void assertExpected(final String name, final String content) throws IOException {
		if (WRITE_EXPECTED) {
			final Path filePath = CONTROL_CARDS_PATH.resolve(name + EXPECTED_FILE_EXT);
			if (!Files.exists(filePath)) {
				Files.createFile(filePath);
			}
			Files.writeString(filePath, content);
		} else {
			final String expected = Files.readString(CONTROL_CARDS_PATH.resolve(name + EXPECTED_FILE_EXT)).replace("\r\n", "\n");
			assertEquals(expected, content.replace("\r\n", "\n"));
		}
	}

	private static void testContributor(final String controlCard, final Map<String, List<String>> dds) throws IOException {
		final JobControl jobControl = Mockito.mock(JobControl.class);
		final JclContributorContext context = Mockito.mock(JclContributorContext.class);
		when(context.getJobControl()).thenReturn(jobControl);

		final DiscoveryBuilder.StatementBuilder statementBuilder = Mockito.mock(DiscoveryBuilder.StatementBuilder.class);
		final DiscoveryBuilder.ModuleBuilder stepModule = Mockito.mock(DiscoveryBuilder.ModuleBuilder.class);
		when(stepModule.declareStatement(StatementType.FTP)).thenReturn(statementBuilder);

		final ArgumentCaptor<Map<String, Object>> propertiesCaptor = ArgumentCaptor.forClass(Map.class);
		when(statementBuilder.setProperties(propertiesCaptor.capture())).thenReturn(statementBuilder);
		when(statementBuilder.setText(any())).thenReturn(statementBuilder);
		when(statementBuilder.setTechnology(any())).thenReturn(statementBuilder);

		final DiscoveryBuilder discoveryBuilder = Mockito.mock(DiscoveryBuilder.class);
		when(context.getDiscoveryBuilder()).thenReturn(discoveryBuilder);
		final String controlCardContent = getControlCard(controlCard);
		final StepExec stepExec = Mockito.mock(StepExec.class);
		when(stepExec.getDDs()).then(invocation->{
			return mockDDs(dds);
		});
		when(jobControl.getControlCardContent(stepExec, "SYSIN")).thenReturn(controlCardContent);
		when(jobControl.getControlCardContent(stepExec, "INPUT")).thenReturn(null);

		final JclFtpContributor contributor = new JclFtpContributor();
		contributor.contribute(context, stepExec, stepModule);
		assertExpected(controlCard, convertToCsv(propertiesCaptor.getValue()));
	}

	private static Map<String, List<IDDModelInternal>> mockDDs(final Map<String, List<String>> ddFiles) {
		final Map<String, List<IDDModelInternal>> dds = new HashMap<>();
		for (final Map.Entry<String, List<String>> entry : ddFiles.entrySet()) {
			final List<IDDModelInternal> ddList = new ArrayList<>();
			dds.put(entry.getKey(), ddList);
			for (String ddFile : entry.getValue()) {
				final IDDModelInternal dd = mock(IDDModelInternal.class);
				final Map<String, String> properties = Map.of("DSN", ddFile);
				when(dd.getProperties()).thenReturn(properties);
				ddList.add(dd);
			}
		}
		return dds;
	}

	private static String getControlCard(final String name) throws IOException {
		return Files.readString(CONTROL_CARDS_PATH.resolve(name));
	}
}

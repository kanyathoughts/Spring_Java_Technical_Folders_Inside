package testcases.discovery;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import com.hp.lft.report.ReportException;
import com.hp.lft.sdk.GeneralLeanFtException;

import appmodels.MiningApplicationModel;
import appmodels.MiningApplicationModel.MainWindow;
import appmodels.MiningApplicationModel.MainWindow.ProjectExplorerTabControl.ProjectExplorerTreeView;
import unittesting.BrowserOperations;
import unittesting.ParameterProvider;
import unittesting.UnitTestClassBase;

/**
 * This test automates project specific settings not taking effect.
 * 
 * @see <a href="https://iris.innowake.de/browse/WQST-582">Test protocol WQST-582</a>
 * 
 * @author avjhingran
 */

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class MiningProjectSpecificSettingsTest extends UnitTestClassBase {
	private MiningApplicationModel appModel;
	private MainWindow mainWindow;
	private ProjectExplorerTreeView projectExplorerTreeView;
	
	/** Constant values */
	private static final String MINING = "mining";
	private static final String CONFIG_FILE = "Discovery_Config.xml";
	private static final String CSV_FILE_NAME_PATTERN = "discovery*.csv";
	private static final String workspaceDir = ParameterProvider.getParameter("workspaceDir");
	private static final String miningServerURL = ParameterProvider.getParameter("miningServerURL");

	/** @inheritDoc */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		instance = new MiningProjectSpecificSettingsTest();
		globalSetup(MiningProjectSpecificSettingsTest.class);
	}

	/** @inheritDoc */
	@AfterClass
	public static void tearDownAfterClass() throws ReportException {
		globalTearDown();
	}

	@Before
	public void setUp() throws GeneralLeanFtException {
		appModel = new MiningApplicationModel();
		mainWindow = appModel.MainWindow();
		projectExplorerTreeView = mainWindow.ProjectExplorerTabControl().ProjectExplorerTreeView();
	}

	/* Step 01 not implemented as its the mining setup */
	/**
	 * Start your mining eclipse with a new workspace. 
	 * The mining eclipse is up and running. 
	 * In the menu bar an additional entry called 'mining' is listed.
	 * 
	 * @throws GeneralLeanFtException
	 */
	@Test
	public void step02() throws GeneralLeanFtException {
		initEclipse();
		mainWindow.MiningMenu().highlight();
		assertAndVerify(true, appModel.MainWindow().MiningMenu().exists());
	}

	/**
	 * In the menu bar click on Window -> Preferences.
	 * In the navigation tree of the Preferences Window click on 'mining'.
	 * Verify that no connection is established if the "Connect" button is available.
	 * Click on "Apply and Close".
	 * 
	 * @throws GeneralLeanFtException
	 */
	@Test
	public void step03() throws GeneralLeanFtException {
		switchPerspective("Natural");
		mainWindow.WindowMenu().preferencesMenu().select();
		mainWindow.PreferencesWindow().Tree().activateNode("mining");
		/* Disconnect first if the connection is already established. */
		if (!mainWindow.PreferencesWindow().URLEditor().isEnabled()) {
			mainWindow.PreferencesWindow().DisconnectButton().click();
			waitUntilActionFinished(2);
		}
		assertAndVerify(true, mainWindow.PreferencesWindow().ConnectButton().exists());
		mainWindow.PreferencesWindow().ApplyAndCloseButton().click();
	}

	/**
	 * Pull the following git project into the workspace:
	 * https://gitlab.consulting.sltc.com/appmod/public/discovery-jumpstart. 
	 * No error messages.
	 * 
	 * @throws GeneralLeanFtException
	 */
	@Test
	public void step04() throws GeneralLeanFtException {
		importProject(workspaceDir);
	}

	/**
	 * Click right on the project and select Properties.
	 * In the project properties window select mining. 
	 * The mining project properties wizard is displayed.
	 * 
	 * @throws GeneralLeanFtException
	 */
	@Test
	public void step05() throws GeneralLeanFtException {
		openMiningProjectProperties();
		assertAndVerify(MINING, mainWindow.ProjectPropertiesWindow().Tree().getNode(MINING).getColumnValue(0));
	}

	/**
	 * Select the checkbox "Enable project specific settings".
	 * Enter a valid api-server URL and click on "Logon".
	 * A browser opens with a login screen.
	 * Login with username "admin" and password "Worx2000".
	 * After successful login, close the browser and switch back to your eclipse.
	 * Click on "Apply and Close".
	 * 
	 * @throws GeneralLeanFtException
	 */
	@Test
	public void step06() throws GeneralLeanFtException {
		mainWindow.ProjectPropertiesWindow().EnableProjectSpecificSettingsCheckBox().click();
		waitUntilActionFinished(2);
		/* Disconnect first if the connection is already established. */
		if (mainWindow.ProjectPropertiesWindow().DisconnectButton().exists()) {
			mainWindow.ProjectPropertiesWindow().DisconnectButton().click();
			waitUntilActionFinished(2);
		}
		mainWindow.ProjectPropertiesWindow().URLEditor().setText(miningServerURL);
		mainWindow.ProjectPropertiesWindow().ConnectButton().click();
		waitUntilActionFinished(3);
		mainWindow.ProjectPropertiesWindow().LogonButton().click();
		BrowserOperations.login();
		waitUntilActionFinished(3);
		pressCtrlAndW();
		waitUntilActionFinished(2);
		mainWindow.ProjectPropertiesWindow().ApplyButton().click();
		waitUntilActionFinished(3);
		mainWindow.ProjectPropertiesWindow().TestAPIConnectionButton().click();
		waitUntilActionFinished(3);
		assertAndVerify(true, mainWindow.ProjectPropertiesWindow().serverInfoWindow().ServerConnectionSuccessText().getText()
				.contains("successful"));
		mainWindow.ProjectPropertiesWindow().serverInfoWindow().OKButton().click();
	}
	
	/**
	 * In the project dropdown list select Demo Project B (Demo Client 1) and
	 * click on "Apply and Close". The project properties window closes.
	 * 
	 * @throws GeneralLeanFtException
	 */
	@Test
	public void step07() throws GeneralLeanFtException {
		mainWindow.ProjectPropertiesWindow().ProjectList().click();
		mainWindow.ProjectPropertiesWindow().ProjectList().select("Demo Project B (Demo Client 1)");
		mainWindow.ProjectPropertiesWindow().ApplyAndCloseButton().click();
		assertAndVerify(false, mainWindow.ProjectPropertiesWindow().exists());
		waitUntilActionFinished(2);
	}

	/**
	 * Click right on the project and select "discovery" -> "Advanced" ->
	 * "Download Configuration". 
	 * The "Discovery_Config.xml" is downloaded into the workspace.
	 * 
	 * @throws GeneralLeanFtException
	 */
	@Test
	public void step08() throws GeneralLeanFtException {
		projectExplorerTreeView.getNode(projectExplorerTreeView.buildNodePath(0)).openContextMenu();
		projectExplorerTreeView.DiscoveryMenu().AdvancedMenu().DownloadConfigurationMenu().select();
		waitUntilActionFinished(20);
		assertAndVerify(true, nodeExists(
				projectExplorerTreeView.getNode(projectExplorerTreeView.buildNodePath(0, CONFIG_FILE))));
	}

	/**
	 * Open discovery-config.xml file. 
	 * Check if this line is present: <output_format>CSV</output_format>
	 * Save the changes.
	 * 
	 * @throws GeneralLeanFtException
	 */
    @Test
    public void step09() throws GeneralLeanFtException {
		projectExplorerTreeView.activateNode(projectExplorerTreeView.buildNodePath(0, CONFIG_FILE));
		waitUntilActionFinished(5);
		String content = mainWindow.StyledTextEditor().getText();
		if (content.contains("<output_format>EXCEL</output_format>")) {
			content = content.replace("<output_format>EXCEL</output_format>", "<output_format>CSV</output_format>");
			waitUntilActionFinished(2);
			mainWindow.StyledTextEditor().setText(content);
			waitUntilActionFinished(2);
			pressCtrlAndS();
			waitUntilActionFinished(2);
		}
		content = "<output_format>CSV</output_format>";
		assertAndVerify(true, mainWindow.StyledTextEditor().getText().contains(content));
		waitUntilActionFinished(3);
    }

	/**
	 * Select the three "undiscovered" folders, right-click and select "discovery" -> 
	 * "Discover Code on Selection".
	 * No error messages are displayed. 
	 * The "src" folder with the discovery results was downloaded into the workspace.
	 * 
	 * @throws GeneralLeanFtException
	 */
	@Test
	public void step10() throws GeneralLeanFtException {
		projectExplorerTreeView.select(projectExplorerTreeView.buildNodePath(0, "undiscovered"),
				projectExplorerTreeView.buildNodePath(0, "undiscovered-cobol"),
				projectExplorerTreeView.buildNodePath(0, "undiscovered-natural"));
		projectExplorerTreeView.getNode(mainWindow.ProjectExplorerTabControl().ProjectExplorerTreeView()
				.buildNodePath("discovery-jumpstart", "undiscovered")).openContextMenu();
		projectExplorerTreeView.DiscoveryMenu().DiscoverCodeMenu().select();
		waitUntilActionFinished(40);
	}

	/**
	 * Click right on the "src" directory and select "discovery" -> "Discover Metrics on Selection".
	 * No error messages are displayed. 
	 * The "Overwrite?" dialog is displayed. 
	 * Select "Yes" when you are asked if "Existing modules are overwritten". 
	 * The dialog disappears.
	 * 
	 * @throws GeneralLeanFtException
	 */
	@Test
	public void step11() throws GeneralLeanFtException {
		projectExplorerTreeView.select(projectExplorerTreeView.buildNodePath(0, "src"));
		projectExplorerTreeView.getNode(projectExplorerTreeView.buildNodePath(0, "src")).openContextMenu();
		projectExplorerTreeView.DiscoveryMenu().DiscoverMetricsMenu().select();
		waitUntilActionFinished(3);
		if (mainWindow.DiscoverMetricsOverrideWindow().exists()) {
			mainWindow.DiscoverMetricsOverrideWindow().YesButton().click();
		}
		waitUntilActionFinished(40);
	}

	/**
	 * Check if the discovery-csv was downloaded into the workspace.
	 * The discovery-csv was downloaded into the workspace.
	 * 
	 * @throws GeneralLeanFtException
	 */
	@Test
	public void step12() throws GeneralLeanFtException {
		assertAndVerify(false, findFileNameWithPatternUsingSearch(CSV_FILE_NAME_PATTERN) ==  null);
	}
}
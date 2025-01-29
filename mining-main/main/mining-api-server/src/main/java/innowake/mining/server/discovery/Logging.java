package innowake.mining.server.discovery;

/**
 * Contains all logger constants.
 */
public final class Logging {

	public static final String OBJECT_RESOLVING = "innowake.mining.server.discovery.objectResolving";

	public static final String PROJECT_CONFIGURATOR = "innowake.mining.server.discovery.categorize.ProjectConfigurator";

	public static final String CONFIG = "innowake.mining.server.discovery.config.core.Config";

	/** Logs specific for Discover Code */
	public static final String CATEGORIZE = "innowake.mining.server.discovery.categorize";
	/** Logs specific for Discover Metrics */
	public static final String METRICS = "innowake.mining.server.discovery.metrics";

	/** Logs specific for Java Discover Metrics */
	public static final String JAVA_METRICS = "innowake.mining.server.discovery.metrics.java";

	/** Logs specific for Discover DNA */
	public static final String DNA = "innowake.mining.server.discovery.dna";
	/** Logs specific for Dead code calculation */
	public static final String DEAD_CODE = "innowake.mining.server.job.deadcode";

	/** Logs specific for storing Discovery models to DB */
	public static final String STORE_MODEL = "innowake.mining.server.discovery.storeModel";

	/** Categorizing file types */
	public static final String CATEGORIZE_FILETYPE_DETECTION = "innowake.mining.server.discovery.categorize.fileTypeDetection";

	public static final String CATEGORIZE_CATEGORIZER = "innowake.mining.server.discovery.categorize.Categorizer";

	public static final String CATEGORIZE_STATISTIC = "innowake.mining.server.discovery.categorize.Statistic";

	/** Module collection */
	public static final String METRICS_MODULE_REPOSITORY = "innowake.mining.server.discovery.metrics.ModuleRepository";

	/** Metrics collection */
	public static final String METRICS_COLLECTOR = "innowake.mining.server.discovery.metrics.MetricsCollector";

	/** Discovery: Incremental Scan */
	public static final String INCREMENTAL_SCAN = "innowake.mining.server.discovery.metrics.IncrementalScan";

	/** Parsing parboiled common */
	public static final String PARSER_PARBOILED = "innowake.mining.server.discovery.parser.parboiled";

	/** Parsing parboiled offsets */
	public static final String PARSER_PARBOILED_POSITION = "innowake.mining.server.discovery.parser.parboiled.position";

	/** Category for JCL parsing */
	public static final String JCL_PARSER = "innowake.mining.server.discovery.parser.batch";

	/** Category for Cobol parsing */
	public static final String COBOL_PARSER = "innowake.mining.server.discovery.parser.cobol";

	/** Category for Natural parsing */
	public static final String NATURAL_PARSER = "innowake.mining.server.discovery.parser.natural";

	/** Category for Easytrieve parsing */
	public static final String EZT_PARSER = "innowake.mining.server.discovery.parser.easytrieve";

	/** Category for C parsing */
	public static final String C_PARSER = "innowake.mining.server.discovery.parser.c";

	/** Category for Java parsing */
	public static final String JAVA_PARSER = "innowake.mining.server.discovery.parser.java";

	/** Category for Basic parsing */
	public static final String BASIC_PARSER = "innowake.mining.server.discovery.parser.basic";
	
	/** Category for SQL parsing */
	public static final String SQL_PARSER = "innowake.mining.server.discovery.parser.sql";

	/** Logs specific for source export */
	public static final String SOURCE_EXPORT = "innowake.mining.server.discovery.source.export";

	private Logging() {
	}

}

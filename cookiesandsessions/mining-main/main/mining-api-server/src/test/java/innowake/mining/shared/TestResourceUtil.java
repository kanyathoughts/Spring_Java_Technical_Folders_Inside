package innowake.mining.shared;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

public class TestResourceUtil {

	public static final String ROOT_FOLDER = "test-resources";
	private static final Logger LOG = LoggerFactory.getLogger(TestResourceUtil.class);

	@Nullable
	public static String getContent(final String path, final Charset cs) throws IOException {
		final Path resPath = Paths.get(ROOT_FOLDER, path);
		if ( ! Files.isReadable(resPath)) {
			LOG.warn("Could not get source for {}", path);
			return null;
		}

		return Files.readString(resPath, cs);
	}

	@Nullable
	public static String getContentNormalized(final String path, final Charset cs) throws IOException {
		final var str = getContent(path, cs);
		return str == null ? null : str.replace("\r\n", "\n");
	}

	public static void write(final String path, final String content, final Charset cs) throws IOException {
		final Path resPath = Paths.get(ROOT_FOLDER, path);
		Files.createDirectories(resPath.getParent());
		Files.writeString(resPath, content, cs);
	}
}

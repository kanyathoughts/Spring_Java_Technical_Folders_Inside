package innowake.mining.moduleextraction;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import innowake.mining.moduleextraction.base.ModuleDescriptionExtractor;


public abstract class AbstractModuleDescriptionTest {
	
	protected String getDescriptionFromFile(String path, ModuleDescriptionExtractor<?> extractor) {
		byte[] content = readFile(new File(path));
		String fileContent = new String(content);
		String description = extractor.getDescription(fileContent);
		return description;
	}
	
	private static byte[] readFile(File x) {
		try {
			return Files.readAllBytes(Paths.get(x.getCanonicalPath()));
		} catch (IOException e) {
			throw new IllegalStateException("file not found", e);
		}
		
	}
	
}

/* Copyright (c) 2023 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.datalineage;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class DataLineageGmlPrototype {
	private static Path JSON_PATH = Paths.get("src\\test\\resources", "TEST21.json");
	
	public static void main(String[] args) throws Exception {
		if (args.length == 1) {
			JSON_PATH = Paths.get(args[0]);
		}
		final String json = new String(Files.readAllBytes(JSON_PATH));
		final DataLineageGmlUtilWithSeparateCopybooks dlGml = new DataLineageGmlUtilWithSeparateCopybooks(json);
		final String gml = dlGml.getGml();
		System.out.println(gml);
	}
}

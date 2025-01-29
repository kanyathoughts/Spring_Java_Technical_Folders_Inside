/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import javax.annotation.PostConstruct;

import org.apache.commons.collections4.map.HashedMap;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringSubstitutor;
import org.apache.groovy.util.Maps;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.service.CookieIdVerifier;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Controller to call the UI component for the mining-ui
 */
@MiningUnsecuredRestController
public class MiningUiController {

	private static final String INDEX_HTML_URL = "/index.html";
	private static final Logger LOG = LoggerFactory.getLogger(MiningUiController.class);
	private static final String DEFAULT_INDEX_LOCATION = "classpath:/resources/index.html";
	private static final Map<String, String> TAGS_TO_REPLACE = new HashedMap<>();
	
	@Value("${server.servlet.context-path}")
	private String servletContextPath;
	
	@Value("${mining.cookieId}")
	private String cookieId;
	
	/* Spring does not implicitly declare this property with its default value, so we declare the default
	   as empty String and handle this case separately. */
	@Value("${spring.web.resources.static-locations:}")
	private String staticLocations;

	@Autowired
	private CookieIdVerifier cookieIdVerifier;
	
	@Autowired
	private ApplicationContext applicationContext;
	
	private String htmlContent;

	@PostConstruct
	private void setup() throws IOException {
		final String indexLocation;
		
		LOG.debug(() -> "Static locations: " + StringUtils.defaultIfEmpty(staticLocations, "<BLANK>"));

		if (StringUtils.isEmpty(staticLocations)) {
			LOG.debug(() -> "Static web resource locations not configured. Using the classpath to resolve the index.html");
			indexLocation = DEFAULT_INDEX_LOCATION;
		} else {
			LOG.debug(() -> "Using explicitly configured static web resource locations for resolving the index.html");
			indexLocation = staticLocations + "index.html";
		}
		
		final Resource file = applicationContext.getResource(indexLocation);

		final List<String> htmlSourceContent;
		try (final InputStream in = file.getInputStream()) {
			htmlSourceContent = IOUtils.readLines(in, StandardCharsets.UTF_8);
		}
		
		/*
		 * Adding elements to replace in index.html with their replaced values in map
		 * then iterating the HTML once and replacing the elements based on the key value pair added in map.
		 */
		insertBaseUrl();
		insertCookieScript();
		if(TAGS_TO_REPLACE.size() > 0) {
			replaceTags(htmlSourceContent);
		}
		
		htmlContent = String.join("\n", htmlSourceContent);
	}

	/**
	 * Returns the cookieId
	 *
	 * @return String cookieId
	 */
	public String getCookieId() {
		return cookieId;
	}
	
	/**
	 * Returns the modified index.html after adding href in base tag 
	 *
	 * @return string modified index.html
	 */
	@GetMapping(value = INDEX_HTML_URL, produces = MediaType.TEXT_HTML_VALUE)
	@MiningUnsecuredRestEndpoint
	@Operation(summary = "Get the modified index.html after adding href in base tag", operationId = "modifiedIndexHtml")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	public String modifyIndexHtml(){
		return htmlContent;
	}
	
	private void replaceTags(final List<String> htmlSourceContent) {
		/*
		 * Iterating the index.html(htmlSourceContent) and replacing the elements based on the key value pair added in map.
		 */
		for(int index = 0; index < htmlSourceContent.size(); index++) {
			for(final Map.Entry<String, String> entry : TAGS_TO_REPLACE.entrySet()) {
				if(htmlSourceContent.get(index).contains(entry.getKey())) {
					htmlSourceContent.set(index, htmlSourceContent.get(index).replace(entry.getKey(), entry.getValue()));
				}
			}
		}

	}

	private void insertBaseUrl() {
		if( ! servletContextPath.equals("/")) {
			TAGS_TO_REPLACE.put("<base href=\"/\">", "<base href=\"" + servletContextPath + "/\">");
		}
	}
	
	private void insertCookieScript() throws IOException {
		if (StringUtils.isBlank(cookieId) || ! cookieIdVerifier.cookieIdVerificationIsEnabled()) {
			return;
		}

		final Resource script = new ClassPathResource("/static/cookie-script.html");
		String scriptContent;
		try (final InputStream in = script.getInputStream()) {
			scriptContent = IOUtils.toString(in, StandardCharsets.UTF_8);
		}
		scriptContent = new StringSubstitutor(Maps.of("cookieId", cookieId)).replace(scriptContent);

		TAGS_TO_REPLACE.put("</body>", scriptContent + "</body>");
	}
}

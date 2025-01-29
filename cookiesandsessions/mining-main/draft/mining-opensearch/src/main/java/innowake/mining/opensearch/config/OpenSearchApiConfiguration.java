package innowake.mining.opensearch.config;

import io.netty.handler.ssl.SslContext;
import io.netty.handler.ssl.SslContextBuilder;
import io.netty.handler.ssl.util.InsecureTrustManagerFactory;

import org.apache.http.Header;
import org.apache.http.HttpHost;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.message.BasicHeader;
import org.apache.http.ssl.SSLContextBuilder;
import org.opensearch.client.RestClient;
import org.opensearch.client.RestClientBuilder;
import org.opensearch.client.json.jackson.JacksonJsonpMapper;
import org.opensearch.client.opensearch.OpenSearchClient;
import org.opensearch.client.transport.OpenSearchTransport;
import org.opensearch.client.transport.rest_client.RestClientTransport;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.reactive.ReactorClientHttpConnector;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.netty.http.client.HttpClient;

import java.net.MalformedURLException;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLException;

/**
 * Provides a {@link WebClient} to talk to OpenSearch's REST API.
 */
@Configuration
public class OpenSearchApiConfiguration {

	@Value("${mining-opensearch.opensearch.url}")
	private String url;
	@Value("${mining-opensearch.opensearch.username}")
	private String username;
	@Value("${mining-opensearch.opensearch.password}")
	private String password;

	OpenSearchClient client;

	@Bean
	@Qualifier("opensearch")
	public WebClient openSearchWebClient() throws SSLException {
		/* disable SSL certificate validation for OpenSearch REST API */
		final SslContext sslContext = SslContextBuilder
				.forClient()
				.trustManager(InsecureTrustManagerFactory.INSTANCE)
				.build();
		final HttpClient httpClient = HttpClient.create().secure(t -> t.sslContext(sslContext));
		return WebClient.builder()
				.clientConnector(new ReactorClientHttpConnector(httpClient))
				.baseUrl(url)
				.defaultHeaders(headers -> headers.setBasicAuth(username, password))
				.build();
	}

	@Bean
	@Qualifier("opensearch")
	public OpenSearchClient getClient() { 
		if (client != null) {
			return client;
		}
		
		URL parsedUrl;
		try {
			parsedUrl = new URL(url);
		} catch (final MalformedURLException e) {
			throw new IllegalStateException(e);
		}

		final String protocol = parsedUrl.getProtocol();
		final String host = parsedUrl.getHost();
		final int port = parsedUrl.getPort();

		SSLContext sslContext;
		try {
			sslContext = SSLContextBuilder
					.create()
					.loadTrustMaterial(new TrustSelfSignedStrategy())
					.build();
		} catch (KeyManagementException | NoSuchAlgorithmException | KeyStoreException e) {
			throw new IllegalStateException(e);
		}

		final RestClientBuilder builder = RestClient.builder(
				new HttpHost(host, port, protocol))
				.setHttpClientConfigCallback(httpClientBuilder -> 
				httpClientBuilder.setSSLContext(sslContext)
				.setSSLHostnameVerifier(NoopHostnameVerifier.INSTANCE))
				.setDefaultHeaders(new Header[] {
						new BasicHeader("Authorization", "Basic " + Base64.getEncoder().encodeToString((username + ":" + password).getBytes()))
				});

		final OpenSearchTransport transport = new RestClientTransport(builder.build(), new JacksonJsonpMapper());
		return new OpenSearchClient(transport);
	}
	
}

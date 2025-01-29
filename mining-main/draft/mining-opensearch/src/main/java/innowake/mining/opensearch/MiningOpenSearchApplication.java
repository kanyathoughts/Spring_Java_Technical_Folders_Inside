package innowake.mining.opensearch;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@ComponentScan("innowake.mining.opensearch.config")
public class MiningOpenSearchApplication {

    public static void main(final String[] args) {
        SpringApplication.run(MiningOpenSearchApplication.class, args);
    }

}

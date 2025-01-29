package com.example.movie_info_service;

import java.time.Duration;
import java.util.Scanner;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.context.annotation.Bean;
import org.springframework.web.client.RestTemplate;

@SpringBootApplication
@EnableDiscoveryClient
public class MovieInfoServiceApplication {

	@Bean
	public RestTemplate restTemplate(RestTemplateBuilder builder) {
		return builder.setConnectTimeout(Duration.ofSeconds(20)).setReadTimeout(Duration.ofSeconds(20)).build();
	}

	public static void main(String[] args) {
		SpringApplication.run(MovieInfoServiceApplication.class, args);
//		Scanner sc = new Scanner(System.in);
//		System.out.println("Enter the number:");
//		int n=sc.nextInt();
//		for (int i = 0; i < n; i++) {
//			for (int j = 0; j < n; j++) {
//				int value = Math.min(Math.min(i, j), Math.min(n-i-1, n-j-1));
//				System.out.print(n-value);
//			}
//			System.out.println();
//		}
	}

}

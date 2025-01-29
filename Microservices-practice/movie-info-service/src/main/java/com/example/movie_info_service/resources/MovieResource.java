package com.example.movie_info_service.resources;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

import com.example.movie_info_service.models.Movie;
import com.example.movie_info_service.models.MovieSummary;

import jakarta.websocket.server.PathParam;

@RestController
@RequestMapping("/movies")
public class MovieResource {
	
	@Value("${api.key}")
	private String apiKey;
	
	@Autowired
	private RestTemplate restTemplate;
	
	@GetMapping("/{movieId}")
	public  String getMovieInfo(@PathVariable("movieId") String movieId) {
//		Map<String,String> moviesMap = new HashMap<>();
//		moviesMap.put("1234", "Kalki");
//		moviesMap.put("5678", "The Goat life");
		
		System.out.println("apiKey: " + apiKey);
		System.out.println("url: "+ "https://api.themoviedb.org/3/movie/" + movieId + "?api_key=" + apiKey);
//		ResponseEntity<MovieSummary> movieSummary = restTemplate.getForEntity("https://api.themoviedb.org/3/movie/" + movieId + "?api_key=" + apiKey, MovieSummary.class);
		ResponseEntity<String> value2 = restTemplate.getForEntity("https://api.themoviedb.org/3/movie/" + movieId + "?api_key=" + apiKey, String.class);
//String value = restTemplate.getForObject("https://api.themoviedb.org/3/movie/" + movieId + "?api_key=" + apiKey, String.class);
//		System.out.println("value: " + value);
		System.out.println("value2: " + value2.getBody());
		return value2.getBody();
//		MovieSummary movieSummary = restTemplate.getForObject("https://api.themoviedb.org/3/movie/" + movieId + "?api_key=" + apiKey, MovieSummary.class);
//		System.out.println(movieSummary.getStatusCode());
		
//		return new Movie(movieId, movieSummary.getBody().getTitle(), movieSummary.getBody().getOverView());
		
	}

}

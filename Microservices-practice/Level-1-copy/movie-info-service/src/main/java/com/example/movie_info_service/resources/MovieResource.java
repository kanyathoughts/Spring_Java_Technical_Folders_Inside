package com.example.movie_info_service.resources;

import java.util.HashMap;
import java.util.Map;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.example.movie_info_service.models.Movie;

import jakarta.websocket.server.PathParam;

@RestController
@RequestMapping("/movies")
public class MovieResource {
	
	@GetMapping("/{movieId}")
	public Movie getMovieInfo(@PathVariable("movieId") String movieId) {
		Map<String,String> moviesMap = new HashMap<>();
		moviesMap.put("1234", "Kalki");
		moviesMap.put("5678", "The Goat life");
		return new Movie(movieId, moviesMap.get(movieId));
		
	}

}

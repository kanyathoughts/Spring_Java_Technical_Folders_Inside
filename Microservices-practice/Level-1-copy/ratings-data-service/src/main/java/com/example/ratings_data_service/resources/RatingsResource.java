package com.example.ratings_data_service.resources;

import java.util.Arrays;
import java.util.List;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.example.ratings_data_service.models.Rating;
import com.example.ratings_data_service.models.UserRating;

@RestController
@RequestMapping("/ratingsdata")
public class RatingsResource {
	
	@GetMapping("/{movieId}")
	public Rating getRating(@PathVariable("movieId") String movieId) {
		return new Rating(movieId, 4);
	}
	
	// Here we are wrapping a list inside an object for consistency and api scalability and api maintenance and for inclusion of other data such as metadata
	@GetMapping("/users/{userId}")
	public UserRating getRatings(@PathVariable("userId") String userId) {
		List<Rating> ratings =  Arrays.asList(
				new Rating("1234", 5),
				new Rating("5678", 3)
				);
		UserRating userRating = new UserRating();
		userRating.setUserRating(ratings);
		return userRating;
	}

}

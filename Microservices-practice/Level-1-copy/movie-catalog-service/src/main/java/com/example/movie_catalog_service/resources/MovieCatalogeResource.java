package com.example.movie_catalog_service.resources;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.reactive.function.client.WebClient;

import com.example.movie_catalog_service.models.CatalogItem;
import com.example.movie_catalog_service.models.Movie;
import com.example.movie_catalog_service.models.Rating;
import com.example.movie_catalog_service.models.UserRating;
import com.netflix.discovery.converters.Auto;


@RestController
@RequestMapping("/catalog")
public class MovieCatalogeResource {
	
	@Autowired
	private RestTemplate restTemplate;
	
	@Autowired
	private DiscoveryClient discoveryClient;
	
	@Autowired
	private WebClient.Builder webClientBuilder;
	
	// Marshaling means converting Object into JSON/XML format
	// Unmarshaling means converting JSON/XML format into object
	
	// serialization is converting object into stream of bytes
	// deserialization is converting stream of bytes into object
	
	// Autowired is useful for injecting the dependency
	// Whenever you use Autowired then it's going to look for the bean which returns the particular type
	// If we have more than one bean which returns the same type then we need to use @Qualifier and @Type annotations
	
	// Here movie-info-service is having movie id and movie name
	// rating service is having movie id and rating for the movie
	// movie catalog service is having userid, movie name, movie description and rating
	
	
	// get all the rated movie id's
	
	// for each movie ID, call movie-info-service get movie info
	
	// put them all together
	

	@GetMapping("/{userId}")
	public List<CatalogItem> getCatalog(@PathVariable(value = "userId") String userId) {
		
//		List<Rating> ratings = Arrays.asList(
//				new Rating("1234", 5),
//				new Rating("5678", 3)
//				);
		
		//UserRating ratings = restTemplate.getForObject("http://localhost:8083/ratingsdata/users/" + userId, UserRating.class);
		UserRating ratings = restTemplate.getForObject("http://ratings-data-service/ratingsdata/users/" + userId, UserRating.class);
		
		return ratings.getUserRating().stream().map(rating -> {
			//rest template is going to perform get operation on the given URL and gives back the converted response into object
			//Movie movie = restTemplate.getForObject("http://localhost:8082/movies/" + rating.getMovieId(), Movie.class);
			Movie movie = restTemplate.getForObject("http://movie-info-service/movies/" + rating.getMovieId(), Movie.class);
			
			/*Movie movie = webClientBuilder.build() //every time a request comes in we need to build instance of web client
											.get() //get request, if you want put /post/patch, ypu need to specify that
											.uri("http://localhost:8082/movies/" + rating.getMovieId()) // specify the url to make the get request
											.retrieve() //retrieve means fetch the output
											.bodyToMono(Movie.class) //Mono means a form of asynchronous, we don't need to wait till request is completed, 
											//we will do something in the mean time, once the request is completed it's going to notify you
											.block(); //Here we want all the movies list to further use them so we want the request to be completed so blocking it
											*/
			
			return new CatalogItem(movie.getName(), "desc", rating.getRating());
		}).collect(Collectors.toList());

	}

}

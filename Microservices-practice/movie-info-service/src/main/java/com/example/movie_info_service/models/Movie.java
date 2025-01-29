package com.example.movie_info_service.models;

public class Movie {
	
	private String movieId;
	
	private String title;
	
	private String overView;

	public String getMovieId() {
		return movieId;
	}

	public void setMovieId(String movieId) {
		this.movieId = movieId;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}
	
	public String getOverView() {
		return overView;
	}

	public void setOverView(String overView) {
		this.overView = overView;
	}

	public Movie(String movieId, String title, String overView) {
		super();
		this.movieId = movieId;
		this.title = title;
		this.overView = overView;
	}
	
	

}

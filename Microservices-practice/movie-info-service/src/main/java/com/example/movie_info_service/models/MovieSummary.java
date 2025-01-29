package com.example.movie_info_service.models;

import com.fasterxml.jackson.annotation.JsonProperty;

public class MovieSummary {
	
//	@JsonProperty("original_title")
	private String original_title;
	
//	@JsonProperty("overview")
	private String overView;
	
	public String getTitle() {
		return original_title;
	}
	public void setTitle(String title) {
		this.original_title = title;
	}
	public String getOverView() {
		return overView;
	}
	public void setOverView(String overView) {
		this.overView = overView;
	}
}

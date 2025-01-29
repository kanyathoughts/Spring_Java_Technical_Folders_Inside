/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai.knowledgeservice;

import com.fasterxml.jackson.annotation.JsonProperty;

public class KnowledgeServiceResultObject {

	private final String id;
	private final double score;
	private final String content;
	private final int rank;
	private final String origin;

	public KnowledgeServiceResultObject(@JsonProperty(value = "id") final String id, @JsonProperty(value = "score") final double score, @JsonProperty(value = "content") final String content,
			@JsonProperty(value = "rank") final int rank, @JsonProperty(value = "origin") final String origin) {
		this.id = id;
		this.score = score;
		this.content = content;
		this.rank = rank;
		this.origin = origin;
	}

	@JsonProperty("id")
	public String getId() {
		return id;
	}

	@JsonProperty("score")
	public double getScore() {
		return score;
	}

	@JsonProperty("content")
	public String getContent() {
		return content;
	}

	@JsonProperty("rank")
	public int getRank() {
		return rank;
	}

	@JsonProperty("origin")
	public String getOrigin() {
		return origin;
	}

	@Override
	public String toString() {
		return "{id=" + id + ", score=" + score + ", content='" + content + "', rank=" + rank + ", origin='" + origin + "'}";
	}

}

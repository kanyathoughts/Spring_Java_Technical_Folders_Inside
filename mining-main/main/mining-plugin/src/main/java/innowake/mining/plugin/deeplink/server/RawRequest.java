/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.server;

import java.io.IOException;
import java.io.Reader;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;

import innowake.mining.plugin.deeplink.command.RequestParseException;
import innowake.mining.plugin.deeplink.command.Request;

/**
 * Class used to parse the raw requests
 * They consist of a request type and a command as a JSON string.
 */
public class RawRequest {
	private RequestType requestType;
	private String commandString;
	
	private static ObjectMapper mapper = new ObjectMapper();
	static {
		mapper.registerModule(new Jdk8Module());
	}
	
	/**
	 * Parses the requests to a command.
	 * It also passes the command type to the request.
	 *
	 * @param reader The reader from which to read the request.
	 * @return The command contained in the request.
	 * @throws RequestParseException if an error occurs parsing the request.
	 */
	public static Request parseRequest(final Reader reader) throws RequestParseException {
		try {
			return mapper.readValue(reader, RawRequest.class).toCommand();
		} catch (final IOException e) {
			throw new RequestParseException("Unable to parse request.", e);
		}
	}
	
	/**
	 * Reads the command string as the correct command type, and passes the command type to it.
	 *
	 * @return the parsed command with the correct command type.
	 * @throws RequestParseException If an error occurs parsing the command.
	 */
	public Request toCommand() throws RequestParseException {
		try {
			return mapper.readValue(commandString, this.requestType.type).addCommandType(this.requestType);
		} catch (final JsonProcessingException e) {
			throw new RequestParseException("Unable to parse command.", e);
		}
	}

	public void setRequestType(final String requestType) {
		this.requestType = RequestType.valueOf(requestType);
	}

	public void setRequest(final String request) {
		this.commandString = request;
	}
}

/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.ims;

import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_DBD;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_HDAMPARM;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_HELPTXT;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_PSB;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import innowake.mining.shared.model.discovery.ResolveTarget;

public enum ImsTokenType{
	
	/* IMS_DBD */
	DBDGEN(IMS_DBD, "DBDGEN"),
	
	/* IMS_HDAMPARM */
	RMNAME(IMS_HDAMPARM, "RMNAME="),
	ACCESS(IMS_HDAMPARM, "ACCESS="),
	
	/* IMS_HELPTXT */
	SCREEN(IMS_HELPTXT, "SCREEN"),
	ONLINE(IMS_HELPTXT, "ONLINE"),
	HELP(IMS_HELPTXT, "HELP"),
	PAGE(IMS_HELPTXT, "PAGE"),
	
	/* IMS_PSB */
	PSBGEN(IMS_PSB, "PSBGEN");
	

	private final ResolveTarget type;
	private final String token;
	
	/* pattern from Joshua Bloch, Effective Java */
	private static final Map<String, ImsTokenType> TOKEN_MAP;
	
	ImsTokenType(final ResolveTarget type, final String token) {
		this.type = type;
		this.token = token;
	}
	
	public String getToken() {
		return token;
	}
	
	public ResolveTarget getType() {
		return type;
	}

    static {
        Map<String, ImsTokenType> map = new ConcurrentHashMap<>();
        for (ImsTokenType token : ImsTokenType.values()) {
            map.put(token.getToken(), token);
        }
        TOKEN_MAP = Collections.unmodifiableMap(map);
    }
	
    public static Optional<ImsTokenType> findEnumOf(final String token) {
		return Optional.ofNullable(TOKEN_MAP.get(token));
	}
    
    public static List<String> getTokens(final ResolveTarget type) {
    	return Arrays.asList(ImsTokenType.values())
    			.stream().filter(token -> token.getType().equals(type))
    			.map(ImsTokenType::getToken)
    			.collect(Collectors.toList());
    }
}

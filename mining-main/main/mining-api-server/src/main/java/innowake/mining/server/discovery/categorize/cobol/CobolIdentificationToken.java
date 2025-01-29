/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.cobol;

import static innowake.mining.shared.model.discovery.ResolveTarget.CICS_BMS_MAPSET;
import static innowake.mining.shared.model.discovery.ResolveTarget.COBOL_COPYBOOK;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_MFS;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import innowake.mining.shared.model.discovery.ResolveTarget;

public enum CobolIdentificationToken{
	/* copy tokens*/
	PIC9(COBOL_COPYBOOK ,"PIC9"),
	PICTURE(COBOL_COPYBOOK, "PICTURE"),
	PIC(COBOL_COPYBOOK, "PIC"),
	X_LeftBracket(COBOL_COPYBOOK, "X("),
	
	/* map tokens*/
	/* BMS */
	DFHMSD(CICS_BMS_MAPSET, "DFHMSD"),
	DFHMDI(CICS_BMS_MAPSET, "DFHMDI"),
	DFHMDF(CICS_BMS_MAPSET, "DFHMDF"),
	/* FMS */
	MSGEND(IMS_MFS, "MSGEND"),
	FMTEND(IMS_MFS, "FMTEND");

	private final ResolveTarget type;
	private final String token;
	
	/* pattern from Joshua Bloch, Effective Java */
	private static final Map<String, CobolIdentificationToken> TOKEN_MAP;
	
	CobolIdentificationToken(final ResolveTarget type, final String token) {
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
        Map<String, CobolIdentificationToken> map = new ConcurrentHashMap<>();
        for (CobolIdentificationToken token : CobolIdentificationToken.values()) {
            map.put(token.getToken(), token);
        }
        TOKEN_MAP = Collections.unmodifiableMap(map);
    }
	
    public static Optional<CobolIdentificationToken> findEnumOf(final String token) {
    	if(token.startsWith(X_LeftBracket.getToken())) {
    		return Optional.of(X_LeftBracket);
    	}
		return Optional.ofNullable(TOKEN_MAP.get(token));
	}
    
    public static List<String> getTokens(final ResolveTarget type) {
    	return Arrays.asList(CobolIdentificationToken.values())
    			.stream().filter(token -> token.getType().equals(type))
    			.map(CobolIdentificationToken::getToken)
    			.collect(Collectors.toList());
    }
}

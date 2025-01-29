/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.ims;

import java.util.EnumMap;

/**
 * Class to check if string content is of type Cobol based on tokens.
 * Tokens are split by a single whitespace & line endings.
 */
public class ImsTokenCounter {

	private EnumMap<ImsTokenType, Boolean> tokenMap = new EnumMap<ImsTokenType, Boolean>(ImsTokenType.class);

	
	public ImsTokenCounter(final String content) {
		/* Initialize the enum map */
		for (ImsTokenType v : ImsTokenType.values() ) {
			if (content.contains(v.getToken())){
				tokenMap.put(v, Boolean.TRUE);
			} else 	{
				tokenMap.put(v, Boolean.FALSE);
			}
		}
	}

	public boolean isDbdh() {
		return (tokenMap.get(ImsTokenType.RMNAME).booleanValue() &&
				tokenMap.get(ImsTokenType.ACCESS).booleanValue());
	}

	public boolean isPSB() {
		return (tokenMap.get(ImsTokenType.PSBGEN).booleanValue());
	}

	public boolean isDBD() {
		return tokenMap.get(ImsTokenType.DBDGEN).booleanValue();
	}
	
	public boolean isDoc() {
		return ( tokenMap.get(ImsTokenType.SCREEN).booleanValue()
				&& tokenMap.get(ImsTokenType.HELP).booleanValue()
				&& tokenMap.get(ImsTokenType.PAGE).booleanValue());
	}
	
	
}

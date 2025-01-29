/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.UUID;

/**
 * Definitions for accessing stored authentication tokens.
 */
public interface TokenInfoService {
	
	interface OfflineTokenInquiryBuilder {
		OfflineTokenInquiryBuilder byId(UUID id);
		OfflineTokenInquiryBuilder ofSubject(String subject);
	}
	
}

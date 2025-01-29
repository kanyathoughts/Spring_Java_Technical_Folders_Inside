/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.util;

import java.util.Date;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import org.springframework.boot.context.event.ApplicationEnvironmentPreparedEvent;
import org.springframework.context.ApplicationListener;

import innowake.base.license.iw.MaxensoLicenseManager;
import innowake.base.license.iw.MaxensoProduct;
import innowake.base.license.iw.NotLicensedException;
import innowake.lib.core.IStatus;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.license.LicenseException;
import innowake.lib.license.core.LicensePropertyGroup;
import innowake.lib.license.spi.IWDescriptors;
import innowake.mining.server.Logging;

/**
 * Checks the innoWake license as part of the server startup.
 * <p>
 * If the license check fails, the corresponding error will be logged at {@value Logging#LICENSE} and
 * the server will fail to start.
 */
public class LicenseChecker implements ApplicationListener<ApplicationEnvironmentPreparedEvent> {

	private static final String MINING_LICENSE_GROUP = "mining";
	private static final String LICENSE_CHECK_FAILED = "The innoWake(TM) license check failed.";
	private static final String LICENSE_NEVER_EXPIRE = "Your mining-core license will never expire.";
	private static final String LICENSE_EXPIRE = "Your mining-core license is about to expire in {} days on {}.";
	private static final Logger LICENSE_LOG = LoggerFactory.getLogger(Logging.LICENSE);

	@Override
	public void onApplicationEvent(final ApplicationEnvironmentPreparedEvent event) {
		verifyLicense();
	}

	/* To allow for testing */
	/* package private */ void verifyLicense() {
		LICENSE_LOG.info(() -> "Verifying license.");
		try {
			MaxensoLicenseManager maxensoLicenseManager = MaxensoLicenseManager.get();
			final Date expiryDateGlobal = maxensoLicenseManager.getLicensePropertyValue(
					LicensePropertyGroup.GLOBAL_GROUP.getId(), 
					IWDescriptors.DESC_EXPIRES_ON, 
					Date.class);
			final Date expiryDateMining = maxensoLicenseManager.getLicensePropertyValue(
					MINING_LICENSE_GROUP, 
					IWDescriptors.DESC_EXPIRES_ON, 
					Date.class);
			if (expiryDateGlobal == null && expiryDateMining == null) {
				LICENSE_LOG.info(LICENSE_NEVER_EXPIRE);
			} else {
				final Date expiryDate;
				if (expiryDateMining == null) {
					expiryDate = expiryDateGlobal;
				} else if (expiryDateGlobal == null) {
					expiryDate = expiryDateMining;
				} else {
					expiryDate = expiryDateMining.before(expiryDateGlobal) ? expiryDateMining : expiryDateGlobal;
				}
				
				LICENSE_LOG.info(LICENSE_EXPIRE, TimeUnit.DAYS.convert(
						Objects.requireNonNull(expiryDate).getTime() - new Date().getTime(), 
						TimeUnit.MILLISECONDS), 
						expiryDate);
			}
			final IStatus status = maxensoLicenseManager.verify(MaxensoProduct.MINING);
			if (status.isError()) {
				final String message = status.getMessage().toString();
				LICENSE_LOG.error(() -> message);
				throw new NotLicensedException(message);
			}
			if (status.isWarning()) {
				final String message = status.getMessage().toString();
				LICENSE_LOG.warn(() -> message);
			}
		} catch (final LicenseException exception) {
			LICENSE_LOG.error(() -> LICENSE_CHECK_FAILED, exception);
			throw new NotLicensedException(LICENSE_CHECK_FAILED, exception);
		}
	}
}

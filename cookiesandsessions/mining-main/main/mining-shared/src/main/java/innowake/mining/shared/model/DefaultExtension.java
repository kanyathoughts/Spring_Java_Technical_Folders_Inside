/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.lib.core.api.lang.Nullable;

/**
 * The default extension for all the {@link Technology} and it's {@link Type}.
 */
public enum DefaultExtension {

	CBL, CPY, MAP, MFS, CPL,
	NSP, NSS, NSN, NSC, NS4, NS5, NS6, NS7, NSH, NS8, NSM, NS3, NR3, NSG, NSA, NSL, NSD, NST, NSIWL, NSIWA, NSIWG,
	JOB, PROC, CRD,
	JAVA, JSP,
	COM, FMS, IFDL,
	CDO, SQLMOD, SQL, DB2,
	ASM, MAC,
	XML, XHTML,
	DBD, HLP, DBDH, PSB, CGEN, SYSGEN,
	PL1, PL1M, PCPY,
	CSDL, CSDE,
	EZT, EZM,
	C,H,
	CPP, HPP,
	BIN,
	BAS, REC, INC,
	LST,
	ECL,
	MAR,
	CLS, CTL, FRM, VBP, VBW, DOB, DSR,
	DLL, OCX,
	CS, CSPROJ, SLN;

	@Nullable public static DefaultExtension resolve(final String extension) {
		try {
			return valueOf(extension.toUpperCase());
		} catch (final IllegalArgumentException e) {
			return null;
		}
	}

	public String getExtension() {
		return name().toLowerCase();
	}
}

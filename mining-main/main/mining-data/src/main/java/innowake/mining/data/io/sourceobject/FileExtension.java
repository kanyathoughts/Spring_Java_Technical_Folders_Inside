/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io.sourceobject;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSetMultimap;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.core.IdentificationMapper;
import innowake.mining.shared.model.DefaultExtension;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * The file extension is kind of utility class, which supports to define the {@link ModuleType} of SourceObject by using its path.
 */
public class FileExtension {
	
	private static final String FOLDER_FOR_JCL_INCLUDE = Config.getDefaultConfig().getMappings().stream()
			.filter(mapping -> mapping.type == ResolveTarget.JCL_INCLUDE)
			.map(a -> a.folder)
			.findFirst()
			.orElse(StringUtils.EMPTY);
	
	private FileExtension() {
	  }
	
	/**
	 * Resolves a {@link ModuleType} for a {@code path} value. If the value is {@code null} or doesn't match with a {@link ModuleType} this method 
	 * returns a {@link ModuleType} with {@link Technology#UNKNOWN} and {@link Type#UNKNOWN}
	 *
	 * @param path the path as {@link String} value
	 * @return the {@link ModuleType}
	 */
	public static ModuleType resolve(@Nullable final String path) {
		try {
			if (StringUtils.isNotBlank(path)) {
				String extension = FilenameUtils.getExtension(path);
				if (StringUtils.isNotBlank(extension)) {
					extension = extension.toUpperCase();
					final ImmutableSet<ModuleType> moduleTypesSet = EXTENSIONS_TO_MODULE_TYPE.get(DefaultExtension.valueOf(extension));
					if (moduleTypesSet.isEmpty()) {
						return ModuleType.UNKNOWN;
					}
					if (moduleTypesSet.size() == 1) {
						return moduleTypesSet.iterator().next();
					} else {
						return resolveToSingleModuleType(extension, assertNotNull(path), moduleTypesSet);
					}
				}
			}
		} catch (final IllegalArgumentException e) {
			/* assume UNKNOWN */
		}

		return ModuleType.UNKNOWN;
	}

	private static ModuleType resolveToSingleModuleType(final String extension, final String path, final ImmutableSet<ModuleType> moduleTypesSet) {
		if (extension.equalsIgnoreCase(DefaultExtension.BAS.toString())) {
			final String[] partsOfPath = path.split("/");
			if (partsOfPath.length >= 2 && partsOfPath[0].equals(IdentificationMapper.SRC_ROOT) && partsOfPath[1].equals("vb")) {
				return resolveModuleTypeByTechnology(moduleTypesSet, Technology.VB);
			}
			return resolveModuleTypeByTechnology(moduleTypesSet, Technology.BASIC);
		}
		if (extension.equalsIgnoreCase(DefaultExtension.PROC.toString())) {
			final List<String> partsOfPath = Arrays.asList(path.split("/"));
			if (partsOfPath.size() > 2 && partsOfPath.get(0).equals(IdentificationMapper.SRC_ROOT)
					&& partsOfPath.get(1).equals("jcl")) {
				final boolean includeExist = partsOfPath.stream().anyMatch(folder -> folder.equals(FOLDER_FOR_JCL_INCLUDE));
				return includeExist ? resolveModuleType(moduleTypesSet, ModuleType.JCL_INCLUDE)
						: resolveModuleType(moduleTypesSet, ModuleType.JCL_PROC);
			}
		}
		return ModuleType.UNKNOWN;
	}

	private static ModuleType resolveModuleTypeByTechnology(final ImmutableSet<ModuleType> moduleTypesSet, final Technology technology) {
		return moduleTypesSet.stream()
				.filter(moduleType -> moduleType.getTechnology() == technology)
				.findAny()
				.orElse(ModuleType.UNKNOWN);
	}

	private static ModuleType resolveModuleType(final ImmutableSet<ModuleType> moduleTypesSet, final ModuleType moduleTypeToSearch) {
		return moduleTypesSet.stream()
				.filter(moduleType -> moduleType == moduleTypeToSearch)
				.findAny()
				.orElse(ModuleType.UNKNOWN);
	}
	
	private static final ImmutableSetMultimap<DefaultExtension, ModuleType> EXTENSIONS_TO_MODULE_TYPE = 
			new ImmutableSetMultimap.Builder<DefaultExtension, ModuleType>()
			.put(DefaultExtension.CBL, ModuleType.COBOL_PROGRAM)
			.put(DefaultExtension.CPY, ModuleType.COBOL_COPYBOOK)
			.put(DefaultExtension.CPL, ModuleType.COBOL_COPYLIB)
			.put(DefaultExtension.MAP, ModuleType.CICS_BMS_MAPSET)
			
			.put(DefaultExtension.NSP, ModuleType.NATURAL_PROGRAM)
			.put(DefaultExtension.NSS, ModuleType.NATURAL_SUBROUTINE)
			.put(DefaultExtension.NSN, ModuleType.NATURAL_SUBPROGRAM)
			.put(DefaultExtension.NSC, ModuleType.NATURAL_COPYCODE)
			.put(DefaultExtension.NS4, ModuleType.NATURAL_CLASS)
			.put(DefaultExtension.NS5, ModuleType.NATURAL_CPM)
			.put(DefaultExtension.NS6, ModuleType.NATURAL_ADAPTVIEW)
			.put(DefaultExtension.NS7, ModuleType.NATURAL_FUNCTION)
			.put(DefaultExtension.NSH, ModuleType.NATURAL_HELP)
			.put(DefaultExtension.NS8, ModuleType.NATURAL_ADAPTER)
			.put(DefaultExtension.NSM, ModuleType.NATURAL_MAP)
			.put(DefaultExtension.NS3, ModuleType.NATURAL_DIALOG)
			.put(DefaultExtension.NR3, ModuleType.NATURAL_DIALOG_PRIV_RES)
			.put(DefaultExtension.NSG, ModuleType.NATURAL_GDA)
			.put(DefaultExtension.NSIWG, ModuleType.NATURAL_IW_GDA)
			.put(DefaultExtension.NSA, ModuleType.NATURAL_PDA)
			.put(DefaultExtension.NSIWA, ModuleType.NATURAL_IW_PDA)
			.put(DefaultExtension.NSL, ModuleType.NATURAL_LDA)
			.put(DefaultExtension.NSIWL, ModuleType.NATURAL_IW_LDA)
			.put(DefaultExtension.NSD, ModuleType.NATURAL_DDM)
			.put(DefaultExtension.NST, ModuleType.NATURAL_TEXT)
			
			.put(DefaultExtension.JOB, ModuleType.JCL_JOB)
			.put(DefaultExtension.PROC, ModuleType.JCL_PROC)
			.put(DefaultExtension.PROC, ModuleType.JCL_INCLUDE)
			.put(DefaultExtension.CRD, ModuleType.JCL_CONTROLCARD)
			
			.put(DefaultExtension.COM, ModuleType.DCL)
			.put(DefaultExtension.FMS, ModuleType.FMS_FORM)
			.put(DefaultExtension.IFDL, ModuleType.IFDL_FORM)
			.put(DefaultExtension.MAR, ModuleType.VAX_MACRO)
			
			/* ORACLE */
			.put(DefaultExtension.CDO, ModuleType.CDO_FILE)
			.put(DefaultExtension.SQLMOD, ModuleType.SQLMOD)
			
			.put(DefaultExtension.SQL, ModuleType.SQL_SCRIPT)
			.put(DefaultExtension.DB2, ModuleType.SQL_SCRIPT)
			
			.put(DefaultExtension.ASM, ModuleType.ASSEMBLER_PROGRAM)
			.put(DefaultExtension.MAC, ModuleType.ASSEMBLER_MACRO)
			
			.put(DefaultExtension.XML, ModuleType.XML)
			.put(DefaultExtension.XHTML, ModuleType.XML_XHTML)
			
			.put(DefaultExtension.DBD, ModuleType.IMS_DBD)
			.put(DefaultExtension.HLP, ModuleType.IMS_HELPTXT)
			.put(DefaultExtension.DBDH, ModuleType.IMS_HDAMPARM)
			.put(DefaultExtension.PSB, ModuleType.IMS_PSB)
			.put(DefaultExtension.SYSGEN, ModuleType.IMS_SYSGEN_EXPORT)
			.put(DefaultExtension.CGEN, ModuleType.IMS_TDFXTRCT)
			.put(DefaultExtension.MFS, ModuleType.IMS_MFS)
			
			.put(DefaultExtension.PL1, ModuleType.PL1_PROGRAM)
			.put(DefaultExtension.PL1M, ModuleType.PL1_MAINPROGRAM)
			.put(DefaultExtension.PCPY, ModuleType.PL1_COPYBOOK)
			
			.put(DefaultExtension.CSDL, ModuleType.CSD_LIST)
			.put(DefaultExtension.CSDE, ModuleType.CSD_EXTRACT)
			
			.put(DefaultExtension.EZT, ModuleType.EASYTRIEVE_PROGRAM)
			.put(DefaultExtension.EZM, ModuleType.EASYTRIEVE_MACRO_FILE)
			
			.put(DefaultExtension.C, ModuleType.C_PROGRAM)
			.put(DefaultExtension.H, ModuleType.C_HEADER)
			
			.put(DefaultExtension.CPP, ModuleType.CPP_PROGRAM)
			.put(DefaultExtension.HPP, ModuleType.CPP_HEADER)
			
			.put(DefaultExtension.BIN, ModuleType.BINARY)
			
			.put(DefaultExtension.BAS, ModuleType.BASIC_PROGRAM)
			.put(DefaultExtension.REC, ModuleType.BASIC_OBJECT)
			.put(DefaultExtension.INC, ModuleType.BASIC_OBJECT)
			
			.put(DefaultExtension.LST, ModuleType.RESOURCE_LISTCAT)
			
			.put(DefaultExtension.JAVA, ModuleType.JAVA_COMPILATION_UNIT)
			.put(DefaultExtension.JSP, ModuleType.JAVA_JSP)
			
			.put(DefaultExtension.ECL, ModuleType.ECL)
			
			.put(DefaultExtension.CLS, ModuleType.VB_CLASS)
			.put(DefaultExtension.FRM, ModuleType.VB_FORM)
			.put(DefaultExtension.CTL, ModuleType.VB_CONTROL)
			.put(DefaultExtension.BAS, ModuleType.VB_MODULE)
			.put(DefaultExtension.VBP, ModuleType.VB_PROJECT)
			.put(DefaultExtension.VBW, ModuleType.VB_WORKSPACE)
			.put(DefaultExtension.DOB, ModuleType.VB_ACTIVEX_DOCUMENT)
			.put(DefaultExtension.DSR, ModuleType.VB_DESIGNER_FILE)
			
			.put(DefaultExtension.DLL, ModuleType.WINDOWS_DLL)
			.put(DefaultExtension.OCX, ModuleType.WINDOWS_OCX)

			.put(DefaultExtension.CS, ModuleType.CSHARP_COMPILATION_UNIT)
			.put(DefaultExtension.CSPROJ, ModuleType.CSHARP_PROJECT)
			.put(DefaultExtension.SLN, ModuleType.CSHARP_SOLUTION)
			.build();
}

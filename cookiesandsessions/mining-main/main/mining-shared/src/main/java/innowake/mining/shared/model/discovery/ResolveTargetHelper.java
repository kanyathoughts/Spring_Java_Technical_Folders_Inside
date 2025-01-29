/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.discovery;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableSetMultimap;

import innowake.mining.shared.model.DefaultExtension;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Helper methods for {@link ResolveTarget}.
 */
public class ResolveTargetHelper {

	private static final ImmutableSetMultimap<ResolveTarget, DefaultExtension> RESOLVE_TARGET_TO_EXTENSION = new ImmutableSetMultimap.Builder<ResolveTarget, DefaultExtension>()
			.put(ResolveTarget.COBOL_PROGRAM,  DefaultExtension.CBL)
			.put(ResolveTarget.COBOL_COPYBOOK, DefaultExtension.CPY)
			.put(ResolveTarget.COBOL_COPYLIB, DefaultExtension.CPL)

			.put(ResolveTarget.NATURAL_PROGRAM, DefaultExtension.NSP)
			.put(ResolveTarget.NATURAL_SUBROUTINE, DefaultExtension.NSS)
			.put(ResolveTarget.NATURAL_SUBPROGRAM, DefaultExtension.NSN)
			.put(ResolveTarget.NATURAL_COPYCODE, DefaultExtension.NSC)
			.put(ResolveTarget.NATURAL_CLASS, DefaultExtension.NS4)
			.put(ResolveTarget.NATURAL_CPM, DefaultExtension.NS5)
			.put(ResolveTarget.NATURAL_ADAPTVIEW, DefaultExtension.NS6)
			.put(ResolveTarget.NATURAL_FUNCTION, DefaultExtension.NS7)
			.put(ResolveTarget.NATURAL_HELP, DefaultExtension.NSH)
			.put(ResolveTarget.NATURAL_ADAPTER, DefaultExtension.NS8)
			.put(ResolveTarget.NATURAL_MAP, DefaultExtension.NSM)
			.put(ResolveTarget.NATURAL_DIALOG, DefaultExtension.NS3)
			.put(ResolveTarget.NATURAL_DIALOG_PRIV_RES, DefaultExtension.NR3)
			.put(ResolveTarget.NATURAL_GDA, DefaultExtension.NSG)
			.put(ResolveTarget.NATURAL_IW_GDA, DefaultExtension.NSIWG)
			.put(ResolveTarget.NATURAL_PDA, DefaultExtension.NSA)
			.put(ResolveTarget.NATURAL_IW_PDA, DefaultExtension.NSIWA)
			.put(ResolveTarget.NATURAL_LDA, DefaultExtension.NSL)
			.put(ResolveTarget.NATURAL_IW_LDA, DefaultExtension.NSIWL)
			.put(ResolveTarget.NATURAL_DDM, DefaultExtension.NSD)
			.put(ResolveTarget.NATURAL_TEXT, DefaultExtension.NST)

			.put(ResolveTarget.JCL_JOB, DefaultExtension.JOB)
			.put(ResolveTarget.JCL_PROC, DefaultExtension.PROC)
			.put(ResolveTarget.JCL_INCLUDE, DefaultExtension.PROC)
			.put(ResolveTarget.JCL_CONTROLCARD, DefaultExtension.CRD)
			
			.put(ResolveTarget.DCL, DefaultExtension.COM)
			.put(ResolveTarget.FMS_FORM, DefaultExtension.FMS)
			.put(ResolveTarget.IFDL_FORM, DefaultExtension.IFDL)
			
			.put(ResolveTarget.CDO_FILE, DefaultExtension.CDO)
			.put(ResolveTarget.SQLMOD, DefaultExtension.SQLMOD)

			.put(ResolveTarget.ASSEMBLER_PROGRAM, DefaultExtension.ASM)
			.put(ResolveTarget.ASSEMBLER_MACRO, DefaultExtension.MAC)

			.put(ResolveTarget.XML, DefaultExtension.XML)
			.put(ResolveTarget.XML_XHTML, DefaultExtension.XHTML)

			.put(ResolveTarget.IMS_DBD, DefaultExtension.DBD)
			.put(ResolveTarget.IMS_HELPTXT, DefaultExtension.HLP)
			.put(ResolveTarget.IMS_HDAMPARM, DefaultExtension.DBDH)
			.put(ResolveTarget.IMS_PSB, DefaultExtension.PSB)
			.put(ResolveTarget.IMS_SYSGEN_EXPORT, DefaultExtension.SYSGEN)
			.put(ResolveTarget.IMS_MFS, DefaultExtension.MFS)
			
			.put(ResolveTarget.PL1_PROGRAM, DefaultExtension.PL1)
			.put(ResolveTarget.PL1_MAINPROGRAM, DefaultExtension.PL1M)
			.put(ResolveTarget.PL1_COPYBOOK, DefaultExtension.PCPY)

			.put(ResolveTarget.BINARY, DefaultExtension.BIN)
			
			.put(ResolveTarget.EASYTRIEVE_PROGRAM, DefaultExtension.EZT)
			.put(ResolveTarget.EASYTRIEVE_MACRO_FILE, DefaultExtension.EZM)
			
			.put(ResolveTarget.C_PROGRAM, DefaultExtension.C)
			.put(ResolveTarget.C_HEADER, DefaultExtension.H)
			
			.put(ResolveTarget.CPP_PROGRAM, DefaultExtension.CPP)
			.put(ResolveTarget.CPP_HEADER, DefaultExtension.HPP)

			.put(ResolveTarget.CSD_LIST, DefaultExtension.CSDL)
			.put(ResolveTarget.CSD_EXTRACT, DefaultExtension.CSDE)
			
			.put(ResolveTarget.BASIC_OBJECT, DefaultExtension.BAS)
			.put(ResolveTarget.BASIC_OBJECT, DefaultExtension.REC)
			.put(ResolveTarget.BASIC_OBJECT, DefaultExtension.INC)
			.put(ResolveTarget.BASIC_OBJECT, DefaultExtension.MAP)
		
			.put(ResolveTarget.JAVA_COMPILATION_UNIT, DefaultExtension.JAVA)
			
			.put(ResolveTarget.SQL_SCRIPT, DefaultExtension.SQL)
			
			.put(ResolveTarget.LISTCAT, DefaultExtension.LST)
			.put(ResolveTarget.VAX_MACRO, DefaultExtension.MAR)
			
			.put(ResolveTarget.ECL_JOB, DefaultExtension.ECL)
			.put(ResolveTarget.CICS_BMS_MAPSET, DefaultExtension.MAP)
			
			.put(ResolveTarget.VB_MODULE, DefaultExtension.BAS)
			.put(ResolveTarget.VB_CONTROL, DefaultExtension.CTL)
			.put(ResolveTarget.VB_CLASS, DefaultExtension.CLS)
			.put(ResolveTarget.VB_PROJECT, DefaultExtension.VBP)
			.put(ResolveTarget.VB_FORM, DefaultExtension.FRM)
			.put(ResolveTarget.VB_WORKSPACE, DefaultExtension.VBW)
			.put(ResolveTarget.VB_ACTIVEX_DOCUMENT, DefaultExtension.DOB)
			.put(ResolveTarget.VB_DESIGNER_FILE, DefaultExtension.DSR)
			
			.put(ResolveTarget.WINDOWS_DLL, DefaultExtension.DLL)
			.put(ResolveTarget.WINDOWS_OCX, DefaultExtension.OCX)

			.put(ResolveTarget.CSHARP_COMPILATION_UNIT, DefaultExtension.CS)
			.put(ResolveTarget.CSHARP_PROJECT, DefaultExtension.CSPROJ)
			.put(ResolveTarget.CSHARP_SOLUTION, DefaultExtension.SLN)
						
			.build();

	private static final ImmutableSetMultimap<DefaultExtension, ResolveTarget> EXTENSION_TO_RESOLVE_TARGET = RESOLVE_TARGET_TO_EXTENSION.inverse();

	private ResolveTargetHelper() {
	}
	
	/**
	 * Find extensions for given type.
	 *
	 * @param target type
	 * @return extension A set of extension matching the given type.
	 */
	public static Set<String> getExtensions(final ResolveTarget target) {
		final Optional<ResolveTarget> parent = target.getParent();
		Set<DefaultExtension> extension = RESOLVE_TARGET_TO_EXTENSION.get(target);
		if (extension.isEmpty() && parent.isPresent()) {
			extension = RESOLVE_TARGET_TO_EXTENSION.get(parent.get());
		}
		return extension.stream()
				.map(DefaultExtension::getExtension)
				.collect(Collectors.toSet());
	}

	/**
	 * Find extension for given language type.
	 *
	 * @param target language type
	 * @return extension
	 */
	public static Set<String> getExtensionsByLanguage(final ResolveTarget target) {
		return target.getChildren().stream().map(ResolveTargetHelper::getExtensions).flatMap(Collection::stream).collect(Collectors.toSet());
	}
	
	public static Optional<ResolveTarget> getResolveTarget(final String extension, final ResolveTarget parent) {
		final Set<ResolveTarget> types = getResolveTargets(extension).stream()
				.filter(type -> type.getRootChain().contains(parent))
				.collect(Collectors.toSet());
		if (types.size() > 1) {
			throw new UnsupportedOperationException("Error while finding resolve target. More than one type found for the given extension and parent");
		} else {
			return types.stream().findFirst();
		}
	}
	
	/**
	 * Find the resolve target for given extension.
	 *
	 * @param extension of file
	 * @return resolve target
	 */
	public static Set<ResolveTarget> getResolveTargets(final String extension) {
		return EXTENSION_TO_RESOLVE_TARGET.get(DefaultExtension.resolve(extension));
	}
	
	/**
	 * Checks if any of the extension matches the given type.
	 *
	 * @param target type.
	 * @param extensions array of extension to be matched.
	 * @return (@code true} if any extensions match the target type extension, {@code false} if none of the extensions match the target type extension.
	 */
	public static boolean matchesAnyExtension(final ResolveTarget target, final String... extensions) {
		if (extensions.length == 0) {
			return false;
		}
		return Arrays.stream(extensions)
				.map(ResolveTargetHelper::getResolveTargets)
				.flatMap(Collection::stream)
				.anyMatch(target::equals);				
	}
	
	/**
	 * Returns the equivalent {@link ResolveTarget} type of the {@link Technology}
	 * 
	 * @param technology value to find.
	 * @return equivalent {@link ResolveTarget}
	 */
	public static ResolveTarget fromTechnology(final Technology technology) {
		return ResolveTarget.valueOf(technology.name().toUpperCase());
	}
	
	/**
	 * Returns a matching {@link ResolveTarget} from a given {@link Technology} and {@link Type}.
	 * <p>
	 * The {@code ResolveTarget} is matched by matching the {@code Technology} against the {@code ResolveTarget}'s language
	 * and the {@code Type} against the {@code ResolveTarget}'s name(s).
	 *
	 * @param technology the technology
	 * @param type the type
	 * @return the matched ResolveTarget
	 * @throws IllegalArgumentException when no matching {@code ResolveTarget} can be found
	 */
	public static ResolveTarget fromTechnologyAndType(final Technology technology, final Type type) {
		for (final ResolveTarget value : ResolveTarget.values()) {
			final ResolveTarget language = value.getLanguage();
			if (language.name().equalsIgnoreCase(technology.name()) || technology.getNames().stream().anyMatch(language.name()::equalsIgnoreCase)) {
				/* technology matches language -> check if type also matches */
				if (type == Type.UNKNOWN) {
					return language;
				}
				if (value.name().equalsIgnoreCase(type.name()) || type.getNames().stream().anyMatch(value.name()::equalsIgnoreCase)) {
					return value;
				}
			}
		}
		throw new IllegalArgumentException("No ResolveTarget matching technology " + technology.name() + " and type " + type.name());
	}
	
	/**
	 * Returns the name of all extensions in the DefaultExtension enum
	 *
	 * @return the list of extension names
	 */
	public static List<String> getExtensions() {
		return Arrays.stream(DefaultExtension.values()).map(Enum::name).collect(Collectors.toList());
	}
	
	/**
	 * Returns the {@link Technology} for a {@link ResolveTarget}.
	 *
	 * @param resolveTarget the {@link ResolveTarget}
	 * @return the {@link Technology}
	 */
	public static Technology toTechnology(final ResolveTarget resolveTarget) {
		return Technology.fromName(resolveTarget.getLanguage().name());
	}

	/**
	 * Returns the {@link Type} as UNKNOWN by matching {@code ResolveTarget}'s parent to LANGUAGE else returns resolveTarget type name for a {@link ResolveTarget}
	 * 
	 * @param resolveTarget the {@link ResolveTarget}
	 * @return the {@link Type}
	 */
	public static Type toType(final ResolveTarget resolveTarget) {
		final Optional<ResolveTarget> parent = resolveTarget.getParent();
		if ( ! parent.isPresent() || parent.get() == ResolveTarget.LANGUAGE) {
			return Type.UNKNOWN;
		}
		return Type.fromName(resolveTarget.name());
	}
}

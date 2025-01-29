/* eslint-disable sonarjs/no-duplicate-string */
import { NODE_COLORS, NodeColor } from './node-colors';

export enum NodeType {

  'NATURAL PROGRAM' = 'NATURAL PROGRAM',
  'NATURAL SUBPROGRAM' = 'NATURAL SUBPROGRAM',
  'NATURAL PDA' = 'NATURAL PDA',
  'NATURAL LDA' = 'NATURAL LDA',
  'NATURAL GDA' = 'NATURAL GDA',
  'NATURAL COPYCODE' = 'NATURAL COPYCODE',
  'NATURAL DDM' = 'NATURAL DDM',
  'NATURAL MAP' = 'NATURAL MAP',
  'NATURAL SUBROUTINE' = 'NATURAL SUBROUTINE',
  'NATURAL FUNCTION' = 'NATURAL FUNCTION',
  'NATURAL HELP' = 'NATURAL HELP',
  'NATURAL TEXT' = 'NATURAL TEXT',
  'NATURAL ERROR MESSAGE' = 'NATURAL ERROR MESSAGE',
  'NATURAL ADAPTVIEW' = 'NATURAL ADAPTVIEW',
  'NATURAL ADAPTER' = 'NATURAL ADAPTER',
  'NATURAL CLASS' = 'NATURAL CLASS',
  'NATURAL CPM' = 'NATURAL CPM',
  'NATURAL DIALOG' = 'NATURAL DIALOG',
  'NATURAL DIALOG PRIV RES' = 'NATURAL DIALOG PRIV RES',

  'JCL JOB' = 'JCL JOB',
  'JCL PROC' = 'JCL PROC',
  'JCL INLINE PROC' = 'JCL INLINE PROC',
  'JCL EXEC' = 'JCL EXEC',
  'JCL EXEC PGM' = 'JCL EXEC PGM',
  'JCL PGM' = 'JCL PGM',
  'JCL CFG' = 'JCL CFG',
  'JCL INFO' = 'JCL INFO',
  'JCL CONTROLCARD' = 'JCL CONTROLCARD',
  'JCL INCLUDE' = 'JCL INCLUDE',

  'COBOL PROGRAM' = 'COBOL PROGRAM',
  'COBOL COPYBOOK' = 'COBOL COPYBOOK',

  'CICS BMS MAP' = 'CICS BMS MAP',
  'CICS BMS MAPSET' = 'CICS BMS MAPSET',

  'PL1 PROGRAM' = 'PL1 PROGRAM',
  'PL1 MAINPROGRAM' = 'PL1 MAINPROGRAM',
  'PL1 COPYBOOK' = 'PL1 COPYBOOK',
  'PL1 SUBROUTINE' = 'PL1 SUBROUTINE',
  'PL1 FUNCTION' = 'PL1 FUNCTION',

  'ASSEMBLER ASSEMBLER' = 'ASSEMBLER',

  'CSD LIST' = 'CSD LIST',
  'CSD TRANSACTION' = 'CSD TRANSACTION',
  'CSD PROGRAM' = 'CSD PROGRAM',
  'CSD FILE' = 'CSD FILE',
  'CSD EXTRACT' = 'CSD EXTRACT',

  'IMS PSB' = 'IMS PSB',
  'IMS PCB' = 'IMS PCB',
  'IMS ALT PCB' = 'IMS ALT PCB',
  'IMS HDAMPARM' = 'IMS HDAMPARM',
  'IMS HELP' = 'IMS HELP',
  'IMS DBD' = 'IMS DBD',
  'IMS DBD COMPRTN' = 'IMS DBD COMPRTN',
  'IMS DBD DATASET' = 'IMS DBD DATASET',
  'IMS ARCHDEF' = 'IMS ARCHDEF',
  'IMS UTILITY' = 'IMS UTILITY',
  'IMS CBLTDLI' = 'IMS CBLTDLI',
  'IMS DLITCBL' = 'IMS DLITCBL',
  'IMS AIBTDLI' = 'IMS AIBTDLI',
  'IMS CA TELON' = 'IMS CA TELON',
  'IMS TDFXTRCT' = 'IMS TDFXTRCT',
  'IMS MFS' = 'IMS MFS',

  'EASYTRIEVE PROGRAM' = 'EASYTRIEVE PROGRAM',
  'EASYTRIEVE INSTREAM' = 'EASYTRIEVE INSTREAM',
  'EASYTRIEVE MACRO FILE' = 'EASYTRIEVE MACRO FILE',

  'RESOURCE DATABASE' = 'RESOURCE DATABASE',
  'RESOURCE TABLE' = 'RESOURCE TABLE',
  'RESOURCE FILE' = 'RESOURCE FILE',
  'RESOURCE URI' = 'RESOURCE URI',
  'RESOURCE URL' = 'RESOURCE URL',
  'RESOURCE RESOURCE' = 'RESOURCE',
  'RESOURCE TSQ' = 'RESOURCE TSQ',
  'RESOURCE TDQ' = 'RESOURCE TDQ',
  'RESOURCE LISTCAT' = 'RESOURCE LISTCAT',

  'XML XML' = 'XML',

  'BINARY BINARY' = 'BINARY',

  'C PROGRAM' = 'C PROGRAM',
  'C HEADER' = 'C HEADER',

  'VMS FMS FORM' = 'VMS FMS FORM',
  'VMS DCL' = 'VMS DCL',
  'VMS IFDL FORM' = 'VMS IFDL FORM',

  'ORACLE CDO FILE' = 'ORACLE CDO FILE',
  'ORACLE CDO RECORD' = 'ORACLE CDO RECORD',
  'ORACLE SQLMOD' = 'ORACLE SQLMOD',
  'ORACLE SQLMOD PROCEDURE' = 'ORACLE SQLMOD PROCEDURE',

  'BASIC PROGRAM' = 'BASIC PROGRAM',

  'UNKNOWN UTILITY' = 'UNKNOWN UTILITY',
  'UNKNOWN UNDISCOVERED' = 'UNKNOWN UNDISCOVERED',
  'UNKNOWN PROGRAM' = 'UNKNOWN PROGRAM',
  'UNKNOWN COPYBOOK' = 'UNKNOWN COPYBOOK',
  'UNKNOWN SUBROUTINE' = 'UNKNOWN SUBROUTINE',
  'UNKNOWN FUNCTION' = 'UNKNOWN FUNCTION',
  'UNKNOWN UNKNOWN' = 'UNKNOWN',

  GENERIC = 'GENERIC',
  'VISUAL_CLUE' = 'VISUAL CLUE'
}

export enum NodeImages {

  'NATURAL PROGRAM' = '/assets/icons/32x32_natural_program.png',
  'NATURAL PDA' = '/assets/icons/32x32_natural_pda.png',
  'NATURAL LDA' = '/assets/icons/32x32_natural_pda.png',
  'NATURAL GDA' = '/assets/icons/32x32_natural_pda.png',

  'NATURAL COPYCODE' = '/assets/icons/32x32_natural_include.png',
  'NATURAL MAP' = '/assets/icons/32x32_natural_maps.png',

  'JCL JOB' = '/assets/icons/32x32_batch_job.png',
  'JCL PROC' = '/assets/icons/32x32_batch_proc.png',
  'JCL INLINE PROC' = '/assets/icons/32x32_batch_proc.png',
  'JCL EXEC' = '/assets/icons/32x32_batch_stepexec.png',
  'JCL EXEC PGM' = '/assets/icons/32x32_batch_stepexec.png',

  'COBOL PROGRAM' = '/assets/icons/32x32_cobol_program.png',
  'COBOL COPYBOOK' = '/assets/icons/32x32_cobol_copybook.png',

  'CICS BMS MAP' = '/assets/icons/32x32_cics_bms.png',

  'ASSEMBLER ASSEMBLER' = '/assets/icons/32x32_assembler_program.png',

  'CSD LIST' = '/assets/icons/32x32_csd_settings.png',
  'CSD TRANSACTION' = '/assets/icons/32x32_csd_exectransaction.png',
  'CSD PROGRAM' = '/assets/icons/32x32_csd_program.png',

  'RESOURCE TABLE' = '/assets/icons/32x32_generic_databasetable.png',
  'RESOURCE FILE' = '/assets/icons/32x32_generic_resource.png',

  'XML XML' = '/assets/icons/32x32_xml.png',

  'BINARY BINARY' = '/assets/icons/32x32_binary_file.png',

  GENERIC = '/assets/icons/32x32_generic_program.png',
  VISUAL_CLUE = ''
}

export interface NodeTypeColorAndImage {
  color: NodeColor;
  imageUrl: string;
}

export const NODE_CONFIG: {[key in keyof typeof NodeType]: NodeTypeColorAndImage} = {

  'NATURAL PROGRAM': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages['NATURAL PROGRAM'] },
  'NATURAL SUBPROGRAM': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL PDA': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages['NATURAL PDA'] },
  'NATURAL LDA': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages['NATURAL LDA'] },
  'NATURAL GDA': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages['NATURAL GDA'] },
  'NATURAL COPYCODE': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages['NATURAL COPYCODE'] },
  'NATURAL DDM': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL MAP': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages['NATURAL MAP'] },
  'NATURAL SUBROUTINE': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL FUNCTION': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL HELP': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL TEXT': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL ERROR MESSAGE': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL ADAPTVIEW': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL ADAPTER': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL CLASS': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL CPM': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL DIALOG': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },
  'NATURAL DIALOG PRIV RES': { color: NODE_COLORS.NATURAL, imageUrl: NodeImages.GENERIC },

  'JCL JOB': { color: NODE_COLORS.JCL, imageUrl: NodeImages['JCL JOB'] },
  'JCL PROC': { color: NODE_COLORS.JCL, imageUrl: NodeImages['JCL PROC'] },
  'JCL INLINE PROC': { color: NODE_COLORS.JCL, imageUrl: NodeImages['JCL INLINE PROC'] },
  'JCL EXEC': { color: NODE_COLORS.JCL, imageUrl: NodeImages['JCL EXEC'] },
  'JCL EXEC PGM': { color: NODE_COLORS.JCL, imageUrl: NodeImages['JCL EXEC PGM'] },
  'JCL PGM': { color: NODE_COLORS.JCL, imageUrl: NodeImages.GENERIC },
  'JCL CFG': { color: NODE_COLORS.JCL, imageUrl: NodeImages.GENERIC },
  'JCL INFO': { color: NODE_COLORS.JCL, imageUrl: NodeImages.GENERIC },
  'JCL CONTROLCARD': { color: NODE_COLORS.JCL, imageUrl: NodeImages.GENERIC },
  'JCL INCLUDE': { color: NODE_COLORS.JCL, imageUrl: NodeImages.GENERIC },

  'COBOL PROGRAM': { color: NODE_COLORS.COBOL, imageUrl: NodeImages['COBOL PROGRAM'] },
  'COBOL COPYBOOK': { color: NODE_COLORS.COBOL, imageUrl: NodeImages['COBOL COPYBOOK'] },

  'CICS BMS MAPSET': { color: NODE_COLORS.CICS, imageUrl: NodeImages.GENERIC },
  'CICS BMS MAP': { color: NODE_COLORS.CICS, imageUrl: NodeImages['CICS BMS MAP'] },

  'PL1 PROGRAM': { color: NODE_COLORS.PL1, imageUrl: NodeImages.GENERIC },
  'PL1 MAINPROGRAM': { color: NODE_COLORS.PL1, imageUrl: NodeImages.GENERIC },
  'PL1 COPYBOOK': { color: NODE_COLORS.PL1, imageUrl: NodeImages.GENERIC },
  'PL1 SUBROUTINE': { color: NODE_COLORS.PL1, imageUrl: NodeImages.GENERIC },
  'PL1 FUNCTION': { color: NODE_COLORS.PL1, imageUrl: NodeImages.GENERIC },

  'ASSEMBLER ASSEMBLER': { color: NODE_COLORS.ASSEMBLER, imageUrl: NodeImages['ASSEMBLER ASSEMBLER'] },

  'CSD LIST': { color: NODE_COLORS.CSD, imageUrl: NodeImages['CSD LIST'] },
  'CSD TRANSACTION': { color: NODE_COLORS.CSD, imageUrl: NodeImages['CSD TRANSACTION'] },
  'CSD PROGRAM': { color: NODE_COLORS.CSD, imageUrl: NodeImages['CSD PROGRAM'] },
  'CSD FILE': { color: NODE_COLORS.CSD, imageUrl: NodeImages.GENERIC },
  'CSD EXTRACT': { color: NODE_COLORS.CSD, imageUrl: NodeImages.GENERIC },

  'IMS PSB': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS PCB': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS ALT PCB': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS HDAMPARM': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS HELP': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS DBD': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS DBD COMPRTN': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS DBD DATASET': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS ARCHDEF': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS UTILITY': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS CBLTDLI': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS DLITCBL': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS AIBTDLI': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS CA TELON': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS TDFXTRCT': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },
  'IMS MFS': { color: NODE_COLORS.IMS, imageUrl: NodeImages.GENERIC },

  'EASYTRIEVE PROGRAM': { color: NODE_COLORS.EASYTRIEVE, imageUrl: NodeImages.GENERIC },
  'EASYTRIEVE INSTREAM': { color: NODE_COLORS.EASYTRIEVE, imageUrl: NodeImages.GENERIC },
  'EASYTRIEVE MACRO FILE': { color: NODE_COLORS.EASYTRIEVE, imageUrl: NodeImages.GENERIC },

  'RESOURCE DATABASE': { color: NODE_COLORS.RESOURCE, imageUrl: NodeImages.GENERIC },
  'RESOURCE TABLE': { color: NODE_COLORS.RESOURCE, imageUrl: NodeImages['RESOURCE TABLE'] },
  'RESOURCE FILE': { color: NODE_COLORS.RESOURCE, imageUrl: NodeImages['RESOURCE FILE'] },
  'RESOURCE URI': { color: NODE_COLORS.RESOURCE, imageUrl: NodeImages.GENERIC },
  'RESOURCE URL': { color: NODE_COLORS.RESOURCE, imageUrl: NodeImages.GENERIC },
  'RESOURCE TSQ': { color: NODE_COLORS.RESOURCE, imageUrl: NodeImages.GENERIC },
  'RESOURCE TDQ': { color: NODE_COLORS.RESOURCE, imageUrl: NodeImages.GENERIC },
  'RESOURCE RESOURCE': { color: NODE_COLORS.RESOURCE, imageUrl: NodeImages.GENERIC },
  'RESOURCE LISTCAT': { color: NODE_COLORS.RESOURCE, imageUrl: NodeImages.GENERIC },

  'XML XML': { color: NODE_COLORS.RESOURCE, imageUrl: NodeImages['XML XML'] },

  'BINARY BINARY': { color: NODE_COLORS.BINARY, imageUrl: NodeImages['BINARY BINARY'] },

  'C PROGRAM': { color: NODE_COLORS.C, imageUrl: NodeImages.GENERIC },
  'C HEADER': { color: NODE_COLORS.C, imageUrl: NodeImages.GENERIC },

  'VMS FMS FORM' : { color: NODE_COLORS.VMS, imageUrl: NodeImages.GENERIC },
  'VMS DCL' : { color: NODE_COLORS.VMS, imageUrl: NodeImages.GENERIC },
  'VMS IFDL FORM' : { color: NODE_COLORS.VMS, imageUrl: NodeImages.GENERIC },

  'ORACLE CDO FILE' : { color: NODE_COLORS.ORACLE, imageUrl: NodeImages.GENERIC },
  'ORACLE CDO RECORD' : { color: NODE_COLORS.ORACLE, imageUrl: NodeImages.GENERIC },
  'ORACLE SQLMOD' : { color: NODE_COLORS.ORACLE, imageUrl: NodeImages.GENERIC },
  'ORACLE SQLMOD PROCEDURE' : { color: NODE_COLORS.ORACLE, imageUrl: NodeImages.GENERIC },

  'BASIC PROGRAM' : { color: NODE_COLORS.BASIC, imageUrl: NodeImages.GENERIC },

  'UNKNOWN UTILITY': { color: NODE_COLORS.UNKNOWN, imageUrl: NodeImages.GENERIC },
  'UNKNOWN UNDISCOVERED': { color: NODE_COLORS.UNKNOWN, imageUrl: NodeImages.GENERIC },
  'UNKNOWN PROGRAM': { color: NODE_COLORS.UNKNOWN, imageUrl: NodeImages.GENERIC },
  'UNKNOWN COPYBOOK': { color: NODE_COLORS.UNKNOWN, imageUrl: NodeImages.GENERIC },
  'UNKNOWN SUBROUTINE': { color: NODE_COLORS.UNKNOWN, imageUrl: NodeImages.GENERIC },
  'UNKNOWN FUNCTION': { color: NODE_COLORS.UNKNOWN, imageUrl: NodeImages.GENERIC },
  'UNKNOWN UNKNOWN': { color: NODE_COLORS.UNKNOWN, imageUrl: NodeImages.GENERIC },

  GENERIC: { color: NODE_COLORS.default, imageUrl: NodeImages.GENERIC },
  'VISUAL_CLUE': { color: NODE_COLORS.default, imageUrl: NodeImages.VISUAL_CLUE }
};

export enum NodeWidthSizes {
  S = 170,
  M = 255,
  L = 285,
  XL = 300
}

export enum NodeHeightSizes {
  S = 30,
  M = 50,
  L = 100
}

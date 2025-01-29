export interface NodeColor {
    fillColor: string;
    strokeColor: string;
}

const DEFAULT_NODE_COLOR: NodeColor = {
    fillColor: '#F5F7FA',
    strokeColor: '#ADB0B3'
};

export const NODE_COLORS: {[name: string]: NodeColor} = {
    'default': DEFAULT_NODE_COLOR,

    'decision' : { fillColor: '#FDFAF0', strokeColor: '#FDCA40' },
    'terminal' : { fillColor: '#6B7F99', strokeColor: '#6B7F99' },
    'process' : DEFAULT_NODE_COLOR,

    'NATURAL' : { fillColor: '#F2FFF8', strokeColor: '#046A38' },
    'JCL' : { fillColor: '#FFFDE6', strokeColor: '#F2D600' },
    'COBOL' : { fillColor: '#F2FAFF', strokeColor: '#195587' },
    'PL1' : { fillColor: '#FFF7F8', strokeColor: '#C5283D' },
    'ASSEMBLER' : { fillColor: '#F2FFFF', strokeColor: '#00ABAB' },
    'CICS' : { fillColor: '#FEF7FF', strokeColor: '#5B1865' },
    'IMS' : { fillColor: '#FFFAFE', strokeColor: '#CB48B7' },
    'EASYTRIEVE' : { fillColor: '#FFF6F2', strokeColor: '#463730' },
    'ORACLE' : { fillColor: '#FAF6ED', strokeColor: '#C74634' },
    'VMS' : { fillColor: '#F6F6F6', strokeColor: '#01A982'},
    'BASIC' : { fillColor: '#F0F0F0', strokeColor: '#3686B3'},
    'XML' : DEFAULT_NODE_COLOR,
    'BINARY' : DEFAULT_NODE_COLOR,
    'C' : DEFAULT_NODE_COLOR,
    'CSD' : DEFAULT_NODE_COLOR,

    'DATABASE' : { fillColor: '#EDFDFF', strokeColor: '#004F59'},
    'RESOURCE' : { fillColor: '#F0F0ED', strokeColor: '#75787B'},
    'UNKNOWN' : { fillColor: '#FFFFFF', strokeColor: '#001021'},
    'UNASSIGNED' : { fillColor: '#FFFAF7', strokeColor: '#E36414'},
    'NONE' : DEFAULT_NODE_COLOR,
    'SCHEDULER' : DEFAULT_NODE_COLOR,

    'GROUP' : { fillColor: '#F5F5F5', strokeColor: 'C6C6C6' },
    'FIELDS' : { fillColor: '#fadb14', strokeColor: '#ADB0B3' },
    'DATA_INTERFACE': { fillColor: '#fff0f6', strokeColor: '#eb2f96' },
    'OUTDATED': { fillColor: '#f0f0f0', strokeColor: '#bfbfbf' },
    'DELETED': { fillColor: DEFAULT_NODE_COLOR.fillColor, strokeColor: '#ff0000' }
};

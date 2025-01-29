import { GraphNodeLabelStyle } from '../models/graph-node-label-style.model';
import { FontWeight } from 'yfiles';

export const LABEL_TEXT_SIZE = 12;

export const DEFAULT_TEXT_FILL = '#333333';
export const INVERS_TEXT_FILL = '#FFFFFF';

export const DEFAULT_LABEL_FONT = '\'Open Sans\', sans-serif';
export const CODE_LABEL_FONT = 'Source Code Pro, \'Open Sans\', sans-serif';

export const NODE_LABEL_CONFIG: {[type: string]: GraphNodeLabelStyle} = {
    'default': { color: DEFAULT_TEXT_FILL, fontFamily: DEFAULT_LABEL_FONT, fontWeight: FontWeight.NORMAL},
    'code': { color: DEFAULT_TEXT_FILL, fontFamily: CODE_LABEL_FONT, fontWeight: FontWeight.NORMAL},
    'heading': {color: DEFAULT_TEXT_FILL, fontFamily: DEFAULT_LABEL_FONT, fontWeight: FontWeight.BOLD},
    'heading_invers': {color: INVERS_TEXT_FILL, fontFamily: DEFAULT_LABEL_FONT, fontWeight: FontWeight.BOLD}
};

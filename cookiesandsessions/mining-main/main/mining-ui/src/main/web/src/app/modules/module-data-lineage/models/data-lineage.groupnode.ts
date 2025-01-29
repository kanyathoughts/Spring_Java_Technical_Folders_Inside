/**
 * Data lineage group node model.
 */
export interface DataLineageGroupNode {
  id: string;
  label: string;
  parentGroup?: string;
  collapsed?: boolean;
  type?: string;
  statementLabel?: string;
  direction?: string;
  offset?: number;
  length?: number;
}

export interface DiscoveryFeatureMatrix{
  moduleType: string;
  technology: string;
  type: string;
  fullySupportedFeatures: string[];
  contains: string[];
  representation: string[];
  dependencies: Dependency[];
}

export interface Dependency {
  moduleType: string;
  relationship: string[];
  attributes: { [attr: string]: string[] }
}
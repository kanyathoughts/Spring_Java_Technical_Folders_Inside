import { MiningDataPointDefinitionWithPath } from "@innowake/mining-api-angular-client";

export const model = {
    columns: [
        {
            "header": "Upper Bound",
            "field": "upperBoundModuleName.name",
            "fieldType": "STRING",
            "filterProperties": {
                "isFilterActive": false
            },
            "displayAs": "link",
            "sortFn": true,
            "columnAction": {
                "type": "hyperlink"
            },
            "sortOrder": "ascend"
        },
        {
            "header": "Upper Bound Technology",
            "field": "upperBoundModuleTechnology.technology",
            "filterProperties": {
                "isFilterActive": false
            },
            "sortFn": false
        },
        {
            "header": "Upper Bound Type",
            "field": "upperBoundModuleType.type",
            "filterProperties": {
                "isFilterActive": false
            },
            "sortFn": false
        },
        {
            "header": "Upper Bound Taxonomy",
            "field": "taxonomy_upper_bound",
            "fieldType": "STRING",
            "filterProperties": {
                "isFilterActive": false
            },
            "displayAs": "tag",
            "sortFn": false
        },
        {
            "header": "Data Access",
            "field": "dataAccessType",
            "fieldType": "STRING",
            "filterProperties": {
                "isFilterActive": false
            },
            "sortFn": false
        },
        {
            "header": "Lower Bound",
            "field": "lowerBoundModuleName.name",
            "fieldType": "STRING",
            "filterProperties": {
                "isFilterActive": false
            },
            "displayAs": "link",
            "sortFn": true,
            "columnAction": {
                "type": "hyperlink"
            }
        },
        {
            "header": "Lower Bound Technology",
            "field": "lowerBoundModuleTechnology.technology",
            "filterProperties": {
                "isFilterActive": false
            },
            "sortFn": false
        },
        {
            "header": "Lower Bound Type",
            "field": "lowerBoundModuleType.type",
            "filterProperties": {
                "isFilterActive": false
            },
            "sortFn": false
        },
        {
            "header": "Lower Bound Taxonomy",
            "field": "taxonomy_lower_bound",
            "fieldType": "STRING",
            "filterProperties": {
                "isFilterActive": false
            },
            "displayAs": "tag",
            "sortFn": false
        },
        {
            "header": "Accessing Module",
            "field": "accessModulesName.name",
            "fieldType": "STRING",
            "filterProperties": {
                "isFilterActive": false
            },
            "displayAs": "link",
            "sortFn": false,
            "columnAction": {
                "type": "hyperlink"
            }
        },
        {
            "header": "Path Taxonomy",
            "field": "path_taxonomies",
            "fieldType": "STRING",
            "filterProperties": {
                "isFilterActive": false
            },
            "displayAs": "tag",
            "sortFn": false,
            "headerToolTip": "Taxonomies of all modules between upper and lower bound"
        }
    ],
    initialDataPoints: [
        {
            "name": "taxonomy_upper_bound",
            "parentTypeName": "ReachabilityData",
            "scalarType": 'String',
            "referenceTypeName": null,
            "aliasFor": {
                "aliasFor": "moduleTaxonomies",
                "subSelection": "",
                "jsonPath": "",
                "parameters": [
                    "moduleType: \"UpperBound\""
                ]
            },
            "path": "content.taxonomy_upper_bound",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": Set[
                "miningUi.reachabilityTable"
            ],
            "usageAttributes": {
                "general.viewMode": {
                    "displayAs": "tag"
                },
                "miningUi.reachabilityTable": {
                    "category": "Taxonomy",
                    "defaultColumnIndex": "3"
                }
            },
            "displayName": "Upper Bound Taxonomy",
            "description": "",
            "array": true,
            "id": "ReachabilityData.taxonomy_upper_bound",
            "nullable": true,
            "alias": true
        },
        {
            "name": "path_taxonomies",
            "parentTypeName": "ReachabilityData",
            "scalarType": "String",
            "referenceTypeName": null,
            "aliasFor": {
                "aliasFor": "moduleTaxonomies",
                "subSelection": "",
                "jsonPath": "",
                "parameters": [
                    "moduleType: \"PathTaxonomies\""
                ]
            },
            "path": "content.path_taxonomies",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": Set[
                "miningUi.reachabilityTable"
            ],
            "usageAttributes": {
                "general.viewMode": {
                    "displayAs": "tag"
                },
                "miningUi.reachabilityTable": {
                    "category": "Taxonomy",
                    "defaultColumnIndex": "10"
                }
            },
            "displayName": "Path Taxonomy",
            "description": "",
            "array": true,
            "id": "ReachabilityData.path_taxonomies",
            "nullable": true,
            "alias": true
        },
        {
            "name": "intermediateModuleName",
            "parentTypeName": "ReachabilityData",
            "scalarType": "String",
            "referenceTypeName": null,
            "aliasFor": {
                "aliasFor": "intermediateModulesData",
                "subSelection": "name",
                "jsonPath": "",
                "parameters": []
            },
            "path": "content.intermediateModuleName.name",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": new Set([
                "general.viewMode",
                "miningUi.reachabilityTable"
            ]),
            "usageAttributes": {
                "general.viewMode": {
                    "linkTemplate": "/project-${$projectId}/module-${linkHash}/details/overview",
                    "displayAs": "link",
                    "togetherWith": "intermediateModuleName.linkHash"
                },
                "miningUi.reachabilityTable": {
                    "category": "Base Data"
                }
            },
            "displayName": "Intermediate Modules",
            "description": "List of intermediate Modules between upper bound and access module",
            "array": false,
            "id": "ReachabilityData.intermediateModuleName",
            "nullable": true,
            "alias": true
        },
        {
            "name": "upperBoundModuleTechnology",
            "parentTypeName": "ReachabilityData",
            "scalarType": null,
            "referenceTypeName": "Technology",
            "aliasFor": {
                "aliasFor": "upperBoundModules",
                "subSelection": "technology",
                "jsonPath": "",
                "parameters": []
            },
            "path": "content.upperBoundModuleTechnology.technology",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": Set[
                "miningUi.reachabilityTable"
            ],
            "usageAttributes": {
                "miningUi.reachabilityTable": {
                    "category": "Base Data",
                    "defaultColumnIndex": "6"
                }
            },
            "displayName": "Upper Bound Technology",
            "description": "Technology of the Upper Bound Module",
            "array": false,
            "id": "ReachabilityData.upperBoundModuleTechnology",
            "nullable": true,
            "alias": true
        },
        {
            "name": "lowerBoundModuleType",
            "parentTypeName": "ReachabilityData",
            "scalarType": null,
            "referenceTypeName": "Type",
            "aliasFor": {
                "aliasFor": "lowerBoundModules",
                "subSelection": "type",
                "jsonPath": "",
                "parameters": []
            },
            "path": "content.lowerBoundModuleType.type",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": Set[
                "miningUi.reachabilityTable"
            ],
            "usageAttributes": {
                "miningUi.reachabilityTable": {
                    "category": "Base Data",
                    "defaultColumnIndex": "2"
                }
            },
            "displayName": "Lower Bound Type",
            "description": "Type of the Upper Bound Module",
            "array": false,
            "id": "ReachabilityData.lowerBoundModuleType",
            "nullable": true,
            "alias": true
        },
        {
            "name": "lowerBoundModuleTechnology",
            "parentTypeName": "ReachabilityData",
            "scalarType": null,
            "referenceTypeName": "Technology",
            "aliasFor": {
                "aliasFor": "lowerBoundModules",
                "subSelection": "technology",
                "jsonPath": "",
                "parameters": []
            },
            "path": "content.lowerBoundModuleTechnology.technology",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": Set[
                "miningUi.reachabilityTable"
            ],
            "usageAttributes": {
                "miningUi.reachabilityTable": {
                    "category": "Base Data",
                    "defaultColumnIndex": "1"
                }
            },
            "displayName": "Lower Bound Technology",
            "description": "Technology of the Lower Bound Module",
            "array": false,
            "id": "ReachabilityData.lowerBoundModuleTechnology",
            "nullable": true,
            "alias": true
        },
        {
            "name": "lowerBoundModuleName",
            "parentTypeName": "ReachabilityData",
            "scalarType": "String",
            "referenceTypeName": null,
            "aliasFor": {
                "aliasFor": "lowerBoundModules",
                "subSelection": "name",
                "jsonPath": "",
                "parameters": []
            },
            "path": "content.lowerBoundModuleName.name",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": new Set([
                "general.viewMode",
                "graphql.query.reachabilityData",
                "general.sortBy",
                "miningUi.reachabilityTable"
            ]),
            "usageAttributes": {
                "general.viewMode": {
                    "linkTemplate": "/project-${$projectId}/module-${linkHash}/details/overview",
                    "displayAs": "link",
                    "togetherWith": "lowerBoundModuleName.linkHash"
                },
                "miningUi.reachabilityTable": {
                    "category": "Base Data",
                    "defaultColumnIndex": "0"
                }
            },
            "displayName": "Lower Bound",
            "description": "Module name of the lower Bound Module",
            "array": false,
            "id": "ReachabilityData.lowerBoundModuleName",
            "nullable": true,
            "alias": true
        },
        {
            "name": "dataAccessType",
            "parentTypeName": "ReachabilityData",
            "scalarType": "String",
            "referenceTypeName": null,
            "aliasFor": null,
            "path": "content.dataAccessType",
            "projectIds": null,
            "providedBy": new Set([
                "innowake.mining.server.graphql.controller.FunctionalBlocksGraphQlController",
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ]),
            "parameters": [],
            "usages": Set[
                "miningUi.reachabilityTable"
            ],
            "usageAttributes": {
                "miningUi.reachabilityTable": {
                    "category": "Base Data",
                    "defaultColumnIndex": "4"
                }
            },
            "displayName": "Data Access",
            "description": "Access types by which the upper bound accesses the lower bound",
            "array": true,
            "id": "ReachabilityData.dataAccessType",
            "nullable": true,
            "alias": false
        },
        {
            "name": "upperBoundModuleType",
            "parentTypeName": "ReachabilityData",
            "scalarType": null,
            "referenceTypeName": "Type",
            "aliasFor": {
                "aliasFor": "upperBoundModules",
                "subSelection": "type",
                "jsonPath": "",
                "parameters": []
            },
            "path": "content.upperBoundModuleType.type",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": Set[
                "miningUi.reachabilityTable"
            ],
            "usageAttributes": {
                "miningUi.reachabilityTable": {
                    "category": "Base Data",
                    "defaultColumnIndex": "7"
                }
            },
            "displayName": "Upper Bound Type",
            "description": "Type of the Upper Bound Module",
            "array": false,
            "id": "ReachabilityData.upperBoundModuleType",
            "nullable": true,
            "alias": true
        },
        {
            "name": "accessModulesName",
            "parentTypeName": "ReachabilityData",
            "scalarType": "String",
            "referenceTypeName": null,
            "aliasFor": {
                "aliasFor": "accessModules",
                "subSelection": "name",
                "jsonPath": "",
                "parameters": []
            },
            "path": "content.accessModulesName.name",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": new Set([
                "general.viewMode",
                "miningUi.reachabilityTable"
            ]),
            "usageAttributes": {
                "general.viewMode": {
                    "linkTemplate": "/project-${$projectId}/module-${linkHash}/details/overview",
                    "displayAs": "link",
                    "togetherWith": "accessModulesName.linkHash"
                },
                "miningUi.reachabilityTable": {
                    "category": "Base Data",
                    "defaultColumnIndex": "9"
                }
            },
            "displayName": "Accessing Module",
            "description": "Module name of the Access Module",
            "array": false,
            "id": "ReachabilityData.accessModulesName",
            "nullable": true,
            "alias": true
        },
        {
            "name": "taxonomy_lower_bound",
            "parentTypeName": "ReachabilityData",
            "scalarType": "String",
            "referenceTypeName": null,
            "aliasFor": {
                "aliasFor": "moduleTaxonomies",
                "subSelection": "",
                "jsonPath": "",
                "parameters": [
                    "moduleType: \"LowerBound\""
                ]
            },
            "path": "content.taxonomy_lower_bound",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": Set[
                "miningUi.reachabilityTable"
            ],
            "usageAttributes": {
                "general.viewMode": {
                    "displayAs": "tag"
                },
                "miningUi.reachabilityTable": {
                    "category": "Taxonomy",
                    "defaultColumnIndex": "8"
                }
            },
            "displayName": "Lower Bound Taxonomy",
            "description": "",
            "array": true,
            "id": "ReachabilityData.taxonomy_lower_bound",
            "nullable": true,
            "alias": true
        },
        {
            "name": "upperBoundModuleName",
            "parentTypeName": "ReachabilityData",
            "scalarType": "String",
            "referenceTypeName": null,
            "aliasFor": {
                "aliasFor": "upperBoundModules",
                "subSelection": "name",
                "jsonPath": "",
                "parameters": []
            },
            "path": "content.upperBoundModuleName.name",
            "projectIds": null,
            "providedBy": Set[
                "innowake.mining.server.graphql.controller.FunctionalBlocksDataPointSource"
            ],
            "parameters": [],
            "usages": new Set([
                "general.viewMode",
                "graphql.query.reachabilityData",
                "general.sortBy",
                "miningUi.reachabilityTable"
            ]),
            "usageAttributes": {
                "general.viewMode": {
                    "linkTemplate": "/project-${$projectId}/module-${linkHash}/details/overview",
                    "displayAs": "link",
                    "togetherWith": "upperBoundModuleName.linkHash"
                },
                "miningUi.reachabilityTable": {
                    "category": "Base Data",
                    "defaultColumnIndex": "5"
                }
            },
            "displayName": "Upper Bound",
            "description": "Module name of the Upper Bound Module",
            "array": false,
            "id": "ReachabilityData.upperBoundModuleName",
            "nullable": true,
            "alias": true
        }
    ] as MiningDataPointDefinitionWithPath[],
    mergeBlockData: {
        blockName: 'Merged block',
        blockDescription: 'Merged block description',
    },
    blocks: {
        uid: '000b38b0-d745-49df-9125-0bdf5bd90da7',
        name: 'READCARD Reachability',
        description: '',
        type: ['RA_TOP_DOWN', 'REACHABILITY'],
        status: 'ACTIVE',
        outdatedBlock: false,
        blocksWithDeletedUB: false,
        resolvedModuleParts: [
            {
                module: {
                    id: 79,
                    errorCount: 0,
                    dependencies: [
                        {
                            module: {
                                id: 81
                            }
                        }
                    ]
                },
                referencedTaxonomies: [
                    {
                        name: 'Batch'
                    },
                    {
                        name: 'Read'
                    }
                ]
            }, {
                module: {
                    id: 121,
                    errorCount: 1,
                    dependencies: []
                },
                referencedTaxonomies: null
            }
        ],
        peers: {
            totalElements: 0
        }
    },
    aggregationResult: [{
        "group": {
            "TECHNOLOGY": "ASSEMBLER",
            "TYPE": "UNKNOWN"
        },
        "fields": {
            "ID": 77
        }
    }, {
        "group": {
            "TECHNOLOGY": "BASIC",
            "TYPE": "OBJECT"
        },
        "fields": {
            "ID": 17
        }
    }, {
        "group": {
            "TECHNOLOGY": "BASIC",
            "TYPE": "PROGRAM"
        },
        "fields": {
            "ID": 2
        }
    }, {
        "group": {
            "TECHNOLOGY": "BASIC",
            "TYPE": "SUBROUTINE"
        },
        "fields": {
            "ID": 2
        }
    }, {
        "group": {
            "TECHNOLOGY": "C",
            "TYPE": "FUNCTION"
        },
        "fields": {
            "ID": 44
        }
    }]
};

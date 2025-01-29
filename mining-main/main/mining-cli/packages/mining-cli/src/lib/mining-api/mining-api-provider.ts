import { AnnotationCategoryControllerApi, AnnotationReportingControllerApi, AnnotationSearchControllerApi, CandidateIdentificationControllerApi, ClientControllerApi, ClientControllerV2Api, ControlFlowControllerApi, DataDictionaryControllerApi, DiscoveryControllerApi, FeatureControllerApi, HotSpotControllerApi, InfoControllerApi, IoControllerApi, JobControllerApi, MemberControllerApi, MetamodelControllerApi, MiningUiExtensionsControllerApi, ModuleControllerApi, ProjectControllerApi, ProjectControllerV2Api, ReferenceControllerApi, TaxonomyControllerApi, TaxonomyTypeControllerApi, VersionControllerApi } from 'mining-api-client';
import { IOC } from 'iw-ioc';
import { MiningApiConfig, MINING_API_CONFIG } from './mining-api-config';
import { Newable } from 'ts-essentials';

const APIS = [
    AnnotationCategoryControllerApi,
    AnnotationReportingControllerApi,
    AnnotationSearchControllerApi,
    CandidateIdentificationControllerApi,
    ClientControllerApi,
    ClientControllerV2Api,
    ControlFlowControllerApi,
    DataDictionaryControllerApi,
    DiscoveryControllerApi,
    FeatureControllerApi,
    HotSpotControllerApi,
    InfoControllerApi,
    IoControllerApi,
    JobControllerApi,
    MemberControllerApi,
    MetamodelControllerApi,
    MiningUiExtensionsControllerApi,
    ModuleControllerApi,
    ProjectControllerApi,
    ProjectControllerV2Api,
    ReferenceControllerApi,
    TaxonomyControllerApi,
    TaxonomyTypeControllerApi,
    VersionControllerApi
];

export async function registerApiProviders(configToken?: Newable<MiningApiConfig> | string | symbol) {
    const config = IOC.get(configToken ?? MINING_API_CONFIG);

    await config?.init();

    APIS.forEach(api => {
        IOC.registerComponent(class extends api {
            constructor() {
                super(config?.config, config?.basePath, config?.axios);
            }
        });
    });
}

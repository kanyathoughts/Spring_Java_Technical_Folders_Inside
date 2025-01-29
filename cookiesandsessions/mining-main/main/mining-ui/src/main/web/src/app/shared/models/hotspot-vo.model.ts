import { MiningTableRow } from '../components/mining-table/mining-table-config.interface';
import { EntityId, ModulePojo } from '@innowake/mining-api-angular-client';

/**
 * Model class to hold the hotspot data
 */
export class HotSpotVO implements MiningTableRow {
    constructor(public id: number,
        public containingModuleId: EntityId,
        private moduleName: string,
        private language: string,
        private type: string,
        private count: string,
        private identification: ModulePojo.IdentificationEnum,
        private storage: ModulePojo.StorageEnum,
        private sourceCodeAvailable: boolean,
        private linkHash: string) {}

        getId(): number {
            return this.id;
        }

        getModuleName(): string {
            return this.moduleName;
        }

        getLanguage(): string {
            return this.language;
        }

        getType(): string {
            return this.type;
        }

        getCount(): string {
            return this.count;
        }

        getcontainingModuleId(): EntityId {
            return this.containingModuleId;
        }

        getlinkHash(): string {
            return this.linkHash;
        }
}


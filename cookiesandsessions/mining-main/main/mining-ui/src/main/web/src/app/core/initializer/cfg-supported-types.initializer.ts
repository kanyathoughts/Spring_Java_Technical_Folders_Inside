import { CfgSupportedTypeService } from '../services/cfg-supported-type.service';

export const initializeCfgSupportedTypes = (cfgSupportService: CfgSupportedTypeService): () => Promise<void> =>
  async () => {
    await cfgSupportService.init();
  };

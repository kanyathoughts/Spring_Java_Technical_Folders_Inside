import { LabelMappingService } from '../services/label-mapping.service';

export const initializeLabelMapping = (mappingService: LabelMappingService): () => Promise<void> =>
  async () => {
    await mappingService.init();
  };

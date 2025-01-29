import { ModulePojo } from '@innowake/mining-api-angular-client';

export const calculateModuleComplexity = (module: ModulePojo): { color: string, description: string; } => {
    if (module.sourceMetrics) {
        const complexity: number | string = module.sourceMetrics.complexityMcCabe !== null ?
            module.sourceMetrics.complexityMcCabe : 'notAvailable';
        let color;
        let description;
        if (+complexity <= 10) {
            color = 'green';
            description = 'module.lowComplexity';
        } else if (+complexity <= 20) {
            color = 'gold';
            description = 'module.mediumComplexity';
        } else if (+complexity <= 50) {
            color = 'red';
            description = 'module.highComplexity';
        } else if (complexity === 'notAvailable') {
            description = 'notAvailable';
        } else {
            color = 'brown';
            description = 'module.veryHighComplexity';
        }
        return { color, description };
    }
};

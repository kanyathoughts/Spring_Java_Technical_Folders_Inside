export {};

declare global {
  const asyncOperations: Promise<any>[];
}

(global as any).asyncOperations = [];
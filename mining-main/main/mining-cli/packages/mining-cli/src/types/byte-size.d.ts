declare module 'byte-size' {
    const byteSize: (size: number) => { value: string, unit: string, long: string };

    export = byteSize;
}
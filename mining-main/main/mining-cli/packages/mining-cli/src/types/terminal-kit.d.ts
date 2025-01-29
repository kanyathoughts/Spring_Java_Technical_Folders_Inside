import { Terminal } from 'terminal-kit';

declare module 'terminal-kit' {
    namespace Terminal {
        interface Impl {
            table(table: string[][], options?: any): void;
        }
    }
}
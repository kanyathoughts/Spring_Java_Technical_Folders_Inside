import axios from 'axios';

export function withDefaultErrorHandling<T extends (...args: any[]) => Promise<any>>(command: T): T {
    return (async (...args: any[]) => {
        try {
            return await command(...args);
        } catch (err) {
            if (axios.isAxiosError(err)) {
                console.error('Request failed:', err.config.method, err.config.url);
                console.error(err.message);
            } else {
                console.error(err.toString());
                if (err.stack) {
                    console.error(err.stack);
                }
            }
            throw err;
        }
    }) as T;
}
import { WindowToken, windowFunction } from './window';

describe('WindowToken', () => {
    it('should have window attributes', () => {
        expect(WindowToken).toBeTruthy();
        const window = windowFunction();
        expect(window).toBeTruthy();
    });
});

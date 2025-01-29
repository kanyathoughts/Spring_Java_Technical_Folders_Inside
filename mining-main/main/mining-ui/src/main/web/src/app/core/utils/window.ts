import { InjectionToken } from '@angular/core';

export const WindowToken = new InjectionToken<string>('Window');

export const  windowFunction = (): Window => window;


import 'reflect-metadata';
import { Newable } from 'ts-essentials';
import { IOC } from './ioc-api';
import { Lifetime, LifetimeType } from 'awilix';
import { forEach, isArray } from 'lodash';

const LIFETIME = Symbol();
const CONSTRUCTOR_PARAMETERS = Symbol();

export function Component(... aliases: Array<string | symbol>) {
  return (type: Newable<any>) => {
    IOC.registerComponent(type, ...aliases);
  };
}

export function Singleton() {
  return (type: Newable<any>) => {
    Reflect.defineMetadata(LIFETIME, Lifetime.SINGLETON, type);
  };
}

export function Transient() {
  return (type: Newable<any>) => {
    Reflect.defineMetadata(LIFETIME, Lifetime.TRANSIENT, type);
  };
}

export function Scoped() {
  return (type: Newable<any>) => {
    Reflect.defineMetadata(LIFETIME, Lifetime.SCOPED, type);
  };
}

export function getLifetime(type: Newable<any>): LifetimeType {
  return Reflect.getMetadata(LIFETIME, type) || Lifetime.SINGLETON;
}

export function Inject(params: Array<string | symbol | Newable<any>>): (type: Newable<any>) => void;
export function Inject(token: string | symbol | Newable<any>): (target: object, propertyKey: string | symbol, index: number) => void;
export function Inject(token: Array<string | symbol | Newable<any>> | string | symbol | Newable<any>) {
  if (isArray(token)) {
    return (type: Newable<any>) => {
      const ctorParams = getConstructorParameters(type);
      forEach(token, (param, i) => {
        if (ctorParams[i] === undefined) {
          ctorParams[i] = param;
        }
      });
      Reflect.defineMetadata(CONSTRUCTOR_PARAMETERS, ctorParams, type);
    };
  } else {
    return (target: object, propertyKey: string | symbol, index: number) => {
      const ctorParams = getConstructorParameters(target);
      ctorParams[index] = token;
      Reflect.defineMetadata(CONSTRUCTOR_PARAMETERS, ctorParams, target);
    };
  }
}

export function getConstructorParameters(type: any): Array<string | symbol | Newable<any>> {
  return Reflect.getMetadata(CONSTRUCTOR_PARAMETERS, type) || {};
}

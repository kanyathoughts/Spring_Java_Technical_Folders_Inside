import { Newable } from 'ts-essentials';
import { ComponentRegistry } from './component-registry';
import * as awilix from 'awilix';
import { getConstructorParameters, getLifetime } from './ioc-annotations';
import { forEach, map } from 'lodash';

function componentFactory(type: Newable<any>) {
  return (cradle: any) => {
    const ctorParameters = getConstructorParameters(type);
    const ctorArguments = map(ctorParameters, token => IOC.get(token, cradle.container));

    return new type(...ctorArguments);
  };
}

const ROOT_CONTAINER = awilix.createContainer({
  injectionMode: 'PROXY'
});
const COMPONENT_REGISTRY = new ComponentRegistry();

export class IOC {

  static getRootContainer() {
    return ROOT_CONTAINER;
  }

  static registerComponent(type: Newable<any>, ...aliases: Array<string | symbol>) {
    const name = COMPONENT_REGISTRY.registerType(type);
    ROOT_CONTAINER.register(name, awilix.asFunction(componentFactory(type), {
      lifetime: getLifetime(type),
      injector: (container) => ({ container })
    }));
    forEach(aliases, alias => {
      ROOT_CONTAINER.register(alias, awilix.aliasTo(name));
    });
  }

  static get<T>(nameOrType: string | symbol | Newable<T>, container = ROOT_CONTAINER): T | undefined {
    if (typeof nameOrType === 'string' || typeof nameOrType === 'symbol') {
      return container.resolve(nameOrType);
    }
    const name = COMPONENT_REGISTRY.resolveType(nameOrType);
    if (name === undefined) {
      return undefined;
    }
    return container.resolve(name);
  }
}
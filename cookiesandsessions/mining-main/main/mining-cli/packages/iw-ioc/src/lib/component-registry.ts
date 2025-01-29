import { Newable } from 'ts-essentials';

class AbstractIocError extends Error {
  constructor(message: string) {
    super(message);
    Error.captureStackTrace(this, AbstractIocError);
  }
}

export class DuplicateTypeError extends AbstractIocError {}

export class ComponentRegistry {

  private readonly componentNames: Set<string | symbol> = new Set();
  private readonly componentTypeToNames: Map<Newable<any>, Array<string | symbol>> = new Map();

  registerType(type: Newable<any>): string {
    let name = type.name;
    let i = 0;
    while (this.componentNames.has(name)) {
      name = `${type.name}$${i++}`;
    }
    this.componentNames.add(name);

    this.addType(type, name);
    let parentType = type;
    while (parentType && parentType.prototype && parentType.prototype !== Object.prototype) {
      parentType = Object.getPrototypeOf(parentType.prototype)?.constructor;
      if (parentType) {
        this.addType(parentType, name);
      }
    }

    return name;
  }

  private addType(type: Newable<any>, name: string | symbol) {
    const names = this.componentTypeToNames.get(type) || [];
    names.push(name);
    this.componentTypeToNames.set(type, names);
  }

  resolveType(type: Newable<any>): string | symbol | undefined {
    const names = this.componentTypeToNames.get(type);
    if (names === undefined) {
      return undefined;
    }
    if (names.length !== 1) {
      throw new DuplicateTypeError(`More than one component is registered that provides the type ${type.name}`);
    }
    return names[0];
  }
}
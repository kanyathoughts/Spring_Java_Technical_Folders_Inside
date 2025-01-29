import { Injectable } from '@angular/core';

export const MINING_STORAGE_STATE_KEY = 'MiningStateServiceData';
import {ClearStateOn, StateStorage, MiningServiceDataInterface, StateKey} from '../../../shared/interfaces/state-maintainance.interface';

/**
 * Class that is used for storing the state information like
 * storageKey, storageType (session, local)
 * and clearStateOn (when the state should be cleared)
 */
export class MiningPageState {
  private inputStorageType: StateStorage;
  private inputStorageKey: StateKey;
  private inputClearStateOn: ClearStateOn;

  constructor(storageType: StateStorage, storageKey: StateKey, clearStateOn: ClearStateOn) {
    this.storageKey = storageKey;
    this.storageType = storageType;
    this.clearStateOn = clearStateOn;
  }

  public get storageType(): StateStorage {
    return this.inputStorageType;
  }

  public get storageKey(): StateKey {
    return this.inputStorageKey;
  }

  public get clearStateOn(): ClearStateOn {
    return this.inputClearStateOn;
  }

  public set storageType(value: StateStorage) {
    this.inputStorageType = value;
  }

  public set storageKey(value: StateKey) {
    this.inputStorageKey = value;
  }

  public set clearStateOn(value: ClearStateOn) {
    this.inputClearStateOn = value;
  }
}

/**
 * Service class that maintains the state of the all the pages
 * and is cleared on request
 */
@Injectable({
  providedIn: 'root'
})
export class StateMaintainenceService {
  constructor() {}

  /**
   * Stores the state information, is called on onSaveSate event of the table
   * @param miningPageState the MiningPageState object
   */
  public setState(miningPageState: MiningPageState): void {
    const miningStateServiceData = sessionStorage.hasOwnProperty(MINING_STORAGE_STATE_KEY)
      ? JSON.parse(sessionStorage.getItem(MINING_STORAGE_STATE_KEY))
      : {};

    miningStateServiceData[miningPageState.storageKey] = {
      storageType: miningPageState.storageType,
      clearState: miningPageState.clearStateOn
    };
    sessionStorage.setItem(MINING_STORAGE_STATE_KEY, JSON.stringify(miningStateServiceData));
  }

  /**
   * Clears all the saved states which can be cleared on the project change
   */
  public clearStateOnProjectChange(): void {
    const miningStateServiceData: MiningServiceDataInterface = JSON.parse(sessionStorage.getItem(MINING_STORAGE_STATE_KEY));
    let keys: string[] = [];

    if (miningStateServiceData != null) {
      keys = Object.keys(miningStateServiceData);
      keys.forEach(key => {
        if (miningStateServiceData[key].clearState === ClearStateOn.ProjectChange) {
          if (miningStateServiceData[key].storageType === StateStorage.Session) {
            sessionStorage.removeItem(key);
          } else {
            localStorage.removeItem(key);
          }
        }
      });
      keys.forEach(key => delete miningStateServiceData[key]);
      sessionStorage.setItem(MINING_STORAGE_STATE_KEY, JSON.stringify(miningStateServiceData));
    }
  }

  /**
   * Clears all the saved states.
   */
  public clearAllTheSavedStates(): void {
    const miningStateServiceData: MiningServiceDataInterface= JSON.parse(sessionStorage.getItem(MINING_STORAGE_STATE_KEY));
    if (miningStateServiceData != null) {
      Object.keys(miningStateServiceData).forEach(key => {
        if (miningStateServiceData[key].storageType === StateStorage.Session) {
          sessionStorage.removeItem(key);
        } else {
          localStorage.removeItem(key);
        }
      });
    }
    sessionStorage.removeItem(MINING_STORAGE_STATE_KEY);
  }
}

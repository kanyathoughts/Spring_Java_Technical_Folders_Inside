import { TestBed } from '@angular/core/testing';

import { StateMaintainenceService, MiningPageState, MINING_STORAGE_STATE_KEY } from './state-maintainence.service';
import {StateStorage , ClearStateOn, StateKey}  from '../../../shared/interfaces/state-maintainance.interface';

describe('StateMaintainenceService', () => {
  let service: StateMaintainenceService;
  beforeEach(() => TestBed.configureTestingModule({}));

  beforeEach(() => {
    service = TestBed.inject(StateMaintainenceService);
    sessionStorage.clear();
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should clear state on project change', () => {
    service.setState(new MiningPageState(StateStorage.Session, StateKey.MiningTableRowSelectionKey, ClearStateOn.ProjectChange));
    service.clearStateOnProjectChange();
    expect(sessionStorage.getItem(MINING_STORAGE_STATE_KEY)).toBe('{}');

    service.setState(new MiningPageState(StateStorage.Local, StateKey.MiningTableRowSelectionKey, ClearStateOn.ProjectChange));
    service.clearStateOnProjectChange();
    expect(sessionStorage.getItem(MINING_STORAGE_STATE_KEY)).toBe('{}');

    service.setState(new MiningPageState(StateStorage.Local, StateKey.MiningTableRowSelectionKey, null));
    service.clearStateOnProjectChange();
    expect(sessionStorage.getItem(MINING_STORAGE_STATE_KEY)).toBe('{}');

    service.clearStateOnProjectChange();
    expect(sessionStorage.getItem(MINING_STORAGE_STATE_KEY)).toBe('{}');

    service.clearAllTheSavedStates();
    service.clearStateOnProjectChange();
    expect(sessionStorage.hasOwnProperty(MINING_STORAGE_STATE_KEY)).toBeFalsy();
  });

  it('should clear all saved states', () => {
    service.setState(new MiningPageState(StateStorage.Session, StateKey.MiningTableRowSelectionKey, ClearStateOn.ProjectChange));
    service.clearAllTheSavedStates();
    expect(sessionStorage.length).toBe(0);

    service.setState(new MiningPageState(StateStorage.Local, StateKey.MiningTableRowSelectionKey, ClearStateOn.ProjectChange));
    service.clearAllTheSavedStates();
    expect(sessionStorage.length).toBe(0);

    service.clearAllTheSavedStates();
    expect(sessionStorage.length).toBe(0);

    service.setState(new MiningPageState(StateStorage.Local, StateKey.MiningTableRowSelectionKey, null));
    service.clearAllTheSavedStates();
    expect(sessionStorage.hasOwnProperty(MINING_STORAGE_STATE_KEY)).toBeFalsy();
  });
});

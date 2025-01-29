import { AnnotationPojo } from "@innowake/mining-api-angular-client";
import { StateChange } from "./stateChange.util";

describe('StateChangeUtil', () => {
  
  it('should get next state', () => {
    const nextState1 = StateChange.getNext(AnnotationPojo.StateEnum.CANDIDATE);
    expect(nextState1[0]).toBe(AnnotationPojo.StateEnum.IN_ANALYSIS);
    const nextState2 = StateChange.getNext(AnnotationPojo.StateEnum.IN_ANALYSIS);
    expect(nextState2[0]).toBe(AnnotationPojo.StateEnum.FOR_REVIEW);
    expect(nextState2[1]).toBe(AnnotationPojo.StateEnum.REJECTED);
    const nextState3 = StateChange.getNext(AnnotationPojo.StateEnum.FOR_REVIEW);
    expect(nextState3[0]).toBe(AnnotationPojo.StateEnum.APPROVED);
    expect(nextState3[1]).toBe(AnnotationPojo.StateEnum.INVALID);
    const nextState4 = StateChange.getNext(AnnotationPojo.StateEnum.INVALID);
    expect(nextState4.length).toBe(0);
    const nextState5 = StateChange.getNext(AnnotationPojo.StateEnum.REJECTED);
    expect(nextState5.length).toBe(0);
    const nextState6 = StateChange.getNext(AnnotationPojo.StateEnum.APPROVED);
    expect(nextState6.length).toBe(0);
  });

  it('should get previous state', () => {
    const nextState1 = StateChange.getPrevious(AnnotationPojo.StateEnum.CANDIDATE);
    expect(nextState1).toBeUndefined();
    const nextState2 = StateChange.getPrevious(AnnotationPojo.StateEnum.IN_ANALYSIS);
    expect(nextState2).toBe(AnnotationPojo.StateEnum.CANDIDATE);
    const nextState3 = StateChange.getPrevious(AnnotationPojo.StateEnum.FOR_REVIEW);
    expect(nextState3).toBe(AnnotationPojo.StateEnum.IN_ANALYSIS);
    const nextState4 = StateChange.getPrevious(AnnotationPojo.StateEnum.REJECTED);
    expect(nextState4).toBe(AnnotationPojo.StateEnum.IN_ANALYSIS);
    const nextState5 = StateChange.getPrevious(AnnotationPojo.StateEnum.INVALID);
    expect(nextState5).toBe(AnnotationPojo.StateEnum.FOR_REVIEW);
    const nextState6 = StateChange.getPrevious(AnnotationPojo.StateEnum.APPROVED);
    expect(nextState6).toBe(AnnotationPojo.StateEnum.FOR_REVIEW);
  });
});

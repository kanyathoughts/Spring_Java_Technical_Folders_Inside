import { OnInit, OnDestroy } from '@angular/core';
import { TestBed } from '@angular/core/testing';
import { ActivatedRoute, ActivatedRouteSnapshot, Route } from '@angular/router';
import { Subject, Subscription } from 'rxjs';

import { RouteReusableStrategy } from './route-reusable-strategy';

describe('RouteReusableStrategy', () => {

    const activatedRouteSnapshotSpy: jasmine.SpyObj<ActivatedRouteSnapshot> =
        jasmine.createSpyObj('ActivatedRouteSnapshot', ['children']);
    let routeReusableStrategy: RouteReusableStrategy;

    beforeEach(() => {
        TestBed.configureTestingModule({
          providers: [
              RouteReusableStrategy,
              { provide: ActivatedRouteSnapshot, useValue: activatedRouteSnapshotSpy}
          ]
        });
        routeReusableStrategy = TestBed.inject(RouteReusableStrategy);
      });

    it('should not attach the current route and its subtree', () => {
        expect(routeReusableStrategy.shouldAttach(null)).toBeFalse();
    });

    it('should not detach the current route and its subtree', () => {
        expect(routeReusableStrategy.shouldDetach(null)).toBeFalse();
    });

    it('should return null on trying to retrieve the previously stored route', () => {
        expect(routeReusableStrategy.retrieve(null as any)).toBeNull();
    });

    it('should reuse route if current and future route config is same', () => {
        const futureRouteSnapshot: ActivatedRouteSnapshot = {...activatedRouteSnapshotSpy};
        const route: Route = {
            data: {
                reuse: true,
                shell: 'project-shell'
            }
        };
        (activatedRouteSnapshotSpy as any).routeConfig = route;
        (futureRouteSnapshot as any).routeConfig = route;
        expect(routeReusableStrategy.shouldReuseRoute(futureRouteSnapshot, activatedRouteSnapshotSpy)).toBeTrue();
    });
});


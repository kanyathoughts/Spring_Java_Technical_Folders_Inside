import { Observable, Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';

const untilDestroyedSymbol = Symbol('untilDestroyed');

/**
 * RxJS operator that unsubscribe from observables on destory.
 * Code forked from https://github.com/NetanelBasal/ngx-take-until-destroy
 *
 * IMPORTANT: Add the `untilDestroyed` operator as the last one to prevent leaks with intermediate observables in the
 * operator chain.
 *
 * @param instance The parent Angular component or object instance.
 * @param destroyMethodName The method to hook on (default: 'ngOnDestroy').
 * @example
 * ```
 * import { untilDestroyed } from '@app/core';
 *
 * @Component({
 * selector: 'app-example',
 * templateUrl: './example.component.html'
 * })
 * export class ExampleComponent implements OnInit, OnDestroy {
 * ngOnInit() {
 * interval(1000)
 * .pipe(untilDestroyed(this))
 * .subscribe(val => console.log(val));
 * }
 *
 * // This method must be present, even if empty.
 * ngOnDestroy() {
 * // To protect you, an error will be thrown if it doesn't exist.
 * }
 * }
 * ```
 */
// eslint-disable-next-line @typescript-eslint/ban-types
export const  untilDestroyed =(instance: object, destroyMethodName: string = 'ngOnDestroy', ...args: any[]) => <T>(source: Observable<T>): Observable<T> => {
    const originalDestroy = instance[destroyMethodName];
    const hasDestroyFunction = typeof originalDestroy === 'function';

    if (!hasDestroyFunction) {
      throw new Error(
        `${instance.constructor.name} is using untilDestroyed but doesn't implement ${destroyMethodName}`
      );
    }

    if (!instance[untilDestroyedSymbol]) {
      instance[untilDestroyedSymbol] = new Subject();

      instance[destroyMethodName] = function() {
        if (hasDestroyFunction) {
          originalDestroy.apply(this, args);
        }
        instance[untilDestroyedSymbol].next();
        instance[untilDestroyedSymbol].complete();
      };
    }
    /* eslint-disable @typescript-eslint/no-unsafe-argument */
    return source.pipe(takeUntil<T>(instance[untilDestroyedSymbol]));
  };

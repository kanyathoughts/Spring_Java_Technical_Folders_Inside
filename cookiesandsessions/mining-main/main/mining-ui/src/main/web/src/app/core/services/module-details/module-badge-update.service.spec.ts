
import { TestBed } from '@angular/core/testing';
import { ModuleBadgeUpdateService } from './module-badge-update.service';


describe('ModuleBadgeUpdateService', () => {
  let moduleBadgeUpdateService: ModuleBadgeUpdateService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [ModuleBadgeUpdateService],
    }).compileComponents();
    moduleBadgeUpdateService = TestBed.inject(ModuleBadgeUpdateService);
  });

  it('should create instance of ModuleBadgeUpdateService', () => {
    expect(moduleBadgeUpdateService).toBeTruthy();

    moduleBadgeUpdateService.getModuleToBeReviewed().subscribe(result => {
      expect(result).toBeTruthy();
    });

    moduleBadgeUpdateService.getAnnotationDataDictionary().subscribe(result => {
      expect(result).toBeTruthy();
    });
  });
});

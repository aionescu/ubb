import { ComponentFixture, TestBed } from '@angular/core/testing';

import { AddPackageVersionComponent } from './add-package-version.component';

describe('AddPackageMaintainerComponent', () => {
  let component: AddPackageVersionComponent;
  let fixture: ComponentFixture<AddPackageVersionComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ AddPackageVersionComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AddPackageVersionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

import { ComponentFixture, TestBed } from '@angular/core/testing';

import { AddPackageMaintainerComponent } from './add-package-maintainer.component';

describe('AddPackageMaintainerComponent', () => {
  let component: AddPackageMaintainerComponent;
  let fixture: ComponentFixture<AddPackageMaintainerComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ AddPackageMaintainerComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AddPackageMaintainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DeletePackageMaintainerComponent } from './delete-package-maintainer.component';

describe('DeletePackageMaintainerComponent', () => {
  let component: DeletePackageMaintainerComponent;
  let fixture: ComponentFixture<DeletePackageMaintainerComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ DeletePackageMaintainerComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(DeletePackageMaintainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

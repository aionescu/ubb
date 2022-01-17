import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DeletePackageVersionComponent } from './delete-package-version.component';

describe('DeletePackageVersionComponent', () => {
  let component: DeletePackageVersionComponent;
  let fixture: ComponentFixture<DeletePackageVersionComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ DeletePackageVersionComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(DeletePackageVersionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

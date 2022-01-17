import { ComponentFixture, TestBed } from '@angular/core/testing';

import { AddMaintainerComponent } from './add-maintainer.component';

describe('AddMaintainerComponent', () => {
  let component: AddMaintainerComponent;
  let fixture: ComponentFixture<AddMaintainerComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ AddMaintainerComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AddMaintainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UpdateMaintainerComponent } from './update-maintainer.component';

describe('UpdateMaintainerComponent', () => {
  let component: UpdateMaintainerComponent;
  let fixture: ComponentFixture<UpdateMaintainerComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ UpdateMaintainerComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(UpdateMaintainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

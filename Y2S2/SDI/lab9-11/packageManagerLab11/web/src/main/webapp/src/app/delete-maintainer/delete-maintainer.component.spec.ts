import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DeleteMaintainerComponent } from './delete-maintainer.component';

describe('DeleteMaintainerComponent', () => {
  let component: DeleteMaintainerComponent;
  let fixture: ComponentFixture<DeleteMaintainerComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ DeleteMaintainerComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(DeleteMaintainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

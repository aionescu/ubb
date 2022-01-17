import { ComponentFixture, TestBed } from '@angular/core/testing';

import { MaintainersComponent } from './maintainers.component';

describe('MaintainersComponent', () => {
  let component: MaintainersComponent;
  let fixture: ComponentFixture<MaintainersComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ MaintainersComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(MaintainersComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

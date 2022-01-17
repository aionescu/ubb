import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UploadPaperComponent } from './upload-paper.component';

describe('UploadPaperComponent', () => {
  let component: UploadPaperComponent;
  let fixture: ComponentFixture<UploadPaperComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ UploadPaperComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(UploadPaperComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

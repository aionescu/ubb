import { Component, OnInit } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";

@Component({
  selector: 'app-upload-paper',
  templateUrl: './upload-paper.component.html',
  styleUrls: ['./upload-paper.component.css']
})
export class UploadPaperComponent implements OnInit {
  uploadForm: FormGroup;
  fileToUpload: File = null;

  constructor() { }

  ngOnInit(): void {
    this.uploadForm = new FormGroup({
      name: new FormControl('', [
        Validators.required,
        Validators.minLength(3),
        Validators.pattern('[a-zA-Z1-9 ]*')
      ]),
      subject: new FormControl('', [
        Validators.required,
        Validators.minLength(3),
        Validators.pattern('[a-zA-Z1-9!?@#$%^&* ]*')
      ])
    });
  }

  get name() { return this.uploadForm.get('name'); }
  get subject() { return this.uploadForm.get('subject'); }

  handleFileInput(event): void {
    this.fileToUpload = event.target.files[0];
    console.log(this.fileToUpload);
  }

  uploadPaper(name: string, subject: string): void {
    console.log(name, subject, this.fileToUpload);
  }
}

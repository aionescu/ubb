import { Component, OnInit } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";

@Component({
  selector: 'app-add-conference',
  templateUrl: './add-conference.component.html',
  styleUrls: ['./add-conference.component.css']
})
export class AddConferenceComponent implements OnInit {
  conferenceForm: FormGroup;
  range = new FormGroup({
    start: new FormControl(),
    end: new FormControl()
  });

  constructor() { }

  ngOnInit(): void {
    this.conferenceForm = new FormGroup({
      name: new FormControl('', [
        Validators.required,
        Validators.minLength(5),
        Validators.pattern('[a-zA-Z1-9 ]*')
      ])
    });
  }

  get name() { return this.conferenceForm.get('name'); }
  get start() { return this.conferenceForm.get('start'); }
  get end() { return this.conferenceForm.get('end'); }

  addConference(name: string, startDate: string, endDate: string): void {
    console.log(name, startDate, endDate);
  }
}

import { Component, OnInit } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {UserService} from "../user/user.service";

@Component({
  selector: 'app-register-component',
  templateUrl: './register-component.component.html',
  styleUrls: ['./register-component.component.css']
})
export class RegisterComponentComponent implements OnInit {
  registerForm: FormGroup;

  constructor(private service: UserService) { }

  ngOnInit(): void {
    this.registerForm = new FormGroup({
      email: new FormControl('', [
        Validators.required,
        Validators.pattern('[a-zA-Z1-9@\._]*')
      ]),
      password: new FormControl('', [
        Validators.required,
        Validators.minLength(8),
        Validators.pattern('[a-zA-Z1-9!?@#$%^&* ]*')
      ]),
      repassword: new FormControl('', [
        Validators.required,
        Validators.minLength(8),
        Validators.pattern('[a-zA-Z1-9!?@#$%^&* ]*')
      ]),
      role: new FormControl('', [
        Validators.required
      ])
    });
  }

  get email() { return this.registerForm.get('email'); }
  get pass_word() { return this.registerForm.get('password'); }
  get repassword() { return this.registerForm.get('repassword'); }
  get role() { return this.registerForm.get('role'); }

  register(email: string, password: string, repassword: string, role: string): void {
    console.log(email, password, repassword, role);
    this.service.register({email, password, role}).subscribe(console.log);
  }
}

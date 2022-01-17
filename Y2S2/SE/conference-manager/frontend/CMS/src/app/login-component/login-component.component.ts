import { Component, OnInit } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {UserService} from "../user/user.service";

@Component({
  selector: 'app-login-component',
  templateUrl: './login-component.component.html',
  styleUrls: ['./login-component.component.css']
})
export class LoginComponentComponent implements OnInit {
  loginForm: FormGroup

  constructor(private service: UserService) { }

  ngOnInit(): void {
    this.loginForm = new FormGroup({
      email: new FormControl('', [
        Validators.required,
      ]),
      password: new FormControl('', [
        Validators.required,
      ])
    });
  }

  get email() { return this.loginForm.get('email'); }
  get pass_word() { return this.loginForm.get('password'); }

  login(email: string, password: string): void {
    console.log(email, password);

    this.service.login({email, password, role: ""}).subscribe(console.log);
  }

  hello(): void {
    this.service.hello().subscribe(console.log);
  }
}

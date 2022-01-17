import { Injectable } from '@angular/core';
import {HttpClient} from "@angular/common/http";
import {User} from "./user.model";
import {Observable} from "rxjs";

@Injectable({
  providedIn: 'root'
})
export class UserService {

  constructor(private httpClient: HttpClient) { }

  login(user: User): Observable<any> {
    return this.httpClient.post('http://localhost:8080/user/login', user);
  }

  register(user: User): Observable<any> {
    return this.httpClient.post('http://localhost:8080/user/register', user);
  }

  hello(): Observable<any> {
    return this.httpClient.get('http://localhost:8080/user/hello');
  }
}

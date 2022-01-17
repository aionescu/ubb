import { HttpClient } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Observable } from "rxjs";
import { User } from "./user.model";

@Injectable()
export class UserService {
  private apiURL = `http://localhost:80/uri-collection/api`;

  constructor(private httpClient: HttpClient) { }

  private action(name: string): string {
    return `${this.apiURL}/${name}.php`
  }

  async login(user: User): Promise<boolean> {
    return this
      .httpClient
      .post<number>(this.action("login"), user)
      .toPromise()
      .then(id => {
        if (id == -1)
          return false
        else {
          localStorage.setItem("userID", id.toString())
          return true
        }
      })
  }
}

import { HttpClient } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Observable } from "rxjs";
import { URI } from "./uri.model";

@Injectable()
export class URIService {
  private apiURL = `http://localhost:80/uri-collection/api`;

  constructor(private httpClient: HttpClient) { }

  private action(name: string): string {
    return `${this.apiURL}/${name}.php`
  }

  private withOwner(uri: URI): URI {
    return { owner: this.userID(), ...uri }
  }

  private userID(): number {
    return parseInt(localStorage.getItem("userID"))
  }

  async login(username: string, password: string): Promise<boolean> {
    return this
      .httpClient
      .post<number>(this.action("login"), { username, password })
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

  uris(): Observable<URI[]> {
    return this.httpClient.get<URI[]>(`${this.action("uris")}/?owner=${this.userID()}`)
  }

  add(uri: URI): Observable<boolean> {
    return this.httpClient.post<boolean>(this.action("add"), this.withOwner(uri))
  }

  update(uri: URI): Observable<boolean> {
    return this.httpClient.post<boolean>(this.action("update"), this.withOwner(uri))
  }

  delete(uri: URI): Observable<boolean> {
    return this.httpClient.post<boolean>(this.action("delete"), this.withOwner(uri))
  }
}

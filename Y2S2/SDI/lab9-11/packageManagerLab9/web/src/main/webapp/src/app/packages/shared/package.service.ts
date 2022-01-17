import { Injectable } from '@angular/core';
import { HttpClient } from "@angular/common/http";
import { Observable } from "rxjs";
import { PackageModel } from './package.model';

@Injectable()
export class PackageService {
  private packagesUrl = 'http://localhost:8080/web-1.0-SNAPSHOT/api/packages';

  constructor(private httpClient: HttpClient) { }

  getPackages(): Observable<PackageModel[]> {
    return this.httpClient.get<Array<PackageModel>>(this.packagesUrl);
  }
}

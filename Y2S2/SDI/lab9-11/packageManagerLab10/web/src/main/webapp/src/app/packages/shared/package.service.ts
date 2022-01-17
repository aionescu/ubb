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

  addPackage(pkg: PackageModel): Observable<PackageModel> {
    return this.httpClient.post<PackageModel>(this.packagesUrl, pkg);
  }

  updatePackage(pkg: PackageModel): Observable<PackageModel> {
    return this.httpClient.put<PackageModel>(`${this.packagesUrl}/${pkg.id}`, pkg);
  }

  deletePackage(pkg: PackageModel): Observable<any> {
    return this.httpClient.delete(`${this.packagesUrl}/${pkg.id}`);
  }
}

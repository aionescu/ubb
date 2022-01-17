import { Injectable } from '@angular/core';
import { HttpClient } from "@angular/common/http";
import { Observable } from "rxjs";
import { PackageVersionModel } from './packageVersion.model';

@Injectable()
export class PackageVersionService {
  private packageMaintainersUrl = 'http://localhost:8080/web-1.0-SNAPSHOT/api/packageVersions';

  constructor(private httpClient: HttpClient) { }

  getPackageVersions(packageID: number): Observable<PackageVersionModel[]> {
    return this.httpClient.get<Array<PackageVersionModel>>(`${this.packageMaintainersUrl}/filter/packageID=${packageID}`);
  }

  addPackageVersion(entity: PackageVersionModel): Observable<PackageVersionModel> {
    return this.httpClient.post<PackageVersionModel>(this.packageMaintainersUrl, entity);
  }

  updatePackageVersion(entity: PackageVersionModel): Observable<PackageVersionModel> {
    return this.httpClient.post<PackageVersionModel>(`${this.packageMaintainersUrl}/${entity.packageID}`, entity);
  }

  deletePackageVersion(entity: PackageVersionModel): Observable<any> {
    return this.httpClient.delete(`${this.packageMaintainersUrl}/${entity.packageID}`);
  }
}

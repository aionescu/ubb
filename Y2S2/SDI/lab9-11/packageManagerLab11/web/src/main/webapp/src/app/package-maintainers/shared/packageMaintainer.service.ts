import { Injectable } from '@angular/core';
import { HttpClient } from "@angular/common/http";
import { Observable } from "rxjs";
import { PackageMaintainerModel } from './packageMaintainer.model';

@Injectable()
export class PackageMaintainerService {
  private packageMaintainersUrl = 'http://localhost:8080/web-1.0-SNAPSHOT/api/packageMaintainers';

  constructor(private httpClient: HttpClient) { }

  getPackageMaintainers(maintainerID: number): Observable<PackageMaintainerModel[]> {
    return this.httpClient.get<Array<PackageMaintainerModel>>(`${this.packageMaintainersUrl}/maintainerID=${maintainerID}`);
  }

  addPackageMaintainer(entity: PackageMaintainerModel): Observable<PackageMaintainerModel> {
    return this.httpClient.post<PackageMaintainerModel>(`${this.packageMaintainersUrl}/maintainerID=${entity.maintainerID}`, entity);
  }

  deletePackageMaintainer(entity: PackageMaintainerModel): Observable<any> {
    return this.httpClient.delete(`${this.packageMaintainersUrl}/maintainerID=${entity.maintainerID}&packageID=${entity.packageID}`);
  }
}
